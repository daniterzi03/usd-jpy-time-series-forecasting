suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forecast)
  library(tseries)
  library(urca)
  library(lmtest)
})

options(stringsAsFactors = FALSE)

parse_investing_numeric <- function(x) {
  as.numeric(gsub(",", ".", gsub("\\.", "", x)))
}

compute_accuracy <- function(actual, predicted) {
  tibble(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE)
  )
}

safe_adf_summary <- function(x, type = "drift", label = deparse(substitute(x))) {
  x <- na.omit(x)
  if (length(x) < 30) {
    cat("\nADF skipped for", label, "- not enough observations.\n")
    return(invisible(NULL))
  }

  cat("\nADF:", label, "\n")
  print(summary(ur.df(x, type = type, selectlags = "AIC")))
}

data_dir <- file.path(getwd(), "data")

usd_jpy_raw <- read.csv(file.path(data_dir, "usd_jpy_monthly.csv"))
usd_chf_raw <- read.csv(file.path(data_dir, "usd_chf_monthly.csv"))
cpi_raw <- read.csv(file.path(data_dir, "us_japan_cpi_monthly.csv"))

usd_jpy <- usd_jpy_raw %>%
  transmute(
    Data = as.Date(gsub("\\.", "-", Data), format = "%d-%m-%Y"),
    Ultimo_numeric = parse_investing_numeric(Ultimo)
  ) %>%
  arrange(Data)

usd_chf <- usd_chf_raw %>%
  transmute(
    Data = as.Date(gsub("\\.", "-", Data), format = "%d-%m-%Y"),
    USD_CHF = parse_investing_numeric(Ultimo)
  ) %>%
  arrange(Data)

cpi <- cpi_raw %>%
  mutate(Data = as.Date(Data)) %>%
  arrange(Data)

analysis_data <- usd_jpy %>%
  inner_join(usd_chf, by = "Data") %>%
  inner_join(cpi, by = "Data") %>%
  arrange(Data) %>%
  mutate(
    cpi_gap = USA_CPI - JPN_CPI,
    us_infl_yoy = 100 * (USA_CPI / lag(USA_CPI, 12) - 1),
    jp_infl_yoy = 100 * (JPN_CPI / lag(JPN_CPI, 12) - 1),
    infl_diff = us_infl_yoy - jp_infl_yoy,
    d_usdjpy = 100 * (log(Ultimo_numeric) - lag(log(Ultimo_numeric))),
    d_usd_chf = 100 * (log(USD_CHF) - lag(log(USD_CHF))),
    post_2007 = as.integer(Data >= as.Date("2007-06-01"))
  )

cat("Merged rows:", nrow(analysis_data), "\n")
cat("Date range:", as.character(min(analysis_data$Data)), "to", as.character(max(analysis_data$Data)), "\n")
cat("Duplicated dates:", sum(duplicated(analysis_data$Data)), "\n")
cat("NA shares:\n")
print(colMeans(is.na(analysis_data)))

summary_table <- analysis_data %>%
  summarise(
    usd_jpy_mean = mean(Ultimo_numeric, na.rm = TRUE),
    usd_chf_mean = mean(USD_CHF, na.rm = TRUE),
    cpi_gap_mean = mean(cpi_gap, na.rm = TRUE),
    infl_diff_mean = mean(infl_diff, na.rm = TRUE),
    usd_jpy_sd = sd(Ultimo_numeric, na.rm = TRUE),
    usd_chf_sd = sd(USD_CHF, na.rm = TRUE),
    cpi_gap_sd = sd(cpi_gap, na.rm = TRUE),
    infl_diff_sd = sd(infl_diff, na.rm = TRUE)
  )

cat("\nSummary table:\n")
print(summary_table)

plot_levels <- analysis_data %>%
  select(Data, Ultimo_numeric, USD_CHF, cpi_gap, infl_diff) %>%
  pivot_longer(-Data, names_to = "series", values_to = "value")

print(
  ggplot(filter(plot_levels, !is.na(value)), aes(x = Data, y = value, color = series)) +
    geom_line() +
    facet_wrap(~series, scales = "free_y", ncol = 2) +
    labs(title = "Original series", x = "Date", y = "Value") +
    theme_minimal()
)

plot_transformed <- analysis_data %>%
  select(Data, d_usdjpy, d_usd_chf, infl_diff) %>%
  pivot_longer(-Data, names_to = "series", values_to = "value")

print(
  ggplot(filter(plot_transformed, !is.na(value)), aes(x = Data, y = value, color = series)) +
    geom_line() +
    facet_wrap(~series, scales = "free_y", ncol = 1) +
    labs(title = "Monthly log returns and inflation differential", x = "Date", y = "Value") +
    theme_minimal()
)

cat("\nClassical unit root diagnostics:\n")
print(adf.test(na.omit(analysis_data$Ultimo_numeric)))
print(adf.test(na.omit(analysis_data$USD_CHF)))
print(adf.test(na.omit(analysis_data$cpi_gap)))
print(adf.test(na.omit(analysis_data$infl_diff)))

safe_adf_summary(analysis_data$Ultimo_numeric, type = "trend", label = "USD/JPY level")
safe_adf_summary(analysis_data$USD_CHF, type = "trend", label = "USD/CHF level")
safe_adf_summary(analysis_data$cpi_gap, type = "trend", label = "CPI gap")
safe_adf_summary(analysis_data$infl_diff, type = "drift", label = "Inflation differential YoY")
safe_adf_summary(analysis_data$d_usdjpy, type = "none", label = "USD/JPY monthly log return")
safe_adf_summary(analysis_data$d_usd_chf, type = "none", label = "USD/CHF monthly log return")

for (lag_i in 1:10) {
  analysis_data[[paste0("d_usdjpy_L", lag_i)]] <- dplyr::lag(analysis_data$d_usdjpy, lag_i)
}

for (lag_i in 1:6) {
  analysis_data[[paste0("infl_diff_L", lag_i)]] <- dplyr::lag(analysis_data$infl_diff, lag_i)
}

for (lag_i in 1:6) {
  analysis_data[[paste0("d_usd_chf_L", lag_i)]] <- dplyr::lag(analysis_data$d_usd_chf, lag_i)
}

model_data <- analysis_data %>%
  drop_na(d_usdjpy, d_usdjpy_L1, d_usdjpy_L2, d_usdjpy_L3, d_usdjpy_L10,
          infl_diff_L1, infl_diff_L2, d_usd_chf_L1, d_usd_chf_L2)

train <- model_data %>% filter(Data < as.Date("2020-06-01"))
test <- model_data %>% filter(Data >= as.Date("2020-06-01"))

cat("\nModel sample rows:", nrow(model_data), "\n")
cat("Train rows:", nrow(train), "\n")
cat("Test rows:", nrow(test), "\n")
cat("Actual test start:", as.character(min(test$Data)), "\n")

ar_orders <- 1:10
ar_fits <- lapply(ar_orders, function(p) arima(train$d_usdjpy, order = c(p, 0, 0)))
names(ar_fits) <- paste0("AR(", ar_orders, ")")

ar_bic <- tibble(
  model = names(ar_fits),
  p = ar_orders,
  BIC = sapply(ar_fits, BIC),
  AIC = sapply(ar_fits, AIC)
) %>% arrange(BIC, AIC)

cat("\nAR order selection:\n")
print(ar_bic)

ar1_fit <- ar_fits[[1]]
best_ar_fit <- ar_fits[[which.min(ar_bic$BIC)]]
best_ar_name <- ar_bic$model[1]

ar1_forecast <- forecast(ar1_fit, h = nrow(test))
best_ar_forecast <- forecast(best_ar_fit, h = nrow(test))

ar_accuracy <- compute_accuracy(test$d_usdjpy, as.numeric(ar1_forecast$mean)) %>%
  mutate(model = "AR(1)")

if (best_ar_name != "AR(1)") {
  ar_accuracy <- bind_rows(
    ar_accuracy,
    compute_accuracy(test$d_usdjpy, as.numeric(best_ar_forecast$mean)) %>% mutate(model = best_ar_name)
  )
}

ar_accuracy <- ar_accuracy %>% select(model, RMSE, MAE)

adl_formulas <- list(
  ADL_111 = d_usdjpy ~ d_usdjpy_L1 + infl_diff_L1 + d_usd_chf_L1 + post_2007,
  ADL_211 = d_usdjpy ~ d_usdjpy_L1 + d_usdjpy_L2 + infl_diff_L1 + d_usd_chf_L1 + post_2007,
  ADL_122 = d_usdjpy ~ d_usdjpy_L1 + infl_diff_L1 + infl_diff_L2 + d_usd_chf_L1 + d_usd_chf_L2 + post_2007,
  ADL_221 = d_usdjpy ~ d_usdjpy_L1 + d_usdjpy_L2 + infl_diff_L1 + infl_diff_L2 + d_usd_chf_L1 + post_2007
)

adl_fits <- lapply(adl_formulas, function(fml) lm(fml, data = train))
adl_bic <- tibble(
  model = names(adl_fits),
  BIC = sapply(adl_fits, BIC),
  AIC = sapply(adl_fits, AIC)
) %>% arrange(BIC, AIC)

best_adl_name <- adl_bic$model[1]
best_adl_train <- adl_fits[[best_adl_name]]
best_adl_formula <- formula(best_adl_train)

adl_test_pred <- predict(best_adl_train, newdata = test)
adl_accuracy <- compute_accuracy(test$d_usdjpy, adl_test_pred) %>%
  mutate(model = best_adl_name) %>%
  select(model, RMSE, MAE)

comparison_table <- bind_rows(ar_accuracy, adl_accuracy) %>% arrange(RMSE, MAE)
cat("\nModel comparison:\n")
print(comparison_table)

final_adl <- lm(best_adl_formula, data = model_data)
last_row <- model_data %>% filter(Data == max(Data)) %>% select(all.vars(best_adl_formula)[-1])

next_return_forecast <- as.numeric(predict(final_adl, newdata = last_row))
forecast_sigma <- sigma(final_adl)
forecast_lower <- next_return_forecast - 1.96 * forecast_sigma
forecast_upper <- next_return_forecast + 1.96 * forecast_sigma

last_level <- model_data$Ultimo_numeric[which.max(model_data$Data)]
next_level_forecast <- last_level * exp(next_return_forecast / 100)

cat("\nNext-month USD/JPY return forecast:", round(next_return_forecast, 3), "\n")
cat("Approx. 95% interval:", round(forecast_lower, 3), "to", round(forecast_upper, 3), "\n")
cat("Implied next USD/JPY level:", round(next_level_forecast, 3), "\n")
