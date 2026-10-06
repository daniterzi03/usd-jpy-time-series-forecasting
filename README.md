# USD/JPY Time-Series Forecasting

## Overview
This project studies monthly USD/JPY dynamics and evaluates whether macro-financial information improves short-horizon forecasting relative to a simple autoregressive benchmark.

The analysis combines USD/JPY, USD/CHF and the US-Japan inflation differential, using autoregressive and ADL models, stationarity diagnostics, a structural-break control and out-of-sample forecast evaluation.

## Research Question
Can lagged exchange-rate dynamics and macro-financial variables improve forecasts of monthly USD/JPY returns relative to a simple autoregressive benchmark?

## Methodology
- Monthly FX and CPI data preparation and merging
- Log-return transformations
- Augmented Dickey-Fuller stationarity tests
- Structural-break control
- AR benchmark models
- ADL specifications with macro-financial predictors
- Train/test split and out-of-sample evaluation using RMSE and MAE

## Key Takeaway
The richer ADL specification produced only a marginal improvement over the simple AR benchmark, highlighting the difficulty of forecasting exchange rates and the importance of out-of-sample model comparison.

## Repository Structure
- `usd_jpy_forecasting.R` — complete R analysis
- `data/` — monthly USD/JPY, USD/CHF and CPI series

## Skills Demonstrated
Time series · Forecasting · AR/ADL models · Structural breaks · Model evaluation · R

## Author
Daniele Terzi — MSc Analytics and Data Science for Economics and Management, University of Brescia
