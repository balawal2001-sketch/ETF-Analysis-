ETF Technical Analysis & Machine Learning Trading System

Overview

This comprehensive R-based trading system performs automated technical analysis on multiple ETFs (AGG, SPY, QQQ) and implements machine learning models to generate trading signals. The system integrates traditional technical indicators with advanced ML algorithms to provide actionable investment recommendations.

Features

1. Technical Analysis Module

Trend Indicators: SMA, EMA, MACD
Momentum Indicators: RSI, Momentum, Rate of Change (ROC)
Volatility Indicators: ATR, Bollinger Bands
Volume Indicators: OBV, VWAP
Automated signal generation (STRONG BUY/BUY/HOLD/SELL)

2. Machine Learning Models

Random Forest - Tree-based ensemble with hyperparameter tuning
Support Vector Machine (SVM) - Non-linear classification with radial kernel
XGBoost - Gradient boosting with early stopping
Logistic Regression - Baseline linear classifier

3. Feature Engineering

Technical indicators transformed into ML features:

Moving averages (SMA_20, EMA_20)
MACD and signal line
RSI (14-day)
Momentum and ROC
Bollinger Bands (upper, middle, lower)
ATR, OBV, VWAP
External features: VIX (Volatility Index), TNX (10-Year Treasury Yield)

4. Backtesting Framework

Simulates trading with configurable initial capital ($10,000 default)
Transaction cost modeling (0.1% commission)

Performance metrics calculation:

Total/Annualized returns
Sharpe ratio
Maximum drawdown
Win rate
Average holding period
Comparison against buy-and-hold benchmark

5. Model Interpretability

SHAP (SHapley Additive exPlanations) value analysis
Feature importance ranking
Dependence plots for key predictors

6. Visualization Suite

Performance comparison bar charts
Radar charts for multi-dimensional analysis
Portfolio growth trajectories
SHAP summary beeswarm plots
Feature importance graphs

Data Sources:

Yahoo Finance - ETF price data (AGG, SPY, QQQ)
VIX Index - Market volatility (^VIX)
TNX Index - 10-Year Treasury Yield (^TNX)
Date Range: January 1, 2020 to present

Required Libraries :

library(quantmod)      # Financial data download
library(dplyr)         # Data manipulation
library(TTR)           # Technical indicators
library(tidyr)         # Data reshaping
library(ggplot2)       # Visualization
library(randomForest)  # Random Forest model
library(caret)         # Preprocessing & evaluation
library(e1071)         # SVM model
library(xgboost)       # XGBoost model
library(SHAPforxgboost)# SHAP values
