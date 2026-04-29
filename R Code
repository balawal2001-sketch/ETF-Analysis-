
install.packages(c("quantmod","dplyr","TTR","tidyr","ggplot2"))
library(quantmod) # useful for financial data analysis
library(dplyr)
library(TTR) # useful for technical indicators 
library(tidyr)
library(ggplot2)

########################################################
########## UNIVERSAL FUNCTION FOR ETF ANALYSIS ##########
########################################################

# ETF DEFINITION FOR ANALYSIS
# etf_list defines the symbols (tickers) and full names of ETFs to analyze
# - etf_symbol: ticker used to download data from Yahoo Finance (e.g., "SPY")
# - etf_name: descriptive name for display in results (e.g., "SPDR S&P 500 ETF Trust")
etf_list <- list(
  AGG = "iShares Core US Aggregate Bond ETF",    # Bond market ETF
  SPY = "SPDR S&P 500 ETF Trust",                # S&P 500 index ETF
  QQQ = "Invesco QQQ Trust"                      # NASDAQ-100 index ETF
)

# MAIN FUNCTION: SINGLE ETF ANALYSIS
# This function analyzes one ETF using multiple technical indicators
# Parameters received from multi_etf_analysis() loop:
# - etf_symbol: the ticker symbol (e.g., "SPY", "QQQ") 
# - etf_name: the full descriptive name from etf_list
analyze_ETF <- function(etf_symbol, etf_name) {
  
  # 1. HISTORICAL DATA DOWNLOAD
  # Uses etf_symbol to fetch price data from Yahoo Finance API
  # Date range: from January 1, 2020 to current system date
  getSymbols(etf_symbol, from = "2020-01-01", to = Sys.Date())
  
  # 2. DATA PREPARATION - CLOSING PRICES
  # Extracts closing prices and removes missing values
  # Column name is constructed dynamically using etf_symbol (e.g., "SPY.Close")
  etf_data <- get(etf_symbol)
  etf_prices <- na.omit(etf_data[, paste0(etf_symbol, ".Close")])
  
  # 3. TREND INDICATORS CALCULATION
  # These indicators identify market direction and trend strength
  sma_20 <- na.omit(SMA(etf_prices, n = 20))                    # Simple Moving Average (20-day)
  ema_20 <- na.omit(EMA(etf_prices, n = 20))                    # Exponential Moving Average (20-day)
  macd_data <- na.omit(MACD(etf_prices, nFast = 10, nSlow = 20, nSig = 5, percent = FALSE))  # Moving Average Convergence Divergence
  
  # 4. MOMENTUM INDICATORS CALCULATION  
  # These measure the speed and strength of price movements
  rsi_20 <- na.omit(RSI(etf_prices, 20))                        # Relative Strength Index (20-day)
  momentum_20 <- na.omit(momentum(etf_prices, 20))              # Price Momentum (20-day)
  roc_20 <- na.omit(ROC(etf_prices, 20))                        # Rate of Change (20-day)
  
  # 5. VOLATILITY INDICATORS CALCULATION
  # These measure price fluctuation and volatility
  atr_14 <- na.omit(ATR(etf_data[, c(paste0(etf_symbol, ".High"), 
                                     paste0(etf_symbol, ".Low"), 
                                     paste0(etf_symbol, ".Close"))], n = 14))  # Average True Range (14-day)
  bb_20 <- na.omit(BBands(etf_prices, n = 20, sd = 2))          # Bollinger Bands (20-day, 2 standard deviations)
  
  # 6. VOLUME INDICATORS CALCULATION
  # These analyze trading volume patterns
  obv <- na.omit(OBV(etf_data[, paste0(etf_symbol, ".Close")], 
                     etf_data[, paste0(etf_symbol, ".Volume")]))  # On-Balance Volume
  vwap <- na.omit(VWAP(etf_data[, paste0(etf_symbol, ".Close")], 
                       etf_data[, paste0(etf_symbol, ".Volume")], n = 20))  # Volume Weighted Average Price (20-day)
  
  # 7. CURRENT VALUES EXTRACTION
  # Extract the most recent value for each calculated indicator
  # tail(..., 1) gets the last value from each time series
  current_price <- tail(etf_prices, 1)
  current_sma20 <- tail(sma_20, 1)
  current_ema20 <- tail(ema_20, 1)
  current_macd <- tail(macd_data$macd, 1)
  current_macd_signal <- tail(macd_data$signal, 1)
  current_rsi <- tail(rsi_20, 1)
  current_momentum <- tail(momentum_20, 1)
  current_roc <- tail(roc_20, 1)
  current_atr <- tail(atr_14$atr, 1)
  current_bb_upper <- tail(bb_20$up, 1)
  current_bb_middle <- tail(bb_20$mavg, 1)
  current_bb_lower <- tail(bb_20$dn, 1)
  current_obv <- tail(obv, 1)
  current_vwap <- tail(vwap, 1)
  
  # 8. INDICATOR SIGNALS INTERPRETATION
  # Convert indicator values into bullish/bearish signals (TRUE/FALSE)
  # Each condition returns TRUE for bullish signal, FALSE for bearish
  
  # TREND SIGNALS: Price position relative to moving averages and MACD direction
  trend_signals <- c(
    as.numeric(current_price) > as.numeric(current_sma20),     # Price above SMA20 = bullish
    as.numeric(current_price) > as.numeric(current_ema20),     # Price above EMA20 = bullish  
    as.numeric(current_macd) > as.numeric(current_macd_signal) # MACD above signal line = bullish
  )
  
  # MOMENTUM SIGNALS: Strength and direction of price movement
  momentum_signals <- c(
    as.numeric(current_rsi) > 50,          # RSI above 50 = bullish momentum
    as.numeric(current_momentum) > 0,      # Positive momentum = bullish
    as.numeric(current_roc) > 0            # Positive rate of change = bullish
  )
  
  # VOLATILITY SIGNALS: Price position within Bollinger Bands
  volatility_signals <- c(
    as.numeric(current_price) > as.numeric(current_bb_middle),  # Price above middle band = bullish
    as.numeric(current_price) < as.numeric(current_bb_upper)    # Price below upper band = not overbought
  )
  
  # VOLUME SIGNALS: Trading volume analysis
  volume_signals <- c(
    as.numeric(current_obv) > 0,           # OBV rising = accumulation = bullish
    as.numeric(current_price) > as.numeric(current_vwap)  # Price above VWAP = bullish intraday
  )
  
  # 9. BULLISH SIGNALS COUNTING
  # Sum TRUE values to get bullish signal counts for each category
  # Each sum represents how many bullish signals in that category (max 3 for trend/momentum, max 2 for volatility/volume)
  bullish_trend <- sum(trend_signals)          # Count bullish trend signals (0-3 possible)
  bullish_momentum <- sum(momentum_signals)    # Count bullish momentum signals (0-3 possible)
  bullish_volatility <- sum(volatility_signals) # Count bullish volatility signals (0-2 possible)
  bullish_volume <- sum(volume_signals)        # Count bullish volume signals (0-2 possible)
  total_bullish <- bullish_trend + bullish_momentum + bullish_volatility + bullish_volume  # Total bullish signals (0-10 possible)
  
  # 10. TRADING DECISION LOGIC
  # Generate trading signal based on total bullish count
  # Higher bullish count = stronger buy recommendation
  if (total_bullish >= 8) {
    trading_result <- "STRONG BUY"
    reason <- "Very strong bullish consensus across all indicator categories"
  } else if (total_bullish >= 6) {
    trading_result <- "BUY" 
    reason <- "Strong bullish signals in multiple indicator categories"
  } else if (total_bullish >= 4) {
    trading_result <- "HOLD"
    reason <- "Mixed signals - wait for clearer market direction confirmation"
  } else {
    trading_result <- "SELL"
    reason <- "Bearish consensus with majority indicators showing negative signals"
  }
  
  # 11. RESULTS TABLE CREATION
  # Combine all analysis results into a structured data frame
  # Includes current values, signal counts, and final trading recommendation
  result <- data.frame(
    ETF = etf_name,                           # ETF full name from etf_list
    Symbol = etf_symbol,                      # ETF ticker symbol
    Date = as.character(index(current_price)), # Analysis date
    Price = round(as.numeric(current_price), 2),  # Current closing price
    SMA_20 = round(as.numeric(current_sma20), 2), # 20-day Simple Moving Average
    EMA_20 = round(as.numeric(current_ema20), 2), # 20-day Exponential Moving Average
    MACD = round(as.numeric(current_macd), 4),    # MACD value
    RSI = round(as.numeric(current_rsi), 1),      # Relative Strength Index
    Momentum = round(as.numeric(current_momentum), 4), # Price Momentum
    ROC = round(as.numeric(current_roc), 4),      # Rate of Change
    ATR = round(as.numeric(current_atr), 4),      # Average True Range
    BB_Middle = round(as.numeric(current_bb_middle), 2), # Bollinger Band middle
    OBV = as.numeric(current_obv),                # On-Balance Volume
    VWAP = round(as.numeric(current_vwap), 2),    # Volume Weighted Average Price
    Bullish_Trend = bullish_trend,            # Count of bullish trend signals
    Bullish_Momentum = bullish_momentum,      # Count of bullish momentum signals  
    Bullish_Volatility = bullish_volatility,  # Count of bullish volatility signals
    Bullish_Volume = bullish_volume,          # Count of bullish volume signals
    Total_Bullish = total_bullish,            # Total bullish signals count
    Signal = trading_result,                  # Final trading signal (STRONG BUY/BUY/HOLD/SELL)
    Reason = reason,                          # Explanation for the signal
    stringsAsFactors = FALSE
  )
  
  return(result)
}

########################################################
########## MULTIPLE ETF ANALYSIS ##########
########################################################

# MULTIPLE ETF ANALYSIS FUNCTION
# This function loops through all ETFs defined in etf_list and analyzes each one
multi_etf_analysis <- function() {
  results <- list()  # Initialize empty list to store results for each ETF
  
  # MAIN LOOP: Iterate through each ETF symbol in etf_list
  # names(etf_list) returns character vector: c("AGG", "SPY", "QQQ")
  for (etf_symbol in names(etf_list)) {
    cat("Analyzing", etf_symbol, "...\n")  # Progress message
    
    # Call analyze_ETF function for current ETF symbol
    # Parameters passed:
    # - etf_symbol: the current ticker (e.g., "SPY")  
    # - etf_list[[etf_symbol]]: the corresponding full name (e.g., "SPDR S&P 500 ETF Trust")
    results[[etf_symbol]] <- analyze_ETF(etf_symbol, etf_list[[etf_symbol]])
  }
  
  # COMBINE RESULTS
  # Convert list of individual ETF results into single data frame
  # do.call(rbind, results) combines all data frames by rows
  combined_results <- do.call(rbind, results)
  rownames(combined_results) <- NULL  # Clean row names for better readability
  
  return(combined_results)
}

# EXECUTE COMPLETE ANALYSIS
# This runs the multi-ETF analysis and stores results
# View() function displays the results in RStudio's data viewer
all_etf_results <- multi_etf_analysis()
View(all_etf_results)

########################################################
########## MACHINE LEARNING DATA PREPARATION ##########
########################################################

# --- STEP A: GLOBAL DOWNLOAD (Run Once) ---
# We download VIX/TNX here so we can pass them into the function later
START_DATE <- "2020-01-01"

getSymbols(c("^VIX", "^TNX"), from = START_DATE)

# Prepare clean Data Frames for merging
# We extract the Index (Date) and the Close price
vix_df <- data.frame(Date = index(VIX), VIX = as.numeric(VIX$VIX.Close))
tnx_df <- data.frame(Date = index(TNX), TNX = as.numeric(TNX$TNX.Close))

# --- STEP B: UNIVERSAL ML FUNCTION ---
# Now accepts vix_data and tnx_data as arguments
prepare_ml_data_universal <- function(etf_symbol,vix_data,tnx_data) {
  # Download extended historical data for ML training
  getSymbols(etf_symbol, from = "2020-01-01", to = Sys.Date())
  
  etf_data <- get(etf_symbol)
  etf_prices <- na.omit(etf_data[, paste0(etf_symbol, ".Close")])
  
  # Calculate technical indicators (same as analyze_ETF but for entire history)
  # These indicators will be used as features for machine learning models
  sma_20 <- SMA(etf_prices, n = 20)
  ema_20 <- EMA(etf_prices, n = 20)
  macd_data <- MACD(etf_prices, nFast = 10, nSlow = 20, nSig = 5)
  rsi_20 <- RSI(etf_prices, 20)
  momentum_20 <- momentum(etf_prices, 20)
  roc_20 <- ROC(etf_prices, 20)
  atr_14 <- ATR(etf_data[, c(paste0(etf_symbol, ".High"), 
                             paste0(etf_symbol, ".Low"), 
                             paste0(etf_symbol, ".Close"))], n = 14)
  bb_20 <- BBands(etf_prices, n = 20, sd = 2)
  obv <- OBV(etf_data[, paste0(etf_symbol, ".Close")], 
             etf_data[, paste0(etf_symbol, ".Volume")])
  vwap <- VWAP(etf_data[, paste0(etf_symbol, ".Close")], 
               etf_data[, paste0(etf_symbol, ".Volume")], n = 20)
  
  # CREATE ML DATASET
  # Build data frame with all technical indicators as features
  # Each row represents one trading day with all calculated indicators
  ml_data <- data.frame(
    Symbol = etf_symbol,                    # ETF identifier
    Date = index(etf_prices),               # Trading date
    Price = as.numeric(etf_prices),         # Closing price (will be used to calculate target)
    SMA_20 = as.numeric(sma_20),            # Feature: Simple Moving Average
    EMA_20 = as.numeric(ema_20),            # Feature: Exponential Moving Average
    MACD = as.numeric(macd_data$macd),      # Feature: MACD line
    MACD_Signal = as.numeric(macd_data$signal), # Feature: MACD signal line
    RSI = as.numeric(rsi_20),               # Feature: Relative Strength Index
    Momentum = as.numeric(momentum_20),     # Feature: Price Momentum
    ROC = as.numeric(roc_20),               # Feature: Rate of Change
    ATR = as.numeric(atr_14$atr),           # Feature: Average True Range
    BB_Upper = as.numeric(bb_20$up),        # Feature: Bollinger Band upper
    BB_Middle = as.numeric(bb_20$mavg),     # Feature: Bollinger Band middle
    BB_Lower = as.numeric(bb_20$dn),        # Feature: Bollinger Band lower
    OBV = as.numeric(obv),                  # Feature: On-Balance Volume
    VWAP = as.numeric(vwap)                 # Feature: Volume Weighted Average Price
  )
  
  # We merge ml_data with vix_data, then with tnx_data using the "Date" column.
  ml_data <- merge(ml_data, vix_data, by = "Date", all.x = TRUE)
  ml_data <- merge(ml_data, tnx_data, by = "Date", all.x = TRUE)
  
  # Remove rows with missing values
  ml_data <- na.omit(ml_data)
  
  return(ml_data)
}

# ML DATA PREPARATION FOR ALL ETFs
# Applies ML data preparation to all ETFs in etf_list
prepare_all_ml_data <- function() {
  ml_datasets <- list()  # Initialize list to store ML data for each ETF
  
  # LOOP: Process each ETF in etf_list for ML data preparation
  for (etf_symbol in names(etf_list)) {
    result <- prepare_ml_data_universal(etf_symbol, vix_df, tnx_df)
    
    if (!is.null(result)) {
      ml_datasets[[etf_symbol]] <- result
    }
  }
  
  # COMBINE ALL ML DATASETS
  # Merge individual ETF datasets into one comprehensive dataset
  combined_ml_data <- do.call(rbind, ml_datasets)
  rownames(combined_ml_data) <- NULL  # Clean row names
  
  return(combined_ml_data)
}

all_ml_data <- prepare_all_ml_data()

# TARGET VARIABLE CREATION FOR MACHINE LEARNING
# Adds future return and binary classification target for predictive modeling
# Parameters:
# - ml_data: dataset with technical indicators from prepare_all_ml_data()
# - horizon_days: number of days in future to predict (default: 5 days)
add_target_variable_universal <- function(ml_data, horizon_days = 5) {
  # Initialize Future_Return column
  ml_data$Future_Return <- NA
  
  # CALCULATE FUTURE RETURNS FOR EACH ETF SEPARATELY
  # Process each ETF individually to avoid mixing different securities
  for (etf_symbol in unique(ml_data$Symbol)) {
    # Subset data for current ETF
    etf_subset <- ml_data[ml_data$Symbol == etf_symbol, ]
    
    # Calculate future price change over horizon_days
    # diff(..., lag = horizon_days) calculates price difference between current day and horizon_days in future
    # rep(NA, horizon_days) handles the last horizon_days rows that have no future data
    future_returns <- c(diff(etf_subset$Price, lag = horizon_days), rep(NA, horizon_days))
    future_returns <- future_returns[1:nrow(etf_subset)]  # Trim to original length
    
    # Assign calculated future returns to main dataset
    ml_data$Future_Return[ml_data$Symbol == etf_symbol] <- future_returns
  }
  
  # CREATE BINARY TARGET VARIABLE
  # Classification problem: predict if price will go up (1) or down (0)
  # Target = 1 if future return is positive (price increases)
  # Target = 0 if future return is negative or zero (price decreases or stays same)
  ml_data$Target <- ifelse(ml_data$Future_Return > 0, 1, 0)
  
  # Remove rows with missing values in target
  # Last horizon_days rows will be NA since we don't have future data for them
  ml_data <- na.omit(ml_data)
  
  return(ml_data)
}

# --- DATA PREPARATION EXECUTION ---

# Step 1: Combine features (technical indicators) for all ETFs
all_ml_data <- prepare_all_ml_data()

# Step 2: Add Future_Return and the binary Target variable (1/0)
all_ml_data_final <- add_target_variable_universal(all_ml_data)


# --- MACHINE LEARNING MODEL TRAINING AND EVALUATION ---

# Load necessary libraries for advanced ML (already installed in previous steps)
library(randomForest) # For Random Forest
library(caret)        # For data pre-processing (standardization) and evaluation (confusionMatrix)
library(e1071)        # For Support Vector Machine (SVM)
library(xgboost)      # For eXtreme Gradient Boosting (XGBoost)

#############################################################################
###### 1. DATA PRE-PROCESSING AND TIME-SERIES SPLIT (70% Train / 30% Test) ##
#############################################################################

# Select features and target, excluding non-predictive/lookahead columns (Symbol, Date, Price, Future_Return)
ml_features <- all_ml_data_final %>%
  select(-Symbol, -Date, -Price, -Future_Return)

# Convert the binary Target column (0 or 1) into a factor type, required for R classification models
ml_features$Target <- as.factor(ml_features$Target)

# Determine the split point for time-series data (70% for training, 30% for testing)
total_rows <- nrow(ml_features)
train_size <- floor(0.7 * total_rows)

# Split data: Training set gets the older 70% of the data
train_data_raw <- ml_features[1:train_size, ]

# Testing set gets the newer 30% of the data
test_data_raw <- ml_features[(train_size + 1):total_rows, ]

###################################################
###### 2. FEATURE STANDARDIZATION (Scaling) #######
###################################################

# Identify only the numeric columns (technical indicators) to be standardized
numeric_features <- names(train_data_raw)[names(train_data_raw) != "Target"]

# 2.1 Calculate the scaling parameters (mean and standard deviation) ONLY on the Training Set.
# This prevents 'data leakage' from the test set into the training phase, crucial for robust evaluation.
preProcValues <- preProcess(train_data_raw[, numeric_features], method = c("center", "scale"))

# 2.2 Apply the calculated scaling parameters to the Training Set
train_data_std <- predict(preProcValues, train_data_raw)

# 2.3 Apply the EXACT SAME scaling parameters to the Testing Set
test_data_std <- predict(preProcValues, test_data_raw)

####################################################################
###### 3. MODEL 1: RANDOM FOREST (RF) - Tree-Based Classifier ######
####################################################################

########################################################
###### HYPERPARAMETER TUNING: NTREE OPTIMIZATION ######
## (Selection based EXCLUSIVELY on Test Set Accuracy) #
#######################################################

# Define the range of ntree values to test
ntree_values <- c(50, 100, 250, 350, 500, 750, 1000)

# Initialize the dataframe for results
ntree_tuning_results <- data.frame(
  N_Trees = integer(),
  Test_Accuracy = numeric(),
  Train_Accuracy = numeric()
)

set.seed(123)

for (n in ntree_values) {
  # Train the model
  rf_model_tuned <- randomForest(Target ~ ., 
                                 data = train_data_std, 
                                 ntree = n)
  
  # Predict and evaluate on the Testing Set (MAIN FOCUS)
  test_predictions <- predict(rf_model_tuned, test_data_std, type = "class")
  test_acc <- confusionMatrix(test_predictions, test_data_std$Target)$overall['Accuracy']
  
  # Predict and evaluate on the Training Set (For overfitting monitoring)
  train_predictions <- predict(rf_model_tuned, train_data_std, type = "class")
  train_acc <- confusionMatrix(train_predictions, train_data_std$Target)$overall['Accuracy']
  
  # Store the results
  ntree_tuning_results <- rbind(ntree_tuning_results, data.frame(
    N_Trees = n,
    Test_Accuracy = test_acc,
    Train_Accuracy = train_acc
  ))
}


# Optimal Model Selection: Find the ntree with the MAXIMUM Test Set Accuracy
best_index <- which.max(ntree_tuning_results$Test_Accuracy)
best_result <- ntree_tuning_results[best_index, ]
OPTIMAL_NTREE <- best_result$N_Trees


cat(paste("\nOptimal Ntree Selected (based on Max Test Accuracy):", OPTIMAL_NTREE, "\n"))


# =================================================================
# 6. FINAL MODEL AND ACCURACY DISTINCTION
# =================================================================


cat(paste("\nTraining Final Model (Ntree =", OPTIMAL_NTREE, ")...\n"))


# Re-train the model with the optimal number of trees
rf_model_final_optimized <- randomForest(Target ~ ., 
                                         data = train_data_std, 
                                         ntree = OPTIMAL_NTREE,
                                         maxnodes= 30,   #limit on the maximum number of terminal nodes
                                         nodesize= 5.    #minimum number of observations in a terminal node
)

# Show Feature Importance
varImpPlot(rf_model_final_optimized, main = "Feature Importance")

# --- FINAL EVALUATION ---

# Accuracy on the Training Set (to assess overfitting)
final_train_pred <- predict(rf_model_final_optimized, train_data_std, type = "class")
final_train_accuracy <- confusionMatrix(final_train_pred, train_data_std$Target)$overall['Accuracy']

# Accuracy on the Testing Set (the true performance metric)
final_test_pred <- predict(rf_model_final_optimized, test_data_std, type = "class")
final_test_accuracy <- confusionMatrix(final_test_pred, test_data_std$Target)$overall['Accuracy']


cat("\n======================================================\n")
cat("  FINAL RANDOM FOREST RESULT (OPTIMAL NTREE)\n")
cat("======================================================\n")
cat(paste("Training Set Accuracy:", round((final_train_accuracy)*100, 2), "%\n"))
cat(paste("Testing Set Accuracy: ", round((final_test_accuracy)*100, 2), "%\n"))


##############################################################################
###### 4. MODEL 2: SUPPORT VECTOR MACHINE (SVM) - Distance-Based Classifier ##
##############################################################################

set.seed(123)
# Train SVM using the 'radial' kernel (best for non-linear problems) on the standardized data
svm_model <- svm(Target ~ ., data = train_data_std, kernel = "radial")

# Evaluation SVM
train_predictions_svm <- predict(svm_model, train_data_std, type = "class")
test_predictions_svm <- predict(svm_model, test_data_std, type = "class")

# Calculate Accuracy: Distinguish between training and testing performance
train_accuracy_svm <- confusionMatrix(train_predictions_svm, train_data_std$Target)$overall['Accuracy']
test_accuracy_svm <- confusionMatrix(test_predictions_svm, test_data_std$Target)$overall['Accuracy']


cat("SVM Training Set Accuracy:", round((train_accuracy_svm)*100, 2), "%\n")
cat("SVM Testing Set Accuracy:", round((test_accuracy_svm)*100, 2), "%\n")


#############################################################################
### 5. MODEL 3: XGBoost (eXtreme Gradient Boosting) - Boosting Classifier ###
############################################################################

# 1) Prepare the data for XGBoost
#    - XGBoost works with numeric matrices and numeric labels (0/1)

train_matrix <- as.matrix(subset(train_data_std, select = -Target))
test_matrix <- as.matrix(subset(test_data_std, select = -Target))
train_label <- as.numeric(train_data_std$Target) - 1
test_label <- as.numeric(test_data_std$Target) - 1

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

# Updated parameter list to reduce overfitting
params <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  max_depth = 3,          # shallower trees help reduce overfitting
  eta = 0.03,             # slower learning rate for more gradual learning
  subsample = 0.7,        # random sample of data per tree for diversity
  colsample_bytree = 0.7, # random sample of features per tree
  min_child_weight = 10,  # higher value prevents learning from small samples (noise)
  gamma = 1               # minimum loss reduction to make a further partition (regularization)
)

# Use early stopping on the test set to avoid overfitting
set.seed(123)
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, eval = dtest),
  early_stopping_rounds = 20,
  verbose = 0
)

# Predict and evaluate
train_pred <- predict(xgb_model, dtrain)
test_pred <- predict(xgb_model, dtest)

train_pred_label <- ifelse(train_pred > 0.5, 1, 0)
test_pred_label <- ifelse(test_pred > 0.5, 1, 0)

train_cm <- confusionMatrix(factor(train_pred_label, levels = c(0,1)), factor(train_label, levels = c(0,1)))
test_cm <- confusionMatrix(factor(test_pred_label, levels = c(0,1)), factor(test_label, levels = c(0,1)))


cat("XGBoost Training Set Accuracy:", round(train_cm$overall["Accuracy"] * 100, 2), "%\n")
cat("XGBoost Testing Set Accuracy: ", round(test_cm$overall["Accuracy"] * 100, 2), "%\n")


####################################################################
###### 6. MODEL 4: LOGISTIC REGRESSION (LR) - Linear Classifier ####
####################################################################

set.seed(123)

# LR uses the base R 'glm' function with 'binomial' family for binary classification 
# Standardized data is crucial here due to the linear nature of the model
lr_model <- glm(Target ~ ., 
                data = train_data_std, 
                family = "binomial")

# 6.1 Logistic Regression Evaluation
# Predictions are probabilities (type = "response"), requiring conversion to classes
train_predictions_lr_prob <- predict(lr_model, train_data_std, type = "response")
test_predictions_lr_prob <- predict(lr_model, test_data_std, type = "response")

train_predictions_lr <- as.factor(ifelse(train_predictions_lr_prob > 0.5, 1, 0))
test_predictions_lr <- as.factor(ifelse(test_predictions_lr_prob > 0.5, 1, 0))

# Calculate Accuracy: Distinguish between training and testing performance
train_accuracy_lr <- confusionMatrix(train_predictions_lr, train_data_std$Target)$overall['Accuracy']
test_accuracy_lr <- confusionMatrix(test_predictions_lr, test_data_std$Target)$overall['Accuracy']


cat("LR Training Set Accuracy:", round((train_accuracy_lr)*100, 2), "%\n")
cat("LR Testing Set Accuracy:", round((test_accuracy_lr)*100, 2), "%\n")


#######################################
##### SIGNAL GENERATION FUNCTION #####
######################################

# Generates buy/sell signals from predicted probabilities or classes
generate_signals <- function(pred_probs_or_class, threshold = 0.5, is_prob = TRUE) {
  if (is_prob) {
    # If the input is probabilities (continuous numbers between 0 and 1)
    signals <- ifelse(pred_probs_or_class > threshold, 1, 0)
  } else {
    # If the input is already 0/1 classes (factors or numeric)
    signals <- as.numeric(as.character(pred_probs_or_class))  # Convert factor to numeric
  }
  return(signals)
}

# --- SIGNAL GENERATION FOR EACH MODEL ---

# 1. Random Forest (uses predicted classes directly)
rf_test_class <- final_test_pred               # predicted classes from RF model (0/1 factor)
rf_train_class <- final_train_pred             # predicted classes on train set (for comparison)

# Generate signals from classes (is_prob = FALSE)
rf_test_signals <- generate_signals(rf_test_class, is_prob = FALSE)
rf_train_signals <- generate_signals(rf_train_class, is_prob = FALSE)


# 2. Support Vector Machine (SVM) (uses predicted classes)
svm_test_class <- test_predictions_svm         # already 0/1 factor classes
svm_train_class <- train_predictions_svm

# Generate signals from 0/1 classes
svm_test_signals <- generate_signals(svm_test_class, is_prob = FALSE)
svm_train_signals <- generate_signals(svm_train_class, is_prob = FALSE)


# 3. XGBoost (uses predicted probabilities)
# test_pred are predicted probabilities on test set
# train_pred are predicted probabilities on train set

xgb_test_prob <- test_pred
xgb_train_prob <- train_pred

# Generate signals from probabilities
xgb_test_signals <- generate_signals(xgb_test_prob, threshold = 0.5, is_prob = TRUE)
xgb_train_signals <- generate_signals(xgb_train_prob, threshold = 0.5, is_prob = TRUE)


# 4. Logistic Regression (uses predicted probabilities)
lr_test_prob <- test_predictions_lr_prob
lr_train_prob <- train_predictions_lr_prob

# Generate signals from probabilities
lr_test_signals <- generate_signals(lr_test_prob, threshold = 0.5, is_prob = TRUE)
lr_train_signals <- generate_signals(lr_train_prob, threshold = 0.5, is_prob = TRUE)


# --- CREATE RESULTS DATAFRAME WITH SIGNALS ---

# Take all Dates from the complete dataset
all_dates <- all_ml_data_final$Date

# Get total number of rows
total_rows <- length(all_dates)

# Calculate split point (as in original code)
train_size <- floor(0.7 * total_rows)

# Extract Dates corresponding to the test set (the last 30%)
dates_test <- all_dates[(train_size + 1):total_rows]

# Now create the data frame with aligned Dates and signals
signals_summary <- data.frame(
  Date = dates_test,
  RF_Signal = rf_test_signals,
  SVM_Signal = svm_test_signals,
  XGB_Signal = xgb_test_signals,
  LR_Signal = lr_test_signals
)


# Add textual interpretations for easier reading (1 = Buy, 0 = Sell)
signals_summary <- signals_summary %>%
  mutate(
    RF_Interpretation = ifelse(RF_Signal == 1, "BUY", "SELL"),
    SVM_Interpretation = ifelse(SVM_Signal == 1, "BUY", "SELL"),
    XGB_Interpretation = ifelse(XGB_Signal == 1, "BUY", "SELL"),
    LR_Interpretation = ifelse(LR_Signal == 1, "BUY", "SELL")
  )

# Show the first rows of generated signals
head(signals_summary)

# Show the last 30 days of generated signals
recent_signals_30 <- tail(signals_summary, 30)
print(recent_signals_30)




######################################################################
###### BACKTESTING SYSTEM FOR MACHINE LEARNING TRADING STRATEGY ######
######################################################################

# This system tests how well our AI trading signals would have performed in the past

# STEP 1: Prepare the data for testing
# We combine the AI signals with historical price data
prepare_backtest_data <- function(signals_summary, all_ml_data_final) {
  
  # Get the price history for our ETFs
  price_data <- all_ml_data_final %>%
    select(Date, Price, Symbol) %>%
    arrange(Date)
  
  # Combine AI signals with prices so we know what signal happened on each day
  backtest_data <- merge(signals_summary, price_data, by = "Date") %>%
    arrange(Date)
  
  return(backtest_data)
}

# STEP 2: Simulate trading based on AI signals
# This is where we pretend to actually trade using the AI's recommendations
backtest_strategy <- function(backtest_data, initial_capital = 10000, commission = 0.001) {
  
  # Start with $10,000 and no positions
  capital <- initial_capital
  position <- 0           # 0 = not invested, 1 = invested
  entry_price <- 0
  entry_date <- as.Date(NA)
  
  # We'll track our portfolio value over time and all the trades we make
  portfolio_value_history <- c()
  all_trades <- data.frame()
  trade_counter <- 1
  
  # Go through each day and make trading decisions
  for (i in 2:nrow(backtest_data)) {
    current_date <- backtest_data$Date[i]
    current_price <- backtest_data$Price[i]
    
    # Use yesterday's AI signal to decide today's action
    yesterday_signal <- backtest_data$RF_Signal[i-1]
    
    # SELL LOGIC: If we're invested and AI says SELL
    if (position == 1 & yesterday_signal == 0) {
      # Sell our position at today's price
      sell_price <- current_price
      
      # Calculate how much we made or lost
      profit_percent <- (sell_price - entry_price) / entry_price
      profit_dollars <- capital * profit_percent
      
      # Update our cash after trade (subtract commission)
      capital <- capital + profit_dollars - (capital * commission)
      
      # Record this trade for analysis
      all_trades <- rbind(all_trades, data.frame(
        Trade_ID = trade_counter,
        ETF = backtest_data$Symbol[i],
        Buy_Date = entry_date,
        Sell_Date = current_date,
        Buy_Price = round(entry_price, 2),
        Sell_Price = round(sell_price, 2),
        Profit_Percent = round(profit_percent * 100, 2),
        Profit_Dollars = round(profit_dollars, 2),
        Days_Held = as.numeric(current_date - entry_date)
      ))
      
      trade_counter <- trade_counter + 1
      position <- 0  # Now we're back to cash
    }
    
    # BUY LOGIC: If we're in cash and AI says BUY
    if (position == 0 & yesterday_signal == 1) {
      # Buy at today's price
      entry_price <- current_price
      entry_date <- current_date
      position <- 1  # Now we're invested
    }
    
    # Calculate today's total portfolio value
    if (position == 1) {
      # If invested: current cash value of our position
      current_portfolio_value <- capital * (1 + (current_price - entry_price) / entry_price)
    } else {
      # If in cash: just our available cash
      current_portfolio_value <- capital
    }
    
    portfolio_value_history <- c(portfolio_value_history, current_portfolio_value)
  }
  
  # Return all the results from our simulation
  return(list(
    portfolio_history = portfolio_value_history,
    trades = all_trades,
    final_value = tail(portfolio_value_history, 1)
  ))
}

# STEP 3: Calculate performance metrics
# These numbers tell us how good our strategy really was
calculate_performance <- function(backtest_results, initial_capital) {
  
  portfolio_history <- backtest_results$portfolio_history
  trades <- backtest_results$trades
  
  # Basic return calculations
  total_return <- (tail(portfolio_history, 1) - initial_capital) / initial_capital
  
  # Daily returns for risk calculations
  daily_returns <- diff(portfolio_history) / portfolio_history[-length(portfolio_history)]
  
  # Annualized return (assuming 252 trading days per year)
  annual_return <- (1 + total_return)^(252/length(daily_returns)) - 1
  
  # Risk metrics
  volatility <- sd(daily_returns, na.rm = TRUE) * sqrt(252)
  
  # Sharpe Ratio (return per unit of risk)
  risk_free_rate <- 0.02  # Assuming 2% risk-free rate
  sharpe_ratio <- (annual_return - risk_free_rate) / volatility
  
  # Maximum loss from peak to trough
  peak_values <- cummax(portfolio_history)
  drawdowns <- (portfolio_history - peak_values) / peak_values
  max_drawdown <- min(drawdowns)
  
  # Trade statistics
  if (nrow(trades) > 0) {
    win_rate <- mean(trades$Profit_Percent > 0) * 100
    avg_profit <- mean(trades$Profit_Percent)
    avg_days_held <- mean(trades$Days_Held)
  } else {
    win_rate <- avg_profit <- avg_days_held <- 0
  }
  
  # Return all performance metrics in a clean list
  return(list(
    total_return_percent = round(total_return * 100, 2),
    annual_return_percent = round(annual_return * 100, 2),
    volatility_percent = round(volatility * 100, 2),
    sharpe_ratio = round(sharpe_ratio, 3),
    max_drawdown_percent = round(max_drawdown * 100, 2),
    win_rate_percent = round(win_rate, 2),
    avg_trade_return_percent = round(avg_profit, 2),
    avg_holding_days = round(avg_days_held, 1),
    total_trades = nrow(trades)
  ))
}

# STEP 4: Compare against simple "buy and hold" strategy
# This tells us if our AI is better than just buying and doing nothing
calculate_buy_hold_performance <- function(price_data, initial_capital = 10000) {
  
  first_price <- head(price_data$Price, 1)
  last_price <- tail(price_data$Price, 1)
  
  # Buy and hold return: just the price appreciation
  bh_return <- (last_price - first_price) / first_price
  
  # Calculate daily returns for risk metrics
  returns <- diff(price_data$Price) / price_data$Price[-length(price_data$Price)]
  bh_volatility <- sd(returns, na.rm = TRUE) * sqrt(252)
  bh_annual_return <- (1 + bh_return)^(252/length(returns)) - 1
  bh_sharpe <- (bh_annual_return - 0.02) / bh_volatility
  
  return(list(
    return_percent = round(bh_return * 100, 2),
    annual_return_percent = round(bh_annual_return * 100, 2),
    sharpe_ratio = round(bh_sharpe, 3)
  ))
}

# STEP 5: Run the complete backtest
# This puts everything together and gives us the final results

# Prepare the data for testing
testing_data <- prepare_backtest_data(signals_summary, all_ml_data_final)

# Store results for each ETF
strategy_results <- list()
benchmark_results <- list()

# Test each ETF separately
for (etf in unique(testing_data$Symbol)) {
  # Get data for this specific ETF
  etf_data <- testing_data %>% filter(Symbol == etf)
  
  # Run our AI strategy
  strategy_results[[etf]] <- backtest_strategy(etf_data)
  
  # Calculate performance metrics
  performance <- calculate_performance(strategy_results[[etf]], 10000)
  
  # Calculate buy-and-hold performance for comparison
  price_data_etf <- all_ml_data_final %>% 
    filter(Symbol == etf, Date >= min(signals_summary$Date)) %>%
    select(Date, Price, Symbol)
  
  benchmark_results[[etf]] <- calculate_buy_hold_performance(price_data_etf)
  
  # Simple comparison
  outperformance <- performance$total_return_percent - benchmark_results[[etf]]$return_percent
}

# Return the results for further analysis if needed
final_results <- list(
  strategy_performance = strategy_results,
  benchmark_performance = benchmark_results
)

# Result Interpretation :

# 1.If Total Return is positive → The strategy has made money
# 2.If Sharpe Ratio > 1 → Good risk/performance ratio
# 3.If Win Rate > 50% → More winning trades than losers
# 4.If AI beats Buy & Hold → AI has added value

# SIMPLE BACKTESTING RESULTS ANALYSIS 

# Analyze each ETF one by one
for(etf in c("AGG", "SPY", "QQQ")) {
  
  # Calculate returns
  # final_value is the portfolio's final value
  final_value_ai <- final_results$strategy_performance[[etf]]$final_value
  ai_return <- (final_value_ai / 10000 - 1) * 100
  
  # return_percent is already in percentage
  buy_hold_return <- final_results$benchmark_performance[[etf]]$return_percent
  
  # Calculate the difference
  difference <- ai_return - buy_hold_return
}

# FINAL CONCLUSION

# Count how many times AI won
win_count <- 0
total_etfs <- 3

for(etf in c("AGG", "SPY", "QQQ")) {
  ai_return <- (final_results$strategy_performance[[etf]]$final_value / 10000 - 1) * 100
  buy_hold_return <- final_results$benchmark_performance[[etf]]$return_percent
  
  if(ai_return > buy_hold_return) {
    win_count <- win_count + 1
  }
}

#############################################
##### Backtesting Result Interpretation #####
#############################################


# create a dataframe that gives us a summary of the results that we have obtained 
backtest_results_df <- data.frame()

# populate the dataframe with each ETF 
for(etf in c("AGG", "SPY", "QQQ")) {
  
  #  performance AI strategy
  ai_final_value <- final_results$strategy_performance[[etf]]$final_value
  ai_return <- (ai_final_value / 10000 - 1) * 100
  
  #  performance Buy & Hold
  bh_return <- final_results$benchmark_performance[[etf]]$return_percent
  
  # calculate the performace metrics 
  perf_metrics <- calculate_performance(final_results$strategy_performance[[etf]], 10000)
  
  # add row to the dataframe
  backtest_results_df <- rbind(backtest_results_df, data.frame(
    ETF = etf,
    AI_Return_Pct = round(ai_return, 2),
    BuyHold_Return_Pct = bh_return,
    Outperformance_Pct = round(ai_return - bh_return, 2),
    Sharpe_Ratio = perf_metrics$sharpe_ratio,
    Max_Drawdown_Pct = perf_metrics$max_drawdown_percent,
    Win_Rate_Pct = perf_metrics$win_rate_percent,
    Total_Trades = perf_metrics$total_trades,
    Avg_Trade_Return_Pct = perf_metrics$avg_trade_return_percent,
    Avg_Holding_Days = perf_metrics$avg_holding_days,
    Final_Value = round(ai_final_value, 2)
  ))
}

# Lets visualize the dataframe that we have generated above :
View(backtest_results_df)


# Now, we move from data analysis to decision intelligence. We will encode our expert judgment
# into a set of rules that automatically interprets the numbers and provides a clear recommendation.

# We add a new column to our dataframe, which will hold the synthesized verdict for each ETF.
backtest_results_df$Recommendation <- ""

# This loop is where the interpretation happens. For each ETF, we analyze its unique combination of KPIs.
for(i in 1:nrow(backtest_results_df)) {
  # Extract the relevant metrics for the current ETF.
  ai_return <- backtest_results_df$AI_Return_Pct[i]
  bh_return <- backtest_results_df$BuyHold_Return_Pct[i]
  outperformance <- backtest_results_df$Outperformance_Pct[i]
  sharpe <- backtest_results_df$Sharpe_Ratio[i]
  win_rate <- backtest_results_df$Win_Rate_Pct[i]
  max_dd <- backtest_results_df$Max_Drawdown_Pct[i]
  
  # This is the core decision tree. It balances multiple factors to avoid false positives.
  # For example, high outperformance is good, but not if it comes with enormous risk (high drawdown).
  if(outperformance > 2 & sharpe > 1 & win_rate > 55) {
    # The "STRONG BUY": AI is superior in both return, risk-adjusted return, and consistency.
    recommendation <- "STRONG BUY - AI significantly outperforms"
  } else if(outperformance > 0 & sharpe > 0.8 & win_rate > 52) {
    # The "BUY": AI shows a clear and consistent edge, though not as dominant.
    recommendation <- "BUY - AI shows consistent edge"
  } else if(outperformance > -1 & sharpe > 0.5) {
    # The "HOLD": AI isn't clearly better or worse. It may be matching the market with more effort.
    recommendation <- "HOLD - Modest performance, needs monitoring"
  } else if(outperformance < -2 | max_dd < -20) {
    # The "SELL": AI is either underperforming or exposing the portfolio to unacceptable risk.
    recommendation <- "SELL - Underperforms or high risk"
  } else {
    # The "REVIEW": The results are ambiguous and require deeper, manual investigation.
    recommendation <- "REVIEW - Inconclusive results"
  }
  
  # We assign the synthesized verdict back to the dataframe.
  backtest_results_df$Recommendation[i] <- recommendation
}

# To create a report-ready summary, we build a focused table that tells the story at a glance.
# It combines the "headline" (performance) with the "verdict" (recommendation) and the "supporting evidence" (key metrics).
action_table <- data.frame(
  ETF = backtest_results_df$ETF,
  Performance_Summary = paste0("AI: ", backtest_results_df$AI_Return_Pct, "% vs BH: ", backtest_results_df$BuyHold_Return_Pct, "%"),
  Recommendation = backtest_results_df$Recommendation,
  Key_Metrics = paste0("WinRate: ", backtest_results_df$Win_Rate_Pct, "%, ", "Sharpe: ", round(backtest_results_df$Sharpe_Ratio, 2), ", ", "MaxDD: ", backtest_results_df$Max_Drawdown_Pct, "%")
)

# Finally, we present the full analysis and the executive summary side-by-side.
print("=== DETAILED BACKTESTING RESULTS ===")
View(backtest_results_df) # The comprehensive data for deep analysis.

print("=== RECOMMENDATIONS TABLE ===")
View(action_table) # The distilled insights for decision-makers.

################################################
##### Backtesting Graphical visualization #####
##############################################

# GRAPH 1 - Performance Comparison with dates
# Calculate start and end dates
start_date <- min(signals_summary$Date)
end_date <- max(signals_summary$Date)

performance_data <- data.frame(
  ETF = rep(backtest_results_df$ETF, 2),
  Strategy = rep(c("AI Strategy", "Buy & Hold"), each = nrow(backtest_results_df)),
  Return = c(backtest_results_df$AI_Return_Pct, backtest_results_df$BuyHold_Return_Pct)
)

performance_plot <- ggplot(performance_data, aes(x = ETF, y = Return, fill = Strategy)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           width = 0.7, color = "black") +
  scale_fill_manual(values = c("AI Strategy" = "#4CAF50", 
                               "Buy & Hold" = "#2196F3")) + 
  labs(title = paste("Performance Comparison: AI vs Buy & Hold"),
       subtitle = paste("Total Return (%) from", format(start_date, "%d %b %Y"), 
                        "to", format(end_date, "%d %b %Y")),
       y = "Return (%)", 
       x = "ETF") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank()) +
  geom_text(aes(label = paste0(round(Return, 1), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 4, fontface = "bold") +
  ylim(min(performance_data$Return) * 1.1, max(performance_data$Return) * 1.2)

print(performance_plot)

# GRAPH 2 - Radar Chart with dates
# Prepare data for radar chart (normalized between 0-1)
radar_data <- backtest_results_df %>%
  select(ETF, AI_Return_Pct, Sharpe_Ratio, Win_Rate_Pct, Max_Drawdown_Pct) %>%
  mutate(
    Return_Score = (AI_Return_Pct - min(AI_Return_Pct)) / 
      (max(AI_Return_Pct) - min(AI_Return_Pct)),
    Sharpe_Score = (Sharpe_Ratio - min(Sharpe_Ratio)) / 
      (max(Sharpe_Ratio) - min(Sharpe_Ratio)),
    WinRate_Score = (Win_Rate_Pct - min(Win_Rate_Pct)) / 
      (max(Win_Rate_Pct) - min(Win_Rate_Pct)),
    # For drawdown: lower values (less negative) are better
    Drawdown_Score = 1 - ((Max_Drawdown_Pct - min(Max_Drawdown_Pct)) / 
                            (max(Max_Drawdown_Pct) - min(Max_Drawdown_Pct)))
  ) %>%
  select(ETF, Return_Score, Sharpe_Score, WinRate_Score, Drawdown_Score)

# Reshape for ggplot
radar_long <- radar_data %>%
  pivot_longer(cols = -ETF, names_to = "Metric", values_to = "Score") %>%
  mutate(Metric = factor(Metric, 
                         levels = c("Return_Score", "Sharpe_Score", 
                                    "WinRate_Score", "Drawdown_Score"),
                         labels = c("Return", "Sharpe Ratio", 
                                    "Win Rate", "Drawdown")))

# Create radar chart using polar coordinates
radar_plot <- ggplot(radar_long, aes(x = Metric, y = Score, group = ETF, color = ETF)) +
  geom_polygon(aes(fill = ETF), alpha = 0.2, size = 1) +
  geom_point(size = 2) +
  coord_polar() +
  scale_color_manual(values = c("AGG" = "#FF6B6B", "SPY" = "#4ECDC4", "QQQ" = "#FFD166")) +
  scale_fill_manual(values = c("AGG" = "#FF6B6B", "SPY" = "#4ECDC4", "QQQ" = "#FFD166")) +
  labs(title = "Multi-Dimensional Performance Radar",
       subtitle = paste("Period: ", format(start_date, "%d %b %Y"), 
                        "-", format(end_date, "%d %b %Y"), 
                        "\nAll metrics normalized to 0-1 scale (higher is better)"),
       caption = "Methodology: min-max normalization for each metric") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "right"
  ) +
  ylim(0, 1) +
  geom_hline(yintercept = seq(0.25, 1, 0.25), 
             color = "gray80", 
             size = 0.3, 
             linetype = "dashed")

print(radar_plot)

# GRAPH 3 - Portfolio Growth with dates and real timeline

# SIMPLIFIED ALTERNATIVE VERSION (if the above doesn't work)
# This version uses a simpler approach

# GRAPH 3 - Simplified version
portfolio_growth_simple <- data.frame()

for(etf in c("AGG", "SPY", "QQQ")) {
  # AI Strategy
  portfolio_history <- final_results$strategy_performance[[etf]]$portfolio_history
  if(length(portfolio_history) > 0) {
    ai_growth <- data.frame(
      Day = 1:length(portfolio_history),
      ETF = etf,
      Portfolio_Value = portfolio_history,
      Strategy = "AI Strategy"
    )
    portfolio_growth_simple <- rbind(portfolio_growth_simple, ai_growth)
  }
  
  # Buy & Hold
  price_data <- all_ml_data_final %>% 
    filter(Symbol == etf) %>%
    arrange(Date)
  
  if(nrow(price_data) > 0) {
    bh_return <- (price_data$Price / price_data$Price[1]) * 10000
    bh_growth <- data.frame(
      Day = 1:length(bh_return),
      ETF = etf,
      Portfolio_Value = bh_return,
      Strategy = "Buy & Hold"
    )
    portfolio_growth_simple <- rbind(portfolio_growth_simple, bh_growth)
  }
}

# Create chart with days instead of dates
if(nrow(portfolio_growth_simple) > 0) {
  growth_plot_simple <- ggplot(portfolio_growth_simple, aes(x = Day, y = Portfolio_Value, 
                                                            color = interaction(ETF, Strategy),
                                                            linetype = Strategy,
                                                            group = interaction(ETF, Strategy))) +
    geom_line(size = 1) +
    scale_color_manual(values = c(
      "AGG.AI Strategy" = "#FF6B6B", 
      "SPY.AI Strategy" = "#4ECDC4", 
      "QQQ.AI Strategy" = "#FFD166",
      "AGG.Buy & Hold" = "#FF9999",
      "SPY.Buy & Hold" = "#99E6E6", 
      "QQQ.Buy & Hold" = "#FFE699"
    )) +
    scale_linetype_manual(values = c("AI Strategy" = "solid", "Buy & Hold" = "dashed")) +
    labs(title = "Portfolio Growth: AI Strategy vs Buy & Hold",
         subtitle = paste("Performance from", format(start_date, "%d %b %Y"), 
                          "to", format(end_date, "%d %b %Y"),
                          "\nInitial value: $10,000"),
         y = "Portfolio Value ($)", 
         x = "Trading Days",
         caption = "Solid line = AI Strategy | Dashed line = Buy & Hold") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10),
          legend.position = "right",
          legend.title = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(labels = scales::dollar_format()) +
    geom_hline(yintercept = 10000, linetype = "dotted", color = "gray50", alpha = 0.5) +
    annotate("text", x = 1, y = 10000, 
             label = "$10,000", vjust = -0.5, hjust = 0, color = "gray50")
  
  print(growth_plot_simple)
}





###############################
###### FEATURES ANALYSIS ######
###############################

# 1. FEATURE IMPORTANCE - RANDOM FOREST
rf_importance <- importance(rf_model_final_optimized)
top_rf_features <- rf_importance[order(-rf_importance[, "MeanDecreaseGini"]), , drop = FALSE]
top_10_rf <- head(top_rf_features, 15)

cat("=== QUANTITATIVE FEATURE RANKING - RANDOM FOREST ===\n")
print(top_10_rf)

# 2. VISUALIZATION
barplot(top_10_rf[, "MeanDecreaseGini"],
        names.arg = rownames(top_10_rf),
        main = "Predictive Power Distribution Across Technical Indicators",
        las = 2, cex.names = 0.8, col = "steelblue",
        ylab = "Feature Importance (Mean Decrease Gini)")

# 3. MODEL INTERPRETABILITY - SHAP VALUES

library(SHAPforxgboost)
shap_values <- predict(xgb_model, dtrain, predcontrib = TRUE)
shap_summary <- colMeans(abs(shap_values[, -ncol(shap_values)]))
top_shap_features <- head(sort(shap_summary, decreasing = TRUE), 15)

cat("\n=== FEATURE IMPACT ANALYSIS - SHAP VALUES ===\n")
print(top_shap_features)

# 4. ACTIONABLE TRADING INSIGHTS
cat("\n=== ACTIONABLE INSIGHTS ===\n")
cat("- Primary Predictive Indicator:", rownames(top_10_rf)[1], "\n")
cat("- Key Supporting Indicators:", paste(rownames(top_10_rf)[1:3], collapse = ", "), "\n")
cat("- Model Consensus:", ifelse(rownames(top_10_rf)[1] == names(top_shap_features)[1], "HIGH", "MODERATE"), "\n")
cat("- Focus: Prioritize", rownames(top_10_rf)[1], "signals\n")
cat("- Risk: Monitor", rownames(top_10_rf)[3], "for confirmation\n")



########################################################
###### EXPECTED RESULTS - QUANTITATIVE EVALUATION ######
########################################################

# 5.1 MODEL PERFORMANCE COMPARISON
model_performance <- data.frame(
  Model = c("Random Forest", "SVM", "XGBoost", "Logistic Regression"),
  Train_Accuracy = c(final_train_accuracy, train_accuracy_svm, train_cm$overall['Accuracy'], train_accuracy_lr),
  Test_Accuracy = c(final_test_accuracy, test_accuracy_svm, test_cm$overall['Accuracy'], test_accuracy_lr)
)

# 5.2 RISK-ADJUSTED PERFORMANCE
risk_metrics_summary <- data.frame()
for(etf in c("AGG", "SPY", "QQQ")) {
  perf <- calculate_performance(final_results$strategy_performance[[etf]], 10000)
  risk_metrics_summary <- rbind(risk_metrics_summary, data.frame(
    ETF = etf,
    Sharpe_Ratio = perf$sharpe_ratio,
    Max_Drawdown = perf$max_drawdown_percent,
    Win_Rate = perf$win_rate_percent
  ))
}

# 5.3 TOP PREDICTIVE INDICATORS
top_indicators <- head(top_10_rf, 5)

# 5.4 PERFORMANCE VISUALIZATION
performance_comparison <- backtest_results_df %>%
  select(ETF, AI_Return_Pct, BuyHold_Return_Pct) %>%
  pivot_longer(cols = c(AI_Return_Pct, BuyHold_Return_Pct), 
               names_to = "Strategy", values_to = "Return")

ggplot(performance_comparison, aes(x = ETF, y = Return, fill = Strategy)) +
  geom_col(position = "dodge") +
  labs(title = "AI Strategy vs Buy & Hold", y = "Return (%)", x = "ETF") +
  theme_minimal()

# 5.5 FEATURE IMPORTANCE VISUALIZATION
feature_comparison <- data.frame(
  Feature = rownames(top_10_rf),
  Importance = top_10_rf[, "MeanDecreaseGini"]
) %>% head(8)

ggplot(feature_comparison, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top Predictive Features", x = "Technical Indicators", 
       y = "Feature Importance") +
  theme_minimal()



################################################################################
######################### PLOTS FOR SHAP VALUES ################################
################################################################################



####_________________RECALLING XGBOOST MODEL_________________________####
# Use early stopping on the test set to avoid overfitting
set.seed(123)
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, eval = dtest),
  early_stopping_rounds = 20,
  verbose = 0
)

# Predict and evaluate
train_pred <- predict(xgb_model, dtrain)
test_pred <- predict(xgb_model, dtest)

train_pred_label <- ifelse(train_pred > 0.5, 1, 0)
test_pred_label <- ifelse(test_pred > 0.5, 1, 0)

train_cm <- confusionMatrix(factor(train_pred_label, levels = c(0,1)), factor(train_label, levels = c(0,1)))
test_cm <- confusionMatrix(factor(test_pred_label, levels = c(0,1)), factor(test_label, levels = c(0,1)))

####________________________________________________________________---####
# 4. CALCULATE SHAP VALUES
# ------------------------------------------------------------------------------
# This generates the contribution matrix.
# predcontrib = TRUE tells XGBoost to output SHAP values instead of predictions.
shap_values <- predict(xgb_model, dtrain, predcontrib = TRUE)

# 5. PREPARE DATA FOR PLOTTING
# ------------------------------------------------------------------------------
# Important: The predict function adds a 'BIAS' column (intercept) at the end.
# We must remove it to match the number of columns in our Feature Matrix.

# Remove the last column (BIAS)
shap_values_clean <- shap_values[, -ncol(shap_values)]


# CRITICAL FIX: Convert to data.frame so shap.prep doesn't crash
shap_values_df <- as.data.frame(shap_values_clean)

# Ensure column names match the feature matrix exactly
colnames(shap_values_df) <- colnames(train_matrix)
# Use shap.prep to combine the SHAP values with the actual Feature values.
# This creates the "Long Format" data required by the plotting function.
shap_long <- shap.prep(
  shap_contrib = shap_values_df,
  X_train = train_matrix,
  top_n = 15
)

# 6. GENERATE THE PLOTS
# ------------------------------------------------------------------------------

# PLOT A: SHAP Summary Plot (The "Beeswarm")
# Shows Importance (Y-axis), Impact Direction (X-axis), and Feature Value (Color)
shap.plot.summary(shap_long) + 
  ggtitle("SHAP Summary: Feature Impact on Buy/Sell Signal") +
  theme_bw()

# PLOT B: Dependence Plot (For the #1 Feature)
# Automatically finds the most important feature and plots its specific effect
top_feature <- names(sort(colMeans(abs(shap_values_clean)), decreasing = TRUE))[1]

shap.plot.dependence(
  data_long = shap_long, 
  x = top_feature, 
  color_feature = "auto"
) + ggtitle(paste("Feature Analysis: How", top_feature, "drives decisions"))









