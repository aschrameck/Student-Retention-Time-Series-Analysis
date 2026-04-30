# -----------------------------------------------------------
# Retention Rates Time Series Modeling (15 Train / 5 Test)
# -----------------------------------------------------------
# Data: retention_long.csv
# Columns:
# ID, Year, Retention, Type (FT/PT)
#
# Goal:
# - Build ARIMA models for FT and PT retention
# - Use first 15 years for training
# - Use last 5 years for testing
# - Evaluate forecast accuracy
# -----------------------------------------------------------

# --- Setup ---
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)

# Load data
df <- read.csv("retention_long.csv")

# --- Aggregate to Average Retention by Year ---
avg_df <- df %>%
  group_by(Year, Type) %>%
  summarise(AvgRetention = mean(Retention, na.rm = TRUE)) %>%
  ungroup()

# Split FT and PT
ft_df <- avg_df %>% filter(Type == "FT") %>% arrange(Year)
pt_df <- avg_df %>% filter(Type == "PT") %>% arrange(Year)

# Convert to time series
ft_ts <- ts(ft_df$AvgRetention, start = min(ft_df$Year), frequency = 1)
pt_ts <- ts(pt_df$AvgRetention, start = min(pt_df$Year), frequency = 1)

# --- Train/Test Split (15 years train, 5 years test) ---

# Ensure correct ordering
years <- ft_df$Year

# Indices
train_idx <- 1:15
test_idx  <- 16:20

# Full-Time
ft_train <- ts(ft_ts[train_idx], start = years[1], frequency = 1)
ft_test  <- ts(ft_ts[test_idx], start = years[16], frequency = 1)

# Part-Time
pt_train <- ts(pt_ts[train_idx], start = years[1], frequency = 1)
pt_test  <- ts(pt_ts[test_idx], start = years[16], frequency = 1)

# --- Preliminary Exploration ---

summary(ft_train)
sd(ft_train)

summary(pt_train)
sd(pt_train)

# Plot training data
plot.ts(ft_train, main = "FT Training Series", ylab = "%")
plot.ts(pt_train, main = "PT Training Series", ylab = "%")

# --- Differencing ---
ft_diff <- diff(ft_train)
pt_diff <- diff(pt_train)

plot.ts(ft_diff, main = "Differenced FT")
plot.ts(pt_diff, main = "Differenced PT")

# --- ACF and PACF ---
par(mfrow = c(1,2))
acf(ft_diff, main = "FT ACF")
pacf(ft_diff, main = "FT PACF")

acf(pt_diff, main = "PT ACF")
pacf(pt_diff, main = "PT PACF")

# --- Fit Candidate ARIMA Models (Training Only) ---
# Full-Time
ft_fit1 <- arima(ft_train, order = c(1,1,0))
ft_fit2 <- arima(ft_train, order = c(0,1,1))
ft_fit3 <- arima(ft_train, order = c(1,1,1))

# Part-Time
pt_fit1 <- arima(pt_train, order = c(1,1,0))
pt_fit2 <- arima(pt_train, order = c(0,1,1))
pt_fit3 <- arima(pt_train, order = c(1,1,1))

# --- Model Comparison (AIC/BIC) ---
ft_ic <- data.frame(
  Model = c("ARIMA(1,1,0)", "ARIMA(0,1,1)", "ARIMA(1,1,1)"),
  AIC = c(ft_fit1$aic, ft_fit2$aic, ft_fit3$aic),
  BIC = c(BIC(ft_fit1), BIC(ft_fit2), BIC(ft_fit3))
)

pt_ic <- data.frame(
  Model = c("ARIMA(1,1,0)", "ARIMA(0,1,1)", "ARIMA(1,1,1)"),
  AIC = c(pt_fit1$aic, pt_fit2$aic, pt_fit3$aic),
  BIC = c(BIC(pt_fit1), BIC(pt_fit2), BIC(pt_fit3))
)

ft_ic
pt_ic

# --- Forecast into Test Period (5 years) ---

h <- length(ft_test)

# FT forecasts
ft_f1 <- predict(ft_fit1, n.ahead = h)
ft_f2 <- predict(ft_fit2, n.ahead = h)
ft_f3 <- predict(ft_fit3, n.ahead = h)

# PT forecasts
pt_f1 <- predict(pt_fit1, n.ahead = h)
pt_f2 <- predict(pt_fit2, n.ahead = h)
pt_f3 <- predict(pt_fit3, n.ahead = h)

# --- Forecast Accuracy (TEST SET) ---

ft_acc <- data.frame(
  Model = c("ARIMA(1,1,0)", "ARIMA(0,1,1)", "ARIMA(1,1,1)"),
  RMSE = c(
    sqrt(mean((ft_test - ft_f1$pred)^2)),
    sqrt(mean((ft_test - ft_f2$pred)^2)),
    sqrt(mean((ft_test - ft_f3$pred)^2))
  ),
  MAE = c(
    mean(abs(ft_test - ft_f1$pred)),
    mean(abs(ft_test - ft_f2$pred)),
    mean(abs(ft_test - ft_f3$pred))
  )
)

pt_acc <- data.frame(
  Model = c("ARIMA(1,1,0)", "ARIMA(0,1,1)", "ARIMA(1,1,1)"),
  RMSE = c(
    sqrt(mean((pt_test - pt_f1$pred)^2)),
    sqrt(mean((pt_test - pt_f2$pred)^2)),
    sqrt(mean((pt_test - pt_f3$pred)^2))
  ),
  MAE = c(
    mean(abs(pt_test - pt_f1$pred)),
    mean(abs(pt_test - pt_f2$pred)),
    mean(abs(pt_test - pt_f3$pred))
  )
)

ft_acc
pt_acc

# Choose best based on RMSE
ft_best <- ft_fit2
pt_best <- pt_fit2

# --- Plot Train vs Test vs Forecast ---
# FT plot
ts.plot(ft_train, ft_test, ft_f2$pred,
        col = c("black", "green", "blue"),
        lwd = 2,
        main = "FT: Train vs Test vs Forecast",
        ylab = "%")

legend("topleft",
       legend = c("Train", "Test", "Forecast"),
       col = c("black", "green", "blue"),
       lty = 1)

# PT plot
ts.plot(pt_train, pt_test, pt_f2$pred,
        col = c("black", "green", "red"),
        lwd = 2,
        main = "PT: Train vs Test vs Forecast",
        ylab = "%")

legend("topleft",
       legend = c("Train", "Test", "Forecast"),
       col = c("black", "green", "red"),
       lty = 1)
