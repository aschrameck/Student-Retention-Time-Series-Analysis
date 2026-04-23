# -----------------------------------------------------------
# Retention Rates Time Series EDA + Modeling Script
# -----------------------------------------------------------
# Data: retention_long.csv
# Columns:
# ID, Year, Retention, Type (FT/PT)
#
# Goal:
# - Analyze trends in retention over time
# - Build ARIMA models for FT and PT averages
# - Compare models and diagnostics
# -----------------------------------------------------------

# --- Setup ---
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)

# Load data
df <- read.csv("retention_long.csv")

# Preview
head(df)

# --- Aggregate to Average Retention by Year ---

avg_df <- df %>%
  group_by(Year, Type) %>%
  summarise(AvgRetention = mean(Retention, na.rm = TRUE)) %>%
  ungroup()

# Split FT and PT
ft_df <- avg_df %>% filter(Type == "FT")
pt_df <- avg_df %>% filter(Type == "PT")


# --- Convert to Time Series Objects ---
# Years should be consecutive (2014–2024)
ft_ts <- ts(ft_df$AvgRetention, start = min(ft_df$Year), frequency = 1)
pt_ts <- ts(pt_df$AvgRetention, start = min(pt_df$Year), frequency = 1)

# --- Preliminary Exploration ---
# Summary statistics
summary(ft_ts)
sd(ft_ts)

summary(pt_ts)
sd(pt_ts)

# Plot time series
plot.ts(ft_ts, main = "Average Full-Time Retention", ylab = "%")
plot.ts(pt_ts, main = "Average Part-Time Retention", ylab = "%")

# Combined plot
ts.plot(ft_ts, pt_ts, col = c("blue", "red"), lwd = 2,
        main = "FT vs PT Retention Over Time", ylab = "%")
legend("topleft", legend = c("FT", "PT"),
       col = c("blue", "red"), lty = 1)

# Differencing to Achieve Stationarity
# First difference
ft_diff <- diff(ft_ts)
pt_diff <- diff(pt_ts)

plot.ts(ft_diff, main = "Differenced FT Series")
plot.ts(pt_diff, main = "Differenced PT Series")

# ACF and PACF Plots
par(mfrow = c(1,2))
acf(ft_diff, main = "FT ACF")
pacf(ft_diff, main = "FT PACF")

acf(pt_diff, main = "PT ACF")
pacf(pt_diff, main = "PT PACF")
