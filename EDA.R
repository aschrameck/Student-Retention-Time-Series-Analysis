# -----------------------------------------------------------
# Retention Rates Time Series EDA + Modeling Script
# -----------------------------------------------------------
# Data: retention_long.csv
# Columns:
# ID, Year, Retention, Type (FT/PT)
#
# Goal:
# - Compute averages
# - Apply differencing
# - Various exploratory plots
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
ft_ts <- ts(ft_df$AvgRetention, start = min(ft_df$Year), frequency = 1)
pt_ts <- ts(pt_df$AvgRetention, start = min(pt_df$Year), frequency = 1)

# --- Preliminary Exploration ---
# Summary statistics
summary(ft_ts)
sd(ft_ts)

summary(pt_ts)
sd(pt_ts)

# Plot time series
plot.ts(ft_ts,
        main = "Average Full-Time Retention",
        ylab = "%",
        ylim = range(ft_ts, na.rm = TRUE))

plot.ts(pt_ts,
        main = "Average Part-Time Retention",
        ylab = "%",
        ylim = range(pt_ts, na.rm = TRUE))

# Combined plot
ts.plot(ft_ts, pt_ts,
        col = c("blue", "red"),
        lwd = 2,
        ylim = range(c(ft_ts, pt_ts), na.rm = TRUE),
        main = "FT vs PT Retention Over Time",
        ylab = "%")

legend("topleft",
       legend = c("FT", "PT"),
       col = c("blue", "red"),
       lty = 1,
       bty = "n")

# Differencing to Achieve Stationarity
# First difference
ft_diff <- diff(ft_ts)
pt_diff <- diff(pt_ts)

plot.ts(ft_diff,
        main = "Differenced FT Series",
        ylim = range(ft_diff, na.rm = TRUE))

plot.ts(pt_diff,
        main = "Differenced PT Series",
        ylim = range(pt_diff, na.rm = TRUE))

# ACF and PACF Plots
par(mfrow = c(1,2))
acf(ft_diff, main = "FT ACF")
pacf(ft_diff, main = "FT PACF")

acf(pt_diff, main = "PT ACF")
pacf(pt_diff, main = "PT PACF")
