# -----------------------------------------------------------
# IPEDS Retention Rates EDA Script
# -----------------------------------------------------------
# Data: retention_rates.csv (cleaned IPEDS data)
#
# Contains:
# - ID (institution)
# - {year}_FT_pct (full-time retention, percent)
# - {year}_PT_pct (part-time retention, percent)
#
# Goal:
# - Explore trends over time
# - Compare FT vs PT retention
# - Examine variation across schools
# -----------------------------------------------------------

# --- Setup ---
library(dplyr)
library(ggplot2)

# Load data
df <- read.csv("retention_rates.csv")

# Preview
head(df)
str(df)

# -- Separate Columns ---
# Extract FT and PT columns
ft_cols <- grep("_FT_pct$", names(df), value = TRUE)
pt_cols <- grep("_PT_pct$", names(df), value = TRUE)

# Extract years from column names
years <- as.numeric(sub("_FT_pct", "", ft_cols))

# --- Average Retention Rates Over Time ---
# Compute yearly averages
avg_ft <- colMeans(df[, ft_cols], na.rm = TRUE)
avg_pt <- colMeans(df[, pt_cols], na.rm = TRUE)

# Create data frame for plotting
avg_df <- data.frame(
  Year = years,
  Full_Time = avg_ft,
  Part_Time = avg_pt
)

# View summary
avg_df

# --- Plot Average Retention Over Time ---
# Full-time retention trend
plot(avg_df$Year, avg_df$Full_Time, type = "o",
     main = "Average Full-Time Retention Over Time",
     xlab = "Year", ylab = "Retention Rate (%)")

# Part-time retention trend
plot(avg_df$Year, avg_df$Part_Time, type = "o",
     main = "Average Part-Time Retention Over Time",
     xlab = "Year", ylab = "Retention Rate (%)")

# Combined comparison plot
plot(avg_df$Year, avg_df$Full_Time, type = "o",
     ylim = range(c(avg_df$Full_Time, avg_df$Part_Time)),
     xlab = "Year", ylab = "Retention Rate (%)",
     main = "FT vs PT Retention Rates")

lines(avg_df$Year, avg_df$Part_Time, type = "o", lty = 2)

legend("topleft",
       legend = c("Full-Time", "Part-Time"),
       lty = c(1,2),
       bty = "n")

# --- Distribution of Retention Rates (All Schools) ---
# Combine all FT values into one vector
ft_all <- unlist(df[, ft_cols])
pt_all <- unlist(df[, pt_cols])

# Histograms
hist(ft_all, main = "Distribution of Full-Time Retention",
     xlab = "Retention (%)")

hist(pt_all, main = "Distribution of Part-Time Retention",
     xlab = "Retention (%)")

# Summary statistics
summary(ft_all)
summary(pt_all)

sd(ft_all)
sd(pt_all)

# --- School-Level Trends ---
# Example: plot a few random schools
set.seed(1)
sample_ids <- sample(df$ID, 5)

for (id in sample_ids) {

  school <- df[df$ID == id, ]

  ft_vals <- as.numeric(school[, ft_cols])
  pt_vals <- as.numeric(school[, pt_cols])

  plot(years, ft_vals, type = "o",
       ylim = range(c(ft_vals, pt_vals), na.rm = TRUE),
       main = paste("School ID:", id),
       xlab = "Year", ylab = "Retention (%)")

  lines(years, pt_vals, type = "o", lty = 2)

  legend("topleft",
         legend = c("FT", "PT"),
         lty = c(1,2),
         bty = "n")
}

# --- Variability Across Schools ---
# Standard deviation across schools for each year
sd_ft <- apply(df[, ft_cols], 2, sd, na.rm = TRUE)
sd_pt <- apply(df[, pt_cols], 2, sd, na.rm = TRUE)

# Plot variability
plot(years, sd_ft, type = "o",
     main = "Variability in Full-Time Retention",
     xlab = "Year", ylab = "SD")

plot(years, sd_pt, type = "o",
     main = "Variability in Part-Time Retention",
     xlab = "Year", ylab = "SD")
