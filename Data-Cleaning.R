# -----------------------------------------------------------
# IPEDS Retention Data Cleaning Script (2014–2024)
# -----------------------------------------------------------
# Data Source:
# Integrated Postsecondary Education Data System (IPEDS)
# National Center for Education Statistics (NCES)
# https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx
#
# Dataset:
# Fall Enrollment (EF) Survey Files:
# Total entering class, retention rates, and student-to-faculty ratio
#
# Files used: ef{year}d_rv.csv (2014–2024)
#
# Variables used:
# UNITID    = Institution ID
# RET_PCF   = Full-time retention rate (PERCENT, 0–100)
# RET_PCP   = Part-time retention rate (PERCENT, 0–100)
# XRET_PCF  = Imputation flag (full-time)
# XRET_PCP  = Imputation flag (part-time)
#
# Goal:
# - Extract retention rates for each year
# - Apply imputation rules
# - Merge into a single dataset
# - Keep only institutions with complete data (all years)
# - Output: retention_rates.csv
# -----------------------------------------------------------

# Set working directory: setwd("path/to/your/data")

# Years of interest
years <- 2014:2024

# Initialize empty dataframe
full_data <- NULL

# --- Function to clean one year's dataset ---
clean_year_data <- function(year) {

  # File name (e.g., ef2014d_rv.csv)
  file_name <- paste0("ef", year, "d_rv.csv")

  # Read CSV
  df <- read.csv(file_name, stringsAsFactors = FALSE)

  # Keep only relevant columns
  df <- df[, c("UNITID", "XRET_PCF", "RET_PCF", "XRET_PCP", "RET_PCP")]

  # Apply imputation rules - Keep only valid/imputed/usable values
  valid_flags <- c("C", "G", "J", "K", "L", "N", "P", "R")

  # Set invalid entries to NA
  df$RET_PCF[!(df$XRET_PCF %in% valid_flags)] <- NA
  df$RET_PCP[!(df$XRET_PCP %in% valid_flags)] <- NA

  # Rename columns
  names(df) <- c(
    "ID",
    "XRET_PCF",
    paste0(year, "_FT"),
    "XRET_PCP",
    paste0(year, "_PT")
  )

  # Drop imputation flag columns
  df <- df[, c("ID", paste0(year, "_FT"), paste0(year, "_PT"))]

  return(df)
}


# --- Loop through all years and merge datasets ---
for (yr in years) {

  cat("Processing year:", yr, "\n")

  year_data <- clean_year_data(yr)

  if (is.null(full_data)) {
    full_data <- year_data
  } else {
    full_data <- merge(full_data, year_data, by = "ID", all = TRUE)
  }
}

# Keep only institutions with complete data across all years
final_data <- full_data[complete.cases(full_data), ]

# Save final cleaned dataset
write.csv(final_data, "retention_rates.csv", row.names = FALSE)

# Summary
cat("Final dataset saved as retention_rates.csv\n")
cat("Number of institutions:", nrow(final_data), "\n")
head(final_data)
