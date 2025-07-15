#================================================
# R Script for Comprehensive Data Preparation
#================================================

library(dplyr)
library(naniar)
library(readr) 

# --- 1. Loading the Data ---

df <- read_csv("~/DATA ANALYSIS & VISUALIZATION/FUTURE INTERNS/F-I task 02/data.csv")
View(df)

# --- 2. Inspecting the Data ---

cat("\n--- 2. Inspecting the Data ---\n")

# View the first few rows 
cat("\nFirst 6 rows of the data:\n")
print(head(df))

# Get a concise summary of the data frame's structure and data types
cat("\nStructure of the data frame:\n")
str(df)

# Get descriptive statistics for each column
cat("\nSummary statistics for all columns:\n")
print(summary(df))

# Check column names
cat("\nColumn names:\n")
print(colnames(df))

# --- 3. Handling Missing Values ---

cat("\n--- 3. Handling Missing Values ---\n")

# Identify missing values: Count NA values per column
cat("\nNumber of missing values per column:\n")
print(colSums(is.na(df)))

# Identify missing values: Total number of NA values in the entire data frame
cat("\nTotal number of missing values in the data frame: ")
print(sum(is.na(df)))

# Visualize missing values 
cat("\nVisualizing missing data (plot will appear):\n")
vis_miss(df)

# --- Automated Imputation of Missing Values ---

# Impute numeric columns with median and categorical/character columns with mode.
cat("\nAutomated Imputation of Missing Values:\n")

for (col_name in colnames(df)) {
  if (any(is.na(df[[col_name]]))) { 
    if (is.numeric(df[[col_name]])) {
      median_val <- median(df[[col_name]], na.rm = TRUE)
      df[[col_name]][is.na(df[[col_name]])] <- median_val
      cat(sprintf("  - Imputed numeric column '%s' with median: %.2f\n", col_name, median_val))
    } else if (is.factor(df[[col_name]]) || is.character(df[[col_name]])) {
      mode_val <- get_mode(df[[col_name]][!is.na(df[[col_name]])])
      df[[col_name]][is.na(df[[col_name]])] <- mode_val
      cat(sprintf("  - Imputed categorical column '%s' with mode: %s\n", col_name, mode_val))
    } else {
      cat(sprintf("  - Skipping imputation for column '%s' (unsupported type or no NAs).\n", col_name))
    }
  }
}

cat("\nMissing values after imputation:\n")
print(colSums(is.na(df)))

# --- 4. Handling Duplicates ---
cat("\n--- 4. Handling Duplicates ---\n")

# Count the number of duplicate rows
num_duplicates <- sum(duplicated(df))
cat("\nNumber of duplicate rows found: ", num_duplicates, "\n")

# Remove duplicate rows
if (num_duplicates > 0) {
  df_unique <- unique(df)
  cat("Dimensions after removing duplicates: ", dim(df_unique), "\n")
  df <- df_unique 
} else {
  cat("No duplicate rows to remove.\n")
}

# --- Final Inspection after all steps ---
cat("\n--- Final Data Inspection After All Preparation Steps ---\n")
#-----08-----
cat("\nFinal structure of the data frame:\n")
str(df)
cat("\nFinal summary statistics of the data frame:\n")
print(summary(df))
cat("\nFinal dimensions of the data frame: ", dim(df), "\n")
cat("\nFirst 6 rows of the processed data:\n")
print(head(df))

# Save the cleaned data 
write.csv(df, "cleaned_data.csv", row.names = FALSE)
