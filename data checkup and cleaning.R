#================================================
# R Script for Comprehensive Data Preparation and Statistical Analysis
#================================================

# Load necessary libraries
library(dplyr)
library(naniar)
library(readr) 
library(e1071) # For skewness and kurtosis
library(ggplot2) # For plotting
library(gridExtra) # For table plotting in PDF
library(reshape2) # For melting data for comprehensive plots

# --- 1. Loading the Data ---
cat("\n--- 1. Loading the Data ---\n")
# Assuming 'data.csv' is in the working directory or provide the full path
tryCatch({
  df <- read_csv("data.csv")
  cat("Dataset 'data.csv' loaded successfully!\n")
}, error = function(e) {
  cat(sprintf("Error: Could not load 'data.csv'. Please ensure the file exists and the path is correct.\nError message: %s\n", e$message))
  stop("Script terminated due to data loading error.") # Stop execution if file not found
})

# View the first few rows (optional, can be commented out for large datasets)
# View(df)

# --- 2. Initial Inspection & Overview ---
cat("\n--- 2. Initial Inspection & Overview ---\n")

# View the first few rows 
cat("\n2.1 First 6 rows of the data:\n")
print(head(df))

# Get a concise summary of the data frame's structure and data types
cat("\n2.2 Structure of the data frame:\n")
str(df)

# Get descriptive statistics for each column
cat("\n2.3 Summary statistics for all columns:\n")
print(summary(df))

# Check column names
cat("\n2.4 Column names:\n")
print(colnames(df))

# Get dimensions of the DataFrame
cat("\n2.5 Dimensions of the DataFrame (Rows, Columns):\n")
print(dim(df))

# --- 3. Handling Missing Values ---
cat("\n--- 3. Handling Missing Values ---\n")

# Identify missing values: Count NA values per column
cat("\n3.1 Number of missing values per column (before imputation):\n")
print(colSums(is.na(df)))

# Identify missing values: Total number of NA values in the entire data frame
cat("\n3.2 Total number of missing values in the data frame (before imputation): ")
print(sum(is.na(df)))

# Helper function to get mode for categorical imputation
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Automated Imputation of Missing Values
# Impute numeric columns with median and categorical/character columns with mode.
cat("\n3.4 Automated Imputation of Missing Values:\n")

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

cat("\n3.5 Missing values after imputation:\n")
print(colSums(is.na(df)))

# --- 4. Handling Duplicates ---
cat("\n--- 4. Handling Duplicates ---\n")

# Count the number of duplicate rows
num_duplicates <- sum(duplicated(df))
cat("\n4.1 Number of duplicate rows found: ", num_duplicates, "\n")

# Remove duplicate rows
if (num_duplicates > 0) {
  df_unique <- unique(df)
  cat("4.2 Dimensions after removing duplicates: ", dim(df_unique), "\n")
  df <- df_unique 
} else {
  cat("4.2 No duplicate rows to remove.\n")
}

# --- 5. Enhanced Statistical Analysis ---
cat("\n--- 5. Enhanced Statistical Analysis ---\n")

# 5.1 Descriptive Statistics for Numerical Variables
cat("\n5.1 Descriptive Statistics for Numerical Variables:\n")
numerical_cols <- names(df)[sapply(df, is.numeric)]
# Exclude ID columns from numerical_cols for descriptive statistics and plots
id_columns_to_exclude <- c("ad_id", "campaign_id", "fb_campaign_id", "SN") # Add any other known ID columns
numerical_cols <- numerical_cols[!numerical_cols %in% id_columns_to_exclude]

# Prepare a data frame to store descriptive statistics for PDF output
desc_stats_df <- data.frame(
  Metric = c("Mean", "Median", "Standard Deviation", "Variance", "Skewness", "Kurtosis", "Min", "Max")
)
for (col in numerical_cols) {
  col_stats <- c(
    mean(df[[col]], na.rm = TRUE),
    median(df[[col]], na.rm = TRUE),
    sd(df[[col]], na.rm = TRUE),
    var(df[[col]], na.rm = TRUE),
    skewness(df[[col]], na.rm = TRUE),
    kurtosis(df[[col]], na.rm = TRUE),
    min(df[[col]], na.rm = TRUE),
    max(df[[col]], na.rm = TRUE)
  )
  desc_stats_df[[col]] <- round(col_stats, 2)
}
print(desc_stats_df)


# 5.2 Frequency Tables and Proportions for Categorical Variables
cat("\n5.2 Frequency Tables and Proportions for Categorical Variables:\n")
categorical_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
# Store frequency and proportion tables in a list for PDF output
freq_prop_tables <- list()
for (col in categorical_cols) {
  cat(sprintf("\n--- Column: %s ---\n", col))
  freq_table <- as.data.frame(table(df[[col]]))
  colnames(freq_table) <- c("Category", "Frequency")
  
  prop_table <- as.data.frame(prop.table(table(df[[col]])))
  colnames(prop_table) <- c("Category", "Proportion")
  prop_table$Proportion <- round(prop_table$Proportion, 4) # Round for readability
  
  print(prop_table) # Print proportions, as frequencies are usually large
  
  freq_prop_tables[[col]] <- list(frequency = freq_table, proportion = prop_table)
}

# 5.3 Correlation Analysis for Numerical Variables
cat("\n5.3 Correlation Analysis for Numerical Variables:\n")
correlation_matrix <- NULL # Initialize to NULL
if (length(numerical_cols) > 1) {
  correlation_matrix <- round(cor(df[numerical_cols], use = "pairwise.complete.obs"), 2)
  print(correlation_matrix)
} else {
  cat("  Not enough numerical columns for correlation analysis.\n")
}

# 5.4 Grouped Statistical Analysis
cat("\n5.4 Grouped Statistical Analysis:\n")

grouped_analysis_results <- list()

# Example 1: Average 'spent', 'impressions', 'clicks' by 'age'
if ("age" %in% colnames(df) && all(c("spent", "impressions", "clicks") %in% colnames(df))) {
  cat("\n  Average 'spent', 'impressions', 'clicks' by 'age':\n")
  grouped_by_age <- df %>%
    group_by(age) %>%
    summarise(
      Avg_Spent = mean(spent, na.rm = TRUE),
      Avg_Impressions = mean(impressions, na.rm = TRUE),
      Avg_Clicks = mean(clicks, na.rm = TRUE)
    ) %>%
    arrange(age) %>%
    as.data.frame() # Convert to data frame for grid.table
  grouped_by_age[, c("Avg_Spent", "Avg_Impressions", "Avg_Clicks")] <- round(grouped_by_age[, c("Avg_Spent", "Avg_Impressions", "Avg_Clicks")], 2)
  print(grouped_by_age)
  grouped_analysis_results[["by_age"]] <- grouped_by_age
} else {
  cat("  Skipping grouped analysis by 'age' due to missing columns.\n")
}

# Example 2: Average 'spent', 'impressions', 'clicks' by 'gender'
if ("gender" %in% colnames(df) && all(c("spent", "impressions", "clicks") %in% colnames(df))) {
  cat("\n  Average 'spent', 'impressions', 'clicks' by 'gender':\n")
  grouped_by_gender <- df %>%
    group_by(gender) %>%
    summarise(
      Avg_Spent = mean(spent, na.rm = TRUE),
      Avg_Impressions = mean(impressions, na.rm = TRUE),
      Avg_Clicks = mean(clicks, na.rm = TRUE)
    ) %>%
    arrange(gender) %>%
    as.data.frame() # Convert to data frame for grid.table
  grouped_by_gender[, c("Avg_Spent", "Avg_Impressions", "Avg_Clicks")] <- round(grouped_by_gender[, c("Avg_Spent", "Avg_Impressions", "Avg_Clicks")], 2)
  print(grouped_by_gender)
  grouped_analysis_results[["by_gender"]] <- grouped_by_gender
} else {
  cat("  Skipping grouped analysis by 'gender' due to missing columns.\n")
}

# 5.5 Outlier Identification (using IQR method)
cat("\n5.5 Outlier Identification (using IQR method):\n")
outlier_summary <- list()
for (col in numerical_cols) { # Use the filtered numerical_cols here too
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  outliers <- df[[col]][df[[col]] < lower_bound | df[[col]] > upper_bound]
  
  if (length(outliers) > 0) {
    cat(sprintf("  - Column '%s': %d outliers found (values outside [%.2f, %.2f])\n", col, length(outliers), lower_bound, upper_bound))
    outlier_summary[[col]] <- data.frame(
      Column = col,
      Num_Outliers = length(outliers),
      Lower_Bound = round(lower_bound, 2),
      Upper_Bound = round(upper_bound, 2)
    )
  } else {
    cat(sprintf("  - Column '%s': No outliers detected by IQR method.\n", col))
  }
}
outlier_df <- do.call(rbind, outlier_summary)
if (!is.null(outlier_df) && nrow(outlier_df) > 0) {
  row.names(outlier_df) <- NULL # Reset row names
}


# --- 6. Plotting and PDF Export ---
cat("\n--- 6. Generating Plots and Exporting to PDF ---\n")

pdf_filename <- "R_Analysis_Plots.pdf"
pdf(pdf_filename, width = 11, height = 8.5) # Open PDF device (standard letter size for better table fit)

# Define a custom dark theme for ggplot2
theme_dark_custom <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(color = "white", face = "bold", size = 14),
      axis.title = element_text(color = "white", size = 10),
      axis.text = element_text(color = "white", size = 8),
      legend.title = element_text(color = "white", size = 9),
      legend.text = element_text(color = "white", size = 8),
      panel.background = element_rect(fill = "black", color = NA), 
      plot.background = element_rect(fill = "black", color = NA),  
      panel.grid.major = element_line(color = "#333333"), 
      panel.grid.minor = element_line(color = "#222222"),
      axis.line = element_line(color = "#666666"),
      strip.text = element_text(color = "white", face = "bold"), 
      strip.background = element_rect(fill = "#333333", color = NA)
    )
}

# Set the custom dark theme as the default for ggplot2
theme_set(theme_dark_custom())

# Define a custom dark theme for grid.table
ttheme_dark <- ttheme_default(
  core = list(fg_params = list(col = "white"), bg_params = list(fill = "black")),
  colhead = list(fg_params = list(col = "white", fontface = "bold"), bg_params = list(fill = "#333333")),
  rowhead = list(fg_params = list(col = "white", fontface = "bold"), bg_params = list(fill = "#333333"))
)

# Set up a blank plot for text output
plot_text_page <- function(title_text, content_df = NULL, content_matrix = NULL, text_lines = NULL) {
  plot.new()
  par(mar = c(0, 0, 2, 0), bg = "black") # Set background to black for text pages
  title(title_text, col.main = "white", cex.main = 1.5)
  
  if (!is.null(content_df)) {
    grid.table(content_df, rows = NULL, theme = ttheme_dark) # Use dark theme for tables
  } else if (!is.null(content_matrix)) {
    # Convert matrix to data frame for grid.table
    grid.table(as.data.frame(content_matrix), rows = NULL, theme = ttheme_dark) # Use dark theme for tables
  } else if (!is.null(text_lines)) {
    # For general text, use text()
    text(0.05, 0.95, paste(text_lines, collapse = "\n"), adj = c(0, 1), col = "white", cex = 0.8, family = "mono")
  }
}

# 6.1 Visualize missing values (if any)
cat("\n6.1 Visualizing missing data (plot will be saved to PDF):\n")
if (sum(is.na(df)) > 0) {
  # vis_miss uses base R graphics, so we set par(bg) before plotting
  par(bg = "black", fg = "white", col.axis = "white", col.lab = "white", col.main = "white")
  vis_miss(df)
  title("Missing Values Heatmap")
  par(bg = "white", fg = "black", col.axis = "black", col.lab = "black", col.main = "black") # Reset for ggplot
} else {
  cat("  No missing values found. Skipping missing values heatmap.\n")
}

# 6.2 Comprehensive Density Plots for Numerical Variables
cat("\n6.2 Generating Comprehensive Density Plots for Numerical Variables:\n")
if (length(numerical_cols) > 0) {
  df_melted_numerical <- melt(df, measure.vars = numerical_cols, variable.name = "variable", value.name = "value")
  p_density <- ggplot(df_melted_numerical, aes(x = value, fill = variable)) +
    geom_density(alpha = 0.6) +
    facet_wrap(~ variable, scales = "free", ncol = 3) + # Use facet_wrap to show all on one page
    ggtitle("Distributions of Numerical Variables") +
    theme(legend.position = "none") # Hide legend as facets label variables
  print(p_density)
  cat("  - Comprehensive Density plot for numerical variables saved.\n")
} else {
  cat("  No numerical columns found for comprehensive density plot.\n")
}

# 6.3 Bar Plots for Categorical Variables (Individual plots for clarity)
cat("\n6.3 Generating Bar Plots for Categorical Variables:\n")
for (col in categorical_cols) {
  # Ensure the column is a factor for proper plotting
  df[[col]] <- as.factor(df[[col]])
  p <- ggplot(df, aes_string(x=col)) +
    geom_bar(fill="steelblue", color="white") +
    ggtitle(paste("Frequency of", col)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Keep angle for readability
  print(p)
  cat(sprintf("  - Bar plot for '%s' saved.\n", col))
}

# 6.4 Comprehensive Box Plots for Numerical Variables (Outlier Visualization)
cat("\n6.4 Generating Comprehensive Box Plots for Numerical Variables (Outlier Visualization):\n")
if (length(numerical_cols) > 0) {
  # Use the melted data from 6.2
  p_boxplot <- ggplot(df_melted_numerical, aes(y = value, x = variable, fill = variable)) +
    geom_boxplot(color = "white") +
    facet_wrap(~ variable, scales = "free_y", ncol = 3) + # Free y-scale for different ranges
    ggtitle("Box Plots of Numerical Variables (Outlier Visualization)") +
    theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) # Hide x-axis labels
  print(p_boxplot)
  cat("  - Comprehensive Box plot for numerical variables saved.\n")
} else {
  cat("  No numerical columns found for comprehensive box plot.\n")
}

# --- Add Statistical Tables to PDF ---
cat("\n6.5 Adding Statistical Tables and Visualizations to PDF:\n")

# Descriptive Statistics Table
plot_text_page("Descriptive Statistics for Numerical Variables", content_df = desc_stats_df)
cat("  - Descriptive Statistics table added to PDF.\n")

# Frequency and Proportion Bar Plots for Categorical Variables
for (col in categorical_cols) {
  # Using the combined_cat_table for plotting proportions
  combined_cat_table <- left_join(freq_prop_tables[[col]]$frequency, freq_prop_tables[[col]]$proportion, by = "Category")
  
  # Bar plot for proportions
  p_prop_bar <- ggplot(combined_cat_table, aes(x = reorder(Category, Proportion), y = Proportion)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "white") +
    geom_text(aes(label = paste0(round(Proportion*100, 2), "%")), hjust = -0.1, color = "white", size = 3) + # Add percentage labels
    coord_flip() + # Flip coordinates for horizontal bars
    ggtitle(paste("Proportion of", col)) +
    xlab("") + ylab("Proportion") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) # Adjust text for horizontal bars
  print(p_prop_bar)
  cat(sprintf("  - Proportion bar plot for '%s' added to PDF.\n", col))
}

# Correlation Matrix Heatmap
if (!is.null(correlation_matrix)) {
  # Melt the correlation matrix for ggplot2 heatmap
  melted_corr <- melt(correlation_matrix, varnames = c("Var1", "Var2"))
  p_corr_heatmap <- ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "darkred", high = "steelblue", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation") +
    theme_minimal() +
    coord_fixed() +
    ggtitle("Correlation Matrix Heatmap") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_blank()) # Remove axis titles
  print(p_corr_heatmap)
  cat("  - Correlation Matrix Heatmap added to PDF.\n")
}

# Grouped Analysis Bar Plots
if ("by_age" %in% names(grouped_analysis_results)) {
  df_age_melted <- melt(grouped_analysis_results$by_age, id.vars = "age", variable.name = "Metric", value.name = "Value")
  p_age_grouped <- ggplot(df_age_melted, aes(x = age, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    ggtitle("Average Metrics by Age Group") +
    xlab("Age Group") + ylab("Average Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_age_grouped)
  cat("  - Grouped Analysis by Age bar plot added to PDF.\n")
}
if ("by_gender" %in% names(grouped_analysis_results)) {
  df_gender_melted <- melt(grouped_analysis_results$by_gender, id.vars = "gender", variable.name = "Metric", value.name = "Value")
  p_gender_grouped <- ggplot(df_gender_melted, aes(x = gender, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    ggtitle("Average Metrics by Gender") +
    xlab("Gender") + ylab("Average Value")
  print(p_gender_grouped)
  cat("  - Grouped Analysis by Gender bar plot added to PDF.\n")
}

# Outlier Summary Table (remains as table)
if (!is.null(outlier_df) && nrow(outlier_df) > 0) {
  plot_text_page("Outlier Identification Summary", content_df = outlier_df)
  cat("  - Outlier Identification Summary table added to PDF.\n")
} else {
  cat("  No outlier summary table generated as no outliers were found or data frame is empty.\n")
}

dev.off() # Close PDF device
cat(sprintf("\nAll plots and statistical tables have been saved to '%s'\n", pdf_filename))

# --- Final Inspection after all steps ---
cat("\n--- Final Data Inspection After All Preparation Steps ---\n")
cat("\nFinal structure of the data frame:\n")
str(df)
cat("\nFinal summary statistics of the data frame:\n")
print(summary(df))
cat("\nFinal dimensions of the data frame: ", dim(df), "\n")
cat("\nFirst 6 rows of the processed data:\n")
print(head(df))

# Save the cleaned data 
write.csv(df, "cleaned_data.csv", row.names = FALSE)
cat("\nCleaned data saved to 'cleaned_data.csv'\n")

# End of script
cat("\n--- R Script Execution Complete ---\n")
