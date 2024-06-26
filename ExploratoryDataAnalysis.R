# Load dataset
credit_data <- read.csv("data/credit_risk_dataset.csv", colClasses = c(
  person_age = "integer",
  person_income = "integer",
  person_home_ownership = "factor",
  person_emp_length = "numeric",
  loan_intent = "factor",
  loan_amnt = "integer",
  loan_int_rate = "numeric",
  loan_status = "factor",
  loan_percent_income = "numeric",
  cb_person_default_on_file = "factor",
  cb_person_cred_hist_length = "integer"
))

# Display the structure of the dataset
str(credit_data)

# View the first few rows of the dataset
head(credit_data)

# View the dataset in a separate viewer window
View(credit_data)

# Calculate frequency counts for categorical variables
frequency_counts <- lapply(credit_data[, sapply(credit_data, is.factor)], table)

# Calculate percentages for categorical variables
percentage_counts <- lapply(frequency_counts, prop.table)

# Display frequency counts
print("Frequency Counts:")
print(frequency_counts)

# Display percentage counts
print("Percentage Counts:")
print(percentage_counts)

# Custom function to find mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate measures of central tendency for numeric variables
central_tendency <- sapply(credit_data[, sapply(credit_data, is.numeric)], function(x) c(mean = mean(x), median = median(x), mode = Mode(x)))

# Display measures of central tendency
print("Measures of Central Tendency:")
print(central_tendency)

# Calculate measures of distribution for numeric variables
distribution_measures <- sapply(credit_data[, sapply(credit_data, is.numeric)], function(x) c(range = diff(range(x)), variance = var(x), standard_deviation = sd(x)))

# Display measures of distribution
print("Measures of Distribution:")
print(distribution_measures)

# Calculate correlation coefficients for numeric variables
correlation_matrix <- cor(credit_data[, sapply(credit_data, is.numeric)])

# Display correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)

# Perform ANOVA tests for numeric variables against a categorical variable
anova_results <- lapply(credit_data[, sapply(credit_data, is.numeric)], function(x) {
  aov_result <- aov(x ~ loan_status, data = credit_data)
  return(summary(aov_result))
})

# Display ANOVA results
print("ANOVA Results:")
print(anova_results)

library(ggplot2)

# Univariate plots for numeric variables
numeric_variables <- credit_data[, sapply(credit_data, is.numeric)]
numeric_plots <- lapply(colnames(numeric_variables), function(variable) {
  ggplot(credit_data, aes(x = !!as.name(variable))) +
    geom_histogram(fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", variable), x = variable, y = "Frequency") +
    theme_minimal()
})

# Univariate plots for categorical variables
categorical_variables <- credit_data[, sapply(credit_data, is.factor)]
categorical_plots <- lapply(colnames(categorical_variables), function(variable) {
  ggplot(credit_data, aes(x = !!as.name(variable))) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = paste("Bar Plot of", variable), x = variable, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

# Display numeric plots
print("Numeric Variable Plots:")
print(numeric_plots)

# Display categorical plots
print("Categorical Variable Plots:")
print(categorical_plots)


# Load necessary libraries
library(ggplot2)

# Scatter plot matrix for numeric variables
numeric_variables <- credit_data[, sapply(credit_data, is.numeric)]
# Scatter plot matrix for numeric variables
scatter_plot_matrix <- ggplot(credit_data, aes(x = .data[[colnames(numeric_variables)[1]]], y = .data[[colnames(numeric_variables)[2]]])) +
  geom_point(alpha = 0.5) +
  facet_wrap(~., scales = "free") +
  theme_minimal()

# Display scatter plot matrix
print("Scatter Plot Matrix:")
print(scatter_plot_matrix)

# Box plot for numeric variables against loan_status
box_plot_numeric <- lapply(colnames(numeric_variables), function(variable) {
  ggplot(credit_data, aes_string(x = "loan_status", y = variable)) +
    geom_boxplot(fill = "skyblue", color = "black") +
    labs(title = paste("Box Plot of", variable, "by Loan Status"), x = "Loan Status", y = variable) +
    theme_minimal()
})

# Grouped bar plot for categorical variables against loan_status
categorical_variables <- credit_data[, sapply(credit_data, is.factor)]
bar_plot_categorical <- lapply(colnames(categorical_variables), function(variable) {
  ggplot(credit_data, aes_string(x = "loan_status", fill = variable)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Grouped Bar Plot of", variable, "by Loan Status"), x = "Loan Status", y = "Count") +
    theme_minimal()
})

# Display scatter plot matrix
print("Scatter Plot Matrix:")
print(scatter_plot_matrix)

# Display box plots for numeric variables
print("Box Plots for Numeric Variables:")
print(box_plot_numeric)

# Display grouped bar plots for categorical variables
print("Grouped Bar Plots for Categorical Variables:")
print(bar_plot_categorical)

