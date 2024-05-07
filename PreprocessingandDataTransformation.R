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

# Count missing values in each column
missing_values <- colSums(is.na(credit_data))

# Display columns with missing values
print("Columns with Missing Values:")
print(names(missing_values[missing_values > 0]))

# Display total number of missing values
print("Total Number of Missing Values:")
print(sum(missing_values))

# Mean imputation for missing values in numeric variables
credit_data_imputed <- credit_data

numeric_variables <- credit_data_imputed[, sapply(credit_data_imputed, is.numeric)]
for (variable in colnames(numeric_variables)) {
  if (anyNA(credit_data_imputed[[variable]])) {
    credit_data_imputed[[variable]][is.na(credit_data_imputed[[variable]])] <- mean(credit_data_imputed[[variable]], na.rm = TRUE)
  }
}

# Check if there are still missing values
missing_values_after_imputation <- colSums(is.na(credit_data_imputed))

# Display columns with missing values after imputation
print("Columns with Missing Values After Imputation:")
print(names(missing_values_after_imputation[missing_values_after_imputation > 0]))

# Standardization (Z-score normalization) for numeric variables
numeric_variables <- credit_data[, sapply(credit_data, is.numeric)]
standardized_numeric <- scale(numeric_variables)

# Create a new dataframe with standardized numeric variables
credit_data_standardized <- as.data.frame(standardized_numeric)
names(credit_data_standardized) <- colnames(numeric_variables)

# Display summary statistics of standardized numeric variables
summary(credit_data_standardized)
