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

# Load necessary libraries
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split the dataset into training and testing sets (70% training, 30% testing)
train_index <- createDataPartition(credit_data$loan_status, p = 0.7, list = FALSE)
train_data <- credit_data[train_index, ]
test_data <- credit_data[-train_index, ]

# Display the dimensions of the training and testing sets
print("Dimensions of Training Set:")
print(dim(train_data))
print("Dimensions of Testing Set:")
print(dim(test_data))
