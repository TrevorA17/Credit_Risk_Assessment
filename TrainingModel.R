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

# Load necessary libraries
library(boot)

# Define a function to calculate the statistic of interest (e.g., mean)
statistic_function <- function(data, index) {
  # Subset data using bootstrap indices
  bootstrap_sample <- data[index, ]
  
  # Calculate the statistic of interest (e.g., mean)
  statistic_value <- mean(bootstrap_sample$loan_amnt)
  
  return(statistic_value)
}

# Set seed for reproducibility
set.seed(123)

# Perform bootstrapping with 1000 bootstrap replicates
bootstrap_results <- boot(data = credit_data, statistic = statistic_function, R = 1000)

# Display bootstrapping results
print("Bootstrapping Results:")
print(bootstrap_results)

# Load necessary libraries
library(caret)

# Set seed for reproducibility
set.seed(123)

# Remove rows with missing values
credit_data_clean <- na.omit(credit_data)

# Define the training control
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train the model using k-fold cross-validation
cross_val_model <- train(loan_status ~ ., data = credit_data_clean, method = "glm", trControl = train_control)

# Print the model
print(cross_val_model)

# Load necessary libraries
library(caret)

# Set seed for reproducibility
set.seed(123)

# Define the training control
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train logistic regression model
logistic_model <- train(loan_status ~ ., data = credit_data_clean, method = "glm", trControl = train_control)

# Train decision tree model
decision_tree_model <- train(loan_status ~ ., data = credit_data_clean, method = "rpart", trControl = train_control)

# Train gradient boosting model
gradient_boosting_model <- train(loan_status ~ ., data = credit_data_clean, method = "gbm", trControl = train_control)

# Print the models
print("Logistic Regression Model:")
print(logistic_model)
print("Decision Tree Model:")
print(decision_tree_model)
print("Gradient Boosting Model:")
print(gradient_boosting_model)

