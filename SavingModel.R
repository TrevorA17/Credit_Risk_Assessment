# Saving the GBM model
saveRDS(gradient_boosting_model, "./models/saved_gbm_model.rds")

# Load the saved model
loaded_gbm_model <- readRDS("./models/saved_gbm_model.rds")

# Prepare new data for prediction (using credit risk dataset variables)
new_data <- data.frame(
  person_age = 35,
  person_income = 60000,
  person_home_ownership = "RENT",
  person_emp_length = 5,
  loan_intent = "EDUCATION",
  loan_amnt = 10000,
  loan_int_rate = 10.5,
  loan_percent_income = 0.2,
  cb_person_default_on_file = "N",
  cb_person_cred_hist_length = 4
)

# Use the loaded model to make predictions
predictions_loaded_model <- predict(loaded_gbm_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
