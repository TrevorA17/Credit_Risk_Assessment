# Load the saved GBM model
loaded_gbm_model <- readRDS("./models/saved_gbm_model.rds")

#* @apiTitle Credit Risk Prediction Model API
#* @apiDescription Used to predict credit risk.

#* @param person_age Age of the person
#* @param person_income Income of the person
#* @param person_home_ownership Home ownership status (e.g., "RENT", "OWN", "MORTGAGE")
#* @param person_emp_length Employment length in years
#* @param loan_intent Purpose of the loan (e.g., "EDUCATION", "PERSONAL", "VENTURE")
#* @param loan_amnt Loan amount
#* @param loan_int_rate Loan interest rate
#* @param loan_percent_income Loan amount as a percentage of income
#* @param cb_person_default_on_file Default history status (Yes/No)
#* @param cb_person_cred_hist_length Credit history length in years

#* @get /credit_risk_prediction

predict_credit_risk <- function(person_age, person_income, person_home_ownership, 
                                person_emp_length, loan_intent, loan_amnt, loan_int_rate, 
                                loan_percent_income, cb_person_default_on_file, 
                                cb_person_cred_hist_length) {
  
  # Create a data frame using the arguments
  to_be_predicted <- data.frame(
    person_age = as.numeric(person_age),
    person_income = as.numeric(person_income),
    person_home_ownership = as.character(person_home_ownership),
    person_emp_length = as.numeric(person_emp_length),
    loan_intent = as.character(loan_intent),
    loan_amnt = as.numeric(loan_amnt),
    loan_int_rate = as.numeric(loan_int_rate),
    loan_percent_income = as.numeric(loan_percent_income),
    cb_person_default_on_file = as.character(cb_person_default_on_file),
    cb_person_cred_hist_length = as.numeric(cb_person_cred_hist_length)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_gbm_model, newdata = to_be_predicted)
  
  # Return the prediction
  return(prediction)
}
