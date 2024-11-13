library(tidyr)
library(dplyr)
library(xgboost)

set.seed(102)

factorize <- function(df){
  # turns categorical variables to factors or ordinal factors (loan grade or if a person defaulted)
  df$person_home_ownership <- as.factor(df$person_home_ownership)
  df$loan_intent <- as.factor(df$loan_intent)
  loan_grade_order <- c("G", "F", "E", "D", "C", "B", "A")
  default_order <- c("Y", "N")
  df$loan_grade <- factor(df$loan_grade, levels = loan_grade_order,ordered = TRUE)
  df$cb_person_default_on_file <- factor(df$cb_person_default_on_file,levels = default_order
                                         , ordered = TRUE)
  return(df)
}
#turning categorical columns to factors
loans <- factorize(loans)
submission <- factorize(submission)

loans <- loans %>% select(!c(person_age, loan_percent_income))
sub_id <- submission$id
submission <- submission %>% select(!c(person_age, id, loan_percent_income))
#splitting data into test and training data
train_df <- loans %>% sample_frac(size = 0.7)
test_df <- loans %>% anti_join(train_df)

#scaling the test and training data
get_center_scale <- function(train_data){
  #returns the mean and standard deviation of the training data 
  #should only be called FOR TRAINING DATA (to scale test data)
  numeric_vars <- c("person_income","person_emp_length","loan_amnt",
                    "loan_int_rate","cb_person_cred_hist_length")
  scale_ <- apply(train_df[, numeric_vars], 2, sd)
  center <- apply(train_df[, numeric_vars], 2, mean)
  return(data.frame(center = center, scale_ = scale_))
}

scale_and_join <- function(df, center_, deviation){
  #scales the test and training data with the given center and deviation/scale
  numeric_vars <- c("person_income","person_emp_length","loan_amnt",
                    "loan_int_rate","cb_person_cred_hist_length")
  categorical_vars <- c("person_home_ownership","loan_intent","loan_grade",
                        "cb_person_default_on_file")
  df_scaled <- data.frame(scale(df[, numeric_vars], 
                                center = center_, scale = deviation))
  df_scaled <- cbind.data.frame(df_scaled, df[, categorical_vars])
  return(df_scaled)
}
train_param <- get_center_scale(train_data = train_df)
train_scaled <- scale_and_join(train_df, train_param$center, train_param$scale_)
train_scaled$loan_status <- train_df$loan_status
test_scaled <- scale_and_join(test_df, train_param$center, train_param$scale_)
test_scaled$loan_status <- test_df$loan_status
submission_scaled <- scale_and_join(submission, train_param$center, 
                                    train_param$scale_)

#splitting data into response and predictor variables
x_train <- train_scaled %>% select(!c(loan_status))
y_train <- train_scaled$loan_status
x_test <- test_scaled %>% select(!c(loan_status))
y_test <- test_scaled$loan_status

#transforming data to matrices for quick calculations
xgb_train <- xgb.DMatrix(data = data.matrix(x_train), label=y_train)
xgb_test <- xgb.DMatrix(data = data.matrix(x_test))
xgb_submission <- xgb.DMatrix(data = data.matrix(submission_scaled))

#running the model
xgb_md <- xgboost(data = xgb_train, nrounds = 70, max.depth = 3, objective = "binary:logistic")
y_pred <- predict(xgb_md, xgb_test, type = "response")
submit_pred <- predict(xgb_md, xgb_submission, type = "response")
submit_pred <- data.frame(id = sub_id, loan_status = submit_pred)

#saving the result
write.table(submit_pred, file = "xgboost_full.csv", sep = ",", dec = ".", 
            row.names = FALSE)
