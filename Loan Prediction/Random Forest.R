library(tidyverse)
library(randomForest)
library(caret)
#loans was imported

set.seed(48)
factorize <- function(df){
  # turns categorical variables to factors or ordinal factors (loan grade or if a person defaulted)
  df$loan_status <- factor(df$loan_status, levels = c(0, 1),labels = c("rejected", "accepted"))
  df$person_home_ownership <- as.factor(df$person_home_ownership)
  df$loan_intent <- as.factor(df$loan_intent)
  loan_grade_order <- c("G", "F", "E", "D", "C", "B", "A")
  default_order <- c("Y", "N")
  df$loan_grade <- factor(df$loan_grade, levels = loan_grade_order,ordered = TRUE)
  df$cb_person_default_on_file <- factor(df$cb_person_default_on_file,levels = default_order
                                         , ordered = TRUE)
  return(df)
}

numeric_vars <- c("person_income","person_emp_length","loan_amnt",
                  "loan_int_rate","loan_percent_income","cb_person_cred_hist_length")
loans <- factorize(loans)
#a subject's age and their credit history length is highly correlated
#thus I will remove one (prob a person's age)
loans <- loans %>% select(!c(person_age))

#splitting data into test and training data
train_df <- loans %>% sample_frac(size = 0.7)
test_df <- loans %>% anti_join(train_df)
x_train <- train_df %>% select(!c(loan_status))
y_train <- train_df$loan_status
x_test <- test_df %>% select(!c(loan_status))
y_test <- test_df$loan_status

#training the model
rand_forest_md <- randomForest(x=x_train, y=y_train, importance=TRUE)
#plotting the number of trees vs error rate
plot(rand_forest_md)
#displaying variable importance
importance(rand_forest_md, type=1)
#testing the model
y_pred <- predict(rand_forest_md, x_test)
#confusion matrix
confusionMatrix(y_test, y_pred)
