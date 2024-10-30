library(tidyverse)
library(randomForest)
library(caret)
#loans and test (for submission) were imported

set.seed(48)
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

loans <- factorize(loans)
submission <- factorize(submission)
#a subject's age and their credit history length is highly correlated
#thus I will remove one (prob a person's age)
#also amount loaned and the loan as percent of income are highly correlated
#will remove loan as percent of income
loans <- loans %>% select(!c(person_age, loan_percent_income))
sub_id <- submission$id
submission <- submission %>% select(!c(person_age, id, loan_percent_income))

#turning loan_status to a factor
loans$loan_status <- factor(loans$loan_status, levels = c(1, 0),labels = c("accepted", "rejected"))


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
                        "cb_person_default_on_file", "loan_status")
  df_scaled <- data.frame(scale(df[, numeric_vars], 
                                   center = center_, scale = deviation))
  df_scaled <- cbind.data.frame(df_scaled, df[, categorical_vars])
  return(df_scaled)
}
train_param <- get_center_scale(train_data = train_df)
train_scaled <- scale_and_join(train_df, train_param$center, train_param$scale_)
test_scaled <- scale_and_join(test_df, train_param$center, train_param$scale_)

x_train <- train_scaled %>% select(!c(loan_status))
y_train <- train_scaled$loan_status
x_test <- test_scaled %>% select(!c(loan_status))
y_test <- test_scaled$loan_status

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

#first submission before I scale and remove less significant variables
#id was not in training set
#the model predicted whether a person is rejected for some reason 
#(chose rejected as positive outcome) REMOVE WHEN THAT IS CHANGED
submit_pred <- predict(rand_forest_md, submission, type = "prob")
submit_pred <- data.frame(id = sub_id, loan_status = submit_pred)
#set current working directory to submissions folder
write.table(submit_pred, file = "rand_forest1.csv", sep = ",", dec = ".", row.names = FALSE)
