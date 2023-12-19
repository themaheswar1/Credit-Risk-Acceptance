library(readxl)
loandata <- read_excel("C:/Users/thema/Desktop/Prac/R Proj/Full project/loan_data.xlsx")
head(loandata)

#structure of data
str(loandata)

# checking for the missing data
any(is.na(loandata)) 

summary(loandata$term)

loandata$term <- as.factor(loandata$term) 
loandata$term

# analysing term as how much time it took person to repay
# Analyzing the person's grade
summary(loandata$grade)
loandata$grade <- as.factor(loandata$grade)
loandata$grade

table(loandata$grade)
#Analyzing home ownership and converting into categorical

summary(loandata$home_ownership)
loandata$home_ownership <- as.factor(loandata$home_ownership)
loandata$home_ownership
table(loandata$home_ownership)

# loan_status to factor
summary(loandata$loan_status)
loandata$loan_status <- as.factor(loandata$loan_status)
loandata$loan_status
table(loandata$loan_status)


library(dplyr)
loandata <- loandata %>% mutate(charge_off = ifelse(loan_status == "Charged Off", 1, 0))
#This is the pipe operator (%>%) used to pass dataframe loandata to the next operation.
# data Splitting
set.seed(567)

#Store training set
train <- sample(1:nrow(loandata), 2/3 * nrow(loandata))
training_set <- loandata[train, ] # creating training set
test_set <- loandata[-train, ]


install.packages("randomForest")
library(randomForest)
rf_model <- randomForest(charge_off ~ home_ownership + annual_inc + loan_amnt + term + int_rate +
                           grade + fico_score + inq_last_6mths, data = training_set)

# Predicting probability of loan Charge Off
# Make predictions using created LR model

predictions <- predict(rf_model, newdata = test_set, type = "response") 
print(rf_model)
# Make predictions using the random forest model on the test set

rf_predictions <- predict(rf_model, newdata = test_set, type = "response")

# Assuming 'charge_off' is binary, convert predictions to binary values (0 or 1)
rf_predictions_binary <- ifelse(rf_predictions > 0.5, 1, 0)

# Evaluate the performance of the random forest model
rf_conf_matrix <- table(test_set$charge_off, rf_predictions_binary)
rf_accuracy <- sum(diag(rf_conf_matrix)) / nrow(test_set)


print("Random Forest Model Confusion Matrix:")
print(rf_conf_matrix)
print("Random Forest Model Accuracy:")
print(rf_accuracy)


# Now we will be giving the random input data of Certain persons who came 
# to apply loan or anything Today .
new_data <- data.frame(
  home_ownership = c("OWN", "RENT", "MORTGAGE"),
  annual_inc = c(50000, 60000, 70000),
  loan_amnt = c(10000, 15000, 20000),
  term = c("36 months", "60 months", "36 months"),
  int_rate = c(10, 12, 15),
  grade = c("A", "B", "C"),
  fico_score = c(720, 680, 650),
  inq_last_6mths = c(2, 1, 3)
)


# Convert categorical variables to factors with the same levels as in the training data
new_data$home_ownership <- factor(new_data$home_ownership, levels = levels(training_set$home_ownership))
new_data$term <- factor(new_data$term, levels = levels(training_set$term))
new_data$grade <- factor(new_data$grade, levels = levels(training_set$grade))
# Make predictions for new data

new_predictions <- predict(rf_model, newdata = new_data, type = "response")
# Print credit risk predictions for new data

credit_risk <- ifelse(new_predictions > 0.80, "High Risk", "Low Risk")
print("Credit Risk Predictions for New Data:")
print(credit_risk)


