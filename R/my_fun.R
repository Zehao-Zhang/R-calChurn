library(data.table)
library(lubridate)
library(dplyr)

# Task 1 & 2
  customer <- fread('/Users/zehao/Downloads/Archive/Day 5/data_customer.csv')
  personal <- fread('/Users/zehao/Downloads/Archive/Day 5/data_personal.csv')
  
  data <- merge(customer, personal, by="CustomerId")
  str(data)
  
  data$Exited <- as.factor(data$Exited)
  data$Gender <- as.factor(data$Gender)
  str(data)
  summary(data)

# Task 3
  #1. Create a model for churn probability using logistic regression. The outcome
  #variable is Exit (showing if a customer churned or not). Use as predictors the following variables: CreditScore, Gender, Age, Tenure, Balance, NumOfProducts, HasCrCard, IsActiveMember, EstimatedSalary. Hint: use the function glm() with the argument family="binomial".
  model <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
               data = data,
               family = "binomial")
  
  data$churn_probability <- predict(model, data, type = "response")
  str(data)
  
  max_customer <- data[which.max(data$churn_probability),]
  min_customer <- data[which.min(data$churn_probability),]
  max_customer
  min_customer
  
  
  # 4. Compute the average churn probability for men and women
  average_churn_prob_by_gender <- data %>%
    group_by(Gender) %>%
    summarise(average_churn_probability = mean(churn_probability))
  
  average_churn_prob_by_gender

# Task 4
  # 1. Create a function that takes as input a data frame and a variable name, and returns the average value of the variable for each category of
my_fun <- function(data, customerId){
  if(!customerId %in% data$CustomerId){
    stop("Customer ID not found")
         }
  
  result <- data[CustomerId == customerId]$churn_probability
  return(result)
}

