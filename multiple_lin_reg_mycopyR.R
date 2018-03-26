# Multiple Linear Regression in R

# importing data

dataset = read.csv('50_Startups.csv')
#dataset = dataset[, 2:3]

# Encoding categorical data
dataset$State = factor(dataset$State, 
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# splitting the dataset into training set and test set
#install.packages('caTolls')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio=0.8)
training_set = subset(dataset, split==TRUE)
test_set = subset(dataset, split==FALSE)

# feature scaling
# training_set[, 2:3] = scale(training_set[, 2:3])
# test_set[, 2:3] = scale(test_set[, 2:3])

# Fitting Linear Regression to the Training Set
regressor = lm(formula = Profit ~.,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Building the Optimal Model using Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor)

# Automatic Backward Elimination
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)