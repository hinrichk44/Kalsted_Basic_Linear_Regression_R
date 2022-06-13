#Here we will doing a MULTIPLE linear regression project in R
#We have data on 50 startups
#We have their R&D spending, their administrative costs, their marketing costs, their location and their profit


#Here we are importing the dataset
#We have 50 observations of five variables
startups = read.csv('startups.csv')


#The first thing we will do is encode the State column as a categorical/dummy variable
#There are only three values in the State column (NY, CA, FL)
#New York = 1, California = 2, and Florida = 3
#In general you probably want 0,1,2 not 1,2,3
startups$State = factor(startups$State,
                        levels = c('New York', 'California', 'Florida'),
                        labels = c(1,2,3))



#Here we are already splitting the data into training and test sets
#We are importing the caTools library to help us with that
library(caTools)


#Here we are setting our seed so we can get consistent results
set.seed(123)


#Here we are making our split 80-20, then creating the actual training and test sets
#It's common practice to split on the dependent variable, which is Profit in this example
split = sample.split(startups$Profit, SplitRatio = 0.8)
training_set = subset(startups, split == TRUE)
test_set = subset(startups, split == FALSE)


#Here we are going to train/fit our multiple linear regression model to the training set
#We use the period after ~ to denote all variables are being used to explain Profit
#"Profit is a linear combination of the independent variables"
#You could separate the independent variables using + if you wanted to be selective
regressor = lm(formula = Profit ~ ., data = training_set)


#Here is a summary of our model
#As of now only R&D Spending is considered statistically significant
#Adjusted R-Squared is .95 though, which is pretty impressive
#It looks like R did not fall for the dummy variable trap. It was smart enough to make distinctions
summary(regressor)


#Now we are going to predict the test set results
#We are creating a vector of predictions called y_pred
#So, essentially we are going to predict the data from the test set and compare it to the actual test set
y_pred = predict(regressor, newdata = test_set)

#Here are the predictions from the test set
#The first real value was $191,050. The first predicted value was $182,097. Basically a 9K discrepancy
y_pred


#Here we are going to build the optimal model using Backward Elimination
#We are going to re-run our model, but this time manually entering each independent variable
#We will then work backward and eliminate each variable that does not seem significant 
#We are going to use the entire dataset for evaluating the model in this instance.
#We are not focused on predictions for this part, so no need to split. We simply want to look at significance
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = startups)


#Here is the summary of our model, this time on the whole dataset instead of just the training set
#The first step is to choose a significance level as a threshold. In this case we will use 0.05
#Step two is fit the full model with all possible predictors, which is what we did
#Step three is to consider the predictor with the highest p-value. If P > SL, then it's not worth it and drop it
#Then fit this model again but without the predictor you just dropped
summary(regressor)


#So, going by the formal steps of backwards elimination, State3 has the highest p-value (.943) and should be dropped
#Technically, since State is a dummy variable, you can just remove all of State, not just State3
#So, here we are creating our regressor without State
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, data = startups)

#Here is a summary of regressor without State
#The highest p-value this time is Administration at .602. Still higher than .05 so we will remove it
summary(regressor)

#So, here we are creating our regressor without Administration
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend, data = startups)

#Here is a summary of our regressor without Administration
#Marketing Spending is at 0.06, which is VERY close to 0.05
#For now, we will remove the variable, but there will be further evaluation down the line
summary(regressor)


#Here we are running our model with just R.D.Spend
regressor = lm(formula = Profit ~ R.D.Spend, data = startups)


#And here is a summary of our model with just R.D.Spend
summary(regressor)



#HERE IS AN AUTOMATIC IMPLEMENTATION OF BACKWARD ELIMINATION
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
