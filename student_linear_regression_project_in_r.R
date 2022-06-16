#Here is a simple Linear Regression project done in R

#Here we are importing our dataset
#Most files are comma separated, but this just happens to be semi-colon separated
#We have 395 observations of 33 variables
students <- read.csv('student-mat.csv',sep=';')


#Here we are taking a quick look at the dataset
head(students)

#Here is a quick statistical summary of our dataset
summary(students)

#Here we are checking if there are any null values in the dataset
#The answer is FALSE, so no null values to worry about this time
any(is.na(students))



#Now we are going to be creating visualizations detailing the correlation between some of our features
#We will import the ggplot2, ggthemes, and dplyr libraries to help us out
library(ggplot2)
#install.packages('ggthemes')
library(ggthemes)
library(dplyr)


#Here we are grabbing just the numeric columns from our dataset
num_cols = sapply(students, is.numeric)

#Now we are creating correlations between our numeric columns
#The comma before num_cols is so we make sure we are grabbing all numeric columns
#We are using num_cols as a FILTER on the students dataset
corr_data = cor(students[, num_cols])
print(corr_data)

#A few extra R packages are needed for our visualizations
#install.packages('corrgram')
library(corrgram)

#install.packages('corrplot')
library(corrplot)

#Here we are printing a correlation map for our numeric variables
#This visualization looks nicer than just printing the columns themselves
print(corrplot(corr_data, method = 'color'))


#Here we are going to use corrgram to draw a "correlogram"
corrgram(corr_data,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)


#Here we are making a histogram detailing the distribution of G3
#There are lots of "0" scores for G3, so students are failing or not even showing up
#Most common G3 score is 10
ggplot(students, aes(x=G3)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')



#We are going to start building our linear regression model

#Here we are going to split the data into a training and test set
#First we will import the caTools library, which helps us split data
library(caTools)

#Then we will set the seed so we get consistent results
set.seed(101)

#Now we do the actual splitting. In general you can split on any column, but it's good practice to split on the dependent variable
#We will split the data 70-30
split = sample.split(students$G3, SplitRatio = 0.7)
#Split == TRUE means here is the 70% of our data
training_set = subset(students, split == TRUE)
test_set = subset(students, split == FALSE)

#Here we are creating our model, which will be trained on the training set
regressor = lm(formula = G3 ~ ., data = training_set)

#Here is a summary of the model
#Adjusted R-Squared is 0.82, which is pretty good
#Looks like only G1, G2, and absences are considered statistically significant
#Remember that residuals are actual - predicted. You want the residuals to look like a normal distribution when plotted
#The Estimate is the value of the slope calculated by the regression
#The standard error is a measure of the variability of the estimate. Lower generally means better, but it's relative to coefficient on estimate.
#You would like to have the standard error be less than the coefficient
#The t-value of the estimate scores whether the coefficient is meaningful for the model.
#Probably won't use t-value on its own, but it is used to calculate p-value. 
#The p-value measures the probably that something is NOT RELEVANT. Meaning, a low p-value says a variable has a low chance of being NOT RELEVANT to the model
summary(regressor)


#Here we are extracting the residuals from our model
residuals = residuals(regressor)
#Then we are converting them into a dataframe
residuals = as.data.frame(residuals)


#Now let's plot the residuals using a histogram
ggplot(residuals, aes(residuals)) + geom_histogram(fill = 'blue', alpha = 0.5)


#A handy visualization device is to simply plot your model
#plot(model) will give you various charts relevant to your model
#In the console you just click "Return" and iterate through the plots
plot(regressor)


#Here we are moving on to predicting our test set results
#We are creating a vector of predictions called y_pred
#So, essentially we are going to predict the data from the test set and compare it to the actual test set
y_pred = predict(regressor, newdata = test_set)

#Here we are combining the predictions and actual results
#Then we are converting the combo into a dataframe
results = cbind(y_pred, test_set$G3)
colnames(results) = c('Predicted', 'Actual')
results = as.data.frame(results)


#When we saw a histogram of the residuals, we saw that there were negative values. The model was predicting negative results on some tests
#However, the lowest score you can get on a test is 0. 
#Let's create a function to convert negative values to 0.
#If the value is negative, the function will convert to 0. If not negative, the function will bring back the original value
to_zero = function(x){
  if (x < 0){
    return(0)
  }else{
    return(x)
  }
}

#Now we are applying our function to the results
#Notice how we are only applying it to the Predicted column in this instance since there are no negative values in the Actual column
results$Predicted = sapply(results$Predicted, to_zero)


#One good way of evaluating your model is to use mean squared error
#We get a value of 3.72
mse = mean((results$Actual - results$Predicted)^2)
print(mse)

#You can get the root mean square error from the mean square error
#We get 1.93
rmse = (mse^0.5)
print(rmse)


#Here we are going to calcuate the R-Squared value for the test set results
#First we start with the Sum of Squared Errors
SSE = sum((results$Predicted - results$Actual)^2)
#Then calculate the Sum of Squared Total
SST = sum( (mean(students$G3) - results$Actual)^2)
#Finally, we calcuate R-Squared. We get .82
#We're explaining about 82% of the variance in the test data
R2 = 1 - SSE/SST
R2
