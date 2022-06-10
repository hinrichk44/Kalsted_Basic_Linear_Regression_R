#Here we are going to go through a simple linear regression project
#We will only be focusing on univariate regression in this project


#Here we are importing our dataset
#We have Years Experience as our independent variable and Salary as the dependent variable
#Is there a correlation between these two variables?
salary = read.csv('Salary_Data.csv')


#Here we are already splitting the data into training and test sets
#We are importing the caTools library to help us with that
library(caTools)


#Here we are setting our seed so we can get consistent results
set.seed(123)


#Here we are making our split 60-40, then creating the actual training and test sets
#In general we want 80-20, but there are only 30 rows of data in this very simple dataset
#It's common practice to split on the dependent variable, which is Salary in this example
split = sample.split(salary$Salary, SplitRatio = 0.6)
training_set = subset(salary, split == TRUE)
test_set = subset(salary, split == FALSE)


#We train and fit the linear model on our training set (duh)
#The formula sets up the relationship between the variables we are analyzing
#"Salary is proportional to YearsExperience"
regressor = lm(formula = Salary ~ YearsExperience, data = training_set)


#Here we are looking at a summary of our model we just trained
#Adjusted R-Squared is 0.96, which means there is a STRONG relationship between Salary and YearsExperience
#The coefficient on YearsExperience is 9245.5, so one extra year of experience is associated with $9245.5 increase in salary
#The p-value is extremely low and there are three stars next to YearsExperience so it's considered a statistically significant variable
summary(regressor)


#Here we are moving on to predicting our test set results
#We are creating a vector of predictions called y_pred
#So, essentially we are going to predict the data from the test set and compare it to the actual test set
y_pred = predict(regressor, newdata = test_set)

#Here are the predictions from the test set
#The first real value was $46,205. The first predicted value was $38,275. Basically an 8K discrepancy
y_pred


#Now we will visualize our results
#We will first import the ggplot2 library
library(ggplot2)

#Here we are building our visualization based on the training set only, which in this case will be a scatter plot
ggplot() + 
  #geom_point() signifies a scatter plot
  #The aesthetic function aes() helps us dictate the x and y axis of our graph
  #We are making the data points red colored
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             color = 'red') +
  #geom_line() signifies a line plot. We will be plotting our regression line through the observed data points
  #The x axis will still be YearsExperience, but our y-axis will be the TRAINING SET predictions
  #We will make the regression line blue
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') + 
  ggtitle("Salary vs. Years Experience (Training Set") +
  xlab("Years of Experience") +
  ylab("Salary")




#Here we are building our visualization based on the test set only, which in this case will be a scatter plot
ggplot() + 
  #geom_point() signifies a scatter plot
  #The aesthetic function aes() helps us dictate the x and y axis of our graph
  #We are making the data points red colored
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             color = 'red') +
  #geom_line() signifies a line plot. We will be plotting our regression line through the observed data points
  #The x axis will still be YearsExperience, but our y-axis will be the TRAINING SET predictions
  #We are not changing the prediction line. The line stays the same. We are seeing how the line fits the observed test set data
  #We will make the regression line blue
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') + 
  ggtitle("Salary vs. Years Experience (Test Set") +
  xlab("Years of Experience") +
  ylab("Salary")
