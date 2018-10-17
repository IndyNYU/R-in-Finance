# FRE 6871 - Final Project by Yidi Wang
# 5/10/2018

# My project is divided by two Part.
# Part 1. Cover as much as possible what I have learned from the textbook and class.
# Part 2. Do machine learning prediction about the Kaggle Titanic Prediction.

# Part 1
# Although the data is quite simple, it is worthwhile to learn from it.
# THis part is organized as the following:
# Introduction and Goal Statement.
# 1. Set the enviroment.
# 2. Load the data.
# 3. Do data visualization and statistics analysis.
# 4. Do analysis of variance.
# 5. Time Series Analyis.

# Introduction
# After the whole semester working with R with the help of professor and classmates,
# I think I make a greate progess in this field.
# Honestly speaking, I really enjoy data analysis with the help of R.
# I choose to work with the classic "iris" dataset to do linear regression and anova analysis.

# 1. Set the working enviroment and load the data.

# 1.1 Set the working drectory to the disp D and the final project file.
#     The setting of working enviroment is very important.
# 1.2 Set efficient digis equal to 7, which is quite reasonable.
rm(list=ls())
setwd('D:/R/Final Project/')
options(digits=7, scipen=0)
opar <- par(no.readonly=TRUE)

# 2. Load the data.
# 2.1 Have an overview of the data.
data("iris")
View(iris)
str(iris)
summary(iris)
# This is a dataset about three kinds of flowers.
# With the data of the length and width of the sepal, length and width of the petal.

# 2.2 Check if there are any missing values.
# Here I want to state the importance of working with the misssing values.

sum(is.na(iris))
# It shows there aren't any missing values.
# I prefer to work with complete dataset, which is easy to work with.
#   Most statistical methods assume that the input data are complete and don't include missing values.
#   But in reality, there are so many missing data for different reasons.
#   There are two popular methods about dealing with the missing data.
#   Either delete the missing data or substitute it.

# 3. Do data visualization and statistics analysis.
# 3.1 Work with graphs.
attach(iris)
plot(Sepal.Width,Sepal.Length)
plot(Petal.Width,Petal.Length)
detach(iris)
# According to this two plots
# I conclude that there may exist a positive relationship between the Petal.Length and Petal Width.

# 3.2 Work with scatter plots and line plots.
attach(iris)
plot(Petal.Width,Petal.Length,pch=21)
abline(lm(Petal.Length~Petal.Width),lty=5,col="red")
title("Regression of Petal Length on Petal Width.")
detach(iris)

# 3.3 Combining graphs.
# In order to have a better overview of the dataset, get a combing graphs analysis.
attach(iris)
par(mfrow = c(2,2))
hist(Petal.Length, main = "Histogram of Petal.Length")
boxplot(Petal.Width, main = "Boxplot of Petal.Width")
plot(Petal.Width, Petal.Length, pch = 21)
hist(Sepal.Length, main = "Histogram of Sepal.Length")
detach(iris)

# 3.4 Data Analysis of each species of the flower.
setosa <- subset(iris, iris$Species == "setosa")
versicolor <- subset(iris, iris$Species == "versicolor")
virginica <- subset(iris, iris$Species == "virginica")

# 3.5 Plot the relationship between Petal.Length and Petal.Width.
par(mfrow = c(1,1))
attach(setosa)
plot(Petal.Width,Petal.Length,pch=21)
abline(lm(Petal.Length~Petal.Width),lty=5,col="red")
title("Regression of Petal Length on Petal Width for setosa.")
detach(setosa)

attach(versicolor)
plot(Petal.Width,Petal.Length,pch=21)
abline(lm(Petal.Length~Petal.Width),lty=5,col="red")
title("Regression of Petal Length on Petal Width for versicolor.")
detach(versicolor)

attach(virginica)
plot(Petal.Width,Petal.Length,pch=21)
abline(lm(Petal.Length~Petal.Width),lty=5,col="red")
title("Regression of Petal Length on Petal Width for versicolor.")
detach(virginica)
# So after subsetting and plot the relationship for each, there doesn't show an obvious relationship between length and width.
# The graphs are really easy for us to analyze the relationship between variables.
# I am fond of working directly with graphs, and it's useful in the communication with cilents and co-workers.

# 4. Do analysis of variance.
# The meaning of ANOVA technology is used to analyze a wide variety of experimental design.
# Try to understand the difference with different groups.

# 4.1 Try to get the distribution between three kinds of flowers.
attach(iris)
table(Species)
aggregate(Sepal.Length, by = list(Species),FUN = mean)
aggregate(Sepal.Width, by = list(Species), FUN = mean)
aggregate(Petal.Length, by = list(Species), FUN = mean)
aggregate(Petal.Width, by = list(Species), FUN = mean)

# 4.2 According to the distribution, it seems that the Petal.Width show obvious difference between different groups.
# Use aovna to analyze the data.
fit <- aov(Petal.Width ~ Species)
summary(fit)

# 4.3 Plot the mean according to the original data.
library(gplots)
plotmeans(Petal.Width ~ Species, xlab = "Kind of Flower", ylab = "Petal.Width",
          main = "Mean PlOt")
# According to the plots, it is obviously showed that different kinds of flowers have different Petal Width.

# 4.4 Tukey HSD pairwise group comparisions.
# Analyze the confidence interval for the mean of different groups.
# Plot the outcomes to show directly the outcomes.
TukeyHSD(fit)
par(las = 2)
par(mar = c(5,8,4,2))
plot(TukeyHSD(fit))

# 4.5 Make use of the glht() function to analyze more specifically.
library(multcomp)
par(mar = c(5, 4, 6, 2))
tuk <- glht(fit, linfct= mcp(Species = "Tukey"))
plot(cld(tuk, level = 0.05), col = "lightgrey")
# According to the plots, it verifies the above statement that the Petal.Width shows difference between groups.

# 4.6 Assess test assumptions.
# In the anova analysis, it's very important to test the assumptions.
# After analyze each outcome, try to test the assumption to get the overall understanding.
# Plot the QQ plot to find if the data is normally distributed with different groups.
library(car)
qqPlot(lm(Petal.Width ~ Species), simulate = T, main = "Q-Q Plot")

# Use the Bartlett's test to analyze if the data have the equality variances.
bartlett.test(Petal.Width ~ Species)

# 4.7 Use ANOVA as regression
library(multcomp)
levels(Species)
fit.aov <- aov(Petal.Width ~ Species)
summary(fit.aov)

# ANOVA analysis is different from the linar model.
fit.lm <- lm(Petal.Width ~ Species)
summary(fit.lm)

# 5. Time Series Analysis.
# Time series data is very common and it's of urgent significance to learn how to deal with time series data.
# Esepecially for financial data, it shows a strong relationship.
# For this part I will first construct a time series data, which is the stock price of MS for two years.

# 5.1 Create the time series data.
stock <- c(24.55, 23.57, 23.87, 25.82, 26.26, 24.93,
           27.57, 30.98, 30.98, 32.43, 40.20, 41.07,
           41.30, 44.59, 41.83, 42.35, 40.94, 43.71,
           46.00, 44.86, 47.50, 49.30, 48.16, 51.99)
tstock <- ts(stock, start = c(2016,1), frequency = 12)

# 5.2 Plot it.
plot(tstock)

# 5.3 Smoothing and seasonal decompositions.
library(forecast)
ylim <- c(min(stock), max(stock))
plot(tstock)
plot(ma(tstock, 1))
plot(ma(tstock, 2))

# 5.4 Fit the Time Series Model.
fit <- ets(tstock, model = "AAA")
fit 
accuracy(fit)

# 5.5 Predict with the ets model.
pred <- forecast(fit, 5)
plot(pred, main = "Forecast for the stock price of MS.")

# Part 2 Machine Learning prediction about the Kaggle Titanic Prediction.
# Reference: Kaggle ML Competition.
# Detailed Information is from Kaggle.

# 1. Data exploration and visualization
# Step 1. Load data and libraries.
# Step 2. Data cleaning and visualisation.
# Step 3. Data analysis.
# Step 4. Fit the machine learning algorithms.


# 1. Load data and libraries.
# 1.1 Load the lirbraries.
library('ggplot2')
library('ggthemes')
library('dplyr')
library('scales')
library('randomForest')
library('corrplot')
library('plyr')

# 1.2 Load the data.
# The original data is csv type.
Train <- read.csv('D:/R/Final Project/train.csv', stringsAsFactors = F)
Test  <- read.csv('D:/R/Final Project/test.csv', stringsAsFactors = F)

# Initial work with the data.
str(Train)
summary(Train)

# 2. Fill the missing data.
# Fill with the mean of each variable.
Train$Age[is.na(Train$Age)] = mean(Train$Age, na.rm = TRUE)
Test$Age[is.na(Test$Age)] = mean(Test$Age, na.rm = TRUE)

# 3. Data Analysis and create variables.
nonvars = c("PassengerId","Name","Ticket","Embarked","Cabin")
Train = Train[,!(names(Train) %in% nonvars)]
str(Train)

# Analyze the correlation between variables.
Train$Sex = as.numeric(Train$Sex)
Test$Sex = as.numeric(Test$Sex)
cor(Train)

# 4. Fit the LR Machine Learning Algorithm.
TitanicLog1 = glm(Survived~., data = Train, family = binomial)
summary(TitanicLog1)

# Analyze the outcomes.
TitanicLog2 = glm(Survived ~ . - Parch, data = Train, family = binomial)
summary(TitanicLog2)
TitanicLog3 = glm(Survived ~ . - Parch - Fare, data = Train, family = binomial)
summary(TitanicLog3)

# Test the accuracy.
predictTest = predict(TitanicLog3, type = "response", newdata = Test)

# Make prediction.
Test$Survived = as.numeric(predictTest >= 0.5)
table(Test$Survived)

Predictions = data.frame(Test[c("PassengerId","Survived")])
