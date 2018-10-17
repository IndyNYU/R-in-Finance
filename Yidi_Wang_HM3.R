# FRE 6871 Homework 3 By Yidi Wang
# 4/18/2018

rm(list=ls())
setwd("D:/R")

# 1. For this problem we will be working with the data set 'cars' from the openintro package.
install.packages("openintro")
library(openintro)

# 1.1 Descriptive statistics.

# Load and explore the data set 'cars'.
data("cars")
summary(cars)
structure(cars)
View(cars)
class(cars)

# Create a dataframe named carSub that only includes the columns price, mpgCity and weight.
carsSub <- cars[c("price", "mpgCity", "weight")]
View(carsSub)

# Write a function that takes in a numeric vector and returns the descriptive statistics.
mystats <- function(x, na.omit = F) {
                if (na.omit)
                   x <- x[!is.na(x)]
                m <- mean(x)
                s <- sd(x)
                minimum <- min(x)
                maximum <- max(x)
                n_unique <- length(unique(x))
                return(c(m = m, s = s, minimum=minimum, maximum=maximum,n_unique=n_unique))
                }

# Using a function from the apply() family, run your function on carSub.
sapply(carsSub, mystats)

# Use aggregate() with carsSub to apply the mean function to each level of the variables drivetrain and type.
aggregate(carsSub, by=list(level=cars$driveTrain), mean)

# Use aggregate() with carsSub to apply the mean function to each combination of levels of the variables driventrain and type.
aggregate(carsSub, by=list(level1=cars$driveTrain, level2=cars$type), mean)

# 1.2 Frequency & contingency tables, associated chi-square tests.

# Create a one-way frequency table with categorical variable drivetrain.
mytable <- table(cars$driveTrain)
mytable

# Create a two way table with categorical variables drivetrain and type.
mytable2 <- table(cars$driveTrain, cars$type)
mytable2

# Similar variables to the last bullet, create a marginal frequency table with.
# values being the percent of the row and a bottom row which is the sum of each column.
addmargins(prop.table(mytable2))

# Use a chi-squared test to check if the two categorical variables are independent.
chisq.test(mytable2)
# The p-value is 0.03468 is larger than 0.01 and i couldn't reject the hypothesis.
# So they are actually independent.

# 1.3 Correlation and covariance.
# Produce the Covariance Matrix for carsSub.
cov(carsSub)

# Produce the Pearson Correlation Matrix for carsSub.
cor(carsSub, method = 'pearson')

# Using cor.test(), test the significance of the correlation between the price and weight variables.
cor.test(cars$price, cars$weight)
# So there exists correlation between the price and the weight.

# 1.4 t-tests.
# Create a dataframe named carsFR that is the subset of cars that only includes rows where driveTrain is either 'front' or 'rear'.
carsFR <- subset(cars, driveTrain=='front' | driveTrain=='rear')
View(carsFR)

# Using an independent t-test, determine whether there is a statistically significant difference between the mean mpgCity in the two groups.
t.test(carsFR$mpgCity, carsFR$driveTrain)

# 1.5 Nonparametric Statistics.
# Use the Wilcox rank sum test to test the same question above, to assess whether the Mile Per Gallon in each category are sampled from the same population.
wilcox.test(cars$mpgCity, cars$driveTrain)
View(cars)

# 2. For this problem we will continue working with the data set 'cars'.

# 2.1 Simple linear regression.
# Using simple linear regression, lm(), predict the mpgCity from weight.
myfit <- lm(mpgCity~weight, data=cars)

# 2.2 Polynomial Regression.
# Fit a polynomial regression to the same data.
fit2 <- lm(mpgCity~weight + I(weight^2), data=cars)

# Explain the coefficients and p-values.
summary(myfit)
summary(fit2)
# Both regression coefficients are significant at the p < 0.0001 level.
# The R-squared values increase. The second model is better.

# 2.3 Scatterplot from the car library.
# Using scatterplot() from the car package, compare the simple linear and Loess curve plots pictorially.
library(car)
scatterplot(mpgCity~weight, data=cars)
scatterplot(mpgCity~weight+I(weight^2), data=cars)

# Confirm your conclusion from the R-squared values visually.

# 2.4 Multiple linear regression.
# Create a dataframe named carsSub that only includes the columns price, mpgCity and weight.
data(cars)
carsSub <- cars[c("price","mpgCity","weight")]
View(carsSub)

# look at the relationships between these variables using scatterplot() from the car package.
cor(carsSub)
library(car)
scatterplotMatrix(carsSub)

# Tell us 3 things you notice from the plots.
# 1. The price and mpgCity is negatively related.
# 2. The price and weight is positively related.
# 3. The mpgCity and weight is negatively related.

# Fit the multiple regression using lm() modeling mpgCity as a function of price and weight.
multifit <- lm(mpgCity~price + weight, data=carsSub)

# From the modeling results, what conclusions can you draw from the p-values.
summary(multifit)
# The p-value of price is a little large.

# Use confint(), produce the 95% confidence intervals for the coefficients of each variable.
confint(multifit)

# Using plot(fit).
plot(multifit)
# Based on these plots, I think, the four assumptions are satisfied.

# Using newer methods.
# Use the residplot() function to see if the errors follow a normal distribution.
residplot <- function(fit, nbreaks = 10) {
                z <- rstudent(fit)
                hist(z, breaks = nbreaks, freq = F)
                rug(jitter(z), col = "brown")
                curve(dnorm(x, mean=mean(z), sd=sd(z)),
                            add=T, col="blue", lwd=2)
                lines(density(z)$x, density(z)$y,
                      col="red", lwd=2, lty=2)
}
residplot(multifit)

# Use the crPlots() from the library 'car' to asses if we have met the linearity assumption.
library(car)
crPlots(multifit)

# Use the ncvTest() from the library 'car' to test if we have met the constant variance assumptions.
ncvTest(multifit)

# Use influencePlot() from the car package, determine which points are outliers.
outlierTest(multifit)

# Selecting the best regression model.
library(MASS)
stepAIC(multifit, direction = "backward")


# 3. For this problem we will be working with the data set "PlantGrwoth".
data("PlantGrowth")

# 3.1 One Way ANOVA.
# Load and explore the data set PlantGrowth.
str(PlantGrowth)
summary(PlantGrowth)
View(PlantGrowth)

# Create a talbe() of the group column to confirm a balanced design.
table(PlantGrowth$group)

# Use aggregate to find the group means.
aggregate(PlantGrowth$weight, by=list(PlantGrowth$group), FUN=mean)

# Use aggregate to find the group standard deviations.
aggregate(PlantGrowth$weight, by=list(PlantGrowth$group), FUN=sd)

# Use ANOVA test for group differences.
fit <- aov(PlantGrowth$weight~PlantGrowth$group)
summary(fit)

# Use TukeyHSD() to answer this.
TukeyHSD(fit)

# From the multicomp package, use glht() combined plot(cld()) to see this result.
library(multcomp)
tuk <- glht(fit)
plot(cld(tuk))

# Use bartlett.test() to determine if the variances in the groups differ significantly/
libray(car)
bartlett.test(PlantGrowth$weight~PlantGrowth$group)
qqPlot(lm(PlantGrowth$weight~PlantGrowth$group))

# Use outliers Test() from the car package.
outlierTest(fit)
