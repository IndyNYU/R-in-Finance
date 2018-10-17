# FRE 6871 Homework 2 by Yidi Wang 4/10/2018

rm(list=ls())
dev.off(dev.list()["RStudioGD"])
options(digits=7, scipen=0);#options(digits=7, scipen=999);
opar <- par(no.readonly=TRUE)
setwd("D:/R")

# 1. For this problem we will be working with the data set 'state.x77'.
# 1.1 Load the data set and save it in a variable named 'states'.
data(state)
states <- state.x77

# 1.2 Analyze its attributes with the functions we have learned.
# Convert the matrix to dataframe.
states <- as.data.frame(states)

# Analyze its structure and descriptive statistics.
View(states)
class(states)
str(states)
summary(states)

# 1.3 Look at the data set 'state.region' 
#     and add it as a column to your dataframe.
View(state.region)
states['region'] <- state.region
View(states)

# 1.4 Make a new ordered factor column that takes the column 
#     representing 'percent high-school graduates' and splits it into three factors.
states <- within(states, {
                 states$percentage <- NA
                 states$percentage[states$`HS Grad` > 60] <- "high"
                 states$percentage[states$`HS Grad` >= 45 & states$`HS Grad` <= 60] <- "medium"
                 states$percentage[states$`HS Grad` < 45] <- "low"})
View(states)

# 1.5 Using the 'Population' and 'Area' columns make a column for Population per Square Mile.
states$PopulationPerSquareMile <- states$Population / states$Area
View(states)

# 1.6 Take out the Population, Frost and Area columns from the dataframe.
states <- states[c(-1,-7,-8)]
View(states)

# 1.7 Change the column name 'Life.Exp' to 'Exp.age'.
names(states)[3] <- 'Exp.age'
View(states)

# 1.8 Choose a random set of 10 states.
#     Find the average income of those 10 states.
#     Repeat the choosing and averaging 10,000 times
#     in a loop collecting the averages in a vector named tenAverages.
#     Plot a histogram of those 10,000 values.

mysample <- states[sample(1:nrow(states), 10, replace = FALSE), ]
View(mysample)
mean(mysample$Income)

mysamplerandom <- c()
for(i in 1:10000) {
  mysamplerandom <- states[sample(1:nrow(states), 10, replace = FALSE),]
  tenAverages[i] <- mean(mysamplerandom$Income)
  }
View(tenAverages)  
hist(tenAverages)

# 2. For this problem we wil be working with the data set 'iris'.
# 2.1 Read about the data set in the help, load and explore the data set.

setwd("D:/R")
data("iris")
help("iris")
class(iris)
str(iris)
summary(iris)
View(iris)

# 2.2 Use at least 3 mathematical functions on individual columns of the data
#     set to confirm at least 3 statistics you see when using summary.
min(iris$Sepal.Length)
median(iris$Sepal.Length)
mean(iris$Sepal.Length)

# 2.3 Using apply() on a subset of the data frame, find the average of the first
#     four columns and confirm the averages you see when you using summary.
apply(iris[,1:4], 2, mean)

# 2.4 Using character functions:
# i. Get a numeric vector each element of which is a count of the number of
#    letters in each column name("Sepal.Length" should be 12)
vector1 <- nchar(names(iris))
vector1
# ii. Get a character vector each element of which is the associated column
#     name without the first and the last letter.
vector2 <- substr(names(iris), start = 2, stop = vector1)
vector2

# 2.5 Write a function named 'AVG' as follows.
myfunction(iris)
myfunction <- function(arg1) 
  for (i in 1:length(names(arg1))) 
  if (is.numeric(arg1$names(arg1)[i])) print(mean(arg1[,1])) else print("Not Numeric")
myfunction(iris)

# 2.6 Using the 'reshape2' package reshape the iris data set using melt()
#     and dcast() to create a data frame of averages taht looks like.
install.packages("reshape2")
library(reshape2)
aggdata <- aggregate(iris, by = list(iris$Species), FUN = mean, na.rm = TRUE)
View(aggdata)
md <- t(aggdata)
View(md)

# 3.1 For this problem we will continue to explore the 'iris' data set.

data(iris)

# Plot 1. Make a barplot of the column representing Sepal Length.
View(iris)
barplot(iris$Sepal.Length)

# Plot 2. Cut the Sepal Length column into three.
counts <- table(iris$Sepal.Length, iris$Species)
counts
barplot(counts)
print("setosa")

# Plot 3. Creating a Mean Bar Plot.
par(mfrow = c(2,2))
means_SepalLength <- aggregate(iris$Sepal.Length, 
                               by = list(iris$Species),
                               FUN = mean)
means_SepalLength
means_SepalLength <- means_SepalLength[order(means_SepalLength$x),]
barplot(means_SepalLength$x)

# Plot 4. Fan Plot.
dev.off()
means_PetalLength <- aggregate(iris$Petal.Length,
                               by = list(iris$Species),
                               FUN = mean)
means_PetalLength
library(plotrix)
fan.plot(means_PetalLength$x)

# Plot 5. Histogram.
hist(iris$Sepal.Length, breaks = 20, col = "blue")
rug(jitter(iris$Sepal.Length))

# Plot 6. Kernal Density.
plot(density(iris$Petal.Width))
rug(iris$Petal.Width, col = "blue")

# Plot 7. Comparative kernel density plots.
install.packages("sm")
library(sm)
attach(iris)
sm.density.compare(Petal.Width, Species)
legend(locator(1), levels(Species))

# Plot 8. Parallel box plots.
boxplot(iris$Petal.Width ~ iris$Species)

# Plot 9. Box plots for two crossed factors.

# Plot 10. Dot plots.
newdata <- iris[order,(iris$Petal.Width),]
newdata$Speciesl <- factor(newdata$Species)
newdata$color[newdata$Species == "setosa"] <- "red"
newdata$color[newdata$Species == "versicolor"] <- "blue"
newdata$color[newdata$Species == "virginica"] <- "darkgreen"

dotchart(newdata$Petal.Width, labels = row.names(newdata),
        groups = newdata$Speciesl, gcolor = "black", color = newdata$color)
