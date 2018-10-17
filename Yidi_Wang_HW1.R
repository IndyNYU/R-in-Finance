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
for(i in 1:10000) {
  mysample <- states[sample(1:nrow(states), 10, replace = FALSE),]
  tenAverages[i] <- mean(mysample$Income)
  }
View(tenAverages)  
hist(tenAverages)
