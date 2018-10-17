rm(list=ls())
setwd('D:/')
options(digits=7, scipen=0)
opar <- par(no.readonly=TRUE)

# FRE 6871 Homework Assignment 5 by Yidi Wang
# 5/2/2018

# 1. Focus on Principal Components and factor analysis.
# 1.1 Dataset Wine.

# (1) Load, explore and prepare your data.
# Load the dataset according to the methods in the pdf.
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
                   sep = ",")
colnames(wine) <- c("Cvs","Alcohol","Malic acid","Ash","Alcalinity of ash",
                    "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols",
                    "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines",
                    "Proline")
# Explore the data.
View(wine)
summary(wine)
str(wine)

# (2) With an understanding of your goal, decide on PCA or EFA.
# In the discription of the data, it says that there are too many variables to explore
# and many measurements may be correlated. So it's natural to use PCA to do reduction.

# (3) I use PCA for this question, so i will leave this problem out.

# (4) Using fa.parallel(), decide on the number components to extract.
library(psych)
fa.parallel(wine[,-1], fa="pc", n.iter=100, show.legend=F, main="Screen plot with parallel analysis.")
# According to the parallel analysis and Kaisser-Harris criterion, pc=3 seems resonable.

# (5) Extract the components using principal() with rotate="none".
library(psych)
pc <- principal(wine[,-1], nfactors=3, rotate="none")
pc

# (6) Describe the following information based on the PCA analysis.
# PC1 is highly correlated with each variable except the Ash.
# PC2 is highly correlated with each variable except the Flavanoids.
# PC3 is not that highly correlated with each variable.

# According to h2, almost each variables is explained by these 3 PCs.
# However, the Malic acid, Magnesium, Nonflavanoid phenols is not that explained.

# According to the Proportion Var, 
# PC1 explains the 36% of the variance.
# PC2 explains the 19% of the variance.
# PC3 explains the 11% of the variance.

# (7) Rotate keeping the components orthogonal using principal() with varimax.
rc <- principal(wine[,-1], nfactors = 3, rotate = "varimax")
rc
# The components after purifying improves and the variance it explains more.

# (8) Compute your principal component scores.
#     Create a new dataframe with the original first column and your new component columns(s).
library(psych)
pc <- principal(wine[,-1], nfactors=3, score=T)
head(pc$scores)
cor(wine$Cvs,pc$scores)

# 1.2 Dataset Harman74.cor from the dataset library.
library(datasets)

# (1) Load, explore and prepare your data.
data("Harman74.cor")
View(Harman74.cor)
summary(Harman74.cor)

# (2) With an understanding of your goal, decide on PCA or EFA.
# Focus on the EFA according to the description of the dataset.

# (3) Choose a specific factoring method.
# Choose to use the 'maximum likelihood' approach.

# (4) Using fa.parallel() decide on the number components to extract.
library(psych)
fa.parallel(Harman74.cor$cov, n.obs=145, fa="both", n.iter=100,
            main="Scree plots with parallel analysis.", show.legend=F)
# According to the parallel analysis, try to extract 4 components.

# (5) Extract the components using fa() with rotate="none".
fa <- fa(Harman74.cor$cov, nfactors=4, rotate="none", fm="ml")
fa

# (6) Descrie the outcomes.
# ML1 is highly correlated with each variable.
# ML2-ML4 is not that correlated with each variable.

# Also, the overall variance explained is about 0.5, not that good.

# (7) Rotate keeping the components orthogonal with fa.varimax().
fa.varimax <- fa(Harman74.cor$cov, nfactors=4, rotate="varimax", fm="ml")
fa.varimax

# (8) Compute the factor scores.
fa.varimax$weights

# 2. Time Series
# For this problem we will be working with a data set containing tractor sales over time.
data = read.csv('http://ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-
Sales.csv')

# 2.1 The first step is to download the data as above and explore.
# (1) Understand what the data is and explore the data.
View(data)
# It is a data with the number of tractor sold from Jan 3rd to Dec 14th.
summary(data$Number.of.Tractor.Sold)

# (2) Create a time sereis object using ts().
data = ts(data[,2], start=c(2003,1), frequency=12)
data

# (3) Explore the time series with functions.
plot(data)
start(data)
end(data)
frequency(data)
window(data)

# (3.1) Plot the time series and three smoothed versions of the time series
#       using ma() and three different odd numbers for k.
library(forecast)
ylim <- c(min(data),max(data))
plot(data, main="tractor sales over time")
plot(ma(data, 3), main="Simple Moving Averages(k=3)", ylim=ylim)
plot(ma(data, 7), main="Simple Moving Averages(k=7)", ylim=ylim)
plot(ma(data, 15), main="Simple Moving Averages(k=15)", ylim=ylim)
# According to the plots, it is apparently that as k increases,
# the plot becomes increasingly smoothed.
# Also, it also shows an obvious trend in the plot.

# (3.2) Think about the data and whether or not it may have a seasonal aspect.
#       Look at the plot of your time series and decide what combination of
#       trend, seasonal and irregular components the time series may have.
# According to the data and the plot, I think it may have a seasonal term.
# I also think that it should be a multiplicative model of trend, seasonal and irregular components.

# (3.3) Use stl() to decompose the time series.
#       Recall that stl() can only handle additive models so need to do a log transformation.
ldata <- log(data)
plot(ldata, ylab="log of the tractors sales.")

# (3.4) View the components for each observations numerically and pictiorially using.
fit <- stl(ldata, s.window="period")
plot(fit)
fit$time.series
# 1. The trend term is about 5 and is pretty stationary.
# 2. The remainder is not that large, which means the model is pretty good.

# (3.5) If the data has a seasonal aspect to it then use monthplot() and seasonplot()
#       to help visualize the seasonal decomposition other points of view.
library(forecast)
monthplot(data, xlab="", ylab="")
seasonplot(data, year.labels="TRUE", main="")

# 2.2 For this problem we will continue with the tractor data set.

# (1) Using ets() and specifying the 'model' parameter based on your time series' error type.
#     trend type and seasonal type, fit the model and make a 1-step ahead forecast.
# I choose to use the tripl exponential model.
library(forecast)
fit <- ets(log(data), model="AAA")
fit
pred <- forecast(fit, 1)

plot(pred, main="Forecast for tractor sales.")
pred$mean <- exp(pred$mean)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)

p <- cbind(pred$mean,pred$lower,pred$upper)
dimnames(p)[[2]] <- c("mean","Lo 80","Lo 95","Hi 80","Hi 95")
p
# (2) Look at the output.

# (2.1) Based on the the alpha, what can you tell about distant vs most recent observations being considered in the forecast.
#       ALpha is about 0.7, it means should care about distant and recent for the level renewing.

# (2.2) What is the forecast?
#       The forecast is 563.9375.

# (2.3) What is the 95% confidence interval for this forecast?
#       [525.5816,605.0926]

# 2.3 ARIMA forecasting models.
# For this problem we will be using the yearly earnings data in "Earnings.txt".
earnings <- read.table("D:/R/Earnings.txt",
                       sep=",", 
                       col.names=c("id", "earning"))
View(earnings)
earning <- ts(earnings$earning,frequency=1)
earning

# (1) Ensure that the time series is stationary.
# (1a) Plot the time series.
plot(earning)
# It seems not that stable and transform with log function.

# (1b) Plot the adjusted time series.
plot(log(earning))
learning <- log(earning)
dearning <- diff(learning)
plot(dearning)

# (1c) Check if the resulting time series is indeed stationary by applying the ADF test.
library(forecast)
library(tseries)
adf.test(dearning)
# The p-value is 0.01 so it is stationary.

# (2) Identifying one or more reasonable models with the transformed time series.
# (2a) Create the ACF and the PACF plots.
Acf(dearning)
Pacf(dearning)

# (2b) According to the plots and table in the book, p=q=1.

# (3) Note that there are a few additional steps for seaonal terms.

# (4) Fit your ARIMA model using the original data set.
# (4a) Fit your model.
library(forecast)
fit <- arima(learning, order=c(1,1,1))
fit

# (4b) Look at the model accuracy() and identity the mean absolute percent error.
# mae = 0.1214483
accuracy(fit)

# (4c) Evaluate the model with qqnorm(), qqline(), and Box.test().
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals,type="Ljung-Box")

# (4d) Look at the resulting auto correlations using.
acf(ts(fit$residuals),main="ACF Residual")
acf(ts(fit$residuals),main="PACF Residual")

# (5) Forecast 4 steps into the future and plot your results.
forecast(fit, 4)
plot(forecast(fit, 4))
