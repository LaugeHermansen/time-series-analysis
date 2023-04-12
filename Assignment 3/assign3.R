setwd("~/Git_repos/time-series-analysis/Assignment 3")
library(forecast)

data <- read.table('A3Data.csv', header=TRUE, sep=",")
names(data)
par(mfrow=c(1,1))

plot(data$Denmark,       type="o", ylab="House prices Denmark", main="House prices Denmark", xlab="time step")
plot(data$InterestRate,  type="o", ylab="Interest rate",        main="Interest rate",        xlab="time step")
plot(data$InflationRate, type="o", ylab="Inflation rate",       main="Inflation rate",       xlab="time step")


data$Denmark.transform <- c(NA, diff(log(data$Denmark), lag=1, differences=1))
# data$Denmark.transform <- c(NA, NA, NA,NA,NA, diff(diff(log(data$Denmark), lag=1, differences=1), lag=4,differences=1))
data$InterestRate.transform <- c(NA, diff(data$InterestRate, lag=1, difference=1))
data$InflationRate.transform <- c(NA,NA,NA,diff(data$InflationRate, lag=1, difference=3))

plot(data$Denmark)
plot(diff(diff(log(data$Denmark),lag=4), lag=1), type="l")
plot(diff(diff(log(data$Denmark),lag=1), lag=4), type="l")


plot(data$Denmark.transform, type='l')
plot(data$InterestRate.transform, type='l')
plot(data$InflationRate.transform, type='l')

pacf(data$Denmark.transform, na.action=na.pass)
pacf(data$InterestRate.transform, na.action=na.pass)
pacf(data$InflationRate.transform, na.action=na.pass)

acf(data$Denmark.transform, na.action=na.pass)
acf(data$InterestRate.transform, na.action=na.pass)
acf(data$InflationRate.transform, na.action=na.pass)

model.Denmark.transform <- arima(data$Denmark.transform,
                                 order=c(3,0,0),
                                 seasonal=list(order = c(0,0,0),period=4),
                                 include.mean = FALSE)
mean(model.Denmark.transform$residuals, na.rm=TRUE)
acf(model.Denmark.transform$residuals, na.action=na.pass)
pacf(model.Denmark.transform$residuals, na.action=na.pass)

checkresiduals(model.Denmark.transform)

qqnorm(model.Denmark.transform$residuals)
qqline(model.Denmark.transform$residuals)



pred <- function(model, xreg){
  
  for (i in 1:){
    
  }
}
