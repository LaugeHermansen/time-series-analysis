setwd("~/Git_repos/time-series-analysis/Assignment 3")
library(forecast)

######### 1-5 ############


data <- read.table('A3Data.csv', header=TRUE, sep=",")
names(data)
par(mfrow=c(1,1))

plot(data$Denmark,       type="o", ylab="House prices Denmark", main="House prices Denmark", xlab="time step")
plot(data$InterestRate,  type="o", ylab="Interest rate",        main="Interest rate",        xlab="time step")
plot(data$InflationRate, type="o", ylab="Inflation rate",       main="Inflation rate",       xlab="time step")


Denmark.transform <- log(data$Denmark)
plot(Denmark.transform)
Denmark.transform <- diff(Denmark.transform)
plot(Denmark.transform)
acf(Denmark.transform, na.action=na.pass)
Denmark.transform <- diff(Denmark.transform, lag=4)
acf(Denmark.transform, na.action=na.pass)
pacf(Denmark.transform, na.action=na.pass)

Denmark.transform <- diff(diff(log(data$Denmark), lag=4, differences=1), lag=1, differences=1)
# Denmark.transform <- diff(log(data$Denmark), lag=4, differences=2)
InterestRate.transform <- diff(data$InterestRate, lag=1, difference=1)
InflationRate.transform <- diff(data$InflationRate, lag=1, difference=3)

data$Denmark.transform <- c(rep(NA, dim(data)[1] - length(Denmark.transform)), Denmark.transform)
data$InterestRate.transform <- c(rep(NA, dim(data)[1] - length(InterestRate.transform)), InterestRate.transform)
data$InflationRate.transform <- c(rep(NA, dim(data)[1] - length(InflationRate.transform)), InflationRate.transform)

plot(data$Denmark.transform, type='l')
plot(data$InterestRate.transform, type='l')
plot(data$InflationRate.transform, type='l')

pacf(data$Denmark.transform, na.action=na.pass)
pacf(data$InterestRate.transform, na.action=na.pass)
pacf(data$InflationRate.transform, na.action=na.pass)

acf(data$Denmark.transform, na.action=na.pass)
acf(data$InterestRate.transform, na.action=na.pass)
acf(data$InflationRate.transform, na.action=na.pass)

m1 <- arima(log(data$Denmark),
                                 order=c(0,1,1),
                                 seasonal=list(order = c(0,1,1),period=4),
                                 include.mean = TRUE)

m2 <- arima(log(data$Denmark),
                                 order=c(0,1,1),
                                 seasonal=list(order = c(1,0,1),period=4),
                                 include.mean = TRUE)

mean(model.Denmark.transform$residuals, na.rm=TRUE)
acf(model.Denmark.transform$residuals, na.action=na.pass)
pacf(model.Denmark.transform$residuals, na.action=na.pass)

checkresiduals(m1)

qqnorm(model.Denmark.transform$residuals)
qqline(model.Denmark.transform$residuals)

preds.transform <- predict(model.Denmark.transform, n.ahead=6)$pred

tmp <- Denmark.transform
tmp[(length(tmp)-5):length(tmp)] <- preds.transform
test <- diffinv(tmp, differences=2, xi=log(data$Denmark[1:2]))

plot(data$Denmark)
lines(exp(test))

sign.test <- function(x){
  signs <- sign(na.omit(x))
  return(binom.test(sum(diff(signs) != 0), length(signs)))
}

sign.test(m1$residuals)

t.test(m1$residuals)

######## 5-

