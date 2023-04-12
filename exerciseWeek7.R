library(mltools)
library(data.table)
library(forecast)

set.seed(1)
nn <- 140000
x1 <- rnorm(nn)
x2 <- rnorm(nn)
y  <- numeric(nn); y[1] <- 0
eta  <- numeric(nn); eta[1] <- 0
theta <- 0.25
beta1  <- 1.5
beta2 <- -0.7
for (ii in 2:nn){
  eta[ii] <- theta * eta[ii - 1]  + rnorm(1)
  y[ii] <- beta1*x1[ii] + beta2 * x2[ii]+eta[ii]
}
Y <- cumsum(y)
Y12 <- diffinv(y,12)[-(1:12)]
Y_1_12 <-cumsum(diffinv(y,12)[-(1:12)])
(test1 <- arima(y,order=c(1,0,0),xreg=cbind(x1,x2)))
(test2 <- arima(Y,order=c(1,1,0),xreg=cbind(cumsum(x1),cumsum(x2))))
(test3 <- arima(Y12,order=c(1,0,0),seasonal=list(order=c(0,1,0),period=12),xreg=cbind(diffinv(x1,12)[-(1:12)],diffinv(x2,12)[-(1:12)])))

(testWRONG <- arima(Y,order=c(1,1,0),xreg=cbind(x1,x2)))
(test4 <- arima(Y_1_12,order=c(1,1,0),seasonal=list(order=c(0,1,0),period=12),xreg=cbind(cumsum(diffinv(x1,12)[-(1:12)]),cumsum(diffinv(x2,12)[-(1:12)]))))






