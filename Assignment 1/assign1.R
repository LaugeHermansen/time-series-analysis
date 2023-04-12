data <- read.table('Assignement 1/A1_co2.txt', header=TRUE)
t = as.integer(rownames(data))
data <- cbind(data, t)

data_train <- data[data$year < 2018,]
data_test <- data[data$year >= 2018,]

N_train = nrow(data_train)
N_test = nrow(data_test)

# question 1

plot_data <- function(use_index = TRUE) {
  if (use_index) {
    plot(data_train$t, data_train$co2, type='l', xlab='Index',
         ylab='CO2', main='CO2 vs Index')
    lines(data_test$t, data_test$co2, type='l', col='red')
  } else {
    plot(data_train$time, data_train$co2, type='l', xlab='Time',
         ylab='CO2', main='CO2 vs Time')
    lines(data_test$time, data_test$co2, type='l', col='red')
  }
}

plot_data(use_index = FALSE)

# add legends to the plots
legend('topleft', legend=c('Training Data', 'Test Data'), col=c('black', 'red'), lty=1:1)

########### question 2 ##########

# 2.1

p <- 12
model2.1 <- lm(co2 ~ t + sin(2*pi/p*t) + cos(2*pi/p*t), data=data_train)

# 2.2
print(model2.1)

# 2.3
y_hat_OLS <- predict(model2.1, newdata=data)



# 2.4
# 2.5

f <- function(t){
  return(cbind(1,t,sin(2*pi/p*t),cos(2*pi/p*t)))
}

sigma_const <- function(rho, N){
  fun1 <- function(i,j) rho^abs(i-j)
  rc <- 1:N
  S <- outer(rc,rc,fun1)
  return(S)
}

sigma_inv_const <- function(rho, N){
  fun2 <- function(i,j){
    ret <- rep(0,N^2)
    ret[i==j] <- 1+rho^2
    ret[abs(i-j) == 1] <- -rho
    ret[((i == 1) & (j == 1)) | ((i == N) & (j == N))] = 1
    ret <- ret/(1-rho^2)
    return(ret)
  }
  rc <- 1:N
  s <- outer(rc,rc,fun2)
  return(s)
}


Sigma_inv <- diag(N_train)
rho <- 0

x <- f(data_train$t)
y <- data_train$co2
n_params <- dim(x)[2]

for (i in 1:5){
  # compute estimates of parameters
  a <- t(x)%*%Sigma_inv
  b <- solve(a%*%x)
  theta_hat <- b%*%(a%*%y)
  r <- y - x%*%theta_hat
  sigma_sq_hat <- (1/(N_train - n_params)
                 *t(r)%*%Sigma_inv%*%r
                 )[1,1]
  var_theta_hat <- sigma_sq_hat * b
  # print variances ish
  print(paste("iteration",i))
  print(cbind(theta_hat, sqrt(diag(var_theta_hat))))
  
  #update Sigma
  rho = cor(r[1:(N_train-1)], r[2:N_train])
  Sigma_inv <- sigma_inv_const(rho,N_train)
  }

y_hat_WLS <- f(data$t)%*%theta_hat

plot_data()
lines(data$t, y_hat_WLS, type="l", col='blue')
lines(data$t, y_hat_OLS, type="l", col='green')
legend('topleft', legend=c('Training Data', 'Test Data', 'WLS', 'OLS'),
       col=c('black', 'red', 'blue', 'green'), lty=1:1)


##################### question 3 ########################

L <- t(matrix(c(1,0,0,0,
             1,1,0,0,
             0,0,cos(2*pi/p), sin(2*pi/p),
             0,0,-sin(2*pi/p), cos(2*pi/p)), 4, 4))
L_inv <- solve(L)


L%*%t(f(4)) - t(f(5))

n <- 10
lambda <- 0.9
x_start <- f((-n+1):0)
y_start <- data$co2[1:n]
Sigma <- diag(1/lambda^((n-1):0))
Sigma_inv <- diag(lambda^((n-1):0))


L_inv%*%t(f(-10)) - t(f(-11))


h <- t(x_start)%*%Sigma_inv%*%y_start
F <- t(x_start)%*%Sigma_inv%*%x_start
theta_hat_LLT <- solve(F)%*%h


y_hat_local_trend <- c()

max_dif <- c()

for (i in n:(N_train-1)){

  y_hat_local_trend <- c(y_hat_local_trend,
                         (f(1)%*%theta_hat_LLT)[1,1])
  F <- F + lambda^i*(t(f(-i))%*%f(-i))
  F_test <- t(f((-i):0))%*%diag(lambda^((i):0))%*%f((-i):0)
  
  h <- lambda*(L_inv%*%h)+t(f(0))*data_train$co2[i+1]
  h_test <- t(f((-i):0))%*%diag(lambda^((i):0))%*%data$co2[1:(i+1)]
  max_dif <- c(max_dif , max(abs(h-h_test)))
  theta_hat_LLT <- solve(F)%*%h
}


plot(max_dif)
plot(y_hat_local_trend)
