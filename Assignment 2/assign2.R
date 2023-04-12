########## Question 1 ###################################333


data <- read.table('A2_sales.txt', header=TRUE)



n_realizations <- 10
n_steps <- 200

my_colors = hcl.colors(n_realizations, palette = "Spectral")

epsilon <- matrix(rnorm(n_realizations*n_steps, sd=0.4), ncol = n_steps)
epsilon <- as.data.frame(epsilon)

X = as.data.frame(matrix(0,n_realizations, n_steps))

Xt <- c(rep(0,10))

for (i in 1:n_steps){
  Xt <- 0.8*Xt
  Xt <- epsilon[i] + Xt
  if (i>1){Xt <- 0.8*epsilon[i-1] + Xt}
  if (i>2){Xt <- -0.5*epsilon[i-2] + Xt}
  X[i] <- Xt
}

plot(y=c(X[1,1:n_steps]), x=1:n_steps, type="l", col=my_colors[1],ylim = c(min(X),max(X)))
for (i in 2:n_realizations){
  if (i == n_realizations){
    col<-"black"
    lwd <- 2
    }
  else {
    col <- my_colors[i]
    lwd  <- NULL}
  lines(y=c(X[i,1:n_steps]), x=1:n_steps, type="l", col=col, lwd=lwd)
}

ACF <- acf(t(X), plot=FALSE, type=c("correlation"))
PACF <- pacf(t(X), plot=FALSE, type=c("correlation"))


ACF_hat <- as.data.frame(matrix(0,n_realizations,as.integer(n_steps/2)))

for (lack in 1:dim(ACF_hat)[2]){
  temp <- c()
  for (r in 1:n_realizations)
  {
    temp <- c(temp, cov(as.numeric(X[r,1:(n_steps-(lack-1))]),as.numeric(X[r,(1+(lack-1)):n_steps])))
  }
  ACF_hat[lack] <- temp
  
}

n <- dim(ACF_hat)[2]

plot(y=c(ACF_hat[1,1:n]), x=1:n, type="l", col=my_colors[1],ylim = c(min(ACF_hat),max(ACF_hat)))
for (i in 2:n_realizations){
  if (i == n_realizations){
    col<-"black"
      lwd <- 2
  }
  else {
    col <- my_colors[i]
    lwd  <- NULL}
  lines(y=c(ACF_hat[i,1:n]), x=1:n, type="l", col=col, lwd=lwd)
}

####################### Question 3 #############################################


run_question_3 <- function(){

series_len <- 300
n_real <- 100



sigmas <- c(0.1, 5)
phi2s <- c(0.52, 0.98)
n_exp = length(sigmas)*length(phi2s)

my_colors = hcl.colors(n_exp, palette = "Spectral")

eps <- as.data.frame(matrix(rnorm(series_len*n_real),nrow=n_real, ncol=series_len))
X <- as.data.frame(matrix(0,nrow=n_real,ncol=series_len))


params <- as.data.frame(matrix(ncol=3, nrow=n_exp))
names(params)=c("exp","phi2","sigma")
exp <- 1
for (phi2 in phi2s){ for (sigma in sigmas){
    params$exp[exp] <- exp
    params$phi2[exp] <- phi2
    params$sigma[exp] <- sigma
  exp <- exp + 1
}}
    
phi1_hat <- matrix(nrow=n_real, ncol=n_exp)
phi2_hat <- matrix(nrow=n_real, ncol=n_exp)

for (exp in 1:n_exp){
  sigma <- params$sigma[exp]
  phi2 <- params$phi2[exp]
  print(c("sigma", sigma))
  print(c("phi2", phi2))
    X[1] <- eps[1]*sigma
    X[2] <- eps[2]*sigma + 1.5*X[1]
    for (i in 3:series_len){
      X[i] <- eps[i]*sigma + 1.5*X[i-1] - phi2*X[i-2]
    }
    for (i in 1:n_real){
      arma <- arima(as.numeric(X[i,1:series_len]), c(2,0,0), include.mean=FALSE)
      phi1_hat[i, exp] <- arma$coef[1]
      phi2_hat[i, exp] <- -arma$coef[2]
    }
  }

plot(c(),
     xlim=c(min(phi1_hat),max(phi1_hat)),
     ylim=c(min(phi2_hat),max(phi2_hat)),
     xlab="phi1",
     ylab="phi2",
     )
for (exp in 1:n_exp){
  sigma <- params$sigma[exp]
  phi2 <- params$phi2[exp]
  print(c("exp",exp))
  points(phi1_hat[1:n_real,exp],phi2_hat[1:n_real,exp],col=my_colors[exp])
  
}

par(mfrow=c(2,2))
for (exp in 1:n_exp){
  sigma <- params$sigma[exp]
  phi2 <- params$phi2[exp]
  print(c("exp",exp))
  hist(phi2_hat[1:n_real,exp])
  
}
}

run_question_3()

