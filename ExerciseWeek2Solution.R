n <- 1E+2
N <- 1 # Run a single simulation. Increase N to run multiple simulations
OLSErrors <- WLSErrors <- c() 
for(i in 1:N){
  x <- runif(n,20,120)
  
  y <- numeric(n)
  eps <- rnorm(n)/5
  theta <- c(1500,25) # True parameters
  y <- theta[1] + theta[2]*x+x^2*eps

  
  # Feature matrix
  X <- cbind(1,x)
  Sigmay <- diag(n) # initial guess of correlation structure
  for(i in 1:5){ 
    thetahat <- solve(t(X)%*%solve(Sigmay)%*%X)%*%(t(X)%*%solve(Sigmay)%*%y) # Estimate parameters using currently assumed correlation structure
    eps <- y - X%*%thetahat # Compute residuals for these parameter estimates
    #Sigmay <- diag(predict(lm(abs(eps) ~ abs(x) -1))^2)
    Sigmay <- diag(predict(lm(I(eps^2) ~ I(x^4) -1)))
  }
  (OLS <- solve(t(X)%*%X)%*%t(X)%*%y)
  (WLS <- thetahat)
  
  
  plot(y ~ x,frame.plot=FALSE)
  xgrid <- seq(min(x)-100,max(x)+100,len=100)
  Xgrid <- cbind(1,xgrid) #,xgrid^2
  lines(Xgrid%*%OLS ~ xgrid,col='steelblue')
  lines(Xgrid%*%WLS ~ xgrid,col='tomato')
  lines(Xgrid%*%theta[1:2] ~ xgrid,col='black')
  legend("topleft",c("True relation","OLS fit","WLS fit"),col=c("black","steelblue","tomato"),lty=1,bty='n',lwd=2)
  
  
  # Assumed variance estimating the variance from either eps^2 or abs(eps)
  plot((sort(x)^2/5)^2 ~ sort(x),type='l',xlab="x",ylab="y",frame.plot=FALSE)
  lines(predict(lm(I(eps[order(x)]^2) ~ I(sort(x)^4) -1)) ~ sort(x),col='tomato')
  lines(predict(lm(abs(eps[order(x)]) ~ abs(sort(x)) -1))^2 ~ sort(x),col='seagreen')
  legend("topleft",c("True variance","Square-based variance","Abselute-based variance"),col=c("black","tomato","seagreen"),lty=1,bty='n',lwd=2)
  
  OLSErrors <- c(OLSErrors,OLS[2]-theta[2])
  WLSErrors <- c(WLSErrors,WLS[2]-theta[2])
}


sqrt(mean(OLSErrors^2))
sqrt(mean(WLSErrors^2))

mean(abs(OLSErrors))
mean(abs(WLSErrors))
