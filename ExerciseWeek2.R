n <- 1E+2
x <- runif(n,20,120)

y <- numeric(n)
eps <- rnorm(n)/5
theta <- c(1500,25) # True parameters
y <- theta[1] + theta[2]*x+x^2*eps


# Feature matrix
X <- cbind(1,x)
(OLS <- solve(t(X)%*%X)%*%t(X)%*%y)
lmOLS <- lm(y ~ x)
summary(lmOLS)


## Use the code below as a starting point for the relaxation algorithm
####
Sigmay <- diag(n) # initial guess of correlation structure

thetahat <- solve(t(X)%*%solve(Sigmay)%*%X)%*%(t(X)%*%solve(Sigmay)%*%y) # Estimate parameters using currently assumed correlation structure
eps <- y - X%*%thetahat # Compute residuals for these parameter estimates

## Estimate the variance as a function of x. Notice that Var(x^2*eps)=x^4*Var(eps)
#Sigmay <- diag(predict(lm(abs(eps) ~ abs(x) -1))^2)
Sigmay <- diag(predict(lm(I(eps^2) ~ I(x^4) -1))) # "I" is used to tell lm that we want to evaluate whatever is inside I() before doing regression.
Sigmay
####

