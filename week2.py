import numpy as np
import statsmodels.api as sm

n = int(1e+2)
x = np.random.uniform(20, 120, n)

y = np.empty(n)
eps = np.random.normal(0, 1, n) / 5
theta = [1500, 25] # True parameters
y = theta[0] + theta[1] * x + x**2 * eps


# Feature matrix
X = np.column_stack((np.ones(n), x))
OLS = np.linalg.inv(X.T.dot(X)).dot(X.T).dot(y)
model = sm.OLS(y, X)
results = model.fit()
print(results.summary())


## Use the code below as a starting point for the relaxation algorithm
####
Sigmay = np.diag(np.ones(n)) # initial guess of correlation structure


converged = False
max_iterations = 100
i = 0

while not converged and i < max_iterations:
    i += 1

    # Find parameters
    thetahat = np.linalg.inv(X.T.dot(np.linalg.inv(Sigmay)).dot(X)).dot(X.T.dot(np.linalg.inv(Sigmay)).dot(y)) # Estimate parameters using currently assumed correlation structure

    # compute epsilons
    eps = y - X.dot(thetahat) # Compute residuals for these parameter estimates



## Estimate the variance as a function of x. Notice that Var(x^2*eps)=x^4*Var(eps)
Sigmay = np.diag(np.abs(eps)**2)
Sigmay = np.diag(sm.OLS(np.abs(eps)**2, np.abs(x)**4).fit().predict()) # "I" is used to tell lm that we want to evaluate whatever is inside I() before doing regression.

print(Sigmay)




####
