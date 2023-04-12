
get_max_add_log <- function(min_value, max_value, n_trials, ds){
  k <- c()
  err <- c()
  width <- max_value - min_value
  for (i in 1:n_trials){
    add <- width/(n_trials-1)*(i-1)+min_value
    x <- 1:124
    y <- log(ds[1:124] + add)
    model <- lm(y~x)
    k <- c(k, add)
    err <- c(err, 1 - sum(model$residuals^2)/sum((y-mean(y))^2))
  }
  return(as.data.frame(cbind(k, err)))}

a <- get_max_add_log(0.1, 5, 1000, data$InterestRate)
plot(a$k, a$err, type='l')
plot(log(data$InflationRate+20))
