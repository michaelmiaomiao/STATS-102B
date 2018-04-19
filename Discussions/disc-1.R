# Albert Pan, 404623352
# Stats 102B Discussion 1B

## (1)
y = c(rep(0,14), rep(1,30), rep(2,36), rep(3,68), 
    rep(4,43), rep(5,43), rep(6,30), rep(7,14), 
    rep(8,10), rep(9,6), rep(10,4), rep(11,1), rep(12,1))

n = length(y)
table(y)

## (2)
hist(y, prob=T, ylim=c(0,0.5), xlab="Number of cars",
     main="Fitting Arbitrary Poissons to traffic data")

x = 0:max(y)

points(x, dpois(x, lambda=1, log=FALSE), col="red", type="o", pch=21, bg="red")
points(x, dpois(x, lambda=2, log=FALSE), col="green", type="o", pch=22, bg="red")
points(x, dpois(x, lambda=3, log=FALSE), col="purple", type="o", pch=24, bg="red")
points(x, dpois(x, lambda=4, log=FALSE), col="brown", type="o", pch=25, bg="red")

legend(6, 0.35, legend=c("lambda=1", "lambda=2", "lambda=3", "lambda=4"), 
       cex=0.75, pch=c(21,22,24,25), col="red", pt.bg="red")

## (3)
fn = function(p) {
  -(sum(y*log(p))) + n*p + sum(log(factorial(y)))
}

out = nlm(fn, p=0.5, hessian=TRUE)
lambda = 0.5
out
out$estimate

estimator = sqrt(diag(solve(out$hessian)))
estimator

## (4)
hist(y, ylim=c(0,0.5), freq=FALSE, main="Histogram of Traffic Data with Fitted Poisson by MLE",
     xlab="number of cars", ylab="density", prob=T)

points(x, dpois(x, lambda=out$estimate, log=FALSE), col="red", type="o", pch=21, bg="red")

## (5)
confidence_Interval = function(level=0.95, std_error, mle) {
  ci = mle + c(-1, 1)*qnorm((1+level)/2)*std_error
  cat("The", level*100, "percent CI for the parameter is (", ci, ")", sep=" ")
  return(ci)
}