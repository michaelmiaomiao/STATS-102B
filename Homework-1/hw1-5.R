data = read.table("http://www.stat.ucla.edu/~jsanchez/data/gamma-arrivals")
x = data[[1]]
n = length(x)

ll = function(lambda) -(n*log(lambda) - lambda*sum(x))
dll = function(lambda) -n/lambda + sum(x)

curve(dll, from=0, to=0.1)
abline(h=0, col="red")

# Initial value = 0.01
initial = 0.01
out = nlm(ll, p=initial, hessian=TRUE)

# MLE is 0.0125104 (out$estimate)
sd = sqrt(diag(solve(out$hessian)))

problem_5 = function(level=0.95, std_error, mle) {
  ci = mle + c(-1, 1)*qnorm((1+level)/2)*std_error
  cat("The value of the MLE estimator is: ", mle, "\n")
  cat("The asymptotic variance of the MLE estimator is: ", std_error^2, "\n")
  cat("The", level*100, "percent CI for the parameter is (", ci, ")\n", sep=" ")
}

problem_5(0.95, sd, out$estimate)