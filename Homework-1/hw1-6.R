x = c(0.14,0.34, 0.89, 0.11, 0.35, 0.56, 0.11, 0.54, 0.56, 0.18, 0.08, 0.89)
n = length(x)
ll = function(theta) { n*log(theta+1) + theta*sum(log(x)) }
dll = function(theta) { n/(theta+1) + sum(log(x)) }
d2ll = function(theta) { -n/((theta+1)^2) }

curve(dll, from=-0.75, to=1)
abline(h=0, col="red")

# Initial value is -0.2
theta = c(-0.2, rep(NA, 19))
ll_val = c(rep(NA,20))
dll_val = c(rep(NA, 20))
d2ll_val = c(rep(NA, 20))

i = 1
tol = 0.0000001
diff = 1
while (diff > tol & i < 20) {
  ll_val[i] = ll(theta[i])
  dll_val[i] = dll(theta[i])
  d2ll_val[i] = d2ll(theta[i])
  theta[i+1] = theta[i] - (dll_val[i]/d2ll_val[i])
  diff = abs(theta[i+1]-theta[i])
  i = i+1
}

problem_6 = data.frame("MLE"=theta, "ll"=ll_val, "dll"=dll_val, "d2ll"=d2ll_val)
problem_6