arrivals=read.csv("http://www.stat.ucla.edu/~jsanchez/data/gamma-arrivals.txt", header=F)
y = arrivals[[1]]
n = length(y)
alpha = seq(0.901, 1.9, by=0.01)
lambda = seq(0.01, 1, by=0.01)
l = matrix(0, nrow=length(alpha), ncol=length(lambda))

for (i in 1:length(alpha)) {
  for (j in 1:length(lambda)) {
    l[i, j] = -n*lgamma(alpha[i]) - lambda[j]*sum(y) + n*alpha[i]*log(lambda[j]) + (alpha[i]-1)*sum(log(y))
  }
}

contour(alpha, lambda, l, nlevels=30)

## Part A
## Contour plot tells us a good range for our initial values:
## I will pick the value (1.5, 0.01)

yt = c(0, 0)
tol = 0.000001
ytp1 = c(1.5, 0.01)

## History of alpha and lambda values
yHist = matrix(ytp1, 2, 1)

## History of likelihood funtion
l = -n*lgamma(ytp1[1]) - ytp1[2]*sum(y) + n*ytp1[1]*log(ytp1[2]) + (ytp1[1]-1)*sum(log(y))
lHist = l

## History of gradient
gHist = matrix(nrow=2, ncol=0)

while (sum((ytp1 - yt)^2) > tol) {
  yt = ytp1
  gradient = as.vector(c(-n*digamma(yt[1]) + n*log(yt[2]) + sum(log(y)), -sum(y) + n*(yt[1]/yt[2])))
  gHist = matrix(c(gHist, gradient), 2)
  hessian = matrix(c(-n*trigamma(yt[1]), n/yt[2], n/yt[2], -n*yt[1]/(yt[2]^2)), nrow=2, ncol=2)
  ytp1 = yt - solve(hessian, gradient)
  yHist = matrix(c(yHist, ytp1), 2)
  l = -n*lgamma(ytp1[1]) - ytp1[2]*sum(y) + n*ytp1[1]*log(ytp1[2]) + (ytp1[1]-1)*sum(log(y))
  lHist = c(lHist, l)
}

## calculate final value of gradient function
yt = ytp1
gradient = as.vector(c(-n*digamma(yt[1]) + n*log(yt[2]) + sum(log(y)), -sum(y) + n*(yt[1]/yt[2])))
gHist = matrix(c(gHist, gradient), 2)

yHist
lHist
gHist

hessian
eigen(hessian)

## Part c
## At convergence, the values of (alpha, lambda) are: (1.02633118, 0.01283953).
##
## The value of the first derivative of the log likelihood at convergence is:
## -0.0008739512 and 0.1754846951, which are both very close to 0, indicating that
## we have found our critical values.
##
## The values of the Hessian are: (  -6237.173       306801.6 )
##                                ( 306801.552    -24537120.2 )
##
## The values of (alpha, lambda) I got in problem 2 were: (1.026365, 0.01284019).
## The values of (alpha, lambda) that I got in problem 3 were very close to it,
## suggesting that the Multivariate Newton method works.

## Part d
-solve(hessian)

## The values of the negative inverse Hessian are:
## ( 4.164828e-04 5.207522e-06 )
## ( 5.207522e-06 1.058672e-07 )
##
## The variances of the estimators in problem 2 were:
## 0.0004223025 (for alpha) and 1.089e-07 (for lambda)
##
## The numbers in the diagonal are very close to the values I got (~2-3 decimal points).