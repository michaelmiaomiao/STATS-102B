## Create contour plot to find possible initial values
x1 = seq(-2.5, 1, by=0.01)
x2 = seq(-6, 2, by=0.01)
f = matrix(0, nrow=length(x1), ncol=length(x2))

for (i in 1:length(x1)) {
  for (j in 1:length(x2)) {
    f[i, j] = 2*x1[i]^4 + 3*x1[i]^3 + 2*x1[i]^2 + x2[j]^2 - 4*x1[i]*x2[j]
  }
}

contour(x1, x2, f, nlevels=30)

## Part A
##
## From our contour plot, it looks like we have three critical values:
## one at (-1.5, -3), at (0, 0), and at (0.3, 0.25)
## Our initial value will be x1=-1.5 and x2=-3, as evidenced by the contour plot.
## Other possible values include (0, 0) and (0.3, 0.25).

xt = c(100, 0)
tol = 0.00001
xtp1 = c(-1.5, 3)

## History of xt values
xHist = matrix(xtp1, 2, 1)

## Original function
f = 2*xtp1[1]^4 + 3*xtp1[1]^3 + 2*xtp1[1]^2 + xtp1[2]^2 - 4*xtp1[1]*xtp1[2]

## History of the original
fHist=f

## History of gradients
gHist = matrix(nrow=2, ncol=0)

while (sum((xtp1 - xt)^2) > tol) {
  xt = xtp1
  gradient = as.vector(c(8*xt[1]^3 + 9*xt[1]^2 + 4*xt[1] - 4*xt[2], 2*xt[2] - 4*xt[1]))
  gHist = matrix(c(gHist, gradient), 2)
  hessian = matrix(c(24*xt[1]^2 + 18*xt[1] + 4, -4, -4, 2), nrow=2, ncol=2)
  xtp1 = xt - solve(hessian, gradient)
  xHist = matrix(c(xHist, xtp1), 2)
  f = 2*xtp1[1]^4 + 3*xtp1[1]^3 + 2*xtp1[1]^2 + xtp1[2]^2 - 4*xtp1[1]*xtp1[2]
  fHist = c(fHist, f)
}

xt = xtp1
gradient = as.vector(c(8*xt[1]^3 + 9*xt[1]^2 + 4*xt[1] - 4*xt[2], 2*xt[2] - 4*xt[1]))
gHist = matrix(c(gHist, gradient), 2)

xHist ## Critical value found here
fHist
gHist

## Part b
## I found that the critical value was (-1.466054, -2.932108).

## Part c
## It took my program three iterations to converge to the critical values.
## The critical values I got were x1=-1.466054 and x2=-2.932108.
## The value of the gradient vector at convergence was: (-4.685156e-05, 0).

hessian
eigen(hessian)

## Part d
## The eigenvalues of the Hessian are: 29.839376 and 1.425274.
## Since all the eigenvalues are positive,
## we know that the critical value we got is a minimum.

