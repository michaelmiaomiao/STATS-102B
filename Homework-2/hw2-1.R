## Create contour plot to find possible initial values
x1 = seq(-2, 1, by=0.01)
x2 = seq(-2, 6, by=0.01)
f = matrix(0, nrow=length(x1), ncol=length(x2))

for (i in 1:length(x1)) {
  for (j in 1:length(x2)) {
    f[i, j] = 2*x1[i]^4 + 3*x1[i]^3 + 2*x1[i]^2 + x2[j]^2 + 4*x1[i]*x2[j]
  }
}

contour(x1, x2, f, nlevels=30)

## From our contour plot, it looks like we have three critical values:
## one at (-1.5, 3), at (0, 0.25), and at (0.3, -0.25)
## Our initial value will be x1=-1.5 and x2=3, as evidenced by the contour plot

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
gradient = as.vector(c(8*xt[1]^3 + 9*xt[1]^2 + 4*xt[1] - 4*xt[2], 2*xt[2] - 4*xt[1]))
gHist = gradient

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

xHist
fHist
gHist

hessian

## All eigenvalues are positive, thus we
## know that the critical point is a minimum
eigen(hessian)

