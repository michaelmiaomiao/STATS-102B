set.seed(111)
data=rnorm(1000, 10,2)
n = length(data)
mean(data)
var(data)
sd(data)

x1 = seq(5, 15, by=0.01)
length(x1)
x2 = seq(2, 6, by=0.01)
f = matrix(0, nrow=length(x1), ncol=length(x2))
for (i in 1:length(x1)) {
  for (j in 1:length(x2)) {
    f[i,j] = -(n/2)*log(x2[j]) - (n/2)*log(2*pi) - (1/(2*x2[j]))*sum((data-x1[i])^2)
  }
}
contour(x1,x2,f,nlevels=300,xlab="mu", ylab="sigma^2")

# Initial values around mu=10, sigma^2=4
yt=c(0,0)
ytp1=c(10,4)
tol=0.00001

while (sum((ytp1-yt)^2) > tol) {
  yt = ytp1
  gradient = c((1/(yt[2]))*sum(data-yt[1]), 
               -(n/(2*yt[2]))+0.5*yt[2]^-2*sum((data-yt[1])^2))
  hessian = matrix(c(-n/(yt[2]), 
                     -(1/(yt[2]^2))*sum(data-yt[1]), 
                     -(1/(yt[1]^2))*sum(data-yt[1]), 
                     (n/(2*yt[2]^2))-(1/(yt[2]^3))*sum((data-yt[1])^2)), nrow=2, ncol=2, byrow=TRUE)
  ytp1 = yt - solve(hessian, gradient)
}

# One final calculation
yt = ytp1
gradient = c((1/(yt[2]^2))*sum(data-yt[1]), -(n/(2*yt[2]^2))+0.5*yt[2]^-4*sum((data-yt[1])^2))

yt
gradient
hessian
eigen(hessian)

# Topic 3
inv = solve(-hessian)
diag(inv)
var_mu = diag(inv)[1]
var_sigma2 = diag(inv)[2]
stderr_mu = sqrt(var_mu)
stderr_sigma2 = sqrt(var_sigma2)