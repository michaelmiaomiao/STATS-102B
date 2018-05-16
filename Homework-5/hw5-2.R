n = 10000
X = rep(NA, n)

alpha1 = 0.7
B = rbinom(n,1,alpha1)
head(B)

mu1 = 10
sigma1 = 2.5
mu2 = 17
sigma2 = 5

X[B==1] = rnorm(length(B[B==1]), mu1, sigma1)
X[B==0] = rnorm(length(B[B==0]), mu2, sigma2)
head(X)

hist(X)