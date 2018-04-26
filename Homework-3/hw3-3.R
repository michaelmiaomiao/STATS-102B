X = read.csv("MVNdata1.csv", header=T)
X = X[, c(-1)]

muhat = colMeans(X)
muhat

sigmahat = var(X)
sigmahat