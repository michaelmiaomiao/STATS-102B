X = read.csv("http://www.stat.ucla.edu/~jsanchez/stat102B2018/instrument.csv", header=F)

X.c = scale(X, scale=F)
S.x = var(X.c)
S.x

# From the sample variance-covariance matrix, we can see that some of the variables are
# highly correlated with each other. For example, if we take a look at the following covarianes:
# Cov(V1, V2)=0.947614468, Cov(V6, V7)=0.76180932, Cov(V7, V8)=0.891784277, etc., we see that
# some of the variables may be associated with each other. Thus, it may be possible to reduce
# the number of variables using principal component analysis.

EP = eigen(S.x)
V = EP$vectors
lambda=EP$values

PC = X.c%*%V
PC

cumsum(lambda)/sum(lambda)

## From the cumsum(Lambda)/sum(Lambda) command, we see that in order to get 95% variability,
## we have to use the first 7 principal components. This is because with 7 principal components,
## we are able to capture 0.9543549 or 95.43549% of the varaibility in our original data, which
## satisfies the 95% threshold that Professor Sanchez wanted us to achieve.



### TODO: Part c
cor(PC, X.c)

