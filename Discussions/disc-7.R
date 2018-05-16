## For normal mixtures (Q2)

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

## E-M Algorithm (Q3)
f = function(X, mu, sigma2) {
  exp((-1/(2*sigma2))*sum((X-mu)^2))
}

EM_algorithm = function(X, sigma2=1) {
  n = length(X) # not sure if this is right
  ## Create initial values for alpha and mus (remember, we have two clusters)
  ## Recommendation: start with equal probabilities for alpha
  ## Remember sum(alphas) = 1
  ## Use two distinct observations as your starting mus.
  
  ## TODO: initialize alphas and mus here
  
  pastIndicator = length(X):1
  indicator = 1:length(X)
  t = 1
  
  while (sum(pastIndicator != indicator) !=0) {
    pastIndicator = indicator
    # E step
    w1 = apply(X,1,function(x) {
      alpha[t,1]*f(x, as.numeric(mu[,1]), sigma2) # mu is a matrix, where the first column is the means for cluster 1, etc.
    })
    w2 = apply(X,1, function(x) {
      # do something similar
    })
    W = cbind(w1, w2)
    W = W/rowSums(W)
    
    indicator = max.col(W) # is this right?
    
    # M step
    # Update our alphas and mus
    nk = colSums(W) # should be right? check over this
    alpha = rbind(alpha, nk/n)
    t = t+1
    
    ## Update mu
    temp1 = (W[,1]*X)/nk[1]
    temp2 = (W[,2]*X)/nk[2]
    
    mu[,1] = colSums(temp1)
    mu[,2] = colSums(temp2)
  }
  ## output everything the question asks for
}