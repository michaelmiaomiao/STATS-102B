# Part a-e
f = function(X, mu, sigma2) {
  exp((-1/(2*sigma2))*sum((X-mu)^2))
}

em = function(X, sigma2=1) {
  n = dim(X)[1]
  
  # Finds the two points that are the furthest away from each other
  distances = as.matrix(dist(X))
  mu_i = which(distances == max(distances), arr.ind=TRUE)
  
  # The two indices are the two points can be grabbed with mu_i[1,1] and mu_i[1,2]
  mu1 = X[mu_i[1,1],]
  mu2 = X[mu_i[1,2],]
  
  alpha = matrix(c(0.5, 0.5), ncol=2)
  mu = matrix(c(mu1,mu2), ncol=2)
  
  pastIndicator = n:1
  indicator = 1:n
  t = 1
  
  while (sum(pastIndicator != indicator) !=0) {
    pastIndicator = indicator
    # E step
    w1 = apply(X,1,function(x) {
      alpha[t,1]*f(x, as.numeric(mu[,1]), sigma2)
    })
    w2 = apply(X,1, function(x) {
      alpha[t,2]*f(x, as.numeric(mu[,2]), sigma2)
    })
    W = cbind(w1, w2)
    W = W/rowSums(W)
    
    indicator = max.col(W)
    
    # M step
    # Update our alphas and mus
    nk = colSums(W)
    alpha = rbind(alpha, nk/n)
    t = t+1
    
    ## Update mu
    temp1 = (W[,1]*X)/nk[1]
    temp2 = (W[,2]*X)/nk[2]
    
    mu[,1] = colSums(temp1)
    mu[,2] = colSums(temp2)
  }
  ## output everything the question asks for
  cat("The maximum likelihood estimators are:\n")
  print(mu)
  
  cat("\nThe probabilities (alphas) are:\n")
  print(alpha)
  
  cat("\nThe final allocation vector is:\n")
  cat(indicator)
  indicator
}

allocation_vector = em(iris[c(1:50, 101:150),c(1,3)])

# Part f
data = iris[c(1:50, 101:150),c(1,3)]
data.kmeans = kmeans(data, 2, nstart=20)
cluster = data.kmeans$cluster
flowers=factor(iris[c(1:50, 101:150),5], labels=c("setosa","virginica"))
table(cluster, flowers)
table(allocation_vector, flowers)

# This program outputs:
# (1) the misclassification table using the clusters that we got from the kmeans algorithm
# (2) the misclassificaiton table using the allocation_vector that we got from the EM algorithm.
#
# From the results, we can see that both algorithms correctly classified all of
# the setosa and viriginica flowers. In the kmeans algorithm, cluster 1 was the
# setosa flower, and cluster 2 was the virginica flower, and we see that all flowers
# were classified correctly. In the EM algorithm, cluster 1 was the virginica flower,
# and cluster 2 was the setosa flower. Again, we see that all flowers were correctly classified.