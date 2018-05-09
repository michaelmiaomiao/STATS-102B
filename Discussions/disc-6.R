####################################################
##### Stat 102B/Sanchez 
#####
##### NAME: Albert
##### ID: 404623355 
##### Date: 5/8/18  
#####  For lecture on k-means optimization algorithm
##### Using simulated MVN data for two different populations
##### to illustrate that it works. 
#####    
#############################################################

#######################################
# Programs for k-means algorithm of 
# made up data. 
#######################################

### generate an artificial X matrix to show 
## that algorithm works. 

X=matrix(0,ncol=2,nrow=200)  # space to put made up data 

# mean and covariance parameters of group 1
mu1 <- c(15, 15)
Sigma1 <- matrix(c(20, -.8, -.8, 15), nrow = 2, ncol = 2)

mu2 <- c(30,30)
Sigma2 <- matrix(c(40,0.6,0.6,60),ncol=2)

rmvn.eigen <-
  function(n, mu, Sigma) {
    # generate n random vectors from MVN(mu, Sigma)
    # dimension is inferred from mu and Sigma
    d <- length(mu)
    ev <- eigen(Sigma, symmetric = TRUE)
    lambda <- ev$values
    V <- ev$vectors
    R <- V %*% diag(sqrt(lambda)) %*% t(V)
    Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
    X <- Z %*% R + matrix(mu, n, d, byrow = TRUE)
    X
  }

# generate the sample
X[1:100,] <- rmvn.eigen(100, mu1, Sigma1)
X[101:200,]<-rmvn.eigen(100,mu2,Sigma2)

#plot to get idea of initial values
plot(X)

###########choose initial values of mu#####
### The ones below are for the artificial data. ###### 

c1=c(12,12) #initial center for cluster 1
c2=c(35,35)  #initial center for cluster 2 

pastIndicator=200:1   #initial value for cluster allocation 
indicator=1:200     # past indicator will be compared with new indicator

### note: we initialize this way to get the algorithm started
## back to our generated data.       
###### We must iterate until z does not chang, e., until pastIndicator=indicator

while(sum(pastIndicator!=indicator)!=0) 
{
  pastIndicator=indicator; 
  
  #distance to current cluster centers
  dc1 =colSums((t(X)-c1)^2)
  dc2=colSums((t(X)-c2)^2)
  dMat=matrix(c(dc1,dc2),ncol=2)
  
  #decide which cluster each point belongs to 
  indicator = max.col(-dMat)
  
  # update the cluster centers
  c1=colMeans(X[indicator==1,])
  c2=colMeans(X[indicator==2,])
  
  # Make plot
}

c1   ## to see the mu's to which I converge for group 1
c2   # to see the mus to which I converge for group 2

library(ggplot2)
df = data.frame(x1=X[,1], x2=X[,2], indicator=ifelse(indicator==1, "cluster1", "cluster2"))
df2 = data.frame(rbind(c1, c2), cl=c("cluster1 mean", "cluster2 mean"))
ggplot(df) + geom_point(aes(x=x1,y=x2, col=indicator))

######## If you want to see which group each observation goes to 
########## you type 

indicator 
pastIndicator  

#### Both should be the same. IF you look at how we generated the data 
#### notice that we have used two multivariate normals with very different 
#### means and var-cov matrices.... You should have the first 100 observations in one 
#### group and the next 100 in another group, or very close. This may or may not 
### be the case for your data.


### Do a plot to show the clusters in different colors.  

# Can also do the same thing with 3 cluster
# X = matrix(0,ncol=3,nrow=200)
# mu3 = c(25,20)
# sigma3 = matrix(c(10, 0.3, 0.3, 10))
# 
# X[1:100,] <- rmvn.eigen(100, mu1, Sigma1)
# X[101:200,]<-rmvn.eigen(100,mu2,Sigma2)
# X[201:300,] = rmvn.eigen(100, mu3, sigma3)

# Do rest (if you'd like)