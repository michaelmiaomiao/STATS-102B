# Part a and b
d1 = read.table(file="http://users.humboldt.edu/rizzardi/Data.dir/canine.txt",
                 header=T, skip=15)

d2 = subset(d1, Group=="jackals" | Group=="prehistoric.dogs" | Group=="cuons")

X=as.matrix(d2[,1:9])
X

distances=as.matrix(dist(X))
n = dim(distances)[1]
which(distances == max(distances), arr.ind = TRUE) # we pick the 32th and 48th observation
distances[32,16]  # distance is 42.0924

# From this, we see the furthest point from these two centers is: 34th obversation
sort(distances[32,]^2 + distances[16,]^2)[n-2]

c1=X[16,] # initial center for cluster 1
c2=X[32,] # initial center for cluster 2
c3=X[18,] # initial center for cluster 3

pastIndicator=dim(X)[[1]]:1
indicator=1:dim(X)[[1]]

while(sum(pastIndicator!=indicator)!=0) 
{
  pastIndicator=indicator; 
  
  #distance to current cluster centers
  dc1 = colSums((t(X)-c1)^2)
  dc2 = colSums((t(X)-c2)^2)
  dc3 = colSums((t(X)-c3)^2)
  dMat = matrix(c(dc1,dc2,dc3), ncol=3)
  
  #decide which cluster each point belongs to 
  indicator = max.col(-dMat)
  
  # update the cluster centers
  c1=colMeans(X[indicator==1,])
  c2=colMeans(X[indicator==2,])
  c3=colMeans(X[indicator==3,])
}

## indicator and pastIndicator are identical, which indicates we can not improve.
indicator
pastIndicator

species=factor(d2[,11])
table(indicator, species)

# Part c
X = cbind(X, indicator)
X = as.data.frame(X)
mnv = manova(as.matrix(X[,1:9])~as.factor(X$indicator), data=X)
summary(mnv)

# Because the p-value is 8.209e-09 (which is < 0.05), 
# that means that there is a statistically significant difference between the three groups/clusters

# Part d
table(indicator, species)

# Cluster 1 is jackals
# Cluster 2 is cuons
# Cluster 3 is prehistoric dogs
# This means that 3 cuons, 2 jackals, and 5 prehistoric dogs are misclassified.
# This comes to a total of 10 misclassified dogs.

# Part e
X = X[,1:9]
X.kmeans = kmeans(X, 3, nstart=20)
cluster = X.kmeans$cluster
cluster
table(cluster, species)
# Cluster 1 is jackals
# Cluster 2 is prehistoric.dogs
# Cluster 3 is cuons

# This means that 7 cuons and 4 prehistoric dogs are misclassified.
# This comes to a total of 11 misclassified dogs, which means the
# kmeans algorithm does worse than our original algorithm (11 errors
# for kmeans to 10 for our algorithm).

# Part f
### From our observations, our kmeans algorithm misclassifies 11 dogs,
### while our original algorithm only misclassifies 10 dogs. The difference
### is only of one dog, so it's hard to say whether or not one of the algorithms
### does better than the other, although it seems that the kmeans algorithm is not
### as good as our algorithm (just because it misclassifies one dog more).

# Part g
plot(X[,4], X[,6],
     col=c("red","blue","green")[unclass(indicator)],pch=c(23,24,25)[unclass(species)],
     main="K-means of jackals, prehistoric dogs, and cuons",xlab="X4", ylab="X6")
legend("topleft",c("cuons","jackals", "prehistoric.dogs"),pch=c(23,24,25) )
legend("bottomright",c("cluster 1","cluster 2","cluster3"),pch=c
       ("R","B","G"),col=c("red","blue","green"))

# As mentioned earlier:
# Cluster 1 = jackals
# Cluster 2 = cuons
# Cluster 3 = prehistoric.dogs