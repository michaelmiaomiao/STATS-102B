class = read.csv("http://www.stat.ucla.edu/~jsanchez/stat102B2018/distancedata-hwk.csv", header=T)

# Part a
scaled = scale(class[-1], center=T, scale=T)
class1 = cbind(class[1], scaled)

# Part b
D1 = dist(scaled)
nn = hclust(D1, method="single", members=NULL)
plot(nn)
cluster.allocation=cutree(nn, k=2)
cluster.allocation
which(cluster.allocation==2)

## All codes are in cluster 1 except for code m, which is in cluster 2.

# Part c
class1$clusterAllocation = cluster.allocation

# install.packages("Hotelling")
# library(Hotelling)
# 
# t2test = hotelling.test(study + height + email + milk ~ clusterAllocation, data=class1)
# t2test

### We are unable to perform the Hotelling T^2 test because we do not have enough
### points in cluster 2. This is because we have outliers in our data, and thus only
### one point remained in cluster 2 while the others remained in cluster 1.
###
### To solve this problem, we would have to do one of the following procedures:
### 1. Remove the outliers from our data set (cleaning up the data set)
### 2. Use the k-means algorithm or the furthest neighbor algorithm

# Part d
cluster.allocation=cutree(nn, k=3)
cluster.allocation

# Part e
class1$clusterAllocation = cluster.allocation
mnv = manova(as.matrix(class1[, -c(1,6)])~as.factor(class1$clusterAllocation), data=class1)
mnv
summary.manova(mnv)
summary.manova(mnv,test="Wilks")
summary.manova(mnv,test="Hotelling-Lawley")
summary.manova(mnv,test="Roy")

## The different MANOVA tests give us the p-values all of which are <0.05, which suggests
##  that there is a significant difference across the clusters. However, it also suggests that
## the estimated effects may be unbalanced, and that is because our data is not
## balanced (all clusters do NOT have the same number of observations). Thus,
## while the MANOVA test tells us there is a significant difference across the clusters,
## we also know that the two clusters that only have one observation are most likely
## outliers rather than an actual "group".