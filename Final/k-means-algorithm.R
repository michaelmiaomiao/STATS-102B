# K-MEANS ALGORITHMICALLY (without using the function)

# (a) Read it again into R using the online version of it.
# d1 <- read.table(file="http://users.humboldt.edu/rizzardi/Data.dir/canine.txt",header=T, skip=15)
# (b) Select only the jackals and the  prehistoric Thai dogs into a data frame called d2. Make a matrix that contains the vectors with the continuous variables only. Call this matrix X. 
# (c) Apply the k-means algorithm R program that is given to you in class on 5/7 and discussed in the TA session of 5/8 to matrix X.  Do not use the kmeans() function in R.
# (d) Make sure that you print the final indicator and pastIndicator vectors. 
# (e) Obtain a table that indicates how many dogs have been misallocated. 
# (f) Obtain a plot similar to that obtained for the iris data set that indicates in two different colors where the dogs stand. Use label and legend that indicate what species each color represents and which cluster is which. Use the variables X4 and X6 to do the plot. 

# (a)
d1 = read.table(file="http://users.humboldt.edu/rizzardi/Data.dir/canine.txt",header=T, skip=15)

# (b)
d2 = subset(d1, Group=="jackals" | Group=="prehistoric.dogs")
X = as.matrix(d2[,1:9]) # make sure to do as.matrix

# (c)
# To get intial values, use the distance matrix to find the points furthest from each other
distances = as.matrix(dist(X))
which(distances==max(distances), arr.ind=TRUE) ## points with max distance at 23,16
distances[16,23]  # distance is 34.22295

### will choose point 1 and 16 as initial values
# not sure why, but put it out in this for
c1 = X[1,]
c2 = X[23,]
pastIndicator=dim(X)[[1]]:1
indicator=1:dim(X)[[1]]

while(sum(pastIndicator!=indicator)!=0) 
{
  pastIndicator=indicator; 
  
  #distance to current cluster centers
  dc1 =colSums((t(X)-c1)^2)
  dc2=colSums((t(X)-c2)^2)
  dMat=matrix(c(dc1,dc2), ncol=2)
  
  #decide which cluster each point belongs to 
  indicator = max.col(-dMat)
  
  # update the cluster centers
  c1=colMeans(X[indicator==1,])
  c2=colMeans(X[indicator==2,])
}

# (d) They should be different
pastIndicator
indicator

# (e)
species=factor(d2[,11], labels=c("jackals","prehistoric.dogs"))
table(indicator, species)

# (f)
plot(X[,4], X[,6],
     col=c("red","blue")[unclass(indicator)],pch=c(23,24)[unclass(species)],
     main="K-means of jackals and prehistoric dogs",xlab="X4", ylab="X6")
legend("topleft",c("jackals","prehistoric"),pch=c(23,24) )
legend("bottomright",c("cluster 1","cluster 2"),pch=c
       ("R","B"),col=c("red","blue"))
