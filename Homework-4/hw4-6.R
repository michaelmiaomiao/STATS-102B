d1 = read.table(file="http://users.humboldt.edu/rizzardi/Data.dir/canine.txt",header=T, skip=15, sep="\t")
d2 = subset(d1, Group=="jackals" | Group=="prehistoric.dogs")
X = d2[, c(2,6,9)]
X_plot = d2[, c(2,6,9,11)]

# Initial values
# jackal: (X2=8, X6=6.75, X9=4.75)
# prehistoric dog: (X2=10.5, X6=8.25, X9=6.25)
library(ggplot2)
ggplot(X_plot) + geom_point(aes(x=X2, y=X6, group=Group, color=Group))
ggplot(X_plot) + geom_point(aes(x=X2, y=X9, group=Group, color=Group))
ggplot(X_plot) + geom_point(aes(x=X6, y=X9, group=Group, color=Group))

c1=c(8, 6.75, 4.75)     #initial center for cluster 1
c2=c(10.5, 8.25, 6.25)  #initial center for cluster 2

pastIndicator=30:1   # initial value for cluster allocation 
indicator=1:30       # past indicator will be compared with new indicator

### note: we initialize this way to get the algorithm started
## back to our generated data.       
###### We must iterate until z does not chang, e., until pastIndicator=indicator

while(sum(pastIndicator!=indicator)!=0) 
{
  pastIndicator = indicator; 
  
  #distance to current cluster centers
  dc1 = colSums((t(X)-c1)^2)
  dc2 = colSums((t(X)-c2)^2)
  dMat = matrix(c(dc1,dc2), ncol=2)
  
  #decide which cluster each point belongs to 
  indicator = max.col(-dMat)
  
  # update the cluster centers
  c1=colMeans(X[indicator==1,])
  c2=colMeans(X[indicator==2,])
}

c1   ## to see the mu's to which I converge for group 1
c2   # to see the mus to which I converge for group 2

# Part d: Final indicator and pastindicator
# They are the same, which means we terminate the k-means algorithm!
pastIndicator
indicator

# Part e
# Only one jackal was predicted to be a prehistoric dog
dogs = factor(d2[,11], labels=c("jackals", "prehistoric.dogs"))
table(indicator, dogs)

# Part f
plot(d2[,4], d2[,6],
     col=c("red","blue")[unclass(indicator)],pch=c(23,24)[unclass(dogs)],main="K-means of canine data containing only two species",xlab="X4", ylab="X6")

legend("topleft",c("jackal","prehistoric.dog"),pch=c(23,24))
legend("bottomright",c("cluster 1","cluster 2"),pch=c
       ("R","B"),col=c("red","blue"))