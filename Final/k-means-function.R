datanew=matrix(c(iris$Petal.Length[51:150],
                 iris$Petal.Width[51:150], iris$Sepal.Length[51:150],
                 iris$Sepal.Width[51:150]),ncol=4)

#### Do k-means clustering, k=2 using R's kmeans() function. Get clusters
iris.kmeans=kmeans(datanew,2,nstart=20)
cluster=iris.kmeans$cluster
cluster  # view allocation

#### Extract the flower names of data
flowers=factor(iris[51:150,5], labels=c("versicolor","virginica"))

#### Figure out if there is any misclassification
table(cluster, flowers)

#### Plot of sepal length, sepal width and classification
plot(datanew[,1], datanew[,2],
     col=c("red","blue")[unclass(cluster)],pch=c(23,24)[unclass
                                                        (flowers)],main="K-means of iris data containing only two species",xlab="sepal length", ylab="sepal width")

legend("topleft",c("versicolor","virginica"),pch=c(23,24))
legend("bottomright",c("cluster 1","cluster 2"),pch=c
       ("R","B"),col=c("red","blue"))