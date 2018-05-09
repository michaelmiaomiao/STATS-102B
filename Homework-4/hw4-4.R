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

install.packages("Hotelling")
library(Hotelling)

t2test = hotelling.test(study + height + email + milk ~ clusterAllocation, data=class1)

# Part d
cluster.allocation=cutree(nn, k=3)
cluster.allocation

# Part e

