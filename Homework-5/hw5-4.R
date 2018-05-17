country = read.csv("http://www.stat.ucla.edu/~jsanchez/stat102B2018/Euroemp.csv",header=TRUE,row.names=1)
country

country.dist = dist(country[,-1])
country.dist

stress.fun = function(datadist,fitteddist) {
  sqrt(sum((datadist-fitteddist)^2)/sum(datadist^2))
}

MDSdim2Vote.cmd <- cmdscale(country.dist, eig=TRUE, k=2)

# (stress.2d.cmd <- stress.fun(country.dist,dist(MDSdim2Vote.cmd$points)))
(stress.2d.cmd <- stress.fun(country.dist, dist(MDSdim2Vote.cmd$points)))

MDSdim3Vote.cmd <- cmdscale(country.dist, eig=TRUE, k=3)

(stress.3d.cmd <- stress.fun(country.dist, dist(MDSdim3Vote.cmd$points)))

MDSdim4Vote.cmd <- cmdscale(country.dist, eig=TRUE, k=4)

(stress.4d.cmd <- stress.fun(country.dist, dist(MDSdim4Vote.cmd$points)))

#

# We run non metric multidimensional scaling with isoMDS from package MASS

# Defaults for the number of iterations and tolerance are used

# Before running library(MASS) make sure you have installed the package

library(MASS)

MDSdim2Vote.iso <- isoMDS(country.dist)        # k=2

MDSdim3Vote.iso <- isoMDS(country.dist, k=3)

MDSdim4Vote.iso <- isoMDS(country.dist, k=4)

# Remark: the configuration produced by isoMDS is only determined up to rotations 

# and reflections. Thus, the result can vary considerably from machine to machine

# This explains why the stress in three dimensions and plots do not match 

# exactly the results given in Example 11.2

#

# Plots similar to those shown in Figure 11.5 are displayed as two separate plots

# Dimension 1 vs Dimension 2

plot(MDSdim3Vote.iso$points[,1],MDSdim3Vote.iso$points[,2], pch=16, col="blue",
     
     xlab="Dimension 1",ylab="Dimension 2", xlim=c(-10,8), ylim=c(-5,8),
     
     main="isoMDS function")

text(MDSdim3Vote.iso$points[,1],MDSdim3Vote.iso$points[,2],
     
     labels=rownames(MDSdim3Vote.iso$points), cex=0.7, pos=3)

abline(h=0)

abline(v=0)

# Dimension 1 vs Dimension 3

dev.new()

plot(MDSdim3Vote.iso$points[,1],MDSdim3Vote.iso$points[,3], pch=16, col="blue",
     
     xlab="Dimension 1",ylab="Dimension 3", xlim=c(-10,8), ylim=c(-5,8),
     
     main="isoMDS function")

text(MDSdim3Vote.iso$points[,1],MDSdim3Vote.iso$points[,3],
     
     labels=rownames(MDSdim3Vote.iso$points), cex=0.7, pos=3)

abline(h=0)

abline(v=0)

# Producing a Shepard plot

MDS2Voteiso.Shep <- Shepard(country.dist,MDSdim3Vote.iso$points, p=3)

dev.new()

plot(MDS2Voteiso.Shep$yf,MDS2Voteiso.Shep$x, pch = 15,cex=0.7,
     
     xlab="Configuration Distance",ylab="Data Dissimilarities",main="Shepard plot")

lines(MDS2Voteiso.Shep$yf,MDS2Voteiso.Shep$x, type = "S",col="red")