install.packages("sas7bdat")
library(sas7bdat)

# Part a
data = read.sas7bdat("http://www.stat.ucla.edu/~jsanchez/stat102B2018/bostonhousing.sas7bdat")
data

# Part b
replace = function(x) {
  ifelse(x=="Far", 0, 1)
}
data$Charles = sapply(data[,5], replace)
data

# Part c
X = as.matrix(data[,-1])

# Part d
X.c = scale(X, scale=F)

# Part e
S.x = var(X.c)
EP = eigen(S.x)
V = EP$vectors
Lambda = diag(EP$values)
lambda = EP$values
PC = X.c%*%V
PC

# Part f
PC = scale(PC, center=FALSE, scale=colSums(PC))
PC

# Part g
cor(PC, X.c)

# TODO: Check over this; is this right?
# PCA is a process that uses orthogonal transformations to transform a set of correlated
# variables into a smaller set of linearly uncorrelated variables, called principal components (PC).
# The correlation between the PC and our centered data matrix tells us which principal components
# we should choose for our principal component analysis. Whichever PCs have the highest correlation
# are the variables that we will choose.

# Part h
cumsum(lambda)/sum(lambda)

# TODO: Why? Do I need to answer that question?
# From the output of the command above, we can see that only one PC is enough to represent
# over 90% of the variability in our original data matrix. In fact, the first component is
# enough to represent 0.9578049 or 95.78049% of the variability in our original data.

# Part i
# The X-tilde matrix has already been normalized in part f. Note: PC=X-tilde matrix

# Part j

model = lm(data$MedianValue~PC[,1]+PC[,2]+PC[,3]+PC[,4]+PC[,5]+PC[,6]+PC[,7]+PC[,8]+PC[,9]+PC[,10]+PC[,11]+PC[,12]-1)
summary(model)

# Part k
coeff = summary(model)$coefficients[,1]
coeff.sq = coeff^2
sort(coeff.sq, decreasing=T)

# By looking at the correlations of the principal components (which we do by squaring 
# the coefficients and sorting it in decreasing order), we see that the principal component
# 11 has the greatest correlation, then PC 12, PC 10, PC 9, and the rest of the components
# have a correlation that is < 1.

model1 = lm(data$MedianValue~PC[,11]+PC[,12]+PC[,10]+PC[,9]-1)
summary(model1)

# TODO: do we run the lm model again? How do we interpret this?
