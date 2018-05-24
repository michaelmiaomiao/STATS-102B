library(haven)

# Part a
data = read_sas("http://www.stat.ucla.edu/~jsanchez/stat102B2018/bostonhousing.sas7bdat")
data = as.data.frame(data)
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
pc1 = PC[,1]/sqrt(as.numeric(crossprod(PC[,1])))
pc2 = PC[,2]/sqrt(as.numeric(crossprod(PC[,2])))
pc3 = PC[,3]/sqrt(as.numeric(crossprod(PC[,3])))
pc4 = PC[,4]/sqrt(as.numeric(crossprod(PC[,4])))
pc5 = PC[,5]/sqrt(as.numeric(crossprod(PC[,5])))
pc6 = PC[,6]/sqrt(as.numeric(crossprod(PC[,6])))
pc7 = PC[,7]/sqrt(as.numeric(crossprod(PC[,7])))
pc8 = PC[,8]/sqrt(as.numeric(crossprod(PC[,8])))
pc9 = PC[,9]/sqrt(as.numeric(crossprod(PC[,9])))
pc10 = PC[,10]/sqrt(as.numeric(crossprod(PC[,10])))
pc11 = PC[,11]/sqrt(as.numeric(crossprod(PC[,11])))
pc12 = PC[,12]/sqrt(as.numeric(crossprod(PC[,12])))

# Part g
cor(PC, X.c)

# PCA is a process that uses orthogonal transformations to transform a set of correlated
# variables into a smaller set of linearly uncorrelated variables, called principal components (PC).
# The correlation between the PC and our centered data matrix tells us which principal components
# we should choose for our principal component analysis. Whichever PCs have the highest correlation
# are the variables that we will choose.

# Part h
cumsum(lambda)/sum(lambda)

# From the output of the command above, we can see that only one PC is enough to represent
# over 90% of the variability in our original data matrix. In fact, the first component is
# enough to represent 0.9578049 or 95.78049% of the variability in our original data.

# Part i
# The X-tilde matrix has already been normalized in part f. Note: PC=X-tilde matrix

# Part j
model = lm(data$MedianValue~pc1+pc2+pc3+pc4+pc5+pc6+pc7+pc8+pc9+pc10+pc11+pc12-1)
summary(model)

# Part k
coeff = summary(model)$coefficients[,1]
coeff.sq = coeff^2
sort(coeff.sq, decreasing=T)

# By looking at the summary of the model, we see that only the principal components
# 6, 1, and 9 have a p-value of less than 0.05, while principal components 2, 4, and
# 11 have a p-value of less than 0.1. We will also look at the correlations of the
# principal components to figure out which components to use.

# By looking at the correlations of the principal components (which we do by squaring 
# the coefficients and sorting it in decreasing order), we see that the principal component
# 6 has the greatest correlation, then PC 1, PC 9, PC 2, PC 4, PC 11, etc. Using the
# results from the summary and the correlations, we would select the principal components
# 6, 1, and 9, since they have statistically significant p-values and their correlations
# are the three highest.

# This is code you would run with your new linear model
# model1 = lm(data$MedianValue~PC[,1]+PC[,6]+PC[,9]-1)
# summary(model1)