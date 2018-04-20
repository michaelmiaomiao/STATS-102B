#########################R program #######################

##################################################################
#  
#MLE estimation of alpha and lambda in a gamma distribution    
#  
#This program fits a Gamma model to a set of 
# data by MLE. It use the functon nlm in R, which is set to 
# minimize a function. So to maximize the log likelihood, we 
# minimize the  -log likelihood
# 
# This program also fits by the MM to use as initia
# values 
#
##################################################################

# Read data 

arrivals=read.csv("http://www.stat.ucla.edu/~jsanchez/data/gamma-arrivals.txt", header=F)


# read the data from a table to a vector
y=arrivals[[1]]

# plot out the histogram (we plot out the normalized histogram)
hist(y, freq = FALSE, main = "Histogram of Gamma-Ray Data", xlab = "interarrival time", ylab = "density")

#Get the sample summary statistics
summary(y)
sd(y)


############################################################################
## Fitting a Gamma to the data by MM (method of moments)
#
# Because it looks like a Gamma might seem reasonable, we can try to 
## fit formally a Gamma model. We estimate first by the MM 
# the alpha and lambda parameter  to obtain initial values. Standard procedure. 
# All commands below are needed.  
##############################################################################

# MM estimation
n=length(y)
m=mean(y)  #sample mean
s2=var(y)   # sample variance 
lambda_hat_MM = (m/s2)*n/(n-1) #  MM lambdahat 
alpha_hat_MM = (m^2/s2)*n/(n-1) # MM alphahat

#Fitting the model fitted with MM to the #histogram of the data

hist(y, freq = FALSE, main = "Histogram of Gamma-Ray Data with fitted Gamma using MM estimates", xlab = "interarrival time", ylab = "density")

x=c(seq(0,max(y), 0.1))
curve(dgamma(x, shape=alpha_hat_MM, scale=1.0/lambda_hat_MM),add=TRUE, col=   "red") 

############################################################################
## Fitting the data with a Gamma by MLE 
## fit formally a Gamma model. We estimate first by the MM 
# the alpha and lambda parameter . 
# All commands below are needed.  
##############################################################################

# Write the negative log likelihood 
# negative log-likelihood: p=(lambda, alpha), y=alpha rays, n=total # locations 


fn=function(p,y){
  -(p[1]-1)*sum(log(y)) + sum(y)*p[2] + (length(y))*(log(gamma(p[1])) -      
                                                       p[1]*log(p[2]))
}

# Minimize negative log likelihood

out=nlm(fn, p=c(1,0.1), y, hessian=TRUE) 
out$estimate
out$hessian
#View MLE parameter estimates

alpha_hat_MLE=out$estimate[1] # MLE alphahat
lambda_hat_MLE=out$estimate[2] # MLE lambdahat

sqrt(diag(solve(out$hessian))) # asymptotic se

#Fitting the model to the histogram of the data. 

hist(y, freq = FALSE, main = "Histogram of Gamma-Ray Data with fitted Gamma using ML  estimates", xlab = "interarrival time", ylab = "density")

x=c(seq(0,max(y), 0.1))

curve(dgamma(x, shape=alpha_hat_MLE, scale=1.0/   lambda_hat_MLE),col='red',add=TRUE)

###########################################
# Finding the standard errors of the estimates
##########################################

s.e= sqrt(diag(solve(out$hessian)))

##################################
# Confidence intervals 
#################################

#Write code to print the  confidence intervals. 

# write comment to interpret the confidence intervals. Use # to write your comment. 
ci = function(level, mle, se) {
  interval = mle + c(-1, 1) * qnorm((1+level)/2)*se
  cat("The", level*100, "percent CI for the parameter is (", interval, ")\n", sep=" ")
}

ci(0.95, alpha_hat_MLE, s.e[1])
ci(0.95, lambda_hat_MLE, s.e[2])

# write comment to interpret the confidence intervals. Use # to write your comment.

# This means that we can be 95% certain that the true parameter alpha is within the range 0.9860833 to 1.066647
# (in other words, 95/100 samples will result in a CI that will contain the true parameter),
# and that we can be 95% certain that the true parameter lambda is within the range 0.01219275 to 0.01348763.