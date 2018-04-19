###############################################################################################################
# Name: Albert Pan
# UID: 404623352
# Date: 4/17/18
###############################################################################################################

###############################################################################################################
# Part I
###############################################################################################################

data.radon=read.table("https://www.stat.berkeley.edu/users/statlabs/data/radon.data", header=T)
attach(data.radon)

### Question 1 ###
table(data.radon$county)
temp <- 1:87
temp2 <- temp %in% data.radon$county
which(temp2==FALSE)

### Question 2 ###
data=data.radon
hist(data$radon, prob=T,ylim=c(0,0.3))
# define discrete values of x over specified range
x = c(seq(0,max(data$radon),by=0.1))
# simulate Log normal distributions for lambda =  to get an idea
points(x,dlnorm(x, meanlog=0, sdlog=1,log=FALSE), col="red", type="o",pch=21, bg="red")
points(x,dlnorm(x, meanlog=1, sdlog=0.5, log=FALSE), col="green", type="o", pch=22, bg="green")
points(x,dlnorm(x, meanlog=3, sdlog=1, log=FALSE), col="purple", type="o", pch=24, bg="purple")
points(x,dlnorm(x, meanlog=1.5, sdlog=0.8, log=FALSE), col="brown", type="o", pch=25, bg="brown")

### Question 3 ###
# negative log-likelihood:
y=data$radon
n=length(data$radon)
loglike1 <- function(p, y, n){
  sum(-dlnorm(y, meanlog=p[2], sdlog=p[1], log=TRUE))
}
# we fit now, using some initial values for the parameters
minim=nlm(loglike1, p=c(0.8, 1.5), data$radon, n, hessian=TRUE)
minim$estimate
sqrt(diag(solve(minim$hessian)))
hist(data$radon, prob=T,ylim=c(0,0.3), main="Parameters calculated using loglike1",
     xlab="Radon data")
points(x,dlnorm(x, meanlog=minim$estimate[2],sdlog=minim$estimate[1], log=FALSE), col="green", type="o", pch=21, bg="green")


### Question 5 ###
q5 = function(p, y, n) {
  mean = p[2]
  sd = p[1]
  return(-1*(-sum(log(y)) -n*log(sd) - (1/(2*sd^2))*sum((log(y) - mean)^2)))
}

minim=nlm(q5, p=c(0.8, 1.5), data$radon, n, hessian=TRUE)
minim$estimate
se = sqrt(diag(solve(minim$hessian)))

ci = function(level=0.95, std_error, mle) {
  interval = mle + c(-1, 1)*qnorm((1+level)/2)*std_error
  cat("The", level*100, "percent CI for the parameter is (", interval, ")\n", sep=" ")
}

# minim$estimate[2] = mean
# minim$estimate[1] = std
ci(0.95, se[1], minim$estimate[1])
ci(0.95, se[2], minim$estimate[2])

detach(data.radon)