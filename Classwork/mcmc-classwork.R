###################################
#  Gibbs sampling
#################################
set.seed(1234)

theta1= rep(0,5000)
theta2=rep(0,5000)
theta2[1]=-2.5

theta1[1]=rnorm(1,0.8*theta2[1],sqrt(0.36))
for(i in 2:5000){
  theta2[i]=rnorm(1,0.8*theta1[i-1],sqrt(0.36)) 
  theta1[i]=rnorm(1,0.8*theta2[i],sqrt(0.36))
}

###########Summarize marginals after ######
##### removing burn in ###################

hist(theta1[500:5000])
summary(theta1[500:5000])
sd(theta1[500:5000])

hist(theta2[500:5000])
summary(theta2[500:5000])
sd(theta2[500:5000])


##### Plot traces of markov process ####
plot(theta1[500:5000],type="l",lty=1)
lines(theta2[500:5000], type="l",lty=2)
title("Traces of markov process")

#### joint posterior for theta1 and theta2 ### 
## This is the the points in the plane, as when we see a contour plot
## 

# Type=l is a line plot
plot(theta1[500:5000],theta2[500:5000],type="l")
title("Line plot for the joint posterior for theta1 and theta2")

# type=p is a point plot
plot(theta1[500:5000],theta2[500:5000],type="p")
title("Point plot for the joint posterior for theta1 and theta2")