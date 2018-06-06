###############################################
# Stat 102 B/ Sanchez     Name        ID   
# TA session 10
# Simulating the posterior distribution of theta and sigma with Gibbs sampling         
# Notice that to do the Gibbs for the posterior standard deviation we 
# make use of the precision. But if you do the calculation, precision=1/variance 
# and sd = sqrt(1/precision)
###############################################

#########  Priors, Data and data summary ##########

mu0=1.9   # mean of prior distribution of theta  (mu_0)
t20 = 0.95^2     # variance of prior distribution of theta  (tau_0^2)
s20=0.01   #  variance of prior distribution of sigma squared (sigma_0^2)
nu0=1  # number of prior observations   (nu_0) 

y=c(1.64,1.7,1.72,1.74,1.82,1.82,1.82,1.9,2.08)  # midge wing length data

mean.y = mean(y)  ; mean.y  # mean of the data (ybar) 

var.y= var(y) ; var.y # variance of the data (s^2)  - s2 in program
n= length(y); n   # sample size of observed data
sd.y=sd(y); sd.y

#initial values ybar and s20

########## Initial values for Gibbs chains #############

S=1000 # number of total draws
mun=rep(0,S)  # open space for the mun
t2n = rep(0,S)  # open space for the tau squaren
s2n=rep(0,S) 
mun[1] = mean.y  ;   t2n[1] = 1/var.y     #initial values

post.mean=rep(0,S)  # room for draws of posterior mean 
post.mean[1] = mean.y # put initial value in the first line
post.tau2 =rep(0,S)  # room for draws of the posterior precision
post.tau2[1]= t2n[1]  # put initial value in the first line

kappan = 1
########### Gibbs sampling ####################
set.seed(1) 
for(i in 2:S) { 
  
  # Generate a new value of the posterior mean from its full conditional
  kappan = kappan + n
  mun[i] = (mu0/t20 + n*mean.y*post.tau2[i-1])/(1/t20 + n*post.tau2[i-1])
  t2n[i]  = 1/(1/t20 + n*post.tau2[i-1] ) 
  post.mean[i] = rnorm(1, mun[i], sqrt(t2n[i]/kappan)) 
  
  # Generate a new 1/sigma^2 value from its full conditional 
  nun = nu0 + n
  s2n[i] = (nu0*s20+(n-1)*var.y+n*(n/kappan)*(mean.y-post.mean[i])^2)/nun
  post.tau2[i] = rgamma(1, nun/2, nun*s2n[i]/2)
}

# plot traces of your gibbs sampling of mean and standard deviation 
# save to your files

#
par(mfrow=c(2,2))
plot(post.mean[500:S], type="l", lty=1, main="Figure 1: Markov chain for post mean")  # plot markov chain for post mean
plot(sqrt(1/post.tau2[500:S]),type="l",lty=2, main="Figure 2: Markov chain for posterior sd") # plot markov chain for post sd
# Plot joint traces   and save to your file 
plot(post.mean[500:S],sqrt(1/post.tau2[500:S]),type="l", main="Fig 3:The steps of the Gibbs")    # line plot to see the traces
# Plot scatter plot of draws from joint posterior distribution
plot(post.mean[500:S],sqrt(1/post.tau2[500:S]), type="p", main="Fig 4:Posterior distribution support")  # point plot

##### Copy paste the graph you got. After that, type 

dev.off()


# Plot marginal distributions of the posterior mean and posterior standard deviation 
## Notice that we have to remove the burn in. Save to your file and interpret it. 
########         
#   par(mfrow=c(1,2))

##### STUDENT MUST REMOVE THE BURN IN PUT [500:S]

hist(post.mean[500:S], main="Marginal dist of post mean") 
hist(sqrt(1/post.tau2[500:S]), main="Marginal dist of posterior sd") 
dev.off()

######## Add the following on your own: 
###### Mean, median, standard deviation of the posterior mean
post.sd = sort(1/sqrt(post.tau2)[500:S], decreasing=F)
post.mean.2 = sort(post.mean[500:S], decreasing=F)
ind1 = floor((S-500)*0.025)
ind2 = ceiling((S-500)*0.975)
post.mean.int = post.mean.2[c(ind1, ind2)]
post.mean.int
post.sd.int = post.sd[c(ind1, ind2)]
post.sd.int

###### Mean, median, standard deviation of the posterior standard deviation

####### 95\% posterior intervals for the posterior mean and posterior standard deviation


## To do the posterior intervals, obtain the value of the first 5% of the observations (that would be the 
# observation 25 in the chain (after removing the burn in).  Then obtain the value of the 97.5th or 487.5.
### Interpret  

gibbs.example = function(mu0=1, t20=0.95^2, s20=0.01, seed1=1, s=1000) {
  mun=rep(0,S)  # open space for the mun
  t2n = rep(0,S)  # open space for the tau squaren
  s2n=rep(0,S) 
  mun[1] = mean.y  ;   t2n[1] = 1/var.y     #initial values
  
  post.mean=rep(0,S)  # room for draws of posterior mean 
  post.mean[1] = mean.y # put initial value in the first line
  post.tau2 =rep(0,S)  # room for draws of the posterior precision
  post.tau2[1]= t2n[1]  # put initial value in the first line
  
  kappan = 1
  ########### Gibbs sampling ####################
  set.seed(seed1) 
  for(i in 2:S) { 
    # Generate a new value of the posterior mean from its full conditional
    kappan = kappan + n
    mun[i] = (mu0/t20 + n*mean.y*post.tau2[i-1])/(1/t20 + n*post.tau2[i-1])
    t2n[i]  = 1/(1/t20 + n*post.tau2[i-1] ) 
    post.mean[i] = rnorm(1, mun[i], sqrt(t2n[i]/kappan)) 
    
    # Generate a new 1/sigma^2 value from its full conditional 
    nun = nu0 + n
    s2n[i] = (nu0*s20+(n-1)*var.y+n*(n/kappan)*(mean.y-post.mean[i])^2)/nun
    post.tau2[i] = rgamma(1, nun/2, nun*s2n[i]/2)
  }
  return(list(post.mean, post.tau2))
}

ex1 = gibbs.example()
post.mean = ex1[[1]]
hist(post.mean)

ex2 = gibbs.example(mu0=100, t20=20, s=10000)
post.mean2 = ex2[[1]]
hist(post.mean2)
