##################################################
# Stat 102B                                      #
# TA session 2           Albert Pan              #
#                        404623352               #
#                        4/10/18                 #
##################################################


###################################
###################################
##  Question 1 
###################################


#########################
# To see what the function looks like 
# to get an idea of where the min is
# and choose an initial value 
#########################

f=function(x) x^4 + 3*x^3 - 2*x^2 - 7 
curve(f, from=-2,to=2)
abline(h=0)


### prepare to do Newton

fprime=function(x) 4*x^3 + 9*x^2 - 4*x   # first derivative


# vector to store initial value and iterations
# One may need to do more iterations to converge. 
#

x=c(0.5,rep(NA,20)) 
fval=rep(NA,20) # vector to store values of f at each iteration
fprimeval=rep(NA,20) # vector to store values of f' at each iteration



#### Do iterations now 

for(i in 1:20){
  fval[i]=f(x[i]) # update value of f
  fprimeval[i]=fprime(x[i]) # update first derivative
  x[i+1]=x[i]-fval[i]/fprimeval[i]  #Newton update of x
}

results=data.frame(x[1:20],fval,fprimeval)
results


## When x does not change any more, we have converged. 



############ADD the following 

## Write comment indicating the zero of the function to which we converge
## 1.347227

## Write comment indicating the absolute value of f at that zero. 
## 0

## Question 2

f = function (x) x^5 - 6*x^4 + x^2 - 5*x + 11
curve(f, from=-2, to=1)
abline(h=0)

df = function(x) 5*x^4 -24*x^3 +2*x -5

x = c(-2, rep(NA,49))
fvals = rep(NA, 50)
dfvals = rep(NA, 50)

i = 1
tol = 10^(-8)
flag = TRUE
while (flag) {
  fvals[i] = f(x[i])
  dfvals[i] = df(x[i])
  x[i+1] = x[i] - (fvals[i] /dfvals[i])
  flag = (abs(fvals[i]) > tol)
  i = i + 1
}

NewtonResults2 = data.frame("x"=x, "f"=fvals, "df/dx"=dfvals)
NewtonResults2

## Zero of function
## -1.270921

## Tolerance
## 4.511946e-13

## Question 3

n = 1000
lambda_true = 3

x = rexp(1000, lambda_true)
ll = function (lambda) n*log(lambda) - lambda*sum(x)
dll = function (lambda) n/lambda - sum(x)
d2ll = function (lambda) -n/lambda^2

curve(dll, from=-5, to=5)
abline(h=0)

lambda = c(2, rep(NA, 29))
dll_vals = rep(NA, 30)
d2ll_vals = rep(NA, 30)

i = 1
tol = 10^(-8)
flag = TRUE
while (flag & i <= 30) {
 dll_vals[i] = dll(lambda[i])
 d2ll_vals[i] = d2ll(lambda[i])
 lambda[i+1] = lambda[i] - (dll_vals[i]/d2ll_vals[i])
 flag = (abs(lambda[i+1] - lambda[i])>tol)
 i = i + 1
}

NewtonResults3 = data.frame("lambda"=lambda, "dll"=dll_vals, "d2ll"=d2ll_vals)
NewtonResults3

