###Generating MVN random variables. Using Choleski. Example 1.  (Example in the notes)
mu=c(-2, 0, 5)  
mu 
Sigma= matrix(c( 1, 0.6, -0.9, 0.6, 1, -0.5, -0.9, -0.5, 1), ncol=3)  
Sigma  
eigen(Sigma)

## Generate one random vector from a normal distribution with that mu 
## and Sigma 

X1.t= rnorm(3)%*%chol(Sigma)+mu 
X1.t

## Now generate many more, a whole matrix n by 3 
rmvn.Choleski = 
  function(n, mu, Sigma) {
    # generate n random vectors from MVN(mu, Sigma)
    # dimension is inferred from mu and Sigma
    d = length(mu)
    Q = chol(Sigma) # Choleski factorization of Sigma
    Z = matrix(rnorm(n*d), nrow=n, ncol=d)
    X = Z %*% Q + matrix(mu, n, d, byrow=TRUE)
    X
  }

#### Generate the random numbers. Give the function something to put in.

X = rmvn.Choleski(200,mu,Sigma) 
Sigma
head(X)  # to see the first MVN vectors (rows)

ll = function(p, x, n) {
  
}

minim = nlm(ll)