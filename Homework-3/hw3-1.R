# Part a
X <- matrix(c(1, 2, 3, 1, 4, 9), ncol = 2)
H = X %*% solve(crossprod(X,X)) %*% t(X)
H

# Part b
ev = eigen(H)
ev

# Part c
sum(diag(H))
sum(ev$values)
cat("The trace of matrix H and the sum of the eigenvalues are the same (they are both 2).\n")

# Part d
detH = det(H)
detH

ev_prod = prod(ev$values)
ev_prod

cat("The determinant of H and the product of the eigenvalues have very similar values (they are both very small and close to 0), so they can be considered to be the same.\n")

# Part e
H %*% X[,1]
H %*% X[,2]

ev$values[1] * X[,1]
ev$values[2] * X[,2]

cat("The code above shows that the columns of X are indeed eigenvectors of the matrix of H by the definition of eigenvectors as HX[,1] = Eigenvalue[1]X[,1] and HX[,2] = Eigenvalue[2]X[,2].\n")
