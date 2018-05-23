houses = read.table("http://www.stat.ucla.edu/~jsanchez/data/montproperty.txt", sep="\t", header=T)
pairs(houses)
corr1 = cor(houses[,2:10])
sort(corr1)

corr2 = cor(houses)
corr2

model = lm(houses[,1]~houses[,2]+houses[,3]+houses[,4]+houses[,5]+houses[,6]+houses[,7]+houses[,8]+houses[,9])
summary(model)

coeff = summary(model)$coefficients[,1]
coeff
coeff.sq = coeff^2
sort(coeff.sq, decreasing=T)

houses.c = scale(houses, center=T, scale=F)
X = houses.c[,2:10]
Sx = var(X)

EP = eigen(Sx)
lambda = EP$values
lambda
cumsum(lambda)/sum(lambda)
V = EP$vectors
V

PC = X%*%V
head(PC)

cor(PC)
cor(PC, X)

pc1 = PC[,1]/sqrt(as.numeric(crossprod(PC[,1])))
pc2 = PC[,2]/sqrt(as.numeric(crossprod(PC[,2])))
pc3 = PC[,3]/sqrt(as.numeric(crossprod(PC[,3])))
pc4 = PC[,4]/sqrt(as.numeric(crossprod(PC[,4])))
pc5 = PC[,5]/sqrt(as.numeric(crossprod(PC[,5])))
pc6 = PC[,6]/sqrt(as.numeric(crossprod(PC[,6])))
pc7 = PC[,7]/sqrt(as.numeric(crossprod(PC[,7])))
pc8 = PC[,8]/sqrt(as.numeric(crossprod(PC[,8])))
pc9 = PC[,9]/sqrt(as.numeric(crossprod(PC[,9])))

y = houses.c[,1]
model1 = lm(y~pc1+pc2+pc3+pc4+pc5+pc6+pc7+pc8+pc9-1)
summary(model1)

plot(houses.c[,1], resid(model1))
abline(h=0)
hist(resid(model1))

a = cor(houses.c[,1], PC[,1])
b = sqrt(crossprod(houses.c[,1]))
a*b

coeff_1 = summary(model1)$coefficients[,1]
coeff_1
coeff_1.sq = coeff_1^2
sort(coeff_1.sq, decreasing=T)

model_final = lm(y~pc2+pc1+pc3)
summary(model_final)