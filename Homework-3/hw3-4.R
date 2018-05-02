canine = read.table(file="http://users.humboldt.edu/rizzardi/Data.dir/canine.txt",header=T, skip=15)

# Exercise 1
canine.mnv <- manova(as.matrix(canine[,-c(10,11)])~Group , data=canine)
canine.mnv

# The manova test tells us that there is a statistically significant overall difference between the 5 species.
summary.manova(canine.mnv)

# p-value is 0.3684, thus there is not enough evidence to reject our null hypothesis.
t.test(canine$X1[canine$Group=="prehistoric.dogs"], canine$X1[canine$Group=="modern"])

# p-value is 0.07083, thus there is not enough evidence to reject our null hypothesis. 
t.test(canine$X2[canine$Group=="prehistoric.dogs"], canine$X2[canine$Group=="modern"])

# p-value is 0.1172, thus there is not enough evidence to reject our null hypothesis.
t.test(canine$X3[canine$Group=="prehistoric.dogs"], canine$X3[canine$Group=="modern"])

# p-value is 0.09607, thus there is not enough evidence to reject our null hypothesis. 
t.test(canine$X4[canine$Group=="prehistoric.dogs"], canine$X4[canine$Group=="modern"])

# p-value is 0.8428, thus there is not enough evidence to reject our null hypothesis. 
t.test(canine$X5[canine$Group=="prehistoric.dogs"], canine$X5[canine$Group=="modern"])

# p-value is 0.07127, thus there is not enough evidence to reject our null hypothesis.
t.test(canine$X6[canine$Group=="prehistoric.dogs"], canine$X6[canine$Group=="modern"])

# p-value is 0.3444, thus there is not enough evidence to reject our null hypothesis. 
t.test(canine$X7[canine$Group=="prehistoric.dogs"], canine$X7[canine$Group=="modern"])

# p-value is 0.4282，thus there is not enough evidence to reject our null hypothesis.
t.test(canine$X8[canine$Group=="prehistoric.dogs"], canine$X8[canine$Group=="modern"])

# p-value is 0.1208, thus there is not enough evidence to reject our null hypothesis.
t.test(canine$X9[canine$Group=="prehistoric.dogs"], canine$X9[canine$Group=="modern"])

# p-value is 0.001461, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X1[canine$Group=="prehistoric.dogs"], canine$X1[canine$Group=="jackals"])

# p-value is 2.129e-06, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X2[canine$Group=="prehistoric.dogs"], canine$X2[canine$Group=="jackals"])

# p-value is 0.05683, thus there is not enough evidence to reject our null hypothesis.
t.test(canine$X3[canine$Group=="prehistoric.dogs"], canine$X3[canine$Group=="jackals"])

# p-value is 7.88e-05,thus there is enough evidence to reject our null hypothesis.
t.test(canine$X4[canine$Group=="prehistoric.dogs"], canine$X4[canine$Group=="jackals"])

# p-value is 0.006615, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X5[canine$Group=="prehistoric.dogs"], canine$X5[canine$Group=="jackals"])

# p-value is 0.0001399, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X6[canine$Group=="prehistoric.dogs"], canine$X6[canine$Group=="jackals"])

# p-value is 0.004976 thus there is enough evidence to reject our null hypothesis.
t.test(canine$X7[canine$Group=="prehistoric.dogs"], canine$X7[canine$Group=="jackals"])

# p-value is 0.00166, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X8[canine$Group=="prehistoric.dogs"], canine$X8[canine$Group=="jackals"])

# p-value is 1.765e-06, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X9[canine$Group=="prehistoric.dogs"], canine$X9[canine$Group=="jackals"])

# p-value is 0.004025, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X1[canine$Group=="prehistoric.dogs"], canine$X1[canine$Group=="cuons"])

# p-value is 0.1975, thus there is not enough evidence to reject our null hypothesis.
t.test(canine$X2[canine$Group=="prehistoric.dogs"], canine$X2[canine$Group=="cuons"])

# p-value is 4.016e-05, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X3[canine$Group=="prehistoric.dogs"], canine$X3[canine$Group=="cuons"])

# p-value is 0.4594, thus there is not enough evidence to reject our null hypothesis.
t.test(canine$X4[canine$Group=="prehistoric.dogs"], canine$X4[canine$Group=="cuons"])

# p-value is 1.732e-05, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X5[canine$Group=="prehistoric.dogs"], canine$X5[canine$Group=="cuons"])

# p-value is 0.2551, thus there is not enough evidence to reject our null hypothesis.
t.test(canine$X6[canine$Group=="prehistoric.dogs"], canine$X6[canine$Group=="cuons"])

# p-value is 0.000168, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X7[canine$Group=="prehistoric.dogs"], canine$X7[canine$Group=="cuons"])

# p-value is 0.02597, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X8[canine$Group=="prehistoric.dogs"], canine$X8[canine$Group=="cuons"])

# p-value is 0.03182, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X9[canine$Group=="prehistoric.dogs"], canine$X9[canine$Group=="cuons"])

# p-value is 4.988e-08, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X1[canine$Group=="prehistoric.dogs"], canine$X1[canine$Group=="wolves"])

# p-value is 0.001581, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X2[canine$Group=="prehistoric.dogs"], canine$X2[canine$Group=="wolves"])

# p-value is 4.549e-05, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X3[canine$Group=="prehistoric.dogs"], canine$X3[canine$Group=="wolves"])

# p-value is 0.1, thus there is not enough evidence to reject our null hypothesis.
t.test(canine$X4[canine$Group=="prehistoric.dogs"], canine$X4[canine$Group=="wolves"])

# p-value is 2.074e-11, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X5[canine$Group=="prehistoric.dogs"], canine$X5[canine$Group=="wolves"])

# p-value is 0.001057, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X6[canine$Group=="prehistoric.dogs"], canine$X6[canine$Group=="wolves"])

# p-value is 4.879e-08, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X7[canine$Group=="prehistoric.dogs"], canine$X7[canine$Group=="wolves"])

# p-value is 1.206e-08, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X8[canine$Group=="prehistoric.dogs"], canine$X8[canine$Group=="wolves"])

# p-value is 2.975e-05, thus there is enough evidence to reject our null hypothesis.
t.test(canine$X9[canine$Group=="prehistoric.dogs"], canine$X9[canine$Group=="wolves"])

library(Hotelling)

# Using Hotelling's t-test, we get a p-value of 0.0001012702, suggesting that the two groups are overall significiantly different.
pdog_modern = subset(canine, Group=="modern" | Group=="prehistoric.dogs")
pdog_modern$Group = factor(pdog_modern$Group)
t2test <- hotelling.test(X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 ~ Group, data=pdog_modern)
t2test$pval

# Using Hotelling's t-test, we get a p-value of 9.423283e-09, suggesting that the two groups are overall significiantly different.
pdog_jackals = subset(canine, Group=="jackals" | Group=="prehistoric.dogs")
pdog_jackals$Group = factor(pdog_jackals$Group)
t2test <- hotelling.test(X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 ~ Group, data=pdog_jackals)
t2test$pval

# Using Hotelling's t-test, we get a p-value of 3.221645e-12, suggesting that the two groups are overall significiantly different.
pdog_cuons = subset(canine, Group=="cuons" | Group=="prehistoric.dogs")
pdog_cuons$Group = factor(pdog_cuons$Group)
t2test <- hotelling.test(X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 ~ Group, data=pdog_cuons)
t2test$pval

# Using Hotelling's t-test, we get a p-value of 4.21345e-06, suggesting that the two groups are overall significiantly different.
pdog_wolves = subset(canine, Group=="wolves" | Group=="prehistoric.dogs")
pdog_wolves$Group = factor(pdog_wolves$Group)
t2test <- hotelling.test(X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 ~ Group, data=pdog_wolves)
t2test$pval

# Exercise 2
modern= subset(canine, Group=="modern")

# There is not enough evidence to reject our null hypothesis.
with(data=modern,t.test(X1[Sex=="M"],X1[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.
with(data=modern,t.test(X2[Sex=="M"],X2[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.
with(data=modern,t.test(X3[Sex=="M"],X3[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis. 
with(data=modern,t.test(X4[Sex=="M"],X4[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis. 
with(data=modern,t.test(X5[Sex=="M"],X5[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis. 
with(data=modern,t.test(X6[Sex=="M"],X6[Sex=="F"],var.equal=TRUE) )

# There is enough evidence to reject our null hypothesis. 
with(data=modern,t.test(X7[Sex=="M"],X7[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis. 
with(data=modern,t.test(X8[Sex=="M"],X8[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis. 
with(data=modern,t.test(X9[Sex=="M"],X9[Sex=="F"],var.equal=TRUE) )

jackal = subset(canine, Group=="jackals")

# There is enough evidence to reject our null hypothesis.  
with(data=jackal,t.test(X1[Sex=="M"],X1[Sex=="F"],var.equal=TRUE) )

# There is enough evidence to reject our null hypothesis.  
with(data=jackal,t.test(X2[Sex=="M"],X2[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.
with(data=jackal,t.test(X3[Sex=="M"],X3[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.  
with(data=jackal,t.test(X4[Sex=="M"],X4[Sex=="F"],var.equal=TRUE) )

# There is enough evidence to reject our null hypothesis. 
with(data=jackal,t.test(X5[Sex=="M"],X5[Sex=="F"],var.equal=TRUE) )

# There is enough evidence to reject our null hypothesis. 
with(data=jackal,t.test(X6[Sex=="M"],X6[Sex=="F"],var.equal=TRUE) )

# There is enough evidence to reject our null hypothesis. 
with(data=jackal,t.test(X7[Sex=="M"],X7[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis. 
with(data=jackal,t.test(X8[Sex=="M"],X8[Sex=="F"],var.equal=TRUE) )

# There is enough evidence to reject our null hypothesis. 
with(data=jackal,t.test(X9[Sex=="M"],X9[Sex=="F"],var.equal=TRUE) )

cuons = subset(canine, Group=="cuons")
# There is not enough evidence to reject our null hypothesis.  
with(data=cuons,t.test(X1[Sex=="M"],X1[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.  
with(data=cuons,t.test(X2[Sex=="M"],X2[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.  
with(data=cuons,t.test(X3[Sex=="M"],X3[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.  
with(data=cuons,t.test(X4[Sex=="M"],X4[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.  
with(data=cuons,t.test(X5[Sex=="M"],X5[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.  
with(data=cuons,t.test(X6[Sex=="M"],X6[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.  
with(data=cuons,t.test(X7[Sex=="M"],X7[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.  
with(data=cuons,t.test(X8[Sex=="M"],X8[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.  
with(data=cuons,t.test(X9[Sex=="M"],X9[Sex=="F"],var.equal=TRUE) )

wolves = subset(canine, Group=="wolves")

# There is not enough evidence to reject our null hypothesis.   
with(data=wolves,t.test(X1[Sex=="M"],X1[Sex=="F"],var.equal=TRUE) )

# There is enough evidence to reject our null hypothesis.  
with(data=wolves,t.test(X2[Sex=="M"],X2[Sex=="F"],var.equal=TRUE) )

# There is enough evidence to reject our null hypothesis.   
with(data=wolves,t.test(X3[Sex=="M"],X3[Sex=="F"],var.equal=TRUE) )

# There is enough evidence to reject our null hypothesis.   
with(data=wolves,t.test(X4[Sex=="M"],X4[Sex=="F"],var.equal=TRUE) )

# There is enough evidence to reject our null hypothesis.  
with(data=wolves,t.test(X5[Sex=="M"],X5[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.  
with(data=wolves,t.test(X6[Sex=="M"],X6[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.  
with(data=wolves,t.test(X7[Sex=="M"],X7[Sex=="F"],var.equal=TRUE) )

# There is not enough evidence to reject our null hypothesis.  
with(data=wolves,t.test(X8[Sex=="M"],X8[Sex=="F"],var.equal=TRUE) )

# There is enough evidence to reject our null hypothesis.  
with(data=wolves,t.test(X9[Sex=="M"],X9[Sex=="F"],var.equal=TRUE) )
