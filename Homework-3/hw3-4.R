canine = read.table(file="http://users.humboldt.edu/rizzardi/Data.dir/canine.txt",header=T, skip=15)

# Exercise 1
canine.aovX1 <- aov(X1~ Group, data=canine)
summary(canine.aovX1)


canine.aovX2 <- aov(X2~ Group, data=canine)
summary(canine.aovX2)

canine.aovX3 <- aov(X3~ Group, data=canine)
summary(canine.aovX3)

canine.aovX4 <- aov(X4~ Group, data=canine)
summary(canine.aoX4)

canine.aovX5 <- aov(X5~ Group, data=canine)
summary(canine.aovX5)

canine.aovX6 <- aov(X6~ Group, data=canine)
summary(canine.aovX6)

canine.aovX7 <- aov(X7~ Group, data=canine)
summary(canine.aovX7)

canine.aovX8 <- aov(X8~ Group, data=canine)
summary(canine.aovX8)

canine.aovX9 <- aov(X9~ Group, data=canine)
summary(canine.aovX9)

canine.mnv <- manova(as.matrix(canine[,-c(10,11)])~Group , data=canine)
canine.mnv

summary.manova(canine.mnv)

# Exercise 2
modern= subset(canine, Group=="modern")
with(data=modern,t.test(X1[Sex=="M"],X1[Sex=="F"],var.equal=TRUE) )
with(data=modern,t.test(X2[Sex=="M"],X2[Sex=="F"],var.equal=TRUE) )
with(data=modern,t.test(X3[Sex=="M"],X3[Sex=="F"],var.equal=TRUE) )
with(data=modern,t.test(X4[Sex=="M"],X4[Sex=="F"],var.equal=TRUE) )
with(data=modern,t.test(X5[Sex=="M"],X5[Sex=="F"],var.equal=TRUE) )
with(data=modern,t.test(X6[Sex=="M"],X6[Sex=="F"],var.equal=TRUE) )
with(data=modern,t.test(X7[Sex=="M"],X7[Sex=="F"],var.equal=TRUE) )
with(data=modern,t.test(X8[Sex=="M"],X8[Sex=="F"],var.equal=TRUE) )
with(data=modern,t.test(X9[Sex=="M"],X9[Sex=="F"],var.equal=TRUE) )

jackal = subset(canine, Group=="jackals")
with(data=jackal,t.test(X1[Sex=="M"],X1[Sex=="F"],var.equal=TRUE) )
with(data=jackal,t.test(X2[Sex=="M"],X2[Sex=="F"],var.equal=TRUE) )
with(data=jackal,t.test(X3[Sex=="M"],X3[Sex=="F"],var.equal=TRUE) )
with(data=jackal,t.test(X4[Sex=="M"],X4[Sex=="F"],var.equal=TRUE) )
with(data=jackal,t.test(X5[Sex=="M"],X5[Sex=="F"],var.equal=TRUE) )
with(data=jackal,t.test(X6[Sex=="M"],X6[Sex=="F"],var.equal=TRUE) )
with(data=jackal,t.test(X7[Sex=="M"],X7[Sex=="F"],var.equal=TRUE) )
with(data=jackal,t.test(X8[Sex=="M"],X8[Sex=="F"],var.equal=TRUE) )
with(data=jackal,t.test(X9[Sex=="M"],X9[Sex=="F"],var.equal=TRUE) )

cuons = subset(canine, Group=="cuons")
with(data=cuons,t.test(X1[Sex=="M"],X1[Sex=="F"],var.equal=TRUE) )
with(data=cuons,t.test(X2[Sex=="M"],X2[Sex=="F"],var.equal=TRUE) )
with(data=cuons,t.test(X3[Sex=="M"],X3[Sex=="F"],var.equal=TRUE) )
with(data=cuons,t.test(X4[Sex=="M"],X4[Sex=="F"],var.equal=TRUE) )
with(data=cuons,t.test(X5[Sex=="M"],X5[Sex=="F"],var.equal=TRUE) )
with(data=cuons,t.test(X6[Sex=="M"],X6[Sex=="F"],var.equal=TRUE) )
with(data=cuons,t.test(X7[Sex=="M"],X7[Sex=="F"],var.equal=TRUE) )
with(data=cuons,t.test(X8[Sex=="M"],X8[Sex=="F"],var.equal=TRUE) )
with(data=cuons,t.test(X9[Sex=="M"],X9[Sex=="F"],var.equal=TRUE) )

wolves = subset(canine, Group=="wolves")
with(data=wolves,t.test(X1[Sex=="M"],X1[Sex=="F"],var.equal=TRUE) )
with(data=wolves,t.test(X2[Sex=="M"],X2[Sex=="F"],var.equal=TRUE) )
with(data=wolves,t.test(X3[Sex=="M"],X3[Sex=="F"],var.equal=TRUE) )
with(data=wolves,t.test(X4[Sex=="M"],X4[Sex=="F"],var.equal=TRUE) )
with(data=wolves,t.test(X5[Sex=="M"],X5[Sex=="F"],var.equal=TRUE) )
with(data=wolves,t.test(X6[Sex=="M"],X6[Sex=="F"],var.equal=TRUE) )
with(data=wolves,t.test(X7[Sex=="M"],X7[Sex=="F"],var.equal=TRUE) )
with(data=wolves,t.test(X8[Sex=="M"],X8[Sex=="F"],var.equal=TRUE) )
with(data=wolves,t.test(X9[Sex=="M"],X9[Sex=="F"],var.equal=TRUE) )
