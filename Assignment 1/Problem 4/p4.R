install.packages("MASS")
library(MASS)
#Boston$chas <- as.factor(Boston$chas)
dim(Boston)
attach(Boston)
#A
par(mfrow = c(2, 2))
plot(Boston$nox, Boston$crim)
plot(Boston$rm, Boston$crim)
plot(Boston$age, Boston$crim)
plot(Boston$dis, Boston$crim)

#B
hist(Boston$crim, breaks = 50)
pairs(Boston[Boston$crim < 20, ])

#C
hist(Boston$crim, breaks = 50)
nrow(Boston[Boston$crim > 20, ])
hist(Boston$tax, breaks = 50)
nrow(Boston[Boston$tax == 666, ])
hist(Boston$ptratio, breaks = 50)

#D

#More than seven rooms per dwelling
nrow(Boston[Boston$rm > 7, ])

#More than eight rooms per dwelling
nrow(Boston[Boston$rm > 8, ])
