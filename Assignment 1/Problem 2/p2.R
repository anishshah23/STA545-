library(ISLR)
setwd("C:\\Users\\Admin\\Desktop\\DS\\Statistical Data Mining\\R")
Auto=read.csv ("Auto.csv", header =T,na.strings ="?")
dim(Auto)
Auto=na.omit(Auto)
dim(Auto)
names(Auto)
attach(Auto)

#A
large_data<- lm(mpg ~ .,data=Auto)
summary(large_data)
par(mfrow=c(2,2))
plot(large_data)

#C
large_data2 <- lm(mpg ~ cylinders * displacement+displacement * weight, data = Auto[, 1:8])
summary(large_data2)
