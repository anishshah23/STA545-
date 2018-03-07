library(ISLR)
setwd("C:\\Users\\Admin\\Desktop\\DS\\Statistical Data Mining\\R")
Auto <- read.table("Auto.data")
fix (Auto)
dim(Auto)
Auto=read.table ("Auto.data", header =T,na.strings ="?")
fix (Auto)
dim(Auto)
Auto=read.csv ("Auto.csv", header =T,na.strings ="?")
fix(Auto)
dim(Auto)
Auto[1:4,]
Auto=na.omit(Auto)
dim(Auto)
names(Auto)
attach(Auto)

#Predictive model for mpg

plot(mpg~., data= Auto)

#Exploratory Data Analysis

#1)Normal Histogram
hist(mpg ,col =2, breaks =15)


#B)
#1)Normal Histogram

hist(mpg,prob=TRUE, col="grey")
lines(density(mpg), col="blue", lwd=2) # add a density estimate with defaults
lines(density(mpg, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

#2)Box Plot
boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon")

#3)Dot Plot
x <- mtcars[order(mtcars$mpg),] 
x$cyl <- factor(x$cyl)
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"	
dotchart(x$mpg,labels=row.names(x),cex=.7,groups= x$cyl,
         main="Gas Milage for Car Models\ngrouped by cylinder",
         xlab="Miles Per Gallon", gcolor="black", color=x$color)

#4)Scatter Plot
attach(mtcars)
plot(wt, mpg, main="Scatterplot", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)



pairs(mpg~., data=Auto)
summary (Auto)













