library(e1071)
library(ISLR)
data(OJ)

set.seed(12345)
training<-sample(1:nrow(OJ),2/3*nrow(OJ))
oj.train<-OJ[training,]
oj.test<-OJ[-training,]

#### A

linear.test.err<-c()
linear.train.err<-c()
for(i in c(0.01,0.05,0.1,0.5,1,5,10)){
  tune.model.linear<-tune(svm,Purchase~.,data=oj.train,kernel="linear",ranges=list(cost=i))
  y.hat.test.linear<-predict(tune.model.linear$best.model,newdata=oj.test)
  y.ture.test<-oj.test$Purchase
  
  test.error<-length(which(y.hat.test.linear!=y.ture.test))/length(y.ture.test)
  linear.test.err<-c(linear.test.err,test.error)
  
  y.hat.train.linear<-predict(tune.model.linear$best.model,newdata=oj.train)
  y.true.train<-oj.train$Purchase
  
  train.error<-length(which(y.hat.train.linear!=y.true.train))/length(y.true.train)
  linear.train.err<-c(linear.train.err,train.error)
}

linear.test.err  #  0.1540616 0.1512605 0.1540616 0.1596639 0.1568627 0.1540616 0.1540616
linear.train.err #  0.1697055 0.1683029 0.1669004 0.1654979 0.1640954 0.1654979 0.1640954

cost<-c(0.01,0.05,0.1,0.5,1,5,10)

x11()
par(mfrow=c(1,2))
plot(cost,linear.test.err,type="b",lty=2,col = "blue",xlab = "cost",main="test.error.linear ",ylab="test.error")
plot(cost,linear.train.err,type="b",lty=1,col="red",xlab = "cost",main=" train.error.linear",ylab="train.error")

linear.err.table<-cbind(linear.test.err,linear.train.err,cost)
linear.err.table
#### B

rad.test.err.1<-c()
rad.train.err.1<-c()
for(i in c(0.01,0.05,0.1,0.5,1,5,10)){
  tune.model.rad.1<-tune(svm,Purchase~.,data=oj.train,kernel="radial",ranges=list(cost=i))
  
  y.hat.test.rad.1<-predict(tune.model.rad.1$best.model,newdata=oj.test)
  y.ture.test<-oj.test$Purchase
  
  test.error.1<-length(which(y.hat.test.rad.1!=y.ture.test))/length(y.ture.test)
  rad.test.err.1<-c(rad.test.err.1,test.error.1)
  
  y.hat.train.rad.1<-predict(tune.model.rad.1$best.model,newdata=oj.train)
  y.true.train<-oj.train$Purchase
  
  train.error.1<-length(which(y.hat.train.rad.1!=y.true.train))/length(y.true.train)
  rad.train.err.1<-c(rad.train.err.1,train.error.1)
}

rad.test.err.1 #  0.3837535 0.2016807 0.1764706 0.1736695 0.1736695 0.1708683 0.1792717
rad.train.err.1 # 0.3927069 0.2033661 0.1739130 0.1640954 0.1598878 0.1430575 0.1290323

cost<-c(0.01,0.05,0.1,0.5,1,5,10)

x11()
par(mfrow=c(1,2))
plot(cost,rad.test.err.1,type="b",lty=2,col = "blue",xlab = "cost",main="test.error.rad.1",ylab="test.error")
plot(cost,rad.train.err.1,type="b",lty=1,col="red",xlab = "cost",main="train.error.rad.1",ylab="train.error")

rad.err.1.table<-cbind(rad.test.err.1,rad.train.err.1,cost)
rad.err.1.table

polynomial.test.err<-c()
polynomial.train.err<-c()
for(i in c(0.01,0.05,0.1,0.5,1,5,10)){
  tune.model.polynomial<-tune(svm,Purchase~.,data=oj.train,degree=2,kernel="polynomial",ranges=list(cost=i))
  
  y.hat.test.polynomial<-predict(tune.model.polynomial$best.model,newdata=oj.test)
  y.ture.test<-oj.test$Purchase
  
  test.error.2<-length(which(y.hat.test.polynomial!=y.ture.test))/length(y.ture.test)
  polynomial.test.err<-c(polynomial.test.err,test.error.2)
  
  y.hat.train.polynomial<-predict(tune.model.polynomial$best.model,newdata=oj.train)
  y.true.train<-oj.train$Purchase
  
  train.error.2<-length(which(y.hat.train.polynomial!=y.true.train))/length(y.true.train)
  polynomial.train.err<-c(polynomial.train.err,train.error.2)
}

polynomial.test.err  #  0.3809524 0.3193277 0.2941176 0.1848739 0.1960784 0.1680672 0.1596639
polynomial.train.err #  0.3927069 0.3281907 0.2973352 0.1935484 0.1935484 0.1556802 0.1514727

cost<-c(0.01,0.05,0.1,0.5,1,5,10)


x11()
par(mfrow=c(1,2))
plot(cost,polynomial.test.err,type="b",lty=2,col = "blue",xlab = "cost",main="test.error.polynomial",ylab="test.error")
plot(cost,polynomial.train.err,type="b",lty=1,col="red",xlab = "cost",main="train.error.polynomial",ylab="train.error")

polynomial.err.table<-cbind(polynomial.test.err,polynomial.train.err,cost)
polynomial.err.table