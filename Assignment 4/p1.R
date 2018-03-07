# best subset

library(ElemStatLearn)
data(prostate)
training<-subset(prostate,train=="TRUE")[,1:9]
test<-subset(prostate,train=="FALSE")[,1:9]
y.training<-training$lpsa
y.test<-test$lpsa

library(leaps)
regfit.full<-regsubsets(lpsa~.,data=training, nbest=1,nvmax=8,method="exhaustive")
my_sum <- summary(regfit.full)

select = my_sum$outmat
training.error.store<-c()
test.error.store<-c()
aic.store<-c()
bic.store<-c()

for(i in 1:8){
  temp<-which(select[i,]=="*")
  red.training<-training[,c(9,temp)]
  red.test<-test[,c(9,temp)]
  red.fit<-lm(lpsa~.,data=red.training)
  aic<-AIC(red.fit)
  bic<-BIC(red.fit)
  predict.training<-predict(red.fit,newdata=red.training)
  predict.test<-predict(red.fit,newdata=red.test)
  training.error<-sum((predict.training-y.training)^2)/length(y.training)
  test.error<-sum((predict.test-y.test)^2)/length(y.test)
  training.error.store<-c(training.error.store,training.error)
  test.error.store<-c(test.error.store,test.error)
  aic.store<-c(aic.store,aic)
  bic.store<-c(bic.store,bic)
  
  
}

training.error.store
test.error.store
aic.store
bic.store

upper= max(aic.store,bic.store)
lower= min(aic.store,bic.store)

quartz()
plot(aic.store,type="o",lty=2,col = "blue",ylim = c(lower-5,upper+5),xlab = "k",main="AIC & BIC",ylab="Value")
lines(bic.store,type="o",lty=1,col="red")
legend("topright",c("AIC", "BIC"),lty=c(2,1),col=c("blue","red"))

quartz()
plot(training.error.store,type="o",lty=2,col = "blue",ylim = c(0,1),xlab = "k",ylab="error",main="Error")
lines(test.error.store,type="o",lty=1,col="red")
legend("topright",c("training.error", "test.error"),lty=c(2,1),col=c("blue","red"))


# cross-validation k=5,10
library(boot)
set.seed(1)
cv.error.k5.store<-c()
cv.error.k10.store<-c()
for(i in 1:8){
  temp<-which(select[i,]=="*")
  glm.data<-prostate[,c(9,temp)]
  glm.fit<-glm(lpsa~.,data =glm.data)
  cv.error.k5<-cv.glm(glm.data, glm.fit,K=5)$delta[2]
  cv.error.k10<-cv.glm(glm.data, glm.fit,K=10)$delta[2]
  cv.error.k5.store<-c(cv.error.k5.store,cv.error.k5)
  cv.error.k10.store<-c(cv.error.k10.store,cv.error.k10)
}

cv.error.k5.store
cv.error.k10.store

which.min(cv.error.k5.store)
which.min(cv.error.k10.store)

quartz()
plot(cv.error.k5.store,type="o",lty=2,col = "blue",ylim = c(0,1),xlab = "k",ylab="error",main="Error")
lines(cv.error.k10.store,type="o",lty=1,col="red")
legend("topright",c("error.k5", "error.k10"),lty=c(2,1),col=c("blue","red"))

# bootstrap.632
library(bootstrap)
x<-prostate[,1:8]
y<-prostate[,9]

theta.fit<-function(x,y){lsfit(x,y)}
theta.predict<-function(fit,x){cbind(1,x)%*%fit$coef}
sq.err<-function(y,yhat){(y-yhat)^2}

bootstrap.632.error.store<-c()
for(i in 1:8){
  temp<-which(select[i,]=="*")
  res<-bootpred(x[,temp],y,nboot = 50,theta.fit,theta.predict,err.meas=sq.err)
  bootstrap.632.error.store<-c(bootstrap.632.error.store,res[[3]])
}

bootstrap.632.error.store

quartz()
plot(bootstrap.632.error.store,type="o",lty=5,col="orange",main="error",xlab = "k",ylab="error")
legend("topright",c(".632"),lty=1,col=c("orange"))

quartz()
plot(training.error.store,type="o",lty=2,col = "blue",ylim = c(0.4,0.7),xlab = "k",ylab="error",main="model selection")
lines(test.error.store,type="o",lty=1,col="red")
lines(cv.error.k5.store,type="o",lty=3,col = "green")
lines(cv.error.k10.store,type="o",lty=4,col="black")
lines(bootstrap.632.error.store,type="o",lty=5,col="orange")
legend("topright",c("k=5", "k=10","training.lm", "test.lm",".632"),lty=c(5,1),col=c("green","black","blue","red","orange"))