set.seed(1)
eps <- rnorm(1000)
xmat <- matrix(rnorm(1000*20), ncol=20)
betas <- sample(-5:5, 20, replace=TRUE)
betas[c(3,6,7,10,13,17)] <- 0
betas
y <- xmat %*% betas + eps

#Splitting the Dataset into into a training set containing 
#100 observations and a test set containing 900 observations.

set.seed(1)
trainid <- sample(1:1000, 100, replace=FALSE)
x.train <- xmat[trainid,]
x.test <- xmat[-trainid,]
y.train <- y[trainid,]
y.test <- y[-trainid,]
train <- data.frame(y=y.train, x.train)
test <- data.frame(y=y.test, x.test)

library(leaps)
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

regfit.full <- regsubsets(y~., data=train, nvmax=20)
err.full <- rep(NA, 20)
for(i in 1:20) {
  pred.full <- predict(regfit.full, train, id=i)
  err.full[i] <- mean((train$y - pred.full)^2)
}
plot(1:20, err.full, type="b", main="Training MSE", xlab="Number of Predictors")
which.min(err.full)

err.full <- rep(NA, 20)
for(i in 1:20) {
  pred.full <- predict(regfit.full, test, id=i)
  err.full[i] <- mean((test$y - pred.full)^2)
}
plot(1:20, err.full, type="b", main="Test MSE", xlab="Number of Predictors")

which.min(err.full)

coef.best <- coef(regfit.full, which.min(err.full))
betas[betas != 0]
names(betas) <- paste0("X", 1:20)
merge(data.frame(beta=names(betas),betas), data.frame(beta=names(coef.best),coef.best), all.x=T, sort=F)