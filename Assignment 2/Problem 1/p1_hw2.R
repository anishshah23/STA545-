library(ISLR)
data(College)
#a
set.seed(7)
train.id <- sample(1:nrow(College), nrow(College)/2)
train <- College[train.id,]
test <- College[-train.id,]
str(College)
fit.lm <- lm(Apps~., data=train)
pred.lm <- predict(fit.lm, test)
(err.lm <- mean((test$Apps - pred.lm)^2))

#b
library(glmnet)
x.train <- model.matrix(Apps~., data=train)[,-1]
x.test <- model.matrix(Apps~., data=test)[,-1]
fit.ridge <- cv.glmnet(x.train, train$Apps, alpha=0)
(lambda <- fit.ridge$lambda.min)
pred.ridge <- predict(fit.ridge, s=lambda, newx=x.test)
(err.ridge <- mean((test$Apps - pred.ridge)^2))

#d
x.train <- model.matrix(Apps~., data=train)[,-1]
x.test <- model.matrix(Apps~., data=test)[,-1]
fit.lasso <- cv.glmnet(x.train, train$Apps, alpha=1)
(lambda <- fit.lasso$lambda.min)
pred.lasso <- predict(fit.lasso, s=lambda, newx=x.test)
(err.lasso <- mean((test$Apps - pred.lasso)^2))
coef.lasso <- predict(fit.lasso, type="coefficients", s=lambda)[1:ncol(College),]
coef.lasso[coef.lasso != 0]
length(coef.lasso[coef.lasso != 0])

#e
library(pls)
set.seed(1)
fit.pcr <- pcr(Apps~., data=train, scale=TRUE, validation="CV")
validationplot(fit.pcr, val.type="MSEP")
summary(fit.pcr)
pred.pcr <- predict(fit.pcr, test, ncomp=16)  
(err.pcr <- mean((test$Apps - pred.pcr)^2))  

#f
set.seed(1)
fit.pls <- plsr(Apps~., data=train, scale=TRUE, validation="CV")
validationplot(fit.pls, val.type="MSEP")
summary(fit.pls)
pred.pls <- predict(fit.pls, test, ncomp=10)  
(err.pls <- mean((test$Apps - pred.pls)^2))  

#g
err.all <- c(err.lm, err.ridge, err.lasso, err.pcr, err.pls)
names(err.all) <- c("lm", "ridge", "lasso", "pcr", "pls")
barplot(err.all )
plot(test$Apps, pred.lm)
