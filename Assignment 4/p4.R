library(kernlab)
library(ggplot2)
library(randomForest)


spamData = data(spam)
spamData = spam
set.seed(12345)
trainIndex <- sample(1:nrow(spamData), .7*nrow(spamData))
spam_train <- spamData[trainIndex,]
spam_test <- spamData[-trainIndex,]

#Error holder
modelName = c()
testErrVector = c()
trainErrVector = c()

for (i in 1:50){
  spamBAGMod = randomForest(type~.,data = spam_train, ntree =1000, mtry = i)
  
  spamBAGPred_test <- predict(spamBAGMod, newdata = spam_test,type='class')
  spamBAGPred_train <- predict(spamBAGMod, newdata = spam_train, type='class')
  
  spamBAGPred_test <- predict(spamBAGMod, newdata = spam_test,type='class')
  spamBAGPred_train <- predict(spamBAGMod, newdata = spam_train, type='class')
  
  BAG_test_err <- mean(spamBAGPred_test != spam_test$type)
  BAG_train_err <- mean(spamBAGPred_train != spam_train$type)
  
  modelName = c(modelName,i)
  testErrVector = c(testErrVector,BAG_test_err)
  trainErrVector = c(trainErrVector,BAG_train_err)
}

errorDF = data.frame(Model_Name = modelName,Training_Error = trainErrVector,Test_Error = testErrVector)

plot(errorDF$Model_Name,errorDF$Test_Error,type='o',xlab="Features",ylab="Error",main='Collection of errors')
errorDF$Model_Name
errorDF$Test_Error