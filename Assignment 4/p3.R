library(ISLR)
library(gbm)
library(randomForest)
attach(Weekly)

set.seed(1)
train <- sample(nrow(Weekly), nrow(Weekly) / 2)
Weekly$Direction <- ifelse(Weekly$Direction == "Up", 1, 0)
Weekly.train <- Weekly[train, ]
Weekly.test <- Weekly[-train, ]

#logistic regression.

logit.fit <- glm(Direction ~ . - Year - Today, data = Weekly.train, family = "binomial")
logit.probs <- predict(logit.fit, newdata = Weekly.test, type = "response")
logit.pred <- ifelse(logit.probs > 0.5, 1, 0)
table(Weekly.test$Direction, logit.pred)

#We have a classification error of r 1 - (11 + 282) / (11 + 244 + 8 + 282).
#We continue with boosting.

boost.fit <- gbm(Direction ~ . - Year - Today, data = Weekly.train, distribution = "bernoulli", n.trees = 5000)
boost.probs <- predict(boost.fit, newdata = Weekly.test, n.trees = 5000)
boost.pred <- ifelse(boost.probs > 0.5, 1, 0)
table(Weekly.test$Direction, boost.pred)
#We have a classification error of r 1 - (166 + 109) / (166 + 89 + 181 + 109). We continue with bagging.

bag.fit <- randomForest(Direction ~ . - Year - Today, data = Weekly.train, mtry = 6)
bag.probs <- predict(bag.fit, newdata = Weekly.test)
bag.pred <- ifelse(bag.probs > 0.5, 1, 0)
table(Weekly.test$Direction, bag.pred)
#We have a classification error of r 1 - (85 + 71) / (85 + 170 + 71 + 219). We end with random forests.

rf.fit <- randomForest(Direction ~ . - Year - Today, data = Weekly.train, mtry = 2)
rf.probs <- predict(rf.fit, newdata = Weekly.test)
rf.pred <- ifelse(rf.probs > 0.5, 1, 0)
table(Weekly.test$Direction, rf.pred)
#We have a classification error of r 1 - (69 + 228) / (69 + 186 + 62 + 228).

#We may conclude that random forests gave the lowest classification error.