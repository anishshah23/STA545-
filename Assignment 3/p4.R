set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

#Computing LOOCV error from fitting the first linear least squares model

library(boot)
set.seed(1)
Data <- data.frame(x, y)
fit.glm.1 <- glm(y ~ x)
cv.glm(Data, fit.glm.1)$delta[1]

#Computing LOOCV error from fitting the second linear least squares model
fit.glm.2 <- glm(y ~ poly(x, 2))
cv.glm(Data, fit.glm.2)$delta[1]

#Computing LOOCV error from fitting the third linear least squares model
fit.glm.3 <- glm(y ~ poly(x, 3))
cv.glm(Data, fit.glm.3)$delta[1]

#Computing LOOCV error from fitting the fourth linear least squares model
fit.glm.4 <- glm(y ~ poly(x, 4))
cv.glm(Data, fit.glm.4)$delta[1]

summary(fit.glm.1)

summary(fit.glm.2)

summary(fit.glm.3)

summary(fit.glm.4)
