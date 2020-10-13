rm(list=ls())

## install.packages("ISLR")
## install.packages("caTools")
##install.packages("glmnet")
##install.packages("pls")

# add to library
library(ISLR)
library(caTools)
library(glmnet)
library(pls)


set.seed(1)
sample = sample.split(College,SplitRatio = .80)
train= subset(College,sample==TRUE)
test = subset(College,sample==FALSE)

fit = lm(Apps~.,data=train)

prediction = predict(fit,test)
mean((test[,"Apps"]-prediction)^2)

# test MSE is 985897.4
#########################################
#  b
########################################


training_matrix = model.matrix(Apps ~ ., data = train)
test_matrix =  model.matrix(Apps ~ ., data = test)
grid = 10 ^ seq(4, -2, length = 100)

#default folds are 10
ridge.fit = glmnet(training_matrix, train$Apps, alpha = 0, thresh = 1e-10 , lambda = grid)
cv.ridge = cv.glmnet(training_matrix, train$Apps, alpha = 0, thresh = 1e-10, lambda = grid)


prediction.ridge = predict(ridge.fit, s = cv.ridge$lambda.min, newx = test_matrix)
mean((prediction.ridge - test$Apps)^2)

# test MSE is 985872.9

#########################################
#  d
########################################
lasso.fit = glmnet(training_matrix, train$Apps, alpha = 1, thresh = 1e-10, lambda = grid)
cv.lasso = cv.glmnet(training_matrix, train$Apps, alpha = 1,thresh = 1e-10, lambda = grid)

pred.lasso = predict(lasso.fit, s = cv.lasso$lambda.min, newx = test_matrix)
mean((pred.lasso - test$Apps)^2)

# non zero co efficients 
predict(lasso.fit, s = cv.lasso$lambda.min, type = "coefficients")

cv.lasso$lambda.min
# test MSE  is 985814 , lambda min is 0.01519911 
#########################################
#  e
########################################

pcr.fit = pcr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

prediction.pcr = predict(pcr.fit, test, ncomp = 10)
mean((prediction.pcr - test$Apps)^2)

test.error.store = c()
for (i in 1:17) {
  test.predict = predict(pcr.fit, test, ncomp = i) 
  pcr.test.error = mean((test[, "Apps"] - test.predict)^2)
  test.error.store = c(test.error.store, pcr.test.error)
}
which.min(test.error.store)
# K is 17 and test MSE is 1541840
#########################################
#  f
########################################
pls.fit = plsr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
validationplot(pls.fit, val.type = "MSEP")

pls.prediction = predict(pls.fit, test, ncomp = 10)
mean((pls.prediction - test$Apps)^2)

pls.test.error.store = c()

for (i in 1:17) {
  pls.prediction.test = predict(pls.fit, test, ncomp = i) 
  pls.test.error = mean((test[, "Apps"] - pls.prediction.test)^2)
  pls.test.error.store = c(pls.test.error.store, pls.test.error)
}

which.min(pls.test.error.store)
# k valus is 7 and test MSE is 995560.1

#########################################
#  g
########################################
# in order to answer for g we can calculate R^2 

test.avg = mean(test$Apps)
lm.r2 = 1 - mean((prediction - test$Apps)^2) / mean((test.avg - test$Apps)^2)
ridge.r2 = 1 - mean((prediction.ridge - test$Apps)^2) / mean((test.avg - test$Apps)^2)
lasso.r2 = 1 - mean((pred.lasso - test$Apps)^2) / mean((test.avg - test$Apps)^2)
pcr.r2 = 1 - mean((prediction.pcr - test$Apps)^2) / mean((test.avg - test$Apps)^2)
pls.r2 = 1 - mean((pls.prediction - test$Apps)^2) / mean((test.avg - test$Apps)^2)

lm.r2
ridge.r2
lasso.r2
pcr.r2
pls.r2
##############################
# r2 of lm is 0.9304255
# r2 of ridge.r2 is 0.9304272
# r2 of lasso.r2 is 0.9304314
# r2 of pcr.r2 is 0.8911928
# r2 of pls.r2 is 0.9297436
# and hence lm , ridge and lasso are more accurate and pcr is the least accurate
##############################


