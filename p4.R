#removing the variables
rm(list = ls())

#install.packages("leaps")
#install.packages("glmnet")
#install.packages("ggplot2")
library(leaps)
library(glmnet)
library(ggplot2)

#read data
train = read.table("training_ticdata2000.txt")
head(train)
summary(train)

#attach target to test
test = read.table("testing_ticeval2000.txt")
test_target = read.table("testtargets_tictgts2000.txt")
test$V86 = test_target$V1

method = c()
error = c()

#linear model
fit = lm(V86~., train)
lm_predictions = predict(fit, test)
#keeping threeshold to 0.5
lm_predictions[lm_predictions>=0.5] =1
lm_predictions[lm_predictions<0.5] =0

#table for prediciton
table(lm_predictions)
#actual table
table(test$V86)
#sum of values which are not equal
res =sum(lm_predictions!=test$V86)

method = append(method, "regression")
error = append(error, res)

fwd_subset = regsubsets(V86~., data=train, method = "forward",nvmax = 84)
bwd_subset = regsubsets(V86~., data=train, method = "backward",nvmax = 84)


#find best fit by considering variables upto size 84
test_matrix = model.matrix(V86 ~., data=test)
fwd_err = rep(NA,84)
bwd_err = rep(NA,84)
for(i in 1:84){
  coef_fwd = coef(fwd_subset, id = i)
  coef_bwd = coef(bwd_subset, id = i)
  
  pred_fwd=test_matrix[,names(coef_fwd)]%*%coef_fwd
  pred_bwd=test_matrix[,names(coef_bwd)]%*%coef_bwd
  #threeshold set to 0.5
  pred_fwd_bool = ifelse(pred_fwd>=0.5,1,0)
  pred_bwd_bool = ifelse(pred_bwd>=0.5,1,0)
  # sum of values which are not equal
  fwd_err[i] = sum(pred_fwd_bool != test$V86 )
  bwd_err[i] = sum(pred_bwd_bool != test$V86 )
}


method = append(method, "forward")
error = append(error, min(fwd_err))
method = append(method, "backward")
error = append(error, min(bwd_err))

# cv for caluclating minimum  lambda
ridge_fit = cv.glmnet(as.matrix(train)[,1:85], train$V86, alpha=0)
lasso_fit = cv.glmnet(as.matrix(train)[,1:85], train$V86, alpha=1)

pred_ridge = predict(ridge_fit, newx=as.matrix(test)[,1:85], s=ridge_fit$lambda.min)
pred_lasso = predict(lasso_fit, newx=as.matrix(test)[,1:85], s=lasso_fit$lambda.min)

#threeshold set to 0.5
pred_ridge[pred_ridge>=0.5]=1
pred_ridge[pred_ridge<=0.5]=0

pred_lasso[pred_lasso>=0.5]=1
pred_lasso[pred_lasso<=0.5]=0

ridge_err =sum(pred_ridge != test$V86 )
lasso_err =sum(pred_lasso != test$V86 )

method = append(method, "ridge")
error = append(error, ridge_err)
method = append(method, "lasso")
error = append(error, lasso_err)



ggplot(data.frame(method, error), aes(x =method, y=error))+
  geom_bar(stat = "identity",width = 0.5,fill="steelblue") + 
  ggtitle("Method comparison" ) +
  geom_text(aes(label =error),vjust=1.6, color="white", size=3.5)+
  theme_minimal()



