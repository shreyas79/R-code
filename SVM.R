rm(list =ls())

#install.packages("ggplot2")
#install.packages("Metrics")
#install.packages('e1071')

library(ISLR)
library(e1071)
library(Metrics)
library(ggplot2)

data("OJ")
data  = OJ
str(data)

#converting to factors
data$STORE = as.factor(as.character(data$STORE))
data$StoreID = as.factor(as.character(data$StoreID))
data$SpecialCH = as.factor(as.character(data$SpecialCH))
data$SpecialMM = as.factor(as.character(data$SpecialMM))


#divide train and test. 
set.seed(1234)
train_index = sample(1:nrow(data), 0.7*nrow(data))
train = data[train_index,]
test = data[-train_index,]


#default svm

train_errors = c()
test_errors = c()
attach(data)
cost_list = seq(0.01, 10.1, 0.1)
for(i in cost_list){
  svm_model = svm(Purchase~., train, cost = i)
  
  predictions = predict(svm_model, test)
  test_errors = append(test_errors, sum(test$Purchase!=predictions))
  
  predictions = predict(svm_model, train)
  train_errors = append(train_errors, sum(train$Purchase!=predictions))
}



plot = data.frame(cost_list, train_errors, test_errors)

ggplot(plot) + 
  geom_line(aes(x = cost_list ,y = train_errors,color ="train_errors"))+
  geom_line(aes(x = cost_list ,y = test_errors,color ="test_errors"))

#radial kernel
#####
train_errors = c()
test_errors = c()
cost_list = seq(0.01, 10.1, 0.1)
for(i in cost_list){
  svm_model = svm(Purchase~., train, cost = i, kernel = "radial")
  
  predictions = predict(svm_model, test)
  test_errors = append(test_errors, sum(test$Purchase!=predictions))
  
  predictions = predict(svm_model, train)
  train_errors = append(train_errors, sum(train$Purchase!=predictions))
}


plot = data.frame(cost_list, train_errors, test_errors)

ggplot(plot) + 
  geom_line(aes(x = cost_list ,y = train_errors,color ="train_errors"))+
  geom_line(aes(x = cost_list ,y = test_errors,color ="test_errors"))
#####


#polynomial kernel. degree = 2
#####
train_errors = c()
test_errors = c()
cost_list = seq(0.01, 10, 0.1)
cost_list = append(cost_list, 10)
for(i in cost_list){
  svm_model = svm(Purchase~., train, cost = i, kernel = "polynomial", degree = 2)
  
  predictions = predict(svm_model, test)
  test_errors = append(test_errors, sum(test$Purchase!=predictions))
  
  predictions = predict(svm_model, train)
  train_errors = append(train_errors, sum(train$Purchase!=predictions))
}


plot = data.frame(cost_list, train_errors, test_errors)

ggplot(plot ) + 
  geom_line(aes(x = cost_list ,y = train_errors,color ="train_errors"))+
  geom_line(aes(x = cost_list ,y = test_errors,color ="test_errors"))

