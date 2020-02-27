rm(list=ls()) 

# importing libraries

#install.packages("FNN")
#install.packages("randomForest")
#install.packages("xgboost")
#install.packages("plotly")

library(MASS)
library(randomForest)
library(GGally)
library(rpart.plot)
library(partykit)
library(rpart)
library(plotly)
library(Metrics)
library(FNN)
library(xgboost)

data = data("Boston")
data = Boston
sum(is.na(data))
dim(data)

###sampling data
set.seed(1)
samp = sample(1:nrow(data), 0.8*nrow(data))
train = data[samp,]
test = data[-samp,]

model = c()
RMSE = c()

#Linear regression
lfit = lm(crim~., train)
pred = predict(lfit, test)
model = append(model, "Regression")
RMSE = append(RMSE, Metrics::rmse(test$crim, pred))
                                                                                              #
#KNN
knn_fit = knn.reg(train = train[,-c(1)], test = test[,-c(1)], y = train$crim, k = 5)
model = append(model, "KNN")
RMSE = append(RMSE, Metrics::rmse(test$crim, knn_fit$pred))

#Random forest
rf.model = randomForest(crim~., train, ntree = 100, mtry = 9, sampsize = 300)
rf.predictions = predict(rf.model, test)
model = append(model, "Random forest")
RMSE = append(RMSE,Metrics::rmse(test$crim, rf.predictions))

#boosting. 
xgboost_train = as.matrix(train[,-c(1)])
xgboost_test = as.matrix(test[,-c(1)])
xgboost_model = xgboost(xgboost_train, train$crim, nrounds = 50, eta = 0.1, max_depth = 4, objective = "reg:linear")
xgboost_predictions = predict(xgboost_model, xgboost_test)
model = append(model, "XGBoost")
RMSE = append(RMSE, Metrics::rmse(test$crim, xgboost_predictions))

#bagging 
tree_RMSE = c()
for(i in seq(1,300)){
  bag_train = train[sample(1:nrow(train), 400),]
  tree_fit = rpart(crim~., bag_train)
  tree_predictions = predict(tree_fit, test)
  tree_RMSE = append(tree_RMSE, Metrics::rmse(test$crim, tree_predictions))
}
mean_tree_RMSE = mean(tree_RMSE)
model = append(model, "Bagging_Tree")
RMSE = append(RMSE, mean_tree_RMSE)


#Plotting the results.
plot_results = data.frame(model, RMSE)
plot_ly(plot_results, x = ~model, y = ~RMSE, name = 'Model', type = 'scatter', mode = 'markers') %>%
  layout(title = "Models Vs RMSE",
         xaxis = list(title = "Models", type = "category"),
         yaxis = list (title = "RMSE"))


