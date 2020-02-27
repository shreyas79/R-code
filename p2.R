rm(list=ls()) 

# importing libraries
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("partykit")
#install.packages("Metrics")

library(rpart)
library(rpart.plot)				    
library(partykit)
library(Metrics)

data = read.table("wine.data",sep = ",")
names(data) = c("wine", "alcohol", "malic_acid", "ash", "alcalinity_ash", "magnesium", "total_phenols", "flavanoids", 
                "nonflavanoid_phenols", "proanthocyanins", "color_intensity","hue","OD280/OD315","proline")

###sampling data
data$wine = as.factor(as.character(data$wine))
set.seed(1)
samp = sample(1:nrow(data), 0.8*nrow(data))
train = data[samp,]
test = data[-samp,]                                                          

tree_fit = rpart(wine~., train, method = "class")

#training and confusion matrix
train_pred = predict(tree_fit, train, type = "class")
cm = table(train_pred, train$wine)
                                                  
#prediction and confusion matrix

test_pred = predict(tree_fit, test, type = "class")
cm = table(test_pred, test$wine)

train_accuracy =mean(train_pred == train$wine)
test_accuracy = mean(test_pred == test$wine)

rpart.plot(tree_fit)


node_5 = 0
#For class = 1
node_4 = sum(test_pred == 1)

#For class = 2
node_6 = sum(test_pred == 2)

#For class = 3
index = which(test_pred == 3)
for(i in index){
  if(test[i,]$proline >= 755){
    node_5 = node_5 + 1
  }
}
node_7 = length(index) - node_5



#node_4 = 14
#node_5 = 1
#node_6 = 16
#node_7 = 5

