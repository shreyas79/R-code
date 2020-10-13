# to load KNN 
library(class)

# Install the dataset
install.packages("ElemStatLearn")
library(ElemStatLearn)

## function for KNN model
knn.performance <- function(trainset, testset, k) {
  knn.fit <- knn(trainset, testset, cl=trainset[, 1], k=k, prob=F)
  correctness <- sum(knn.fit == testset[, 1]) / length(knn.fit)
}


## function for linear model
linear.predict <- function(linear_model, testset) {
  pred <- predict(linear_model, newdata = testset)
  pred[pred > 2.5] = 3
  pred[pred <= 2.5] = 2
  correctness <- sum(pred == testset[, 1]) / length(pred)
}

# Get training and test dataset from the zip.train and zip.test
train<-zip.train[zip.train[,1] %in% c(2,3),]
trainset<- as.data.frame(train[,1:257])
test<-zip.test[zip.test[,1] %in% c(2,3),]
testset<- as.data.frame(test[,1:257])

# Analysis of Linear Regression and error estimation
linear.fit <- lm(V1 ~ ., data = as.data.frame((trainset)))
linear.train <- linear.predict(linear.fit, trainset)
linear.test <- linear.predict(linear.fit, testset)
linear.error<-c(1-linear.train,1-linear.test)

# Analysis of KNN model for K = 1, 3, 5, 7,9,11,13 and  15
knn_error<-c()
for (i in c(1, 3, 5, 7,9,11,13 ,15)) {
  correctness.train <- knn.performance(trainset, trainset, i)
  correctness.test <- knn.performance(trainset, testset, i)
  print(sprintf(paste('The error of %2d-nn\'s training dataset is',
                                ' %.3f, and error in testing dataset is %.3f'),
                          i, 1-correctness.train, 1-correctness.test))
  
}
sprintf(paste('The error for Liner Regression training dataset is %.3f,',
              'and error for testing data set is %.3f'), 1-linear.train, 1-linear.test)
