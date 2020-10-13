rm(list=ls()) 

# importing libraries
install.packages("kernlab")
install.packages("MLmetrics")

library(kernlab)
library(randomForest)
library(MLmetrics)
library(ggplot2)


data = data("spam")
data = spam
sum(is.na(data))
dim(data)

###sampling data
set.seed(1234)
samp = sample(1:nrow(data), 0.8*nrow(data))
train = data[samp,]
test = data[-samp,]

list = c()

OOBlist = rep(0,100)
test_error = c()
#select the best m. 
for(i in c(3,4,5,9)){
  rf.fit = randomForest(type~., spam, ntree = 100, mtry = i)
  modelPredictions = predict(rf.fit, test)
  
  OOBlist = cbind(OOBlist, rf.fit$err.rate[,c(1)])
  list = append(list, i)
  test_error = append(test_error, MLmetrics::Accuracy(modelPredictions, test$type))
}

#Plotting data
dat = data.frame(list, test_error)
names(dat) = c("features", "accuracy")

ggplot(dat, aes(x = features, y = accuracy)) + geom_line() + geom_point() +
  ggtitle("features vs Accuracy") + theme(plot.title = element_text(hjust = 0.5))

#Plotting the Out of Bag errors. 
OOBlist_plot = data.frame(OOBlist)[,-c(1)]
names(OOBlist_plot) = c("m_3", "m_4","m_5","m_9")
OOBlist_plot$NTrees = seq(1, 100)

#plotting 
ggplot(OOBlist_plot, aes(NTrees)) + 
  geom_line(aes(y = m_3, colour = "m_3")) + geom_point(aes(y = m_3), size = 0.5) + 
  geom_line(aes(y = m_4, colour = "m_4")) + geom_point(aes(y = m_4), size = 0.5) + 
  geom_line(aes(y = m_5, colour = "m_5")) + geom_point(aes(y = m_5), size = 0.5) + 
  geom_line(aes(y = m_9, colour = "m_9")) + geom_point(aes(y = m_9), size = 0.5) + 
                                                                                                #
ggtitle("Out of Bag Errors.") + xlab("No of features") + ylab("OOB Error") +
  theme(plot.title = element_text(hjust = 0.5))





