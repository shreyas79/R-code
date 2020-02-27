rm(list=ls()) 

#installing and loading packages

#install.packages("lasso2")
#install.packages("bootstrap")
#install.packages("leaps")
#install.packages("stats")
#install.packages("ggplot2")

library(lasso2)
library(bootstrap)
library(leaps)
library(stats)
library(ggplot2)

data =data(Prostate)
data = Prostate
sum(is.na(Prostate))
dim(data)
summary(data)

set.seed(1234)
samp = sample(1:nrow(Prostate), 0.8*nrow(Prostate))
train = Prostate[samp,]
test = Prostate[-samp,]


#Best subset selection
regfit.full = regsubsets(lcavol~., data = train, nbest = 1, nvmax = 8, method = "exhaustive")
best_subset = summary(regfit.full)
best_subset

which(best_subset$cp == min(best_subset$cp)) 

# considering lcp and lpsa
which(best_subset$outmat[2,] == "*")


#Computing the AIC and BIC.
AICVal = c()
BICVal = c()
for (i in 1:8){
  temp = which(best_subset$outmat[i,] == "*")
  temp = temp + 1
  train_subset = train[, c(1,temp)]
  fit = lm(lcavol~., data = train_subset)
  AICVal = append(AICVal, AIC(fit))
  BICVal = append(BICVal, BIC(fit))
}
features = seq(1,8)
values = data.frame(features, AICVal, BICVal)


ggplot(values, aes(features)) + 
  geom_line(aes(y = AICVal, colour = "AICVal")) + 
  geom_line(aes(y = BICVal, colour = "BICVal")) +
  ggtitle("AIC vs BIC") + xlab("No of features") + ylab("values") +
  theme(plot.title = element_text(hjust = 0.5))

##CV

CV = function(dataframe, fold = 5){
  dataframe=dataframe[sample(nrow(dataframe)),]

  folds = cut(seq(1,nrow(dataframe)),breaks=fold,labels=FALSE)

  results = c()
  for(i in 2:fold){

    test_Indices = which(folds==i,arr.ind=TRUE)
    test_data = dataframe[test_Indices, ]
    train_data = dataframe[-test_Indices, ]

    fit = lm(lcavol~., train_data)
    predictions = predict(fit, test_data)
    results = append(results, (1/length(predictions))*sum((predictions - test_data$lcavol)^2))
  }
  return(mean(results))
}



#5 Fold CV 

train_error_5cv = c()
test_error_5cv = c()
for (i in 1:8){
  temp = which(best_subset$outmat[i,] == "*")
  temp = temp + 1
  
  training = train[, c(1,temp)]
  testing = test[,c(1,temp)]
  fit = lm(lcavol~., data = training)
  pred.test = predict(fit, testing)
  
  test.error = (1/length(testing$lcavol))*sum((pred.test - testing$lcavol)^2)
  train.error = CV(training, 5) 
  
  train_error_5cv = append(train_error_5cv, train.error)
  test_error_5cv = append(test_error_5cv, test.error)
}

#10 fold cv 

train_error_10cv = c()
test_error_10cv = c()
for (i in 1:8){
  temp = which(best_subset$outmat[i,] == "*")
  temp = temp + 1
  
  training = train[, c(1,temp)]
  testing = test[,c(1,temp)]
  fit = lm(lcavol~., data = training)
  pred.test = predict(fit, testing)
  
  test.error = (1/length(testing$lcavol))*sum((pred.test - testing$lcavol)^2)
  train.error = CV(training, 10) 
  
  train_error_10cv = append(train_error_10cv, train.error)
  test_error_10cv = append(test_error_10cv, test.error)
  
}

# bootstrap 0.632 using function for bootpred 
beta.fit = function(X,Y){
  lsfit(X,Y)	
}

beta.predict = function(fit, X){
  cbind(1,X)%*%fit$coef
}

sq.error = function(Y,Yhat){
  (Y-Yhat)^2
}

X = train[,2:9]
Y = train[,1]

#searching over the most appropriate subsets of size K

boot_error = c()
for (i in 1:8){
  temp = which(best_subset$outmat[i,] == "*")
  
  res = bootpred(X[,temp], Y, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
  boot_error = c(boot_error, res[[3]])
  
}


features = seq(1,8)
values = data.frame(features, train_error_5cv, test_error_5cv, train_error_10cv, test_error_10cv, 
                            boot_error)

#Plotting our data. 

ggplot(values, aes(features)) + 
  geom_line(aes(y = boot_error, colour = "boot_error")) + geom_point(aes(y = boot_error)) + 
  geom_line(aes(y = train_error_5cv, colour = "train_error_5cv")) + geom_point(aes(y = train_error_5cv)) + 
  geom_line(aes(y = test_error_5cv, colour = "test_error_5cv")) + geom_point(aes(y = test_error_5cv)) + 
  geom_line(aes(y = train_error_10cv, colour = "train_error_10cv")) + geom_point(aes(y = train_error_10cv)) + 
  geom_line(aes(y = test_error_10cv, colour = "test_error_10cv")) + geom_point(aes(y = test_error_10cv)) + 
  
  ggtitle("Different errors.") + xlab("No of features") + ylab("MSE") +
  theme(plot.title = element_text(hjust = 0.5))

