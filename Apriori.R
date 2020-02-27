#this is a problem where association rules/apriori algorithm was used to find rules 
# and was used for prediction
rm(list=ls())

#install.packages("ggplot2")
#install.packages("arules")
#install.packages("psych")
#install.packages("arules")

library(ggplot2)
library(MASS)
library(arules)
library(psych)

mydata =data(Boston)
mydata = Boston
head(mydata)

attach(mydata)
##a)#########
ggplot(mydata = mydata )+geom_histogram(aes(mydata$dis) , binwidth = 2,col ="red")

multi.hist(mydata)



m = 0.67*mean(mydata$crim)
mydata$crim = sapply(mydata$crim , function(x) ifelse(x>m, 0 ,1))

m = mean(mydata$zn)
mydata$zn = sapply(mydata$zn , function(x) ifelse(x<m, 0 ,1))

m = mean(mydata$indus)
mydata$indus = sapply(mydata$indus , function(x) ifelse(x<m, 0 ,1))

m = mean(mydata$chas)
mydata$chas = sapply(mydata$chas , function(x) ifelse(x<m, 0 ,1))

m = mean(mydata$nox)
mydata$nox = sapply(mydata$nox , function(x) ifelse(x<m, 0 ,1))

m = mean(mydata$rm)
mydata$rm = sapply(mydata$rm , function(x) ifelse(x<m, 0 ,1))

m = mean(mydata$age)
mydata$age = sapply(mydata$age , function(x) ifelse(x<m, 0 ,1))

m=mean(mydata$dis)
mydata$dis = sapply(mydata$dis , function(x) ifelse(x<m,0 ,1))

m = mean(mydata$rad)
mydata$rad = sapply(mydata$rad , function(x) ifelse(x<m, 0 ,1))

m = mean(mydata$ptratio)
mydata$ptratio = sapply(mydata$ptratio , function(x) ifelse(x<m, 0 ,1))

m = mean(mydata$black)
mydata$black = sapply(mydata$black , function(x) ifelse(x<m, 0 ,1))

m = mean(mydata$lstat)
mydata$lstat = sapply(mydata$lstat , function(x) ifelse(x<m, 0 ,1))

m = mean(mydata$medv)
mydata$medv = sapply(mydata$medv , function(x) ifelse(x<m, 0 ,1))

m = mean(mydata$tax)
mydata$tax = sapply(mydata$tax , function(x) ifelse(x<m, 0 ,1))


##b)

subset = mydata[,c(1:14)]
subset = data.frame(lapply(subset,as.factor))
trans = as(subset,"transactions")
itemFrequencyPlot(trans)
apr = apriori(subset,parameter = list(confidence = 0.75 , support = 0.25))
summary(apr)

##c)

lowcrime = subset(apr, subset = lhs %in% "crim=1" & rhs %in% "dis=1")
inspect(head(sort(lowcrime),n = 5),by ="lift")

##d)
stratio = subset(apr, subset = rhs %in% "ptratio=1")
inspect( head(sort(stratio,by ="lift"),n = 10))


##e)
fit = lm(ptratio ~ ., Boston)
summary(fit)
fit = lm(ptratio~crim+rad+tax+nox+dis+zn+indus)
summary(fit)

plot(fit)


