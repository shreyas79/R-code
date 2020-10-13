rm(list=ls())
#setting working directory 
setwd(getwd())

##reading data 
cereal_data<-readRDS(file="newData.rds")

##installing and adding packages
install.packages("Hmisc")
install.packages("psych")
install.packages("car")

library(Hmisc)
library(psych)
library(car)

##to get the basic summary of our data
summary(cereal_data)

##subsetting all the data to the see the dependence and plotting scatterplot
data.sel<-subset(cereal_data,select=c(names(cereal_data)))
pairs.panels(data.sel,color="red")


fit <- lm(rating~ mfr+calories+protein+fat+sodium+carbo+sugars+potass+vitamins+shelf+weight+cups, data=cereal_data)
summary(fit) # R2=97%
#can remove shelf,fat and cups not that important


fit <- lm(rating~ mfr+calories+protein+carbo+sugars+potass+vitamins+weight+sodium, data=cereal_data)
summary(fit) # R2=97%, removing fat


fit <- lm(rating~ mfr+calories+protein+carbo+sugars+potass+vitamins+weight, data=cereal_data)
summary(fit) # R2=93%  removing weight


fit <- lm(rating~ calories+protein+carbo+sugars+potass+vitamins+cups+sodium+fat, data=cereal_data)
summary(fit) # R2=94%  removing mfr



fit <- lm(rating~ sugars, data=cereal_data)
summary(fit) # R2=57% only sugar

### fit the model
fit <- lm(rating~ calories*protein+sugars*vitamins+potass+cups+sodium+fat, data=cereal_data)
summary(fit) # R2=94% 



