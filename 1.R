rm(list=ls())
#setting working directory 
setwd(getwd())

data<-data <- read.csv("cereal.csv")
str(data)

## installing the packages
install.packages('DataExplorer') 
install.packages("GGally")

## adding to the library
library(GGally)
library(DataExplorer)

##summary of the data for overview
summary(data)

## plot for missing data 
plot_missing(data)

## plotting of basic graphs for better understanding of the data density and variation
plot_histogram(data)
plot_density(data)
plot_bar(data)

graphics.off()

##boxplot for finding outliers
boxplot(data)

## values thet do not make sense , for example -ve values are replaced by means
data$carbo[data$carbo<0]<-mean(data$carbo)
data$sugars[data$sugars<0]<-mean(data$sugars)
data$potass[data$potass<0]<-mean(data$potass)
data$sodium[data$sodium>fivenum(data$sodium)[4]]<-mean(data$sodium)
data$sodium[data$sodium<fivenum(data$sodium)[1]]<-mean(data$sodium)


## function to remove outliers , which substiutes outliers with mean
remove_outliers <- function(x , ...) 
{
  quant <- quantile(x, probs=c(.25, .75))
  
  iqr <- 1.5 * IQR(x)
  y <- x
  y[x < (quant[1] - iqr)] <- mean(x)
  y[x > (quant[2] + iqr)] <- mean(x)
  y
}

## function call to the specific data set
data$calories<-remove_outliers(data$calories)
data$protein<-remove_outliers(data$protein)
data$sodium<-remove_outliers(data$sodium)
data$fiber<-remove_outliers(data$fiber)
data$carbo<-remove_outliers(data$carbo)
data$potass<-remove_outliers(data$potass)
data$weight<-remove_outliers(data$weight)
data$cups<-remove_outliers(data$cups)
data$vitamins<-remove_outliers(data$vitamins)
data$rating<-remove_outliers(data$rating)
data[31,"sodium"]<-mean(data$sodium)

##box plot and summary
boxplot(data)
summary(data)

## corerelation graph to find correlation between different variables
plot_correlation(data, type = 'continuous')

round(cor(data[4:16], use = "complete.obs"), 2)

## scatter plots in new window for better analysis
## sometimes take too long can be skipped ,
quartz()
ggpairs(data[4:16])


## creating report which can be further analysed 
create_report(data)

## Here I am not removing the data which has less correlation due to the 
## fact that , those data may have effect on the response variable, indirectly

saveRDS(data, "newData.rds")

