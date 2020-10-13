rm(list = ls())

getwd()
data =read.csv("Ch10Ex11.csv",header = FALSE)

head(data)
dim(data)

dd = as.dist(1-cor(data))
hc = hclust(dd , method = "complete")             

quartz()
plot(hc)
quartz()
plot(hclust(dd,method = "average"))
quartz()
plot(hclust(dd,method = "single"))

par(mfrow=c(1,3))
plot(hc,main= "Complete Linkage")
plot(hclust(dd,method = "average"),main = "Average Linkage")
plot(hclust(dd,method = "single"), main = "Single Linkage")


pr.out = prcomp(t(data))
head(pr.out$rotation)
total.load = apply(pr.out$rotation, 1, sum)
index = order(abs(total.load), decreasing = TRUE)
index[1:5]

