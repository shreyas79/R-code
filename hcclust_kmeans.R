rm(list=ls())

install.packages("fossil")
install.packages("fpc")

library(fpc)
library(fossil)
library(cluster)

dat = read.table("seeds_dataset.txt",fill=TRUE,header = TRUE)
head(dat)

colnames(dat)=c("Area","Perimeter","Compactness","Length_Kernel","Width_Kernel","Asymmetry","Length_Kernel_Grove","Seed_Group")
head(dat)
dat = dat[,1:8]

data = dat[,1:7]
head(data)

hc.complete = hclust(dist(data),method = "complete")
hc.average = hclust(dist(data),method = "average")
hc.single = hclust(dist(data),method = "single")


clusters =cutree(hc.complete, k=3)
table(clusters,dat$Seed_Group)

rand.index(clusters, as.numeric(dat$Seed_Group))



clusters =cutree(hc.average, k=3)
table(clusters,dat$Seed_Group)

rand.index(clusters, as.numeric(dat$Seed_Group))


clusters =cutree(hc.single, k=3)
table(clusters,dat$Seed_Group)

rand.index(clusters, as.numeric(dat$Seed_Group))

################################
##b)

gap_kmeans <- clusGap(data, kmeans, nstart = 20, K.max = 10, B = 100)

plot(gap_kmeans, main = "Gap Statistic: kmeans")

km = kmeans(data,centers = 3 ,nstart = 20)
table(km$cluster,dat$Seed_Group)
rand.index(km$cluster, as.numeric(dat$Seed_Group))




