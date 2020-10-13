rm(list=ls())

#install.packages('kohonen')
library(kohonen)
library(ISLR)

data("USArrests")
data = USArrests
head(data)
summary(data)
data = scale(data)

cluster = hclust(dist(data,method = "euclidean"), method = "complete")
plot(cluster)
rect.hclust(cluster , k = 4, border = 2:6)
hirearchial_cut <- cutree(cluster, k = 4)


som_grid = somgrid(5,5,"hexagonal")
data_som = som(data,rlen=1000,grid=som_grid)

names(data_som)
dataset_codes <- data_som$codes[[1]]

plot(data_som, main = "US Arrests data")
plot(data_som, type = "changes", main = "USArrests Data - changes")
plot(data_som, type = "count", main = "USArrests data - count")
coolBlueHotRed<-function(n,alpha=1){rainbow(n,end=4/6, alpha=alpha)[n:1]}
plot(data_som, type="dist.neighbours", palette.name = coolBlueHotRed)
pretty_pallete = c("lightblue", "grey", "lightgreen")


som_hclust <- hclust(dist(dataset_codes))
plot(som_hclust, main = "SOM Hclust")
rect.hclust(som_hclust , k = 4, border = 2:6)

som_cut <- cutree(som_hclust,k= 4)

my_pallete <- pretty_pallete[som_cut]
plot(data_som, type = "mapping", bgcol = my_pallete, 
      main = "USArrests Mapping Plot")
add.cluster.boundaries(data_som, som_cut)




