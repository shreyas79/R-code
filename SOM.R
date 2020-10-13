rm(list=ls())
library(ElemStatLearn)
library(kohonen)
data(nci)
data = nci
head(data)
dim(data)
unique(colnames(data))

data = scale(data)
head(data)

set.seed(123)
som_grid = somgrid(5,5,"hexagonal")
data_som = som(data,som_grid,rlen=3000)

code = data_som$codes[[1]]

plot(data_som, main = "nci Data")
plot(data_som, type = "changes", main = "nci Data")
plot(data_som, type = "count")
plot(data_som, type = "mapping")
coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}
plot(data_som, type = "dist.neighbours", palette.name = coolBlueHotRed)

 

d <- dist(code)
hc <- hclust(d)
hc <- hclust(dist(code, method = "euclidean"), method = "ward.D2")


plot(hc)
rect.hclust(hc , k = 12, border = 1:7)


som_cluster <- cutree(hc, k = 12)

# plot the SOM with the found clusters



pretty_palette <- c("#f72707", '#a53221', '#f7c709', '#d3f709', 
                    '#899b20', '#0bf707', '#07f7cf',
                    '#07aff7', '#0767f7', '#d307f7',
                    '#894e71', '#330711', '#f7f6d4',
                    '#d4d9f7')

my_bhcol = pretty_palette[som_cluster]

data <- t(data)

plot(data_som, type = "mapping", col = "black", bgcol = my_bhcol)
add.cluster.boundaries(data_som, som_cluster)

plot(data_som, type = 'mapping', main = 'mapping', labels = row.names(data), bgcol = my_bhcol)
add.cluster.boundaries(data_som, som_cluster)


