#Install Package
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
install.packages("data.table")
install.packages("dbscan")

#Library
library(tidyverse)
library(cluster)
library(factoextra)
library(data.table)
library(dbscan)

#Initial Data use tibble
df <- as_tibble(bisni4)

#filter Data use tibble
df1<- df %>% select(DAYA,KWHLWBP,KWHWBP)


#Visualisasi Data
View(df2)


#Sample Row
df2 <- df1 %>% slice(1:200000)

#Filter Condition
df3 <- df %>% filter(BLTH > 202007)
df4<- df3 %>% select(DAYA,KWHLWBP,KWHWBP)

#Remove Missing Value
data1<-na.omit(df1)
data2<-na.omit(df2)
data3<-na.omit(df4)

#Scale
data4<- scale(data1)
data5<- scale(data2)
data6<- scale(data3)
head(data6)

set.seed(123456789)
data1 <- df1[,1:2]
plot(data1)

km_res <- kmeans(data1, 3, nstart = 25)
plot(data1, col=km_res$cluster+1, main="K-means")

dbscan_res <- dbscan(df1, eps = 0.15, minPts = 5)
plot(data1, col=dbscan_res$cluster+1, main="DBSCAN")

dbscan_res_changed <- dbscan(df1[c('DAYA', 'KWHWBP')], eps = 0.4, minPts = 5)
plot(multishapes, col=dbscan_res_changed$cluster+1, main="DBSCAN")
