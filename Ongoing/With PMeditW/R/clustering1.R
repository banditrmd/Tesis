#Install Package
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
install.packages("data.table")
install.packages("gridExtra")

#Library
library(tidyverse)
library(cluster)
library(factoextra)
library(data.table)
library(gridExtra)

#Intial Data
data<-data.table(bisni4)

#Visualisasi Data
View(data)
View(data9)

#Filter Coloumn
data1<-subset(data,select = c(DAYA,JAMNYALA,KWHLWBP,KWHWBP))

#Sample Row
data2<- data1[sample(.N,200000)]

#Filter Condition
data3<- filter(data, BLTH == 202007)
data4<-subset(data3,select = c(DAYA,KWHLWBP,KWHWBP))

#Remove Missing Value
data5<-na.omit(data4)
data6<-na.omit(data2)
data8<-na.omit(data1)

#Scale
data10 <- scale(data5)
data9<- scale(data8)
data7<- scale(data6)
head(data9)

#Clustering Distance Measure
distance <- get_dist(data10)
fviz_dist(distance, gradient = list(low = "#00AFBB", 
                                    mid = "white", high = "#FC4E07"))

#K-Means Clustering
k2 <- kmeans(data7, centers = 2, nstart = 25)
str(k2)
k2

#Cluster Plot
fviz_cluster(k2, data = data7)

#Alternative Cluster Plot
data7 %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(data7)) %>%
  ggplot(aes(DAYA,JAMNYALA, color = factor(cluster), label = KWHWBP)) +
  geom_text()

#Alternative Cluster Plot1
k3 <- kmeans(data7, centers = 3, nstart = 25)
k4 <- kmeans(data7, centers = 4, nstart = 25)
k5 <- kmeans(data7, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = data7) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = data7) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = data7) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = data7) + ggtitle("k = 5")


grid.arrange(p1, p2, p3, p4, nrow = 2)


#Determining Optimal Clusters
#Elbow Method

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data7, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Optimal Number Cluster
set.seed(123)
fviz_nbclust(data10, kmeans, method = "wss")

#Average Silhouette Method

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(data10, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data10))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#Optimal Number Cluster
fviz_nbclust(data10, kmeans, method = "silhouette")

#Gap Statistic Method

set.seed(123)
gap_stat <- clusGap(data10, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

#Final 
set.seed(123)
final <- kmeans(data7, 9, nstart = 25)
print(final)
fviz_cluster(final, data = data7)
