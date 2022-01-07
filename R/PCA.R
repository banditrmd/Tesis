#Install Package
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("data.table")
install.packages("cluster")
install.packages("factoextra")

#Library
library(tidyverse)
library(gridExtra)
library(data.table)
library(cluster)
library(factoextra)

#Initial Data use tibble
df <- as_tibble(bisni4)

#filter Data use tibble
df1<- df %>% select(DAYA,KWHLWBP,KWHWBP)
data1<-df1

#Visualisasi Data
View(df2)


#Sample Row
df2 <- df1 %>% slice(1:200000)

#Filter Condition
df3 <- df %>% filter(BLTH > 202007)
df4<- df3 %>% select(DAYA,KWHLWBP,KWHWBP)

#Built-in PCA Functions
pca_result <- prcomp(data1, scale = TRUE)
names(pca_result)

# means
pca_result$center

# standard deviations
pca_result$scale

pca_result$rotation

pca_result$rotation <- -pca_result$rotation
pca_result$rotation

pca_result$x <- - pca_result$x
head(pca_result$x)

biplot(pca_result, scale = 0)

pca_result$sdev

(VE <- pca_result$sdev^2)


data2<-pca_result$x


#Elbow Method

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data2, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)
final <- kmeans(data2, 3, nstart = 25)
print(final)
fviz_cluster(final, data = data2)

data1 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
