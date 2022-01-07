#Install Package
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
install.packages("data.table")
install.packages("dendextend")

#Library
library(tidyverse)
library(cluster)
library(factoextra)
library(data.table)
library(dendextend)

#Visualisasi Data
View(df2)

#Initial Data use tibble
df <- as_tibble(bisni4)

#Filter Condition
df1 <- df %>% filter(BLTH == 202007)
df2<- df1 %>% select(DAYA,KWHLWBP,KWHWBP)


#Random Sampling
set.seed(1234)
df3 <- df2 %>% sample_frac(0.1, replace = FALSE)


#Remove Missing Value
data1<-na.omit(df3)

#Scale
data2<- scale(data1)
head(data2)

#Agglomerative Hierarchical Clustering

# Dissimilarity matrix
d <- dist(data2, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Compute with agnes
hc2 <- agnes(data2, method = "complete")

# Agglomerative coefficient
hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(data2, method = x)$ac
}

map_dbl(m, ac)

hc3 <- agnes(data2, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

#Divisive Hierarchical Clustering
# compute divisive hierarchical clustering
hc4 <- diana(data2)

# Divise coefficient; amount of clustering structure found
hc4$dc


# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 3 groups
sub_grp <- cutree(hc5, k = 3)

# Number of members in each cluster
table(sub_grp)

df3 %>%
  mutate(cluster = sub_grp) %>%
  head