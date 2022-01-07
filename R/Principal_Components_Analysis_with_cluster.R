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

#Intial Data
data<-data.table(bisni4)

#Filter Coloumn
data1<-subset(data,select = c(DAYA,KWHLWBP,KWHWBP))

#Sample Row
data2<- data1[sample(.N,200000)]

#Filter Condition
data3<- filter(data, BLTH == 202007)
data4<-subset(data3,select = c(DAYA,KWHLWBP,KWHWBP))

head(data1,10)

# compute variance of each variable
apply(data1, 2, var)

# create new data frame with centered variables
scaled_df <- apply(data1, 2, scale)
head(scaled_df)

# Calculate eigenvalues & eigenvectors
arrests.cov <- cov(scaled_df)
arrests.eigen <- eigen(arrests.cov)
str(arrests.eigen)

# Extract the loadings
(phi <- arrests.eigen$vectors[,1:2])

phi <- -phi
row.names(phi) <- c("DAYA", "KWHLWBP", "KWHWBP")
colnames(phi) <- c("PC1", "PC2")

# Calculate Principal Components scores
PC1 <- as.matrix(scaled_df) %*% phi[,1]
PC2 <- as.matrix(scaled_df) %*% phi[,2]

# Create data frame with Principal Components scores
PC <- data.frame(State = row.names(data1), PC1, PC2)
head(PC)


# Plot Principal Components for each State
ggplot(PC, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = State), size = 3) +
  xlab("First Principal Component") + 
  ylab("Second Principal Component") + 
  ggtitle("First Two Principal Components of PLN Data")


#Selecting the Number of Principal Components
#The Proportion of Variance Explained
PVE <- arrests.eigen$values / sum(arrests.eigen$values)
round(PVE, 2)

# PVE (aka scree) plot
PVEplot <- qplot(c(1:3), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# Cumulative PVE plot
cumPVE <- qplot(c(1:3), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)

grid.arrange(PVEplot, cumPVE, ncol = 2)

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


data1<-pca_result$x


#Elbow Method

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data1, k, nstart = 10 )$tot.withinss
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
final <- kmeans(data1, 2, nstart = 25)
print(final)
fviz_cluster(final, data = data1)

data1 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
