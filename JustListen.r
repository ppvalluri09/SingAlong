library(corrplot)
library(cluster)
library(animation)
library(NbClust)

data <- read.csv('F:/Programs/2020/Rstudio/SongRecommender/song_tracks.csv', header = TRUE)
head(data,5)

data[is.na(data)] <- 0
summary(data)

#removing unnecessary columns
datadf <- subset(data, select = -c(album,id, name, artist, uri, track_number))
head(datadf,5)

# Random sample indexes
train_index <- sample(1:nrow(datadf), 0.8 * nrow(datadf))
test_index <- setdiff(1:nrow(datadf), train_index)

# Build train, test
train <- datadf[train_index, -15]
test <- datadf[test_index, -15]


#checking the object type
sapply(train, class)
corrmatrix <- cor(train)
corrplot(corrmatrix, method = 'number')

silhouette_score <- function(k){
  km <- kmeans(train, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(train))
  mean(ss[,3])
}

k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab = 'Average Silhouette Scores', frame = FALSE)




boxplot(train)
Num_Data_Norm <- sapply(train, scale)   #Normalized Numerical Data
boxplot(Num_Data_Norm)
summary(Num_Data_Norm)


optimum <- NbClust(Num_Data_Norm, distance='euclidean', max.nc=12, method="kmeans")
#table(optimum$Best.nc[1,])
barplot(table(optimum$Best.nc[1,]), xlab="Number of Clusters", ylab="Number of criteria", 
        main="Number of clusters chosen by criteria")

fit.km <- kmeans(Num_Data_Norm, 5, nstart=25, iter.max = 30)

train$Cluster <- fit.km$cluster

rint("Size of clusters:")
fit.km$size
print("Centers:")
fit.km$centers

print("Total Sum of Squares (SS):")
fit.km$totss

print("Within Clusters SS")
fit.km$withinss

print("Total Within Clusters SS")
fit.km$tot.withinss

print("Between Clusters SS")
fit.km$betweenss

clusplot(train, fit.km$cluster, color=TRUE, shade=TRUE, labels=5, lines=0)


#Visualizing clusters in a magnificent way
plot(datadf, col=train$Cluster)

x<-c()
for(i in 1:nrow(test)) {
  dist <- sqrt(sum((test[i,]- fit.km$centers[1,])^2))
  x <- c(x,dist)
  print(x)
}

test<-cbind(test,x)
m=mean(x)

for(i in 1:length(x)) {
  if (x[i]<m) {
    recommended<-cbind(recommended,test[i,])
  }
}
print(recommended)
