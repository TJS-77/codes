data <- iris
View(data)

iris_features <- data
iris_features$Species <- NULL
View(iris_features)

library(ggplot2)
sepal<-ggplot(data,aes(x = data$Sepal.Length, y = data$Sepal.Width, col= data$Species)) + geom_point()
sepal

petal<-ggplot(data,aes(x = data$Petal.Length, y = data$Petal.Width, col= data$Species)) + geom_point()
petal

set.seed(100)
classification <- kmeans(iris_features, centers = 3, nstart = 20)
classification

table(classification$cluster,data$Species)

library(cluster)
clusplot(data, classification$cluster, color=T, shade=T, labels=0, lines=0)

