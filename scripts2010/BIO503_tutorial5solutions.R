## Code for Tutorial 5

# Problem 1 

# 1A
swiss <- read.csv("swiss.csv", header=T, row.names=1)
?swiss

# 1B
# create a distance matrix first
dist.swiss <- dist(swiss)
hout.swiss <- hclust(dist.swiss)
dist.man <- dist(swiss, method="manhattan")

plot(hout.swiss)

?dist
?hclust

# clustering the indicator variables
dist.ind <- dist(t(swiss))
hout.ind <- hclust(dist.ind) 
plot(hout.ind)

# 1C
clust3 <- cutree(hout.swiss, k=3)
clust3

table(clust3)

# 1D
clust.km <- kmeans(swiss, 3)

# 1E
source("http://www.bioconductor.org/biocLite.R")
biocLite("SAGx")

gp.stat <- NULL 
for( i in 2:5 ){
	myclust <- kmeans(swiss, i)
	gp <- gap(swiss, myclust$cluster)
	gp.stat <- c(gp.stat, gp[1])
}

plot(2:5, gp.stat, xlab="Number of Clusters (k)", ylab="Gap Statistic", main="Finding the Number of Clusters using the Gap Statistic", type="b", lwd=3)
	
# Problem 2

# 2A
crabs <- read.table("crabs.txt", sep="\t", header=T)

# 2B
library(MASS)
help(lda)

input.data <- crabs[1:100,-(1:3)]
crab.gender <- crabs$sex[1:100]
crab.classifier <- lda(x=input.data, grouping=crab.gender)
crab.classifier 

pred.out <- predict(crab.classifier, crabs[101:200,-(1:3)])
names(pred.out)

table(prediction=pred.out$class, truth=crabs$sex[101:200])

class.tab <- table(prediction=pred.out$class, truth=crabs$sex[101:200])
class.rate <- sum(diag(class.tab))/sum(class.tab)
class.rate

# 2C 
library(e1071) 
help(svm)

svm.out <- svm(x=input.data, y=crab.gender)
svm.pred <- predict(svm.out, crabs[101:200,-(1:3)])
svm.table <- table(prediction=svm.pred, truth=crabs$sex[101:200])
svm.rate <- sum(diag(svm.table))/sum(svm.table)
svm.rate

library(class)
help(knn)

knn.out <- knn(train=input.data, test=crabs[101:200,-(1:3)], cl=crab.gender)
knn.out 

knn.table <- table(prediction=knn.out, truth=crabs$sex[101:200])
knn.rate <- sum(diag(knn.table))/sum(knn.table)
knn.rate 

# Problem 3 

# 3A
iris <- read.table("iris.txt", sep="\t", header=T)

# 3B
pca.out <- princomp(t(iris[,1:4]))
pc1 <- pca.out$rotation[,1] 
pc2 <- pca.out$rotation[,2]
plot(pc1, pc2, pch=20, xlab="First PC", ylab="Second PC", main="PCA of the Iris Data")

# 3C
biplot(pca.out)

# 3D
plot(pc1, pc2, pch=20, xlab="First PC", ylab="Second PC", main="PCA of the Iris Data")
iris.type <- iris[,5]
points(pc1[iris.type == "setosa"], pc2[iris.type == "setosa"], col="turquoise", pch=20, cex=2)
points(pc1[iris.type == "virginica"], pc2[iris.type == "virginica"], col="magenta", pch=20, cex=2)
points(pc1[iris.type == "versicolor"], pc2[iris.type == "versicolor"], col="purple", pch=20, cex=2)
legend(-.11, -.1, c("setosa", "virginica", "versicolor"), pt.cex=rep(2,3), pch=rep(20,3), col=c("turquoise", "magenta", "purple"))
