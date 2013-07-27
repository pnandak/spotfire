class(iris)
names(iris)
attach(iris)
########################
summary(iris)
########################
plot(iris)
########################
plot(iris[c(1,3)],col=as.numeric(Species),pch="o",cex=1)
class(Species)
########################3
# 2 and 4 are for the ass
plot(iris[c(2,4)],col=as.numeric(Species))
########################
summary(Petal.Length[which(Species=="setosa")])
########################
Petal.Length.V<-Petal.Length[-which(Species=="setosa")]
Sepal.Length.V<-Sepal.Length[-which(Species=="setosa")]
########################
boxplot(Petal.Length~Species)
########################
plot(Petal.Length.V~Sepal.Length.V)
########################
l<-lm(Petal.Length.V~Sepal.Length.V)
# better way
l<-lm(Petal.Length~Sepal.Length,subset=which(Species!="setosa"))
########################
summary(l)
########################
names(l)
names(summary(l))
########################
par(mfrow=c(2,2))
plot(l)
par(mfrow=c(1,1))
########################
plot(Petal.Length~Sepal.Length,main="Iris Length Data",col=as.numeric(Species))
abline(coef(l),col="purple")
######################## 
setosa.rows<-which(Species=="setosa")
points(Sepal.Length[setosa.rows],Petal.Length[setosa.rows],col="blue")



