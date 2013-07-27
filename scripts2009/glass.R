# DATA MINING AND INFORMATION SYSTEMS
# DISCRIMINANT ANALYSIS MARCH, 27TH 2009 
# GLASS DATA
############################################################################################################################################################
# DATA DESCRIPTION
# n = 214 GLASS SPECIMENS
# p = 9 VARIABLES
# Ri: refractive index
# Na, Mg, Al, Si, K, Ca, Ba, Fe: sodium, magnesium, aluminium, silicon,
# potassium, calcium, barium, iron
# Class: classes
# 1: building windows, glass type "float" (piatto, semplice)
# 2: building windows, glass type non "float" (lavorato)
# 3: car windows
# 4: other (table ware, lamp bulbs)
##############################################################################
# DATA INPUT
glass <- read.csv("http://venus.unive.it/romanaz/datamin/dati/glass_data.csv",
 header=TRUE)
glass <- read.csv("D:/corso_datam&sinfo/esercitazioni/glass/glass_data.csv",
 header=TRUE)
str(glass)
##############################################################################
# INITIAL EXPLORATION
summary(glass[,2:10])
round(cor(glass[,2:10]),2)
class1 <- glass[,11]                           #classification variable
class1[glass[,11] > 4] <- 4
table(glass[,11],class1)
colore <- rep("black",dim(glass)[1])
colore[class1 == 2] <- "red"
colore[class1 == 3] <- "green" 
colore[class1 == 4] <- "cyan" 
################################################################################
# VARIANCE DECOMPOSITION
nclas <- table(class1)
n <- sum(nclas)
covtot <- cov(glass[,2:10])*(n-1)/n
cov1 <- cov(glass[class1 == 1,2:10])*(nclas[1]-1)/nclas[1]
cov2 <- cov(glass[class1 == 2,2:10])*(nclas[2]-1)/nclas[2]
cov3 <- cov(glass[class1 == 3,2:10])*(nclas[3]-1)/nclas[3]
cov4 <- cov(glass[class1 == 4,2:10])*(nclas[4]-1)/nclas[4]
covwith <- (nclas[1]*cov1+nclas[2]*cov2+nclas[3]*cov3+nclas[4]*cov4)/n  
vratio <- 1- diag(covwith)/diag(covtot)
100*round(vratio,3)
################################################################################
# SOME PLOTS
boxplot(glass$Mg~class1,horizontal=TRUE,col="lavender",names=1:4,
 xlab="Magnesium (%)",main="Glass Data")
boxplot(glass$Al~class1,horizontal=TRUE,col="lavender",names=1:4,
 xlab="Aluminium (%)",main="Glass Data")
plot(scale(glass$Mg),scale(glass$Al),xlab="Magnesium (std units)",ylab="Aluminium (std units)",
 pch=as.character(class1),col=colore,main="Glass Data",cex=0.7)
################################################################################
# PCA (dati standardizzati) 
pc <- princomp(glass[,2:10],scores=TRUE,cor=TRUE)
summary(pc)
round(cor(glass[,2:10],pc$scores[,1:4]),2)
plot(pc$scores[,1:2],pch=as.character(class1),
 xlab="PC1 (28%)",ylab="PC2 (23%)",cex=0.5,col=colore)
layout(matrix(1:4,2,2,byrow=TRUE))
plot(pc$scores[,1:2],pch=as.character(class1),
 xlab="PC1 (28%)",ylab="PC2 (23%)",cex=0.5,col=colore)
plot(pc$scores[,c(1,3)],pch=as.character(class1),
 xlab="PC1 (28%)",ylab="PC3 (16%)",cex=0.5,col=colore)
plot(pc$scores[,2:3],pch=as.character(class1),
 xlab="PC2 (23%)",ylab="PC3 (16%)",cex=0.5,col=colore)
plot(pc$scores[,c(1,4)],pch=as.character(class1),
 xlab="PC1 (28%)",ylab="PC4 (13%)",cex=0.5,col=colore)
layout(matrix(1,1,1))
################################################################################
# CVA
library(MASS)
cv <- lda(glass[,2:10],class1)
cv$svd^2
prev_cv <- predict(cv)
str(prev_cv)
#CANONICAL VARIATES MEANS
cv_means <- matrix(0,length(nclas),length(nclas)-1) 
for (i in 1:length(nclas)) {
 cv_means[i,] <- lapply(split(as.data.frame(prev_cv$x),class1),mean)[[i]]
}
plot(prev_cv$x[,1:2],xlab="CV1",ylab="CV2",main="Glass Data",
 pch=as.character(class1),col=colore,cex=0.5)
points(cv_means[,1],cv_means[,2],pch="*",cex=1.5,col="magenta")
#################################################################################
# BACK CLASSIFICATION BY NEAREST CENTROID RULE
dist <- matrix(0,n,4);st_classe <- c()
# CYCLE FOR EUCLIDEAN DISTANCES
for(i in 1:n) {
 for(j in 1:4) {
  dist[i,j] <- sum((prev_cv$x[i,]-cv_means[j,])^2)
  }
  st_classe[i] <- which.min(dist[i,]) 
  }
cv_conf <- table(class1,st_classe)               #confusion matrix 
100*(1-sum(diag(cv_conf))/n)                     #32.7% class. error 
#glass[class1 != st_classe,]
#################################################################################
# LINEAR DISCRIMINANT ANALYSIS
ld <- lda(glass[,2:10],class1,CV=TRUE)
str(ld)
round(ld$posterior[1:2,],3)                      #posterior  probs of first 2 units
ld_conf <- table(class1,ld$class)                #confusion matrix
100*(1-sum(diag(ld_conf))/n)                     #33.6% class. error
#################################################################################
# QUADRATIC DISCRIMINANT ANALYSIS
qd <- qda(glass[,2:10],class1,CV=TRUE)
str(qd)
round(qd$posterior[1:5,],3)                      #posterior probability of first 5 glasses
qd_conf <- table(class1,qd$class)                #confusion matrix
100*(1-sum(diag(qd_conf))/n)                     #41.6% class. error 
#################################################################################
# NEAREST NEIGHBOUR CLASSIFICATION (LEAVING ONE OUT CLASSIFICATIONS)
library(class)
#ITERATIVE SEARCH FOR BEST SIZE OF NEIGHBOURHOODS (k PARAMETER)
ris <- data.frame(1:n,class1)
kk <- floor(n/2)
err <- c()
for(i in 1:40) {
 clas <- knn.cv(glass[,2:9],class1,2*i-1,prob=FALSE)
 ris <- data.frame(ris,clas)
 conf <- table(class1,clas)
 err[i] <- 1-sum(diag(conf)/n)
 }
plot(2*(1:40)-1,100*err,type="b",pch=20,xlab="K",ylab="Classification Errors (%)",
 ylim=c(0,100),main="Leave-one-out Cross-Validation")
err                                           # k = 1 best value of k
#################################################################################
nn3 <- knn.cv(glass[,2:9],class1,k=1,prob=TRUE)
str(nn3)
nn3[c(15,188)]
round(attr(nn3,"prob")[1:5],3)                   #probability of winning class 
nn3_conf <- table(class1,nn3)                    #confusion matrix
100*(1-sum(diag(nn3_conf))/n)                    #24.3% class. error
#################################################################################

