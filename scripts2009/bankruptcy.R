# DATA MINING AND INFORMATION SYSTEMS
# PREDICTING FIRM BANKRUPTCY APRIL, 2ND 2009 
# (COURTESY SHMUELI, PATEL, BRUCE)
############################################################################################################################################################
# DATA DESCRIPTION
# n = 166 NUMBER OF FIRMS IN SAMPLE
# p = 24 FINANCIAL RATIOS
# 2 CLASSES: FAILED (66), HEALTHY (66)
##############################################################################
# DATA INPUT
df <- read.csv2("http://venus.unive.it/romanaz/datamin/dati/Bankruptcy.csv",
 header=TRUE)
df <- read.csv2("D:/corso_datam&sinfo/esercitazioni/bankruptcy/Bankruptcy.csv",
 header=TRUE)
str(df)
attach(df)
n <- dim(df)[1]
p <- dim(df)[2]-3
##############################################################################
# INITIAL EXPLORATION
summary(df[,-(1:3)])
round(cor(df[,-(1:3)]),2)
colore <- rep("black",n)
colore[D == 0] <- "red"
################################################################################
# VARIANCE DECOMPOSITION
nclas <- table(D)
covtot <- cov(df[,-(1:3)])*(n-1)/n
cov1 <- cov(df[D == 0,-(1:3)])*(nclas[1]-1)/nclas[1]
cov2 <- cov(df[D == 1,-(1:3)])*(nclas[2]-1)/nclas[2]
covwith <- (nclas[1]*cov1+nclas[2]*cov2)/n  
vratio <- 1- diag(covwith)/diag(covtot)  #between-groups var. to total variance ratios
100*round(vratio,3)
plot(1:p,vratio,type="h",lwd=2,xlab="Variable No.",
 ylab="Between-Gr. Variance to Total Variance Ratio",main="Bankruptcy Data")
# Potentially useful variables: R4,R9,R14,R17,R23 vratio > 15%
################################################################################
# GAINING MORE INSIGHTS THROUGH PLOTS
boxplot(R9~D,horizontal=TRUE,col="lavender",names=c("Failed","Healthy"),
 xlab="R9",main="Bankruptcy Data")
boxplot(R17~D,horizontal=TRUE,col="lavender",names=c("Failed","Healthy"),
 xlab="R17",main="Bankruptcy Data")
boxplot(R23~D,horizontal=TRUE,col="lavender",names=c("Failed","Healthy"),
 xlab="R23",main="Bankruptcy Data")
pairs(df[,c(7,12,17,20,26)],col=colore,pch=20)
################################################################################
# PCA (dati standardizzati) 
pc <- princomp(df[,-(1:3)],cor=TRUE)
summary(pc)
#1st pc: 37% expl. var., 2nd pc: 13%; cum. 50%
round(cor(df[,-(1:3)],pc$scores[,1:5]),2)
#highest correlations with "group discriminating" variables
plot(pc$scores[,1:2],
 xlab="PC1 (37%)",ylab="PC2 (13%)",cex=0.5,col=colore)
#2 dangerous outliers
df[pc$scores[,1] < -10 | pc$scores[,1] > 10,] # units no. 44 and 100
# discard units no. 44 and 100 and repeat pca
pc1 <- princomp(df[-c(44,100),-(1:3)],cor=TRUE)
summary(pc1)
round(cor(df[-c(44,100),-(1:3)],pc1$scores[,1:5]),2)
plot(pc1$scores[,1:2],
 xlab="PC1 (40%)",ylab="PC2 (14%)",cex=0.5,col=colore)
layout(matrix(1:2,2,2,byrow=TRUE))
plot(scale(pc1$scores[,1:2]),
 xlab="PC1 (40%)",ylab="PC2 (14%)",cex=0.5,col=colore)
plot(scale(pc1$scores[,c(1,3)]),
 xlab="PC1 (40%)",ylab="PC3 (12%)",cex=0.5,col=colore)
plot(scale(pc1$scores[,2:3]),
 xlab="PC2 (14%)",ylab="PC3 (12%)",cex=0.5,col=colore)
plot(scale(pc1$scores[,c(1,4)]),
 xlab="PC1 (40%)",ylab="PC4 (7%)",cex=0.5,col=colore)
layout(matrix(1,1,1))
################################################################################
# CVA
library(MASS)
cv <- lda(df[-c(44,100),-(1:3)],D[-c(44,100)])
cv$svd^2
prev_cv <- predict(cv)
str(prev_cv)
round(cor(df[-c(44,100),-(1:3)],prev_cv$x[,1]),2)
#plotting cv versus 1st pc
plot(pc1$scores[,1],prev_cv$x[,1], 
 xlab="PC1",ylab="CV1",main="Bankruptcy Data",
 col=colore,cex=0.5)
points(cv_means[,1],cv_means[,2],pch="*",cex=1.5,col="magenta")
#################################################################################
# BACK CLASSIFICATION BY NEAREST CENTROID RULE
# CANONICAL VARIATES MEANS
cv_means <- matrix(0,length(nclas),length(nclas)-1) 
for (i in 1:length(nclas)) {
 cv_means[i,] <- lapply(split(as.data.frame(prev_cv$x),D[-c(44,100)]),mean)[[i]]
}
# CYCLE FOR EUCLIDEAN DISTANCES
dist <- matrix(0,n,2);st_classe <- c()
for(i in 1:(n-2)) {
 for(j in 1:2) {
  dist[i,j] <- sum((prev_cv$x[i,]-cv_means[j,])^2)
  }
  st_classe[i] <- which.min(dist[i,]) 
  }
cv_conf <- table(D[-c(44,100)],st_classe)        #confusion matrix 
100*(1-sum(diag(cv_conf))/(n-2))                 #14.6% class. error may be optimistic!
#################################################################################
# LINEAR DISCRIMINANT
ld <- lda(df[-c(44,100),-(1:3)],D[-c(44,100)],CV=TRUE)
str(ld)
round(ld$posterior[1:2,],3)                      #posterior  probs of first 2 units
ld_conf <- table(D[-c(44,100)],ld$class)         #confusion matrix
100*(1-sum(diag(ld_conf))/(n-2))                 #26.1% class. error
#################################################################################
# QUADRATIC DISCRIMINANT
qd <- qda(df[-c(44,100),-(1:3)],D[-c(44,100)],CV=TRUE)
str(qd)
round(qd$posterior[1:5,],3)                      #posterior probability of first 5 glasses
qd_conf <- table(D[-c(44,100)],qd$class)         #confusion matrix
100*(1-sum(diag(qd_conf))/(n-2))                 #24.6% class. error 
#################################################################################
# NEAREST NEIGHBOUR CLASSIFICATION (LEAVING-ONE-OUT CV)
library(class)
#ITERATIVE SEARCH FOR BEST SIZE OF NEIGHBOURHOODS (k PARAMETER)
ris <- data.frame(1:(n-2),D[-c(44,100)])
err <- c()
for(i in 1:40) {
 clas <- knn.cv(df[-c(44,100),-(1:3)],D[-c(44,100)],2*i-1,prob=FALSE)
 ris <- data.frame(ris,clas)
 conf <- table(D[-c(44,100)],clas)
 err[i] <- 1-sum(diag(conf)/(n-2))
 }
plot(2*(1:40)-1,100*err,type="b",pch=20,xlab="K",ylab="Classification Errors (%)",
 ylim=c(0,100),main="Leave-one-out Cross-Validation")
# k = 11 best value of k
#################################################################################
nn <- knn.cv(df[-c(44,100),-(1:3)],D[-c(44,100)],k=11,prob=TRUE)
str(nn)
nn[c(15,18)]
round(attr(nn,"prob")[1:5],3)                   #probability of winning class 
nn_conf <- table(D[-c(44,100)],nn)              #confusion matrix
100*(1-sum(diag(nn_conf))/(n-2))                #31.5% class. error
#################################################################################
# LOGISTIC REGRESSION
############################################################################
df1 <- df[-c(44,100),]
n1 <- dim(df1)[1]
logit <- glm(D~R1+R2+R3+R4+R5+R6+R7+R8+R9+R10+R11+R12+R13+R14+R15
             +R16+R17+R18+R19+R20+R21+R22+R23+R24,df1,family="binomial")
summary(logit)
#Too optimistic results!
#USING LEAVING-ONE-OUT
lg_pred <- c()
for (i in 1:n1) {
 logit1 <- glm(D~R1+R2+R3+R4+R5+R6+R7+R8+R9+R10+R11+R12+R13+R14+R15
             +R16+R17+R18+R19+R20+R21+R22+R23+R24,df1[-i,],family="binomial")
 lg_pred[i] <- predict.glm(logit1,newdata=df1[i,],type="response")
}
plot(logit$fitted.values,lg_pred,
 xlab="Raw Estimates",ylab="Leave-One-Out Estimates",main="Bankruptcy Data")
abline(h=0.5,v=0.5,col="red")
##############################################################################
# ROC CURVE
##############################################################################
# FUNCTION roc COMPUTES THE POINT OF THE ROC CURVE FOR CUTOFF VALUE x
roc <- function(x,class,probs) {
 roc_x <- length(class[class == 0 & probs >= x])/length(class[class == 0])
 roc_y <- length(class[class == 1 & probs >= x])/length(class[class == 1])
 return(c(roc_x,roc_y))
}
# FUNCTION roc.plot PLOTS THE ROC CURVE
roc.plot <- function(step,class,probs,col) {
  roc1 <- c();roc2 <- c()
  for (i in seq(0,1,step)) {
   roc1 <- c(roc1,roc(i,class,probs)[1])
   roc2 <- c(roc2,roc(i,class,probs)[2])
   }
 points(roc1,roc2,type="b",pch=20,col=col)
 }
##############################################################################
plot(c(0,1),c(0,1),pch=20,xlim=c(0,1),ylim=c(0,1),
 xlab="1-Specificity",ylab="Sensitivity",main="Bankruptcy Data",
 sub="ROC CURVE")
roc.plot(0.001,,logit$fitted.values,"red")   #LOGISTIC
roc.plot(0.001,D[-c(44,100)],lg_pred,"magenta")           #LOGISTIC WITH LOO
roc.plot(0.001,D[-c(44,100)],ld$posterior[,2],"blue")     #LDAD[-c(44,100)]
roc.plot(0.001,D[-c(44,100)],qd$posterior[,2],"green")    #QDA
abline(a=0,b=1)
legend("bottomright",
 legend=c("Logistic Regr. (Raw)","Logistic Regr. (Leaving-One-Out)",
 "Lda (Leaving-One-Out)","Qda (Leaving-One-Out)"),
 pch=20,col=c("red","magenta","blue","green"),
 pt.cex=0.8,cex=0.8)

##############################################################################
# LIFT CURVE (NORMALIZED VERSION)
##############################################################################
# FUNCTION lift COMPUTES THE POINT OF THE LIFT CURVE FOR CUTOFF VALUE x
lift <- function(x,class,probs) {
 lift <- length(class[class == 1 & probs >= x])/length(class[class == 1])
 }
# FUNCTION lift.plot PLOTS THE LIFT CURVE
lift.plot <- function(step,class,probs,col) {
  lift1 <- c();lift2 <- c()
  for (i in seq(0,1,step)) {
   lift1 <- c(lift1,i)
   lift2 <- c(lift2,lift(1-i,class,probs))
   }
 points(lift1,lift2,type="b",pch=20,col=col)
 }
plot(c(0,1),c(0,1),pch=20,xlim=c(0,1),ylim=c(0,1),
 xlab="Fraction of Sample Size",ylab="Sensitivity",
 main="Bankruptcy Data",sub="LIFT CURVE")
lift.plot(0.001,D[-c(44,100)],lg_pred,"magenta")#LOG (LOO)
lift.plot(0.001,D[-c(44,100)],ld$posterior[,2],"blue")   #LDA
lift.plot(0.001,D[-c(44,100)],qd$posterior[,2],"green")  #QDA
abline(a=0,b=1)

