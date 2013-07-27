# DATA MINING AND INFORMATION SYSTEMS
# CLASSIFICATION METHODS, MARCH, 18TH 2009 
# RIDING MOWERS DATA (COURTESY SHMUELI, PATEL, BRUCE)
############################################################################
# UNITS 25 GARDEN OWNERS
# VARIABLES 3
# V1 INCOME *NUMERICAL
# V2 LOT SIZE *NUMERICAL
# V3 RIDING MOWER OWNERSHIP *CATEGORICAL
############################################################################
# INPUT DATA TABLE
rmow <- read.csv2("http://venus.unive.it/romanaz/datamin/dati/RidingMowers.csv",
 header=TRUE)
rmow <- read.csv2("D:/corso_datam&sinfo/esercitazioni/ridingmower/RidingMowers.csv",
 header=TRUE)
str(rmow)
n <- dim(rmow)[1]
attach(rmow)
colore <- rep("black",n)
colore[Ownership == "owner"] <- "green"
############################################################################
# INITIAL ANALYSIS
summary(rmow)
tapply(Income,Ownership,summary)
tapply(Lot_Size,Ownership,summary)
boxplot(Income~Ownership,horizontal=TRUE,names=c("Nonow","Ow"),
 xlab="Income",main="Riding Mowers Data")
boxplot(Lot_Size~Ownership,horizontal=TRUE,names=c("Nonow","Ow"),
 xlab="Lot Size",main="Riding Mowers Data")
plot(Income,Lot_Size,col=colore,pch=20,
 xlab="Income",ylab="Lot Size",main="Riding Mowers Data")
abline(h=mean(Lot_Size),v=mean(Income),col="red",lty="dashed")
points(mean(Income),mean(Lot_Size),pch="*",cex=1.5)
cor(Income,Lot_Size)
############################################################################
# KNN CLASSIFIER
############################################################################
library(class) # load package class including functions for KNN
############################################################################
#ITERATIVE SEARCH FOR BEST SIZE OF NEIGHBOURHOODS (k PARAMETER)
ris <- data.frame(1:n,Ownership)
kk <- floor(n/2)
err <- c()
for(i in 1:kk) {
 clas <- knn.cv(rmow[,-3],Ownership,2*i-1,prob=FALSE)
 ris <- data.frame(ris,clas)
 conf <- table(rmow$Ownership,clas)
 err[i] <- (conf[1,2]+conf[2,1])/n
 }
plot(2*(1:kk)-1,100*err,type="b",pch=20,xlab="K",ylab="Classification Errors (%)",
 ylim=c(0,100),main="Leave-one-out Cross-Validation")
err
############################################################################
clas <- knn.cv(rmow[,-3],Ownership,15,prob=TRUE)
clas
data.frame(Ownership,clas,attr(clas,"prob"))
conf_knn <- table(Ownership,clas)
100*(1-sum(diag(conf_knn)/n)) #classification error 25%
plot(Income,Lot_Size,col=colore,pch=20,
 xlab="Income",ylab="Lot Size",main="Riding Mowers Data")
abline(h=mean(Lot_Size),v=mean(Income),col="red",lty="dashed")
points(mean(Income),mean(Lot_Size),pch="*",cex=1.5)
text(Income[Ownership != clas],Lot_Size[Ownership != clas],labels="ERR",col="red",
 cex=0.5,pos=3)
############################################################################
# MULTIPLE REGRESSION CLASSIFIER
############################################################################
class <- rep(0,n);class[Ownership == "owner"] <- 1
rmow1 <- data.frame(rmow,class)
regr <- lm(class~Income+Lot_Size,rmow1)
summary(regr)
plot(Income,Lot_Size,col=colore,pch=20,
 xlab="Income",ylab="Lot Size",main="Riding Mowers Data")
#abline(h=mean(Lot_Size),v=mean(Income),col="red",lty="dashed")
#points(mean(Income),mean(Lot_Size),pch="*",cex=1.5)
coeff <- regr$coefficients
abline(a=(1-2*coeff[1])/(2*coeff[3]),b=-coeff[2]/coeff[3],lwd=2,
 lty="dashed")
text(Income,Lot_Size,labels=as.character(round(regr$fitted.values,2)),cex=0.5,
 pos=4)
############################################################################
# LOGISTIC REGRESSION CLASSIFIER
############################################################################
logit <- glm(class~Income+Lot_Size,rmow1,family="binomial")
summary(logit)
plot(Income,Lot_Size,col=colore,pch=20,
 xlab="Income",ylab="Lot Size",main="Riding Mowers Data")
#abline(h=mean(Lot_Size),v=mean(Income),col="red",lty="dashed")
#points(mean(Income),mean(Lot_Size),pch="*",cex=1.5)
lcoeff <- logit$coefficients
abline(a=(1-2*lcoeff[1])/(2*lcoeff[3]),b=-lcoeff[2]/lcoeff[3],col="red",lwd=2,
 lty="dashed")
text(Income,Lot_Size,labels=as.character(round(logit$fitted.values,2)),cex=0.5,
 pos=2,col="red")
legend("topright",legend=c("OLS","LOGIT"),lty="dashed",col=c("black","red"),
 cex=0.8,lwd=2)
legend("topleft",legend=c("Owner","Non-Owner"),pch=20,col=c("green","black"),
 pt.cex=0.8,cex=0.8)
############################################################################
# CANONICAL VARIATE CLASSIFIER (FISHER'S LINEAR DISCRIMINANT FUNCTION)
############################################################################
library(MASS)
cva <- lda(Ownership~Income+Lot_Size,rmow)
str(cva)
prev_cva <- predict(cva)
str(prev_cva)
round(cor(rmow[,-3],prev_cva$x),2)
boxplot(prev_cva$x~Ownership,horizontal=TRUE,xlab="Canonical Variate",
 names=c("Nonow","Ow"))
plot(scale(Income),scale(Lot_Size),col=colore,pch=20,
 xlab="Income",ylab="Lot Size",main="Riding Mowers Data")
abline(a=0,b=-8.5*cva$scaling[1]/cva$scaling[2],col="red",lwd=2)
points(0,0,pch="*")
text(scale(Income),scale(Lot_Size),labels=as.character(round(prev_cva$posterior[,1],2)),
 pos=2,cex=0.6)
legend("topleft",legend=c("Owner","Non-Owner"),pch=20,col=c("green","black"),
 pt.cex=0.8,cex=0.8)
#CONFUSION MATRIX
conf_m1 <- table(Ownership,prev_cva$class) # possible bias (training set is used)
100*(1-sum(diag(conf_m1)/n)) #classification error 12.5%
#REMOVING BIAS
cva1 <- lda(Ownership~Income+Lot_Size,rmow,CV=TRUE)
conf_m2 <- table(Ownership,cva1$class) # leave-one-out cross validation
100*(1-sum(diag(conf_m2)/n)) #classification error 20.8%
#TRACING ERRORS
rmow[Ownership != cva1$class,] 
text(scale(Income)[Ownership != cva1$class],scale(Lot_Size)[Ownership != cva1$class],
 labels="ERR",cex=0.6,col="red",pos=4)
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
 xlab="1-Specificity",ylab="Sensitivity",main="Riding Mowers",sub="ROC CURVE")
roc.plot(0.001,class,logit$fitted.values,"red")      #LOGIT REGRESSION
roc.plot(0.001,class,prev_cva$posterior[,2],"blue")  #FISHER LDA
abline(a=0,b=1)
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
 main="Riding Mowers",sub="LIFT CURVE")
lift.plot(0.001,class,logit$fitted.values,"red") #LOGIT REGRESSION
lift.plot(0.001,class,prev_cva$posterior[,2],"blue")  #FISHER LDA
abline(a=0,b=1)

