#
# http://jsekhon.fas.harvard.edu/gov1000/mr1.R
#

data.approval <-
read.table(file="http://www.courses.fas.harvard.edu/~gov1000/Data/approval.asc",header=T)

#what are the variables in the approval data set?
names(data.approval)
attach(data.approval)

lm3  <- lm(approval~unrate+inflation)
cat("\n*****************************************\n")
cat("\nResults from using the R regression command\n")
print(summary(lm3))

Y  <- approval
X1 <- unrate
X2 <- inflation

base  <- var(X1)*var(X2)-cov(X1,X2)^2
b1  <- (cov(X1,Y)*var(X2) - cov(X2,Y)*cov(X1,X2))/base
b2  <- (cov(X2,Y)*var(X1) - cov(X1,Y)*cov(X1,X2))/base

a1  <- mean(Y) - b1*mean(X1) - b2*mean(X2);
cat("\n*****************************************\n")
cat("\nResults from doing it by hand\n")
cat("intercept:",a1,"\n")
cat("unrate   :",b1,"\n")
cat("inflation:",b2,"\n")
cat("\n*****************************************\n")
