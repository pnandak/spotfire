## Course 02441: Applied Statistics and Statistical Software
## January 2007 by Lasse Engbo Christiansen
##
## This file contains examples given during lectures

###############################################
## Day 1: First R session
###############################################
a<-1:20
b<-runif(20)
c<-rnorm(20,mean=3,sd=4)
plot(a,b)
plot(a,c)
points(a,rnorm(20,3,1),col=2,pch=19)
boxplot(b,c,rnorm(20,3,1))
d<-rnorm(20,3,1)

mean(b)
mean(c)
var(b)
sd(d)
hist(b)
hist(d)

plot(a,c)
points(a[c<2],c[c<2],pch=19)
abline(h=2,col=3,lwd=2)

dat<-read.table("humerus.txt",header=TRUE)
which(dat[,2]==1)

e<-matrix(1:12,ncol=3)
e
e[,-2]

dev.print(postscript,file="test.ps")
?postscript
dev.print(postscript,file="test.ps",horizontal=FALSE)
rm(list=ls())

