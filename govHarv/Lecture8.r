## Lecture 8 R code ##
## The usual warnings apply ##

library(car)
library(mgcv)
library(MASS)

data(SLID)
SLIDna <- na.omit(SLID)
summary(SLIDna)
SLID2 <- subset(SLIDna,education >= 8 & age >= 25 )
attach(SLID2)


#mod.edu<- lm(wages~education)
#mod.edu.quad <- lm(wages ~  education + I(education^2))

mod.log.edu<- lm(log(wages)~education)
#mod.log.edu.quad <- lm(log(wages) ~  education + I(education^2))

#summary(mod.edu)
summary(mod.log.edu)
summary(mod.log.edu.age)


pdf("oneVarplots.pdf")
#par(mfrow=c(1,2))
#plot(education, wages,pch=".")
#abline(mod.edu,col="red",lwd=2)
#xrange <- 0:20
#lines(xrange,coef(mod.edu.quad)[1] + coef(mod.edu.quad)[2]*xrange  + coef(mod.edu.quad)[3]*xrange^2,col="blue",lwd=2)
#lines(lowess(education, wages),col="blue",lwd=2)
plot(education, log(wages),pch=".")
abline(mod.log.edu,col="red",lwd=2)
#xrange <- 0:20
#lines(xrange,coef(mod.log.edu.quad)[1] + coef(mod.log.edu.quad)[2]*xrange  + coef(mod.log.edu.quad)[3]*xrange^2,col="blue",lwd=2)
lines(lowess(education, log(wages)),col="blue",lwd=2)
legend(x=16,y=1.5,legend=c("OLS","lowess"),lwd=2,col=c("red","blue"))
dev.off()

#pdf("plot1.pdf")
#par(mfrow=c(1,2))
#plot(mod.edu,which=1)
#plot(mod.log.edu,which=1)
#dev.off()

pdf("plot2.pdf")
#par(mfrow=c(1,2))
#plot(mod.edu,which=2)
plot(mod.log.edu,which=2)
dev.off()

pdf("plot3.pdf")
#par(mfrow=c(1,2))
#plot(mod.edu,which=3)
plot(mod.log.edu,which=3)
dev.off()

### Age stuff ###

mod.log.edu.age <- lm(log(wages)~ education + age)
summary(mod.log.edu.age)
mod.log.edu
cor(education,age)

pdf("plot4.pdf")
par(mfrow=c(1,1))
av.plot(mod.log.edu.age,variable=education,identify.points=FALSE)
#plot(mod.log.edu.age,which=1)
dev.off()

pdf("plot4a.pdf")
par(mfrow=c(1,1))
cr.plot(mod.log.edu.age,variable=education,identify.points=FALSE)
#plot(mod.log.edu.age,which=1)
dev.off()

pdf("plot5.pdf")
par(mfrow=c(1,1))
mod.gam <- gam(log(wages)~ s(education) + s(age))
plot(mod.gam,select=1)
dev.off()

pdf("plot6.pdf")
par(mfrow=c(1,1))
plot(mod.log.edu.age,which=2)
dev.off()

pdf("plot7.pdf")
par(mfrow=c(1,1))

plot(mod.log.edu.age,which=3)
dev.off()


#### Matix Stuff ####

X <- matrix(c(1,1,1,27,46,33,12,16,12,0,1,1),nc=4)
solve(t(X) %*% X)

X <- matrix(c(1,1,1,1,27,46,33,42,1980,1961,1974,1965),nc=3)
solve(t(X) %*% X)

X <- matrix(c(1,1,1,1,27,46,33,42,27000,46000,33000,42000),nc=3)
solve(t(X) %*% X)

X <- matrix(c(1,1,1,1,0,1,1,0,1,0,0,1),nc=3)
solve(t(X) %*% X)


#### Treisman Data ####

treisman <- read.table("Treisman98TI.csv", na.strings="NA", header=TRUE, sep=",")
summary(treisman)


treisman.mod <- lm(TI98 ~ commonlaw + britcolony+ noncolony + pctprot +elf,data=treisman)

model.matrix(mod)

