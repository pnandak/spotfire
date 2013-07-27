#### Lecture 9 R code ####
## The usual warnings apply

treisman <- read.table("Treisman98TI.csv", na.strings="NA", header=TRUE, sep=",")

mod1 <- lm(TI98~commonlaw+britcolony+noncolony+pctprot+elf+FMMexports93, data=treisman)
summary(mod1)

## Simulation Example ##
n <- 100
k <- 20
set.seed(12345)
Xmat <- matrix(rnorm(n*k),nr=n,nc=k)
y <- rnorm(n)
simData <- data.frame(y,Xmat)
modSim <- lm(y ~ .,data=simData)
summary(modSim)

# F-dist plot Omnibus test# 

pdf(file="df.pdf")
x=seq(from=0,to=12,by=.1)
plot(x=x,df(x = x,df1=6,df2=57),type="l")
abline(v=11.68,col="red")   
abline(v=qf(p=.95,df1=6,df2=57),col="blue")
legend(x=4,y=.7,legend=c("test statistic","critical value"),lwd = 2,col=c("red","blue"))
dev.off()

# some slopes = 0 #

pdf(file="df2.pdf")
x=seq(from=0,to=6,by=.1)
plot(x=x,df(x = x,df1=3,df2=57),type="l",col="green")
lines(x=x,df(x = x,df1=6,df2=57),type="l")
abline(v=3.9114,col="red")
#abline(v=qf(p=.95,df1=6,df2=57),col="blue")
abline(v=qf(p=.95,df1=3,df2=57),col="blue")
legend(x=4,y=.7,legend=c("test statistic","critical value","omnibus null","subset null"),lwd = 2,col=c("red","blue","black","green"))
dev.off()

##

library(car)

L <- matrix(data=0, nr=3, nc=7)
L[1,2] <- L[2,3] <- L[3,4] <- 1
linear.hypothesis(mod1, L)

 betahat <- coef(mod1)
 m <- L %*% betahat
 V <- vcov(mod1)


 F <- ( t(m) %*% solve(L %*% V %*% t(L)) %*% m ) / nrow(L)
 F
 1 - pf(F, 3, 57)

## intervals for interaction terms ##

library(car)
library(mgcv)
library(MASS)

data(SLID)
SLIDna <- na.omit(SLID)
summary(SLIDna)
SLID2 <- subset(SLIDna,education >= 8 & age >= 25 )
attach(SLID2)


mod.edu.age <- lm(log(wages)~education+age)
summary(mod.edu.age)
mod.edu.age.int <- lm(log(wages)~education*age)
summary(mod.edu.age.int)


# Interaction CI plots

beta <- coef(mod.edu.age.int)
gamma <- beta["education"] + beta["education:age"] * age
pdf("intCI.pdf")
par(mar=c(5,5,1,1))
plot(x = age, y = gamma,ylim=c(0,max(gamma)),ylab=expression(paste(beta[1], "+", beta[3],"*age")),  type = "l", pch = 19)

# 95% CI
VCV <- vcov(mod.edu.age.int)
gamma.se <- sqrt(diag(VCV)["education"] + (age^2) * diag(VCV)["education:age"] + 
                 2 * age * VCV["education", "education:age"])
upper <- gamma + qt(.975, df = length(age) - mod.edu.age.int$rank) * gamma.se
lower <- gamma + qt(.025, df = length(age) - mod.edu.age.int$rank) * gamma.se

sorter <- order(age)
lines(x = age[sorter], y = upper[sorter], col = "red", lty = "dashed")
lines(x = age[sorter], y = lower[sorter], col = "red", lty = "dashed")
lines(density(age))

dev.off()

# joint confidence regions #
mod1 <- lm(TI98~commonlaw+britcolony+noncolony+pctprot+elf+FMMexports93, data=treisman)
summary(mod1)

mod.CR <- lm(TI98 ~ elf+FMMexports93 , data=treisman)
summary(mod.CR)
confint(mod.CR,level=.90)

pdf(file="2varEllipse.pdf")
confidence.ellipse(mod.CR,level=.90,xlim=c(-.01,.06),ylim=c(-.01,.06))
points(x=0,y=0,pch=16)
abline(h=confint(mod.CR,level=.90)[3,],col="red",lwd=1.5)
abline(v=confint(mod.CR,level=.90)[2,],col="red",lwd=1.5)
dev.off()

pdf(file="Ellipse.pdf")
par(mfrow=c(1,2),mar=c(5,5,3,1))
confidence.ellipse(mod1,which.coef=c("elf","FMMexports93"),xlim=c(-.01,.06),ylim=c(-.01,.06),main="Standard Ellipse")
points(0,0,pch=16)
abline(h=confint(mod1,level=.90)[7,],col="red",lwd=1.5)
abline(v=confint(mod1,level=.90)[6,],col="red",lwd=1.5)
confidence.ellipse(mod1,which.coef=c("elf","FMMexports93"),Scheffe=TRUE,col="blue",xlim=c(-.01,.06),ylim=c(-.01,.06),main="Scheffe Ellipse")
points(0,0,pch=16)
abline(h=confint(mod1,level=.90)[7,],col="red",lwd=1.5)
abline(v=confint(mod1,level=.90)[6,],col="red",lwd=1.5)
dev.off()



