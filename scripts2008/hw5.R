##HW 5
##R Code

##Problem 2
set.seed(12345)
##Plot using 
xs1<-seq(from=2, to=6, length.out=50)
xs2<-seq(from=30,to=34, length.out=50)

ys1a<-2*xs1-6+rnorm(length(xs1),0,4)
ys1b<-2*xs1-6+rnorm(length(xs1),0,4)
ys1c<-2*xs1-6+rnorm(length(xs1),0,4)
ys2a<-2*xs2-6+rnorm(length(xs1),0,4)
ys2b<-2*xs2-6+rnorm(length(xs1),0,4)
ys2c<-2*xs2-6+rnorm(length(xs1),0,4)

pdf(file="intercept.pdf")
plot(ys1a~xs1, xlim=c(0, 34), ylim=c(-30,80), xlab="X", ylab="Y")
abline(lm(ys1a~xs1), lty=2)
points(ys1b~xs1)
abline(lm(ys1b~xs1), lty=2)
points(ys1c~xs1)
abline(lm(ys1c~xs1), lty=2)
points(ys2a~xs2)
abline(lm(ys2a~xs2), lty=1)
points(ys2b~xs2)
abline(lm(ys2b~xs2), lty=1)
points(ys2c~xs2)
abline(lm(ys2c~xs2), lty=1)
dev.off()

##load iraq data
setwd("N:/gov 2000/")
iraq<-read.csv("iraq.csv", header=T, as.is=T)
library(car)
##Simple regression
lm.1.out<-lm(bushgain~socialcap, data=iraq)
lm.2.out<-lm(bushgain~deathrate, data=iraq)
Par(mfrow=c(1,2))
plot(iraq$bushgain~iraq$deathrate, xlab="War Deaths Per Million", ylab="Bush Vote Share Gain, Pts, 2000-2004")
identify(iraq$deathrate, iraq$bushgain, iraq$state)
abline(lm.2.out, lty=2)
plot(iraq$bushgain~iraq$socialcap, xlab="Social Capital Index", ylab="Bush Vote Share Gain, Pts, 2000-2004")
identify(iraq$socialcap, iraq$bushgain, iraq$state)
abline(lm.1.out, lty=2)
dev.print(device=pdf, file="simplebush.pdf")

##av plots

y.1.out<-lm(bushgain~deathrate, data=iraq)
y.2.out<-lm(bushgain~socialcap, data=iraq)

x.1.out<-lm(socialcap~deathrate, data=iraq)
x.2.out<-lm(deathrate~socialcap, data=iraq)

##Y residuals
y.s<-y.1.out$residuals
y.d<-y.2.out$residuals

##X residuals
x.s<-x.1.out$residuals
x.d<-x.2.out$residuals

##Fit regression
av.s<-lm(y.s~x.s)
av.d<-lm(y.d~x.d)

##Now AV plots
par(mfrow=c(1,2))
plot(y.s~x.s, xlab="Residuals of Social Cap~Deathrate", ylab="Residuals of Bushgain~Deathrate")
identify(x.s, y.s, iraq$state)
abline(av.s)
plot(y.d~x.d, xlab="Residuals of Deathrate~Social Cap", ylab="Residuals of Bushgain~Social Cap")
identify(x.d, y.d, iraq$state)
abline(av.d)
dev.print(device=pdf, "aviraq.pdf")

##Multiple regression
lm.4.out<-lm(bushgain~deathrate+socialcap, data=iraq)
lm.4.out<-lm(bushgain~deathrate+socialcap, data=iraq[-c(32,43),])
summary(lm.4.out)
##multiple R^2
lm.5.out<-lm(deathrate~socialcap, data=iraq)

##Now look at the joint coefficients.
dd<-mvrnorm(10000, mu=lm.4.out$coefficients[2:3], Sigma=vcov(lm.4.out)[-1,-1])
plot(dd)
abline(h=0)
abline(v=0)

##Simulating values
##Concept of fixed x's, randomly generated residuals.
fits<-3.7-0.24*iraq$deathrate

sims<-matrix(data=NA, nrow=1000, ncol=2)
##Residuals
for(i in 1:nrow(sims)){
	ysim<-fits+rnorm(length(fits),sd=1.82)
      xsim<-iraq$deathrate
	sims[i,]<-lm(ysim~xsim)$coefficients
}
pdf(file="simcoeffs.pdf")
par(mfrow=c(1,2))
plot(density(sims[,1]), main="Coeff. on Intercept")
plot(density(sims[,2]), main="Coeff. on Deathrate")
dev.off()
mean(sims[,1])
mean(sims[,2])
sd(sims[,1])
sd(sims[,2])