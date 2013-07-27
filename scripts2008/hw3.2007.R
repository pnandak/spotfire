##Problem 1

cnes<-read.table("cnes.dat")
n<-nrow(cnes) ## No NAs

df<-n-1

crit95 <- qt(0.975,df) ## This gives us the critical values
crit99 <- qt(0.995,df) ## Using the t-distribution
                         ## n is large enough here that you could also
                         ## use the normal, and this is, in fact, common practice.
                         ## (recall that the t approaches the normal as df appr. 
                         ## infinity.)
##For simplicity, we'll use the normal.
crit95<-qnorm(0.975)
crit99<-qnorm(0.995)


c.mean <- mean(cnes$clinton)
c.mean
p.mean <- mean(cnes$perot)
p.mean

## Calculate sample standard deviation.
## Don't confuse this with the standard error of your estimate of the pop mean.
sd.clinton <- sd(cnes$clinton)
sd.clinton
sd.perot <- sd(cnes$perot)
sd.perot

## Now calculate standard error of 
## your estimate of the population mean.
## (refer to the week's Wooldridge readings)
c.se <- sd.clinton/(sqrt(n))
c.se <- sd.perot/(sqrt(n))
c.se
p.se

CI95.clinton <- c(c.mean - (crit95*c.se), c.mean + (crit95*c.se))
CI95.clinton #95% CI for clinton

CI95.perot <- c(p.mean - (crit95*p.se), p.mean + (crit95*p.se))
CI95.perot #95% CI for perot


CI99.clinton <- c(c.mean - (crit99*c.se), c.mean + (crit99*c.se))
CI99.clinton #99% CI for clinton

CI99.perot <- c(p.mean - (crit99*p.se), p.mean + (crit99*p.se))
CI99.perot #99% CI for perot



##LOCAL AVERAGING AND LOWESS

##loads function loc.avg() for future use
##args: x variables, y variables, start index, end index
##returns coordinates for local (x,y) mean for given values

par(mfrow=c(1,2))
loc.avg <- function(xvar, yvar, a, b){
xandy.means <- array() ##initialize storage
o <- order(xvar) ## order of x values
y.ord <- yvar[o] ##reorder y's according to x's
y.ord
x.ord <-xvar[o] #reorder x's according to the x's provided by "order"
xandy.means<-c(mean(x.ord[a:b]),mean(y.ord[a:b]))
print(xandy.means)
}

##Fox 97, Exercise 2.2
library("car")
data(Sahlins)
attach(Sahlins)

##Creates the three groups for averaging in part b
##Data already ordered
firstgroup<-Sahlins[1:7,]
firstgroup
secondgroup<-Sahlins[8:13,]
secondgroup
thirdgroup<-Sahlins[14:20,]
thirdgroup

summary(firstgroup)
##meanx: 1.18 meany: 2.07
summary(secondgroup)
##meanx: 1.50 meany: 1.94
summary(thirdgroup)
##meanx: 1.89 meany: 2.44

xmeans<-c(1.18, 1.50, 1.89)
ymeans<-c(2.07, 1.94, 2.44)
xmeans
ymeans

##naive non-parametric regression plot
plot(consumers,acres, col="blue", pch=16, main="Relationship Between
Gardener Productivity and Consumer:Gardener Ratio", xlab="Consumers/Gardener",
     ylab="Acres/Gardener") 
points(xmeans, ymeans, col="red", pch=15) 
lines(xmeans, ymeans, type="l", col="red")
identify(consumers, acres, row.names(Sahlins), 2)
dev.copy(device=pdf, file='SahlinsNaive.pdf', height=7, width=6)
dev.off()

##naive non-parametric regression WITH TWO OUTLIERS removed from mean calc.
detach(Sahlins) 
firstgroup<-Sahlins[c(1:3, 5:7),]
firstgroup
secondgroup<-Sahlins[c(8:11, 13),]
secondgroup
thirdgroup<-Sahlins[14:20,]
thirdgroup
Sahlins2<-rbind(firstgroup, secondgroup, thirdgroup)


summary(firstgroup)
##meanx: 1.18 meany: 1.90
summary(secondgroup)
##meanx: 1.49 meany: 2.07
summary(thirdgroup)
##meanx: 1.89 meany: 2.44

xmeans<-c(1.18, 1.49, 1.89)
ymeans<-c(1.90, 2.07, 2.44)
xmeans #test code
ymeans #test code

attach(Sahlins2)
plot(consumers,acres, col="blue", pch=16, main="Relationship Between
Gardener Productivity and Consumer:Gardener Ratio", xlab="Consumers/Gardener",
     ylab="Acres/Gardener") 
points(xmeans, ymeans, col="red", pch=15) 
lines(xmeans, ymeans, type="l", col="red")
identify(consumers, acres, row.names(Sahlins), 2)
dev.copy(device=pdf, file='SahlinsNaiveExclude.pdf', height=7, width=6)
dev.off()
detach(Sahlins2)

dev.print(device=pdf, file="Sahlins.Both.pdf")
##Problem 2
##Fox 97, Exercise 2.3
attach(Robey)
Robey
loc.avg(contraceptors, tfr, 1,17)
##[1] 10.118  6.165
loc.avg(contraceptors, tfr, 18,33)
##[1] 40.062  4.769
loc.avg(contraceptors, tfr,  34,50)
##[1] 62.294  3.135
xavgs<-c(10.118, 40.062, 62.294)
yavgs<-c(6.165, 4.769, 3.135)

plot(contraceptors, tfr, col="blue", pch=16, main="Relationship
Between Fertility Rates and Contraceptor Use", xlab="% of Reproduction Age Women Using Contraceptives",
     ylab="Total Fertility Rate") 
points(xavgs, yavgs, col="red", pch=15) 
lines(xavgs, yavgs, type="l", col="red")
dev.copy(device=pdf, file='Robey.pdf', height=7, width=7)
dev.off()
detach(Robey)

##Problem 3
##Fox 97, Exercise 2.4
data(Sahlins)
attach(Sahlins)
Sahlins

##local averaging without weighting function
plot(consumers, acres, main="Relationship Between Gardener
Productivity and Consumer:Gardener Ratio: Local Unweighted Averaging", xlab="Consumers/ Gardener", ylab="Acres/Gardener")
lines(ksmooth(consumers,acres, "box", bandwidth=.25), col="blue", lwd=3)
lines(ksmooth(consumers,acres, "box", bandwidth=.5), col="green", lwd=3)
lines(ksmooth(consumers,acres, "box", bandwidth=1), col="red", lwd=3)
##points(ksmooth(consumers,acres, "box", bandwidth=.25), col="blue")
##points(ksmooth(consumers,acres, "box", bandwidth=.5), col="green")
##points(ksmooth(consumers,acres, "box", bandwidth=1), col="red")
legend(1.7 ,1.7, c("0.25", "0.5", "1"),
       col=c("blue", "green", "red"), lwd=3,
       title="Bandwidths", bty="n")
dev.copy(device=pdf, file='SahlinsUnweight.pdf', height=7, width=7)
dev.off()

##locally weighted averaging using normal distribution as weight
plot(consumers, acres, main="Relationship Between Gardener
Productivity and Consumer:Gardener Ratio:\ Local Averaging with Normal Wt. Function", xlab="Consumers/Gardener", ylab="Acres/Gardener")
lines(ksmooth(consumers,acres, "normal", bandwidth=.25), col="blue", lwd=3)
lines(ksmooth(consumers,acres, "normal", bandwidth=.5), col="green", lwd=3)
lines(ksmooth(consumers,acres, "normal", bandwidth=1), col="red", lwd=3)
legend(1.7 ,1.7, c("0.25", "0.5", "1"),
       col=c("blue", "green", "red"), lwd=3,
       title="Bandwidths", bty="n")
dev.copy(device=pdf, file='SahlinsNormal.pdf', height=7, width=7)
dev.off()

##regression using lowess function
plot(consumers, acres, main="Relationship Between Gardener
Productivity and Consumer:Gardener Ratio:\ Locally Linear Fitting Using Lowess()", xlab="Consumers/ Gardener", ylab="Acres/Gardener")
lines(lowess(consumers,acres, f=1/4, iter=0), col="blue",lwd=2)
lines(lowess(consumers,acres, f=1/4, iter=2), col="red", lwd=2)
lines(lowess(consumers,acres, f=1/4, iter=5), col="green", lwd=2)
lines(lowess(consumers,acres, f=1/2, iter=0), col="orange", lwd=2)
lines(lowess(consumers,acres, f=1/2, iter=2), col="brown", lwd=2)
lines(lowess(consumers,acres, f=1/2, iter=5), col="black", lwd=2)
legend(1.7 ,1.9, c("f=1/4 iter=0", "f=1/4 iter=2", "f=1/4 iter=5","f=1/2 iter=0", "f=1/2 iter=2", "f=1/2 iter=5"),
       col=c("blue", "red", "green", "orange", "brown", "black"),
       lwd=2, title="Bandwidths", bty="n")
dev.copy(device=pdf, file='SahlinsLowess.pdf', height=7, width=6)
dev.off()

detach(Sahlins)
