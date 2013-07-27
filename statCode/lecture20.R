library(RWinEdt)
library(MASS)
library(epitools)
library(lattice)
options(width=75)

# structural zeros

wave <- read.table('wavedamage.txt',header=T)
wave2 <- wave[wave$Service > 0,]

wave.glm <- glm(Damage ~ Type + Construct + Operation, offset=log(Service),
  data=wave2, family=poisson())
sum(wave.glm)

# food poisoning - Bishop, Fienberg & Holland pp 90 - 91

countfp <- c(120, 4, 22, 0, 80, 31, 24, 23)
ill <- c(rep('Yes',4),rep('No',4))
crab <- rep(c('Yes','Yes','No','No'),2)
potato <- rep(c('Yes','No'),4)

poison <- data.frame(count=countfp, ill=ill, crab=crab, potato=potato)

poison.icp <- glm(count ~ .^3, family=poisson(), data=poison,
 epsilon=1e-16, maxit=50)
summary(poison.icp)

poison.ic.ip.cp <- glm(count ~ .^2, family=poisson(), data=poison)
summary(poison.ic.ip.cp)


# sampling zero problems

count0a <- c(20, 0, 0, 5)
count0b <- c(20, 5, 0, 0)
X0 <- c(1,1,2,2)
Y0 <- c(1,2,1,2)

zeroa <- data.frame(count=count0a, X=as.factor(X0), Y=as.factor(Y0))
zerob <- data.frame(count=count0b, X=as.factor(X0), Y=as.factor(Y0))

zeroa.glm <- glm(count ~ X + Y, family=poisson(), data=zeroa)
zerob.glm <- glm(count ~ X + Y, family=poisson(), data=zerob)
zero2b.glm <- glm(count ~ Y, family=poisson(), data=zerob)

summary(zeroa.glm)
summary(zerob.glm)
summary(zero2b.glm)

count3 <- c(0, 10, 20, 30, 5, 6, 15, 0)
X3 <- c(rep(1,4), rep(2,4))
Y3 <- rep(c(1,1,2,2),2)
Z3 <- rep(1:2,4)

X3a <- 3-X3
Y3a <- 3-Y3
Z3a <- 3-Z3

zero3 <- data.frame(count=count3, X=as.factor(X3), Y=as.factor(Y3),
   Z=as.factor(Z3))
   
zero3a <- data.frame(count=count3, X=as.factor(X3a), Y=as.factor(Y3a),
   Z=as.factor(Z3a))
   
zero3.glm <- glm(count ~ (X + Y + Z)^3, family=poisson(), data=zero3)
summary(zero3.glm)

zero3a.glm <- glm(count ~ (X + Y + Z)^3, family=poisson(), data=zero3a)
summary(zero3a.glm)


options(contrasts=c("contr.sum","contr.poly"))
zero3a.glm <- glm(count ~ (X + Y + Z)^3, family=poisson(), data=zero3)
summary(zero3a.glm)
options(contrasts=c("contr.treatment","contr.poly"))

zero3h.glm <- glm(count ~ (X + Y + Z)^2, family=poisson(), data=zero3)
summary(zero3h.glm)

options(contrasts=c("contr.sum","contr.poly"))
zero3ha.glm <- glm(count ~ (X + Y + Z)^2, family=poisson(), data=zero3)
summary(zero3ha.glm)
options(contrasts=c("contr.treatment","contr.poly"))

zero3mi.glm <- glm(count ~ (X + Y + Z), family=poisson(), data=zero3)
summary(zero3mi.glm)

options(contrasts=c("contr.sum","contr.poly"))
zero3mia.glm <- glm(count ~ (X + Y + Z), family=poisson(), data=zero3)
summary(zero3mia.glm)
options(contrasts=c("contr.treatment","contr.poly"))

zero3.xy.xz <- glm(count ~ X*Y + X*Z, family=poisson(), data=zero3)
zero3.xy.yz <- glm(count ~ X*Y + Y*Z, family=poisson(), data=zero3)
zero3.xz.yz <- glm(count ~ Y*Z + X*Z, family=poisson(), data=zero3)
summary(zero3.xy.xz)
summary(zero3.xy.yz)
summary(zero3.xz.yz)

cbind(zero3,fitted(zero3.xy.xz),fitted(zero3.xy.yz),fitted(zero3.xz.yz))


# separating hyperplanes

xsp <- runif(50, 0, 10)
xsp <- sort(xsp)
ysp <- xsp > 5

postscript("../sep.planes.eps", width=5, height=3.0, horiz=F)
par(mar=c(4,4,0,1) + 0.1, pty="m")
par(mfrow=c(1,1))

plot(xsp, ysp, xlab="X", ylab='Success',  pch=16)
abline(v=4.6, lty=2)
dev.off()

sp.glm <- glm(ysp ~ xsp, family=binomial())
summary(sp.glm)

x1sp <- runif(50,0,10)
x2sp <- runif(50,0,10)
y2sp <- (x1sp + x2sp) < 10

postscript("../sep.planes.2.eps", width=5, height=3.0, horiz=F)
par(mar=c(4,4,0,1) + 0.1, pty="m")
par(mfrow=c(1,1))

plot(x1sp[y2sp==1], x2sp[y2sp==1], xlab="X1", ylab='X2', 
  xlim=c(0,10), ylim=c(0,10), pch=16)
abline(a=10, b=-1, lty=2)
points(x1sp[y2sp==0], x2sp[y2sp==0], pch=16, col=2)

dev.off()
