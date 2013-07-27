library(RWinEdt)
library(MASS)
library(epitools)
options(width=75)

return.ident.glm <- glm(ret.prop[,1] ~ deposit, 
   family=quasi(link="identity", variance="mu(1-mu)"))
summary(return.ident.glm)

ri.coef <- coef(return.ident.glm)
ri.coef[1]/ri.coef[2]
(1-ri.coef[1])/ri.coef[2]

fasten.load <- seq(2500, 4300, by=200)
fasten.n <- c(50, 70, 100, 60, 40, 85, 90, 50, 80, 65)
fasten.fail <- c(10, 17, 30, 21, 18, 43, 54, 33, 60, 51)
fasten.prop <- fasten.fail/fasten.n
fasten <- data.frame(load=fasten.load, n=fasten.n, 
    fail=fasten.fail, hold=fasten.n-fasten.fail, prop=fasten.prop)
fasten.res <- cbind(fasten.fail,fasten.n-fasten.fail)

fasten.glm <- glm(fasten.res ~ fasten.load, family=binomial())
summary(fasten.glm)

fasten.pred.levels <- data.frame(fasten.load=2500:4300)
fasten.pred <- predict(fasten.glm, fasten.pred.levels, type="response")



postscript("../fasten.eps", width=8, height=4, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")

plot(fasten.load, fasten.prop, xlab="Load (psi)", 
  ylab="Proportion Failing", ylim=c(0,1), pch=16)
lines(2500:4300, fasten.pred)
text(2500,0.9,
  expression(hat(pi) == frac(e^{-5.34+0.0015*Load},1+e^{-5.34+0.0015*Load})), 
  adj=0, cex=1.25)
dev.off()


logit <- function(x) { log(x/(1-x))}

level.f <- 3750
prob.f <- 0.5

pred.f <- predict(fasten.glm,data.frame(fasten.load=level.f),
  type="response")
coef.f <- coef(fasten.glm)
load.f <- (logit(prob.f) - coef.f[1])/coef.f[2]
(logit(c(0.25, 0.5)) - coef.f[1])/coef.f[2]

postscript("../forward-inverse.eps", width=8, height=6, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(2500:4300, fasten.pred, xlab="Load (psi)", ylab="Proportion Failing", 
  main="Probability Given Load", ylim=c(0,1), type="l", xaxt="n", yaxt="n")
arrows(level.f,0,level.f,pred.f,length=0.1)
arrows(level.f,pred.f,2500,pred.f, length=0.1)
axis(1,c(2500,4300),c(2500,4300))
axis(2,0:1, 0:1)
axis(1,level.f,expression(x[0]))
axis(2,pred.f,expression(pi(x[0])))

plot(2500:4300, fasten.pred, xlab="Load (psi)", ylab="Proportion Failing", 
  main="Load for Given Probability", ylim=c(0,1), type="l", xaxt="n", yaxt="n")
arrows(load.f,prob.f,load.f,0,length=0.1)
arrows(2500,prob.f,load.f,prob.f, length=0.1)
axis(1,c(2500,4300),c(2500,4300))
axis(2,0:1, 0:1)
axis(1,load.f,expression(x(pi[0])))
axis(2,prob.f,expression(pi[0]))

dev.off()

quad <- function(a,b,d) {
disc <- sqrt(b^2 -4*a*d)
c((-b-disc)/(2*a), (-b+disc)/(2*a))
}

fieller <- function(coefest, varest, conflevel=0.95) {
  alpha <- (1-conflevel) / 2
  critval <- qnorm(alpha, lower.tail=F)
  a <- coefest[2]^2 - critval^2*varest[2,2]
  b <- -(2*varest[1,2]*critval^2-2*prod(coefest))
  d <- coefest[1]^2-critval^2*varest[1,1]
  quad(a,b,d)
}

fieller(coef(fasten.glm), vcov(fasten.glm))
