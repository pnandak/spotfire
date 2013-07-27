# load the data
load("shuttle.RData")
library(Zelig)
zelig.out <- zelig(Incident ~ Temperature + Pressure, 
                   data = data, model = "")
summary(zelig.out)

# Check answers with Zelig
X.high <- setx(zelig.out, Temperature=53)
X.low <- setx(zelig.out, Temperature=31)
zelig.sim <- sim(zelig.out, x=X.high, x1=X.low)
summary(zelig.sim)

X.evs <- setx(zelig.out, Temperature=c(31:53))
X.evs2 <- setx(zelig.out, Temperature=c(54:76))
X.evs3 <- setx(zelig.out, Temperature=c(77:85))
out <- rbind(X.evs, X.evs2, X.evs3)
zelig.sim <- sim(zelig.out, x=out)
names(zelig.sim)

#> names(zelig.sim)
#[1] "x"          "x1"         "call"       "zelig.call" "par"       
#[6] "qi$ev"      "qi$pr

dim(zelig.sim$qi$ev)
ev.bounds <- t(apply(zelig.sim$qi$ev, 2, function(x) quantile(x, c(.025, .975))))
ev <- apply(zelig.sim$qi$ev, 2, mean)
plot(x=31:85, y=ev, type="l", lwd=2, xlab="Temperature at Launch", 
	ylab="Expected Probability of O-Ring Failure", 
	main="O-Ring Failure Probability", ylim = c(0,1))
lines(31:85, ev.bounds[,1], lwd=1.75, col="red", lty=2)
lines(31:85, ev.bounds[,2], lwd=1.75, col="red", lty=2)
legend("topright", c("Expected Prob.","95% Confidence Intervals"),col=c("black", "red"), lwd=c(2, 1.75), lty=c(1,2))
rug(jitter(data$Temperature), lwd=1)



####
invlogit <- function(x) 1/(1+exp(-x))
invprobit <- function(x) pnorm(x, mean=0, sd=1) 

plot(invlogit, xlim=c(-5, 5), xlab=expression(paste("X", beta)),
     ylab="Probability of a 1", main="The Logit and Probit Curves",
	 col="red", lwd=2)
curve(invprobit, add=T, lty=2, col="blue", lwd=2)
legend("topleft", c("Inverse Logit Link", "Inverse Probit Link"),col=c("red", "blue"), lwd=c(2, 2), lty=c(1,2))


load("shuttle.RData")
library(Zelig)
zelig.probit <- zelig(Incident ~ Temperature + Pressure, 
                   data = data, model = "probit")
summary(zelig.out)
pcoefs <- coef(zelig.out)

zelig.logit <- zelig(Incident ~ Temperature + Pressure, 
                   data = data, model = "logit")
summary(zelig.out)
lcoefs <- coef(zelig.out)
library(xtable)
xtable(cbind(pcoefs, lcoefs))


X.high <- setx(zelig.probit, Temperature=53)
X.low <- setx(zelig.probit, Temperature=31)

summary(sim(zelig.probit, x=X.high, x1=X.low))





