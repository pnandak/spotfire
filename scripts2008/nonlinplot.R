
#Set Monte Carlo Parameters
#Sample Size
n <- 1000
#True Values
sig2.true <- 2
alpha.true <- 1
beta.true <-2

#Non-linearity
X <- rnorm(n)
Y <- alpha.true + beta.true * (X^2) + rnorm(n, sd=sqrt(sig2.true))
   
x <- seq(-3.5,3.5,.1)
y.pred <- 1 + 2*x^2

#An Illustrative Plot of The Problem
fm <- lm(Y ~ X)
y.true <- 1 + 2*x^2
y.pred <- fm$coef[1] + fm$coef[2]*x

#Figure 1.1
plot(X,Y, type="n", xaxt="n", bty="l")
points(X,Y, pch=".", cex=1.5)
#abline(fm, lty=1)
lines(x,y.true,lty=2)
lines(x,y.pred,lty=1)
axis(1, -4:4)
legend(-2,25, c("OLS Fit", "True Fit"), lty=c(1,2), bty="n", y.intersp=1)

#Figure 1.2
plot(fm, which = 1, bty="l", id.n=0, lty=1, caption = "", sub.caption="", pch=".")

#Next Simulation
set.seed(356293)

X <- runif(500,1, 50)
Y <- 1 + 0.50* log(X) + rnorm(500)

#Model 1 - Linear
mod.1 <- lm(Y ~ X)

#Model 2 - Logged
mod.2 <- lm(Y ~ log(X))

#Model 3 - Quadratic
mod.3 <- lm(Y ~ X + I(X^2))

#The Results
summary(mod.1)
summary(mod.2)
summary(mod.3)

#AIC
AIC(mod.1)
AIC(mod.2)
AIC(mod.3)

