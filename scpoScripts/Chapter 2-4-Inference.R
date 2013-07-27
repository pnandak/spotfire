
#Set Working Directory Here
setwd()

#Read the data
library(foreign)
jacob <- read.dta("jacob.dta")
attach(jacob)

#Confidence Band Plots
#By Hand
nonpar.fit <- loess(chal.vote ~ perotvote, span=.5, degree=1, data=jacob)
perot <- seq(min(perotvote), max(perotvote), length=312)
fit <- predict(nonpar.fit, data.frame(perotvote=perot), se=TRUE)

#Figure 2.14
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(perot, fit$fit, lwd=1)
lines(perot, fit$fit + 1.96*fit$se.fit, lty=2, lwd=1)
lines(perot, fit$fit - 1.96*fit$se.fit, lty=2, lwd=1)

#Using Locfit To Form CI Bands
library(locfit)
fit <- locfit(chal.vote~perotvote, alpha=0.5)
plot(fit, band="global", , ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", cex=2)
points(perotvote, chal.vote, pch=".", cex=1.75)

#Hypothesis Tests

#Estimate Loess Fit
mod.nonpar <- loess(chal.vote ~ perotvote, span=.5, degree=1, data=jacob) 

#Form Total Sum of Squares
TSS <- sum((chal.vote - mean(chal.vote))^2)
#From Residual Sum of Squares For Loess Fit
RSS.npar <- sum(residuals(mod.nonpar)^2)

mod.df <- mod.nonpar$trace
res.df <- mod.nonpar$n - mod.df

f <- ((TSS - RSS.npar)/(mod.df - 1)) / (RSS.npar/(res.df))
1 - (pf(f, df1=(mod.df-1), df2=res.df, lower.tail=TRUE))

#Testing for Nonlinearity and Against Power Transformations
#Linear
mod.linear <- lm(chal.vote ~ perotvote, data=jacob)
#Residual Sum of Squares For Linear Model
RSS.lm <- sum(residuals(mod.linear)^2)
#Test Statistic
f.2 <- ((RSS.lm - RSS.npar)/(mod.df - 2)) / (RSS.npar/(res.df))
#P-Value
1 - (pf(f.2, (mod.df - 2) , res.df, lower.tail=TRUE))

#Quadratic
perot.sqd <- perotvote^2
mod.quadratic <- lm(chal.vote ~ perotvote + perot.sqd, data=jacob)
#Residual Sum of Squares Quadratic Fit
RSS.quad <- sum(residuals(mod.quadratic)^2)
#Test Statistic
f.3 <- ((RSS.quad - RSS.npar)/(mod.df - 3)) / (RSS.npar/(res.df))
#P-Value
1 - (pf(f.3, mod.df - 3, res.df, lower.tail=TRUE))

#Logarithmic
mod.log <- lm(chal.vote ~ log.perot, data=jacob)
#Residual Sum of Squares Quadratic Fit
RSS.log <- sum(residuals(mod.log)^2)
#Test Statistic
f.4 <- ((RSS.log - RSS.npar)/(mod.df - 2)) / (RSS.npar/(res.df))
#P-Value
1 - (pf(f.4, mod.df - 2, res.df, lower.tail=TRUE))



