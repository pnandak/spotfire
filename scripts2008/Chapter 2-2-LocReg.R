
library(foreign)

#Set Working Directory Here
setwd()

#Read the data
jacob <- read.dta("jacob.dta")
attach(jacob)

#Data Prep For Local Average Regression Step-by-Step
cong <- as.data.frame(jacob[,2:3])
cong <- cong[order(cong$perotvote),1:2]

y <- as.matrix(cong$chal.vote)
x <- as.matrix(cong$perotvote)
n <- length(y)

tricube <- function(z) {
    ifelse (abs(z) < 1, (1 - (abs(z))^3)^3, 0)
    }

ord <- order(perotvote)
perot <- perotvote[ord]
#perot <- sort(perotvote)
pre <- sort(chal.vote)
x0 <- perot[75]
diffs <- abs(perot - x0)
which.diff <- sort(diffs)[120]

x.n <- perot[diffs<= which.diff]
y.n <- chal.vote[diffs <= which.diff]
mod <- lm(y.n ~ x.n)
weights=tricube((x.n-x0)/which.diff)

#Figure 2.7
plot(perotvote, chal.vote, type="n", cex=.65, xlab="Perot Vote (%)", ylab="Challenger's Vote Share (%)", bty="l")
abline(v=c(x0 - which.diff, x0 + which.diff), lty = 2)
abline(v=x0)
points(perot[diffs > which.diff], chal.vote[diffs > which.diff], pch=16, cex=1, col=gray(.80))
points(perot[diffs <= which.diff], chal.vote[diffs <= which.diff], cex=.85)
abline(mod, lwd=2, col=1)
text(27.5, 50, expression(paste("Fitted Value of y at ", x[0])))
arrows(25, 47, 15, 37, code =2, length = .10)


#Now Putting It Together For Local Regression Demonstration.

#OLS Fit for Comparison
ols <- lm(chal.vote ~ perotvote, data=jacob)

#The loess fit
model.loess <- loess(chal.vote ~ perotvote, data=jacob, span = 0.5)
n <- length(chal.vote)
xhatloess <- seq(min(perotvote), max(perotvote), length=n)
fit <- predict(model.loess, data.frame(perotvote=xhatloess))

#Figure 2.8
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(xhatloess, fit)
#Now a lowess fit
lines(lowess(perotvote, chal.vote, f = 0.5), lty=2)
abline(ols)
legend(15,20, c("Loess","Lowess", "OLS") , lty=c(1,2, 1), bty="n", cex=.8)

cbind(fit, xhatloess)
