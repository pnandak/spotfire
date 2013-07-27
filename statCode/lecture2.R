library(RWinEdt)
library(MASS)
options(width=75)

puffin <- read.table('puffin.txt',header=T)

# showing regression is a projection

puffin.lm <- lm(nesting ~ grass + soil + angle + distance, data=puffin)

puffin.res <- resid(puffin.lm)

summary(puffin.lm)

resid.lm <- lm(puffin.res ~ grass + soil + angle + distance, data=puffin)

summary(resid.lm)

max(abs(fitted(resid.lm)))

max(abs(puffin.res - resid(resid.lm)))

# what are leverages

mu <- c(0,0)
sigma <- matrix(c(1,0.7,0.7,1),ncol=2)
xmat <- mvrnorm(100,mu,sigma)
xmat <- rbind(xmat,c(1.5,-1.5))
hat <- xmat %*% solve(t(xmat) %*% xmat) %*% t(xmat)
diag(hat)


postscript("../highleve.eps", horiz=F, width=4, height=4)
par(mar=c(4,4,1,1) + 0.1, pty="s")
plot(xmat, xlab=expression(X[1]),  ylab=expression(X[2]))
points(xmat[101,1],xmat[101,2],col="red", pch=19)
dev.off()
