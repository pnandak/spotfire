# Fig. 4.6: Types of nonlinear relationships

# using par(mfrow=c(m, n))

windows(length=5, width=5)
par(mfrow=c(2, 2))

X <- seq(0, 1, length=200)
EY <- rev(1 - X^2)
Y <- EY + 0.1*rnorm(200)
plot(X, Y, axes=FALSE, frame=TRUE, main="(a)", cex.main=1,
    xlab="", ylab="")
lines(X, EY, lwd=3)
text(locator(2), c("X", "Y"), xpd=TRUE)

X <- seq(0.02, 0.99, length=200)
EY <- log(X/(1 - X))
Y <- EY + 0.5*rnorm(200)
plot (X, Y, axes=FALSE, frame=TRUE, main="(b)", cex.main=1,
    xlab="", ylab="")
lines(X, EY, lwd=3)
text(locator(2), c("X", "Y"), xpd=TRUE)

X <- seq(0.2, 1, length=200)
EY <- (X - 0.5)^2
Y <- EY + 0.04*rnorm(200)
plot(X, Y, axes=FALSE, frame=TRUE, main="(c)", cex.main=1,
    xlab="", ylab="")
lines(X, EY, lwd=3)
text(locator(2), c("X", "Y"), xpd=TRUE)


# using par(fig=c(x1, x2, y1, y2))

windows()

par(fig=c(0, .5, .5, 1))
X <- seq(0, 1, length=200)
EY <- rev(1 - X^2)
Y <- EY + 0.1*rnorm(200)
plot(X, Y, axes=FALSE, frame=TRUE, main="(a)", cex.main=1,
    xlab="", ylab="")
lines(X, EY, lwd=3)
text(locator(2), c("X", "Y"), xpd=TRUE)

par(new=TRUE)
par(fig=c(.5, 1, .5, 1))
X <- seq(0.02, 0.99, length=200)
EY <- log(X/(1 - X))
Y <- EY + 0.5*rnorm(200)
plot (X, Y, axes=FALSE, frame=TRUE, main="(b)", cex.main=1,
    xlab="", ylab="")
lines(X, EY, lwd=3)
text(locator(2), c("X", "Y"), xpd=TRUE)

par(new=TRUE)
par(fig=c(.25, .75, 0, .5))
X <- seq(0.2, 1, length=200)
EY <- (X - 0.5)^2
Y <- EY + 0.04*rnorm(200)
plot(X, Y, axes=FALSE, frame=TRUE, main="(c)", cex.main=1,
    xlab="", ylab="")
lines(X, EY, lwd=3)
text(locator(2), c("X", "Y"), xpd=TRUE)

