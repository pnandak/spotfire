#### Housing data: just price (in $100,000) vs size (in 1000 sq.ft.)
size <- c(.8,.9,1,1.1,1.4,1.4,1.5,1.6,1.8,2,2.4,2.5,2.7,3.2,3.5)
price <- c(70,83,74,93,89,58,85,114,95,100,138,111,124,161,172)
plot(size, price, pch=20)
print( n <- length(size) )

#### Simple regression; compare R's calculations to our own
reg <- lm(price ~ size) 
b1 <- cor(price,size)*sd(price)/sd(size)
b0 <- mean(price) - mean(size)*b1
cbind(b0,b1)

#### Plots of fitted values and residuals
plot(size, reg$fitted, pch=20, xlab="X", ylab="Fitted Values")
text(x=3, y=80, paste("corr(y.hat, x) =", cor(size, reg$fitted)), col=2, cex=1.5)
#
plot(size, reg$fitted-price, pch=20, xlab="X", ylab="Residuals")
text(x=3.2, y=25, paste("corr(e, x) =",
              round(cor(size, reg$fitted-price),2)), col=2, cex=1.5)
text(x=3.2, y=17,
     paste("mean(e) =", round(mean(reg$fitted-price),0)), col=4, cex=1.5)
abline(h=0, col=8, lty=2)

#### "Crazy line" illustration
plot(size, price, pch=20, xlab="X", ylab="Y")
lines(xx <- 0:4, b0 + b1*xx, col=4)
lines(xx, 10 + 50*xx, col=2)
text(x=3, y=80, paste("LS line:", round(b0,1), "+", round(b1,1), "X"),
     col=4, cex=1.25)
text(x=1.5, y=140, "Crazy line: 10 + 50 X", col=2, cex=1.25)
#
crazyresid <- price - (10 + 50*size)
#
plot(size, crazyresid, pch=20, xlab="X", ylab="Crazy Residuals")
text(x=3, y=20, paste("corr(e, x) =",
            round(cor(size, crazyresid),1)), col=2, cex=1.5)
text(x=3, y=13, paste("mean(e) =",
            round(mean(crazyresid),1)), col=4, cex=1.5)
lines(size, lm(crazyresid ~ size)$fitted.values, col=2)
abline(h=0, col=8, lty=2)

#### ANOVA for Regression and R^2
anova(reg)
var(reg$fitted)*(n-1)
var(reg$resid)*(n-1)
#
summary(reg)
var(reg$fitted)/var(price)

#### CAPM Example #####

mfund <- read.csv("MFUNDS.csv")
round(mu <- mean(mfund),4)
round(stdev <- sd(mfund),4)
plot(mu, stdev, col=0) # create an empty plot with the right limits
text(x=mu, y=stdev, labels=names(mfund), col=4)

## Run the regression for each of the 6 mutual funds
print(CAPM <- lm(as.matrix(mfund[,1:6]) ~ mfund$valmrkt))
plot(CAPM$coeff[2,], CAPM$coeff[1,],
     ylab="alpha", xlab="beta", col=0, xlim=c(.3,1.6))
text(x=CAPM$coeff[2,], y=CAPM$coeff[1,], labels=names(mfund)[1:6], col=2)

## Print the summaries and anova tables for just the windsor regression
summary(reg <- lm(mfund$windsor ~ mfund$valmrkt))
anova(reg)

#######  Code for your homework assignment, Q1.3

attach( mkt <- read.csv("mktmodel.csv") )
stocks <- mkt[,-1]
plot(SP500, col=0, ## Just get the plot up
     xlab = "Month", ylab = "Returns",
     main = "Monthly returns for 1992-1996",
     ylim=range(unlist(mkt)))
colors <- rainbow(30)  ## 30 different colors
## this is how you do 'loops' in R... this is useful!
for(i in 1:30){ lines(stocks[,i], col=colors[i], lty=2) }
lines(SP500, lwd=2)
