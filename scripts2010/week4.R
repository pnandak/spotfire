attach( anscombe <- read.csv("anscombe.csv") )
round( c(x.mean1=mean(x1), x.mean2=mean(x2), x.mean3=mean(x3), x.mean4=mean(x4)), 2 )
round( c(y.mean1=mean(y1), y.mean2=mean(y2), y.mean3=mean(y3), y.mean4=mean(y4)), 2 )
round( c(x.sd1=sd(x1), x.sd2=sd(x2), x.sd3=sd(x3), x.sd3=sd(x4)), 3 )
round( c(y.sd1=sd(y1), y.sd2=sd(y2), y.sd3=sd(y3), y.sd3=sd(y4)), 3 )
round( c(cor1=cor(x1,y1), cor2=cor(x2,y2), cor3=cor(x3,y3), cor4=cor(x4,y4)), 3 )

### Some new cosmetic plot parameters for you (if you're interested)
# mai gives panel margins (left, top, bottom, right) in inches
# cex is a magnification factor (i.e. 1.1 is 10% bigger)
par(mfrow=c(2,2), mai=c(.7,.7,.1,.1))
plot(x1,y1, col=1, pch=20, cex=1.5)
plot(x2,y2, col=2, pch=20, cex=1.5)
plot(x3,y3, col=3, pch=20, cex=1.5)
plot(x4,y4, col=4, pch=20, cex=1.5)

# 'list' is just a list of named R objects
ansreg <- list(reg1=lm(y1~x1), reg2=lm(y2~x2), reg3=lm(y3~x3), reg4=lm(y4~x4))
attach(ansreg) # attach the names of each regression
round( cbind(reg1$coef, reg1$coef, reg1$coef, reg1$coef), 1 )
smry <- lapply(ansreg, summary) # apply the function summary to each element of the list
round( c(smry$reg1$r.sq, smry$reg1$r.sq, smry$reg1$r.sq, smry$reg1$r.sq), 1 )

# Re-plot with the regression lines
par(mfrow=c(2,2), mai=c(.7,.7,.1,.1))
plot(x1,y1, col=1, pch=20, cex=1.5)
abline(reg1, col=1)
plot(x2,y2, col=2, pch=20, cex=1.5)
abline(reg2, col=2)
plot(x3,y3, col=3, pch=20, cex=1.5)
abline(reg3, col=3)
plot(x4,y4, col=4, pch=20, cex=1.5)
abline(reg4, col=4)

# plot fitted values against residuals
par(mfrow=c(2,2), mai=c(.7,.7,.1,.1))
plot(reg1$fitted,reg1$residuals, col=1, pch=20, cex=1.5)
plot(reg2$fitted,reg2$residuals, col=2, pch=20, cex=1.5)
plot(reg3$fitted,reg3$residuals, col=3, pch=20, cex=1.5)
plot(reg4$fitted,reg4$residuals, col=4, pch=20, cex=1.5)


# take a look at studentized residuals for dataset 3
par(mfrow=c(1,2))
plot(reg3$fitted,reg3$residuals, col=3, pch=20, cex=1.5)
plot(reg3$fitted,rstudent(reg3), col=3, pch=20, cex=1.5)


#######  rent example; SqFt outliers

attach(rent <- read.csv("rent.csv"))
par(mfrow=c(1,2))
rentreg <- lm(Rent ~ SqFt)
plot(SqFt, Rent,  pch=20, col=8)
abline(rentreg)
plot(SqFt, rstudent(rentreg), pch=20, col=8, ylim=c(-4,4))
abline(h=2.5, lty=2)
abline(h=-2.5, lty=2)



### Q-Q norm plots:

# Redo the regression for houses < 2000 sqft only

rentreg <- lm(Rent[SqFt<20] ~ SqFt[SqFt<20])
par(mfrow=c(1,2))
plot( SqFt[SqFt<20], Rent[SqFt<20], pch=20, col=7, main="Regression for <2000 sqft Rent")
abline(rentreg)
hist(rstudent(rentreg), col=7)

qqnorm(rstudent(rentreg), col=4)
abline(a=0, b=1)

znorm <- rnorm(500)
zexp <- rexp(500)
zt <- rt(500, df=3)
par(mfrow=c(2,3), mai=c(.6,.6,.2,.1))
hist(znorm, col=3)
hist(zexp, col=4)
hist(zt, col=6)
qqnorm(znorm, main="Normal Q-Q plot for znorm", col=3, pch=20)
abline(a=0,b=1)
qqnorm(zexp, main="Normal Q-Q plot for zexp", col=4, pch=20)
abline(a=0,b=1)
qqnorm(zt, main="Normal Q-Q plot for zt", col=6, pch=20)
abline(a=0,b=1)


## Pickup truck data
# Regression onto years

attach(pickup <- read.csv("pickup.csv"))

truckreg <- lm(price ~ year)
r <- rstudent(truckreg)

par(mfrow=c(1,3))
plot(truckreg$fitted, truckreg$residuals,
     xlab="y.hat",
     ylab = "e",
     main="residuals vs fitted", pch=20)
abline(h=0, col=2, lty=2)
hist(r, col=8)
qqnorm(r, main="Normal Q-Q plot for r")
abline(a=0, b=1, col=4, lty=2)

### Redo the regression without > 15 year old trucks
# compare log(price) to price models

truckreg <- lm(price[year>1992] ~ year[year>1992])
logtruckreg <- lm(log(price[year>1992]) ~ year[year>1992])

par(mfrow=c(2,2), mai=c(.7,.7,.4,.1))
plot(year[year>1992], price[year>1992], main="price ~ year", pch=20)
abline(truckreg, col=2)
plot(year[year>1992], log(price[year>1992]), main="log(price) ~ year", pch=20)
abline(logtruckreg, col=4)
plot(truckreg$fitted, truckreg$residuals,
     xlab="fitted", ylab = "residuals", pch=20)
abline(h=0, col=2, lty=2)
plot(logtruckreg$fitted, logtruckreg$residuals,
     xlab="fitted", ylab = "residuals", pch=20)
abline(h=0, col=4, lty=2)


### Nonlinear transformations

# 2nd anscombe dataset

par(mfrow=c(1,2))
plot(x2, y2, col=2, pch=20)
abline(reg2)
plot(x2, rstudent(reg2), pch=20, col=2)
abline(h=0, lty=2)

par(mfrow=c(1,1))
x2squared <- x2^2
NLreg2 <- lm(y2 ~ x2 + x2squared)
plot(x2, y2, col=2, pch=20)
xgrid <- seq(4,14,length=100)
lines(xgrid, NLreg2$coef[1] + NLreg2$coef[2]*xgrid + NLreg2$coef[3]*xgrid^2)



### Telemarketing example: "calls per day" vs "length of employement".
# Fit a model with Y=calls vs. X=months

attach(telemkt <- read.csv("telemarketing.csv") )
tele1 <- lm(calls~months) 
xgrid <- data.frame( months = 10:30 )
par(mfrow=c(1,2)); plot(months, calls, pch=20, col=4)
lines(xgrid$months, predict(tele1, newdata=xgrid) )
r <- rstudent(tele1)
plot(months, r, pch=20, col=4); abline(h=0, lty=2)
months2 <- months^2
summary(tele2 <- lm(calls~ months + months2) )

xgrid <- data.frame( months = 10:30, months2 = (10:30)^2 )
par(mfrow=c(1,2)); plot(months, calls, pch=20, col=4)
lines(xgrid$months, predict(tele2, newdata=xgrid) )
plot(months, rstudent(tele2), pch=20, col=4); abline(h=0, lty=2)



### Log-Log Model example: imports vs GDP

par(mfrow=c(1,2))
attach(trade <- read.csv("imports.csv"))
plot(IMPORTS, GDP, col=0, xlim=c(0,1300))
text(IMPORTS, GDP, labels=Country)
plot(log(IMPORTS), log(GDP), col=0, xlim=c(-2.5, 9))
text(log(IMPORTS), log(GDP), labels=Country)

lm( log(GDP) ~ log(IMPORTS) )

### Price elasticity example


attach( confood <- read.csv("confood.csv") )
par(mfrow=c(1,2))
plot(Price,Sales, pch=20)
plot(log(Price),log(Sales), pch=20)

par(mfrow=c(1,1))
print(confood.reg <- lm(log(Sales) ~ log(Price) ) )
plot(log(Price),log(Sales), pch=20)
abline(confood.reg, col=4)
