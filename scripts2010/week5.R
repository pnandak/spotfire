attach(salesdata <- read.csv("Sales.csv"))
n <- nrow(salesdata)
salesMLR <- lm( Sales ~ P1 + P2)

#### Residuals and Transformations ####
par(mfrow=c(1,3))
plot(salesMLR$fitted, salesMLR$resid,
     pch=20, ylab="residuals", xlab="fitted")
plot(P1, salesMLR$resid, pch=20, col=4, ylab="residuals")
plot(P2, salesMLR$resid, pch=20, col=2, ylab="residuals")
par(mfrow=c(1,1))
plot(salesMLR$fitted, Sales, main= "Fitted vs True Response for Sales data",
     pch=20, col=4, ylab="Y", xlab="Y.hat")
abline(0,1,lty=2, col=8)
## model with the untransformed prices and sales
expsalesMLR <- lm( exp(Sales) ~ exp(P1) + exp(P2))
par(mfrow=c(1,3))
plot(expsalesMLR$fitted, expsalesMLR$resid,
     pch=20, ylab="residuals", xlab="fitted")
plot(exp(P1), expsalesMLR$resid, pch=20, col=4, ylab="residuals")
plot(exp(P2), expsalesMLR$resid, pch=20, col=2, ylab="residuals")
par(mfrow=c(1,1))
hist(rstudent(expsalesMLR), col=7, xlab="Studentized Residuals", main="")

#### Parameter inference ####
## calculate the coefficient standard errors ourselves
# X.hat from slide 9:
X <- cbind(1,P1,P2) 
# covariance of b =  s^2 (X'X)^-1 :
print( cov.b <- summary(salesMLR)$sigma^2*solve(t(X)%*%X) )
# the standard errors of the b's (match it to summary):
round( se.b <- sqrt(diag(cov.b)), 6)
## Get the errors from the summary function
summary( salesMLR )

######  Prediction #######
## get the intervals
predict( salesMLR, data.frame(P1=1, P2=1), interval="prediction", level=0.95)
## look at se.fit
xf <- matrix(c(1,1,1), ncol=1)
X <- cbind(1, P1, P2)
sqrt(t(xf)%*%solve(t(X)%*%X)%*%xf)*summary(salesMLR)$sigma ## this is se.fit
predict( salesMLR, data.frame(P1=1, P2=1), se.fit=TRUE)

###### F-testing ########
R2 <- summary(salesMLR)$r.square
round( f.stat <- (R2/2)/( (1-R2)/(n-3) ), 2)
1-pf(f.stat, df1=2, df2=n-2)


################  Wage Data Examples  ##############

census <- read.csv("census2000.csv")
workers <- census$income > 1000 # remove folks earning less than $1000
log.wagerate <- log(census$income/census$hours)[workers]
edu <- census$education[workers]
plot(edu, log.wagerate, col=7, xlab="Education Level", ylab="log Hourly Rate")

summary( reg <- lm(log.wagerate ~ edu) )
anova(reg)
