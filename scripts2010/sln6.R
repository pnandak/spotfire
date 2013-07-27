library(MASS)
data(Cars93)
Cars93[is.na(Cars93)] <- 0
Cars93  <- Cars93[Cars93$Cylinders!="rotary", ]
Cars93$Cylinders  <- as.numeric(Cars93$Cylinders) 
train <- 1:60
covars <- Cars93[,c(3,7:26)]
logp <- log(Cars93$Price)
n <- length(train)

# this is different than the example in class, but I think less sensitive to mistakes.
YX <- cbind(logp,covars) 
# First the full model 
full <- lm(logp ~ ., data=YX[train,])
# now a null model
null <- lm(logp ~ 1, data=YX[train,])
# Use fwd regression with BIC to build from this
fwd <- step(null, scope=formula(full), direction="forward", k=log(n))
# Also look to build interactions based on fwdreg
fwd2 <- step(fwd, scope=~.+.^2, direction="forward", k=log(n))
# Finally a couple candidates reduced from the full (F-test rejects the first)
reduced1 <- lm(logp ~ AirBags + Horsepower + Width, data=YX[train,])
anova(reduced1,full) # too small, so lets include "Type" 
reduced2 <- lm(logp ~ AirBags + Horsepower + Width + Type, data=YX[train,])
anova(reduced2,full) # F-test says OK

# So we have 6 models: , one null, one full, two fwd BIC, and two F-test reduced
#  BIC calculations:
BIC <- cbind(null=extractAIC(null, k=log(n)),
             full=extractAIC(full, k=log(n)),
             fwd=extractAIC(fwd, k=log(n)),
             fwd2=extractAIC(fwd2, k=log(n)),
             reduced1=extractAIC(reduced1, k=log(n)),
             reduced2=extractAIC(reduced2, k=log(n)) )
# Model probabilities
eBIC <- exp(-.5*(BIC[2,]-min(BIC[2,])))
round(probs <- eBIC/sum(eBIC), 2)
# We are 23% v 77% on the two fwd models.

# Lets see how they perform out-of-sample
error <- cbind(null=predict(null, newdata=covars[-train,])- logp[-train],
               full=predict(full, newdata=covars[-train,])- logp[-train],
               fwd=predict(fwd, newdata=covars[-train,])- logp[-train],
               fwd2=predict(fwd2, newdata=covars[-train,])- logp[-train],
               reduced1=predict(reduced1, newdata=covars[-train,])- logp[-train],
               reduced2=predict(reduced2, newdata=covars[-train,])- logp[-train] )
round(MSE <- apply(error^2,2,mean),5)

# The fwd2 model wins... Check the residuals for it:
par(mfrow=c(2,2)) 
plot(fwd2$fitted, rstudent(fwd2), 
     xlab="Fitted Values", ylab="Studentized Residuals", pch=20)
plot(c(fwd2$fitted,predict(fwd2, newdata=covars[-train,])), logp, 
     col=c(rep(1,n),rep(2,length(logp)-n)),
      xlab="Fitted Values", ylab="Log Price", pch=20)
legend("topleft", fill=2, legend="left-out")
qqnorm(rstudent(fwd2), pch=20, main="" )
abline(a=0,b=1,lty=2)
hist( rstudent(fwd2), col=8, xlab="Studentized Residuals", main="")
