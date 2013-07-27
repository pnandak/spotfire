
library(foreign)

#Set the Working Directory
setwd()

#Poisson Example
Scourt <- read.dta("scourt.dta")
attach(Scourt)
library(mgcv)

mod.1 <- gam(nulls ~ tenure + congress + unified, data=Scourt, family=poisson)
summary(mod.1)

mod.2 <- gam(nulls ~ tenure + s(congress, bs="cr") + unified, data=Scourt, family=poisson)
summary(mod.2)

anova(mod.1, mod.2, test="Chisq")

mod.3 <- gam(nulls~ tenure + congress + I(congress^2) + I(congress^3) + unified, data=Scourt, family=poisson)

anova(mod.3, mod.2, test="Chisq")


predict.data <- data.frame(expand.grid(list(congress=seq(1, 101, by=1), tenure = 10.35, unified = 0)))
                          
#Created Fitted Values Using Fake Data                                                    
predict.fit <- predict.gam(mod.2, newdata = predict.data, se.fit=TRUE)
#Transform into Probabilities
mu.fit <- exp(predict.fit$fit)

#Figure 6.7
plot(Scourt$congress, mod.2$fitted.values, type="n", ylab="Congressional Acts Overturned", xlab = "Congress", bty="l", ylim = c(0,8))
#Effect of Tenure 
lines(predict.data$congress, mu.fit)
lines(predict.data$congress, exp(predict.fit$fit-1.96*predict.fit$se.fit), lty=2)
lines(predict.data$congress, exp(predict.fit$fit+1.96*predict.fit$se.fit), lty=2)

plot(mod.2, rug=FALSE, ylab="Propensity to Overturn Congressional Acts", xlab="Congress", shift=-3.31, bty="l")


#Plot To See Change in Effect of Tenure - Note Effect Dependent on Which Congress Used For Prediction
#Create Fake Data Set 
predict.data <- data.frame(expand.grid(list(congress=50, tenure = seq(.8, 18, length.out=42), unified = 0)))
                          
#Created Fitted Values Using Fake Data                                                    
predict.fit.1 <- predict.gam(mod.1, newdata = predict.data, se.fit=TRUE)
predict.fit.2 <- predict.gam(mod.2, newdata = predict.data, se.fit=TRUE)
#Transform into Probabilities
mu.fit.1 <- exp(predict.fit.1$fit)
mu.fit.2 <- exp(predict.fit.2$fit)

#Figure 6.6
plot(Scourt$tenure, mod.1$fitted.values, type="n", ylab="Congressional Acts Overturned", xlab = "Average Tenure on Supreme Court", bty="l")
#Effect of Tenure 
lines(predict.data$tenure, mu.fit.1)
lines(predict.data$tenure, mu.fit.2, lty = 2)
#points(Scourt$tenure, mod.1$fitted.values, pch=".")
points(Scourt$tenure, mod.1$fitted.values, pch=".")
legend(1,4, lty=1:2, lwd=1, bty= "n", legend=c("Parametric Model", "Semi-Parametric Model"), cex = 0.95, y.intersp = 1.25)

#Confidence Bands
lines(predict.data$tenure, exp(predict.fit.1$fit-predict.fit.1$se.fit), lty=2)
lines(predict.data$tenure, exp(predict.fit.1$fit+predict.fit.1$se.fit), lty=2)

lines(predict.data$tenure, exp(predict.fit.2$fit-predict.fit.2$se.fit), lty=2)
lines(predict.data$tenure, exp(predict.fit.2$fit+predict.fit.2$se.fit), lty=2)

#Neg Bin
library(MASS)

mod.4 <- gam(nulls~ tenure + s(congress, bs="cr",fx=TRUE, k=10) + unified, data=Scourt, family=negative.binomial(1), control = gam.control(maxit = 150))

anova(mod.2, mod.4, test="Chisq")


