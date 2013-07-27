
#Set the Working Directory
setwd()

library(foreign)
library(mgcv)
war <- read.dta("PHdata.dta")
attach(war)

#Replication 
war.glm <- gam(dispute ~ nudem + nugrow + allies + contig + nucapab + trade + sumdisp + s(year, bs="cr"), data = war, family=binomial) 

summary(war.glm)     

#Model With All C. Variables Smoothed.
war.smooth.2 <- gam(dispute ~ s(nudem, bs="cr") + s(nugrow, bs="cr") + allies + contig + s(nucapab, bs="cr") + s(trade, bs="cr") + s(year, bs="cr") + sumdisp, data = war, family=binomial) 

#figure 6.1
par(mfrow = c(2,2))
plot(war.smooth.2, select=1, rug=FALSE, ylab="Propensity For Conflict", xlab = "Democracy", bty="l",  ylim=c(-2,2))

plot(war.smooth.2, select=2, rug=FALSE, ylab="Propensity For Conflict", xlab = "Economic Growth", bty="l")

plot(war.smooth.2, select=3, rug=FALSE, ylab="Propensity For Conflict", xlab = "Capability Ratio", bty="l")

plot(war.smooth.2, select=4, rug=FALSE, ylab="Propensity For Conflict", xlab = "Trade", bty="l")
dev.off()


#Models to test Against
#Drop For Democ.
war.smooth.1 <- gam(dispute ~ nudem + s(nugrow, bs="cr") + allies + contig + s(nucapab, bs="cr") + s(trade, bs="cr") + s(year, bs="cr") + sumdisp, data = war, family=binomial) 
                  

war.smooth.3 <- gam(dispute ~ nudem + s(nugrow, bs="cr") + allies + contig + s(nucapab, bs="cr") + s(trade, bs="cr") + s(year, bs="cr") + sumdisp, data = war, family=binomial) 
                 
#Drop For Growth                  
war.smooth.4 <- gam(dispute ~ s(nudem, bs="cr") + nugrow + allies + contig + s(nucapab, bs="cr") + s(trade, bs="cr") + s(year, bs="cr") + sumdisp, data = war, family=binomial) 

#Drop For Capability Ratio
war.smooth.5 <- gam(dispute ~ s(nudem, bs="cr") + s(nugrow, bs="cr") + allies + contig + nucapab + s(trade, bs="cr") + s(year, bs="cr") + sumdisp, data = war, family=binomial) 
   
#Drop For Trade               
war.smooth.6 <- gam(dispute ~ s(nudem, bs="cr") + s(nugrow, bs="cr") + allies + contig + s(nucapab, bs="cr") +  trade + s(year, bs="cr") + sumdisp, data = war, family=binomial) 
   
#Final Model               
war.smooth.7 <- gam(dispute ~ s(nudem, bs="cr") + nugrow + allies + contig + nucapab + trade + s(year, bs="cr") + sumdisp, data = war, family=binomial) 
                  
summary(war.smooth.7)

#Test Smoothed Democracy Against Fully Smoothed Model
anova(war.smooth.2, war.smooth.3, test="Chisq")
#Test Smoothed Growth Against Fully Smoothed Model
anova(war.smooth.2, war.smooth.4, test="Chisq")
#Test Smoothed Capability Ratio Against Fully Smoothed Model
anova(war.smooth.2, war.smooth.5, test="Chisq")
#Test Smoothed Trade Against Fully Smoothed Model
anova(war.smooth.2, war.smooth.6, test="Chisq")

par(mfrow = c(2,2))
plot(war.smooth.5, select=1, rug=FALSE, ylab="Propensity For Conflict", xlab = "Democracy", bty="l")

plot(war.smooth.7, select=1, rug=FALSE, ylab="Propensity For Conflict", xlab = "Democracy", bty="l")


#Test GLM Against Smoothed Model
anova(war.glm, war.smooth.7, test="Chisq")

#Plot On probability scale
#Logit Transformation
logit <- function(xb){
    1/(1+exp(-xb))
    }

#Create Fake Data Set For Effect of Age and Educ Held Constant

#Democracy
predict.data <- data.frame(expand.grid(list(nudem = seq(-1,1, by=.1), nugrow= .008, allies = 0, contig=0, nucapab = 1.67, trade = .002, year=1970, sumdisp=0.27)))
                        
#Created Fitted Values Using Fake Data                                                    
predict.fit <- predict.gam(war.smooth.7, newdata = predict.data, se.fit=TRUE)

#Transform into Probabilities
mu.fit <- logit(predict.fit$fit)

#Created Fitted Values Using Fake Data                                                    
predict.fit.2 <- predict.gam(war.glm, newdata = predict.data, se.fit=TRUE) 

#Transform into Probabilities
mu.fit.2 <- logit(predict.fit.2$fit)

#Figure 6.2
par(mfrow=c(1,2))
plot(nudem, war.smooth.7$fitted.values, type="n", ylab="Predicted Probability of Conflict", xlab="Democracy", bty="l", ylim=c(0.00, 0.03), main="GAM")
#Effect of Trade on Probability Scale
lines(predict.data$nudem, mu.fit)
#Confidence Bands
lines(predict.data$nudem, logit(predict.fit$fit-2*predict.fit$se.fit), lty=2)
lines(predict.data$nudem, logit(predict.fit$fit+2*predict.fit$se.fit), lty=2)

plot(nudem, war.glm$fitted.values, type="n", ylab="Predicted Probability of Conflict", xlab="Democracy", bty="l", ylim=c(0.00, 0.03), main="GLM")
#Effect of Trade on Probability Scale
lines(predict.data$nudem, mu.fit.2)
#Confidence Bands
lines(predict.data$nudem, logit(predict.fit.2$fit-2*predict.fit.2$se.fit), lty=2)
lines(predict.data$nudem, logit(predict.fit.2$fit+2*predict.fit.2$se.fit), lty=2)







