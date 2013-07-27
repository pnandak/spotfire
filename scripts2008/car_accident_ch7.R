#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  3-22-02                                                    #
# UPDATE: 1-10-03, 12-14-03 for R, 1-2-08                           #
# PURPOSE: Car accident example using loglinear models              #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#Create contingency table - notice the data is entered by columns
n.table<-array(c(350, 26, 150, 23, 60, 19, 112, 80), dim = c(2, 2, 2), 
        dimnames = list(Driver.eject=c("No", "Yes"), 
        Injury=c("not severe", "severe"), Accident.type=c("Collison", "Rollover")))
n.table


#Convert data
car.accident.data.frame<-as.data.frame(as.table(n.table))
car.accident.data.frame




####################################################################

#Set first level to 0
#options(contrasts=c("contr.treatment", "contr.poly")) 

#Model = (DI, DA, IA)  
mod.fit.Ha<-glm(formula = Freq ~ (Driver.eject + Injury + Accident.type)^2, 
        data = car.accident.data.frame, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit.Ha)


#Model = (DA, IA)
mod.fit.Ho<-glm(formula = Freq ~ (Driver.eject + Accident.type)^2 + (Injury  + Accident.type)^2, 
        data = car.accident.data.frame, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit.Ho)


G.sq<-mod.fit.Ho$deviance-mod.fit.Ha$deviance
G.sq.df<-mod.fit.Ho$df.residual-mod.fit.Ha$df.residual
cat("G^2 =", round(G.sq,4), "with p-value =", round(1-pchisq(G.sq, G.sq.df),4), "\n")


#Put predictions and residuals in a contingency table format
mu.hat<-predict(object = mod.fit.Ho, type="response")
pearson<-residuals(object = mod.fit.Ho, type="pearson")
h<-lm.influence(model = mod.fit.Ho)$h  
standard.pearson<-pearson/sqrt(1-h)
all.car.accident<-data.frame(car.accident.data.frame, mu.hat = round(mu.hat,4), pearson = round(pearson,4), standard.pearson = round(standard.pearson,4))
xtabs(mu.hat ~ Driver.eject + Injury + Accident.type, data = all.car.accident)
xtabs(pearson ~ Driver.eject + Injury + Accident.type, data = all.car.accident)
xtabs(standard.pearson ~ Driver.eject + Injury + Accident.type, data = all.car.accident)



#Calculate goodness-of-fit measures
pearson.stat<-sum(pearson^2)
cat("X^2 =", round(pearson.stat, 4), "with p-value =", round(1-pchisq(pearson.stat, mod.fit.Ho$df.residual),4), "\n")

dev<-mod.fit.Ho$deviance
cat("G^2 =", round(dev, 4), "with p-value =", round(1-pchisq(dev, mod.fit.Ho$df.residual),4), "\n")





################################################################################
################################################################################
# Backward selection to find the best model.  
# Step #1: Fit complete independence and two-way interaction models to get starting spot 

options(contrasts=c("contr.treatment", "contr.poly")) 
#Model = (A, D, I) 
mod.fit<-glm(formula = Freq  ~ Accident.type + Driver.eject + Injury, 
        data = car.accident.data.frame, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
cat("G^2 =", round(mod.fit$deviance, 2), "with p-value =", round(1-pchisq(mod.fit$deviance, mod.fit$df.residual),4), "\n")


#Model = (AD, AI, DI)  
mod.fit<-glm(formula = Freq ~ (Accident.type + Driver.eject + Injury)^2, 
        data = car.accident.data.frame, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
cat("G^2 =", round(mod.fit$deviance, 2), "with p-value =", round(1-pchisq(mod.fit$deviance, mod.fit$df.residual),4), "\n")


##########################################################################
# A simple function to help with the loglinear model building process
fit.loglinear<-function(Ho, Ha, data)
{
 mod.fit.Ho<-glm(formula = Ho, data = data, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = F))
 mod.fit.Ha<-glm(formula = Ha, data = data, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = F))
 G.sq<-round(mod.fit.Ho$deviance - mod.fit.Ha$deviance,2)
 df<-mod.fit.Ho$df.residual - mod.fit.Ha$df.residual
 round(1-pchisq(G.sq, df),4) #p-value
 #c(round(1-pchisq(G.sq, df),4), G.sq)
}


#Look for a model between all two-way interaction and complete independence
Ha.model<-Freq ~ (Accident.type + Driver.eject + Injury)^2
AD<-fit.loglinear(Freq ~ Accident.type*Injury + Driver.eject*Injury, Ha.model, car.accident.data.frame)
AI<-fit.loglinear(Freq ~ Accident.type*Driver.eject + Driver.eject*Injury, Ha.model, car.accident.data.frame)
DI<-fit.loglinear(Freq ~ Accident.type*Driver.eject + Accident.type*Injury,  Ha.model, car.accident.data.frame)
data.frame(AD, AI, DI)

#ALL interaction terms are important so the backward selection stops.

###########################################################################
# Check out the model

#Note: The last mod.fit was for (AD, AI, DI)
summary(mod.fit)

#Put predictions and residuals in a contingency table format
mu.hat<-predict(mod.fit, type="response")
pearson<-residuals(mod.fit, type="pearson")
h<-lm.influence(model = mod.fit)$h  
standard.pearson<-pearson/sqrt(1-h)

all.car.accident<-data.frame(car.accident.data.frame, mu.hat = round(mu.hat,2), pearson = round(pearson,2),standard.pearson = round(standard.pearson,4))
xtabs(mu.hat ~ Driver.eject + Injury + Accident.type, data = all.car.accident)
xtabs(pearson ~ Driver.eject + Injury + Accident.type, data = all.car.accident)
xtabs(standard.pearson ~ Driver.eject + Injury + Accident.type, data = all.car.accident)


#Calculate goodness-of-fit measures
pearson.stat<-sum(pearson^2)
cat("X^2 =", round(pearson.stat, 4), "with p-value =", round(1-pchisq(pearson.stat, mod.fit$df.residual),4), "\n")





#Find ORs
save<-xtabs(mu.hat ~ Driver.eject + Injury + Accident.type, data = all.car.accident)
save
or.col<-save[1,1,1]*save[2,2,1]/(save[1,2,1]*save[2,1,1])
or.roll<-save[1,1,2]*save[2,2,2] /(save[1,2,2]*save[2,1,2])

cat("The estimated odds of a severe injury are", round(or.col,2), 
    "times higher when the driver is ejected than when the driver is not ejected GIVEN it is a collision accident \n")
cat("The estimated odds of a severe injury are", round(or.roll,2), 
    "times higher when the driver is ejected than when the driver is not ejected GIVEN it is a rollover accident \n")
#The reason why these are the same is because there is no three-way interaction!



#Reorder table to help construct the ORs.
save2<-xtabs(mu.hat ~ Accident.type + Injury + Driver.eject, data = all.car.accident)
save2

or.no<-save2[1,1,1]*save2[2,2,1]/(save2[1,2,1]*save2[2,1,1])
or.yes<-save2[1,1,2]*save2[2,2,2] /(save2[1,2,2]*save2[2,1,2])

cat("The estimated odds of a severe injury are", round(or.no,2), 
    "times higher when the driver is in a rollover accident than when the driver is in collison a GIVEN the driver was not ejected \n")
cat("The estimated odds of a severe injury are", round(or.yes,2), 
    "times higher when the driver is in a rollover accident than when the driver is in collison a GIVEN the driver was ejected \n")
#The reason why these are the same is because there is no three-way interaction!


#C.I.s for OR
mod.fit$coefficients
sum.mod.fit<-summary(mod.fit)
sum.mod.fit$cov.unscaled[7,7]
alpha<-0.05
exp(mod.fit$coefficients[7]+qnorm(p = c(alpha/2, 1-alpha/2), mean = 0, sd = 1)*sqrt(sum.mod.fit$cov.unscaled[7,7]))


#######################################################################
#######################################################################
# Use sum to 0 constraints

options(contrasts=c("contr.sum", "contr.poly")) 
#options(contrasts=c("contr.treatment", "contr.poly")) 
#Model = (DI, DA, IA)  
mod.fit<-glm(formula = Freq ~ (Driver.eject + Injury + Accident.type)^2, 
        data = car.accident.data.frame, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)        
model.matrix(mod.fit)







################################################################################
################################################################################
# Forward selection to find the best model.  
# Step #1: Fit complete independence and two-way interaction models to get starting spot 
#          This was done above for backward selection - start here with adding terms to the one-way (complete ind.) model


#########################################################################
#  Step #2: Add terms to model
Ho.model<-Freq ~ Driver.eject + Injury + Accident.type
AD<-fit.loglinear(Ho.model, Freq ~ Injury + Driver.eject*Accident.type, car.accident.data.frame)
AI<-fit.loglinear(Ho.model, Freq ~ Driver.eject + Injury*Accident.type, car.accident.data.frame)
DI<-fit.loglinear(Ho.model, Freq ~ Accident.type + Driver.eject*Injury, car.accident.data.frame)
data.frame(AD, AI, DI)

#All three have p-values of "0" to 4 decimal places - what should you do?
#  Examine the actual G^2 statistics to see which one is larger.  AI is the largest.

Ho.model<-Freq ~ Driver.eject + Injury*Accident.type
AD<-fit.loglinear(Ho.model, Freq ~ Driver.eject + Injury*Accident.type + Driver.eject*Accident.type, car.accident.data.frame)
DI<-fit.loglinear(Ho.model, Freq ~ Driver.eject + Injury*Accident.type + Driver.eject*Injury, car.accident.data.frame)
data.frame(AD, DI)

#AD is added since it has the largest G^2

Ho.model<-Freq ~ Driver.eject + Injury*Accident.type + Driver.eject*Accident.type
DI<-fit.loglinear(Ho.model, Freq ~ Driver.eject*Injury + Injury*Accident.type + Driver.eject*Accident.type, car.accident.data.frame)
DI

#DI is added



#
