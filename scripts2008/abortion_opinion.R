#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  3-23-02, 12-14-03 - R, 12-26-05, 1-2-08                    #
# PURPOSE: Table IV.6 data from Christensen (1990, p.129)           #
#                                                                   #
# NOTES:                                                            #
#####################################################################



abortion.opinion<-read.table("C:\\chris\\UNL\\STAT875\\Chapter7\\abortion_opinion.csv", header = TRUE, sep = ",")
abortion.opinion

#Show contingency table format
xtabs(Count ~ Opinion + Age + Sex + Race, data = abortion.opinion) 


######################################################################
# Start by fitting all one-way, two-way, and three-way interaction models

mod.fit<-glm(formula = Count ~ Opinion + Age + Sex + Race, 
        data = abortion.opinion, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
sum.mod.fit.one<-data.frame(model = "one-way", G.sq = round(mod.fit$deviance,2), df = mod.fit$df.residual, 
                            p.value = round(1-pchisq(mod.fit$deviance, mod.fit$df.residual),4))
                                     
mod.fit<-glm(formula = Count ~ (Opinion + Age + Sex + Race)^2, 
        data = abortion.opinion, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
sum.mod.fit.two<-data.frame(model = "two-way", G.sq = round(mod.fit$deviance,2), df = mod.fit$df.residual, 
                            p.value = round(1-pchisq(mod.fit$deviance, mod.fit$df.residual),4))

mod.fit<-glm(formula = Count ~ (Opinion + Age + Sex + Race)^3, 
        data = abortion.opinion, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
sum.mod.fit.three<-data.frame(model = "three-way", G.sq = round(mod.fit$deviance,2), df = mod.fit$df.residual, 
                              p.value = round(1-pchisq(mod.fit$deviance, mod.fit$df.residual),4))
                            
rbind.data.frame(sum.mod.fit.one, sum.mod.fit.two, sum.mod.fit.three)                           
                            

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
}

#########################################################################
# Duplicate example on p.130-133 - see note in 2nd to last paragraph on p. 133
#   for why this model should maybe not be used.  

Ho.model<-Count ~ Opinion + Age + Sex + Race
#Remember the model can also be written as Count ~ Opinion + Age + Sex + Race + Sex:Race
RS<-fit.loglinear(Ho = Ho.model, Ha = Count ~ Opinion + Age + Sex*Race, data = abortion.opinion)
RO<-fit.loglinear(Ho.model, Count ~ Opinion*Race + Age + Sex, abortion.opinion)
RA<-fit.loglinear(Ho.model, Count ~ Opinion + Age*Race + Sex, abortion.opinion)
SO<-fit.loglinear(Ho.model, Count ~ Opinion*Sex + Age + Race, abortion.opinion)
SA<-fit.loglinear(Ho.model, Count ~ Opinion + Age*Sex + Race, abortion.opinion)
OA<-fit.loglinear(Ho.model, Count ~ Opinion*Age + Sex + Race, abortion.opinion)
data.frame(RS, RO, RA, SO, SA, OA)


#Add opinion*age to the model - model is (R, S, OA)
Ho.model<-Count ~ Race + Sex + Opinion*Age
RS<-fit.loglinear(Ho.model, Count ~ Opinion*Age + Race*Sex, abortion.opinion)
RO<-fit.loglinear(Ho.model, Count ~ Opinion*Age + Race*Opinion + Sex, abortion.opinion)
RA<-fit.loglinear(Ho.model, Count ~ Opinion*Age + Race*Age + Sex, abortion.opinion)
SO<-fit.loglinear(Ho.model, Count ~ Opinion*Age + Sex*Opinion + Race, abortion.opinion)
SA<-fit.loglinear(Ho.model, Count ~ Opinion*Age + Sex*Age + Race, abortion.opinion)
SOA<-fit.loglinear(Ho.model, Count ~ Sex*Opinion*Age + Race, abortion.opinion)
ROA<-fit.loglinear(Ho.model, Count ~ Race*Opinion*Age + Sex, abortion.opinion)
data.frame(RS, RO, RA, SO, SA, SOA, ROA)


#Add opinion*race to the model - model is (RO, OA, S)
Ho.model<-Count ~ Race*Opinion + Opinion*Age + Sex
RS<-fit.loglinear(Ho.model, Count ~ Race*Opinion + Opinion*Age + Race*Sex, abortion.opinion)
RA<-fit.loglinear(Ho.model, Count ~ Race*Opinion + Opinion*Age + Race*Age + Sex, abortion.opinion)
SO<-fit.loglinear(Ho.model, Count ~ Race*Opinion + Opinion*Age + Sex*Opinion, abortion.opinion)
SA<-fit.loglinear(Ho.model, Count ~ Race*Opinion + Opinion*Age + Sex*Age, abortion.opinion)
RSO<-fit.loglinear(Ho.model, Count ~ Race*Sex*Opinion + Opinion*Age, abortion.opinion)
ROA<-fit.loglinear(Ho.model, Count ~ Race*Opinion*Age + Sex, abortion.opinion)
SOA<-fit.loglinear(Ho.model, Count ~ Race*Opinion + Sex*Opinion*Age, abortion.opinion)
data.frame(SA, RA, SO, RS, SOA, ROA, RSO)



#Add RSO to the model - model is (RSO, OA)
Ho.model<-Count ~ Race*Sex*Opinion + Opinion*Age
RSOA<-fit.loglinear(Ho.model, Count ~ Race*Sex*Opinion*Age, abortion.opinion)
SA<-fit.loglinear(Ho.model, Count ~ Race*Sex*Opinion + Opinion*Age + Sex*Age, abortion.opinion)
RA<-fit.loglinear(Ho.model, Count ~ Race*Sex*Opinion + Opinion*Age + Race*Age, abortion.opinion)
SOA<-fit.loglinear(Ho.model, Count ~ Race*Sex*Opinion + Sex*Opinion*Age, abortion.opinion)
ROA<-fit.loglinear(Ho.model, Count ~ Race*Sex*Opinion + Race*Opinion*Age, abortion.opinion)
RSA<-fit.loglinear(Ho.model, Count ~ Race*Sex*Opinion + Opinion*Age + Race*Sex*Age, abortion.opinion)
data.frame(RSOA, SA, RA, SOA, ROA, RSA)


#Investigate model chosen further.
mod.fit<-glm(formula = Count ~ Race*Sex*Opinion + Opinion*Age, 
        data = abortion.opinion, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)


#Put predictions and residuals in a contingency table format
mu.hat<-predict(object = mod.fit, type="response")
pearson<-residuals(object = mod.fit, type="pearson")
all.abortion.opinion<-data.frame(abortion.opinion, mu.hat = round(mu.hat,2), pearson = round(pearson,2))
xtabs(mu.hat ~ Opinion + Age + Sex + Race, data = all.abortion.opinion)
xtabs(pearson ~ Opinion + Age + Sex + Race, data = all.abortion.opinion)


#Calculate goodness-of-fit measures
pearson.stat<-sum(pearson^2)
cat("X^2 =", round(pearson.stat, 4), "with p-value =", round(1-pchisq(pearson.stat, mod.fit$df.residual),4), "\n")

dev<-summary(mod.fit)$deviance
cat("G^2 =", round(dev, 4), "with p-value =", round(1-pchisq(dev, mod.fit$df.residual),4), "\n")


###########################################################################################
# Final model interpretation

mod.fit<-glm(formula = Count ~ Race*Sex*Opinion + Opinion*Age, 
        data = abortion.opinion, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)


#Put predictions and residuals in a contingency table format
mu.hat<-predict(mod.fit, type="response")
pearson<-residuals(object = mod.fit, type="pearson")
h<-lm.influence(model = mod.fit)$h  
standard.pearson<-pearson/sqrt(1-h)
all.abortion.opinion<-data.frame(abortion.opinion, mu.hat = round(mu.hat,2), pearson = round(pearson,2), standard.pearson = round(standard.pearson,2))

xtabs(mu.hat ~ Opinion + Age + Sex + Race, data = all.abortion.opinion)
xtabs(pearson ~ Opinion + Age + Sex + Race, data = all.abortion.opinion)
xtabs(standard.pearson ~ Opinion + Age + Sex + Race, data = all.abortion.opinion)




#Use xtabs() to collapse
save.A<-xtabs(mu.hat ~ Opinion + Sex + Race, data = all.abortion.opinion)
save.A


#Use gsummary() to collapse - notice using xtabs() alone saves time
library(nlme)
Collapse.A2<-gsummary(all.abortion.opinion, sum, form = ~ mu.hat | Opinion / Sex / Race)
Collapse.A2
xtabs(mu.hat ~ Opinion + Sex + Race, data = Collapse.A2)


#Use xtabs() to collapse - reorder race and sex
save.A2<-xtabs(mu.hat ~ Opinion + Race + Sex, data = all.abortion.opinion)
save.A2


#Use xtabs() to collapse over Race and Sex
save.RS<-xtabs(mu.hat ~ Opinion + Age, data = all.abortion.opinion)
round(save.RS,2)

save.all<-xtabs(mu.hat ~ Opinion + Age + Sex + Race, data = all.abortion.opinion)
save.all

#OR for Age=18-25, 66+ and Opinion=Yes,No given Gender=Female and Race=White
141.04*65.98/(44.61*105.91)
save.all[3,1,1,2]
save.all[1,6,1,2]
save.all[1,1,1,2]
save.all[3,1,2,2]
save.all[3,1,1,2]*save.all[1,6,1,2]/(save.all[1,1,1,2]*save.all[3,1,2,2])


###########################################################################################
# C.I. OR

#C.I. for OA odds ratio with two most extreme age groups and opinion is yes vs. no
#  Odds of a yes instead of no response are theta times larger for age = 18-25 than age = 66+
mod.fit$coefficients  #Examine all lambdas
mod.fit$coefficients[16:25]  #Examine lambdas for OA
as.numeric(exp(-mod.fit$coefficients[25])) #OR for OA
sum.mod.fit<-summary(mod.fit)
sum.mod.fit$cov.unscaled[25,25]
alpha<-0.05
exp(-mod.fit$coefficients[25]+qnorm(p = c(alpha/2, 1-alpha/2), mean = 0, sd = 1)*sqrt(sum.mod.fit$cov.unscaled[25,25]))


#Odds ratio C.I. for SO given Race = white
#  Odds of a yes instead of no response are theta times larger for females than males GIVEN white
mod.fit$coefficients[14:15]  #Examine all lambdas 
mod.fit$coefficients[26:27] 
as.numeric(exp(-mod.fit$coefficients[15]-mod.fit$coefficients[27])) #OR
sum.mod.fit$cov.unscaled[15,15]  #Variances and covariances
sum.mod.fit$cov.unscaled[27,27]
sum.mod.fit$cov.unscaled[15,27]
var.or<-(-1)^2*sum.mod.fit$cov.unscaled[15,15]+(-1)^2*sum.mod.fit$cov.unscaled[27,27]+2*(-1)^2*(-1)^2*sum.mod.fit$cov.unscaled[15,27]
exp(-mod.fit$coefficients[15]-mod.fit$coefficients[27]+qnorm(p = c(alpha/2, 1-alpha/2), mean = 0, sd = 1)*sqrt(var.or))




######################################################################
# R automated procedure for model selection

mod.fit.one<-glm(formula = Count ~ Opinion + Age + Sex + Race, data = abortion.opinion, 
          family = poisson(link = log), na.action = na.exclude, 
          control = list(epsilon = 0.0001, maxit = 50, trace = F))
logLik(mod.fit.one)
AIC(mod.fit.one)

step(mod.fit.one, direction="forward", scope=list(lower=mod.fit.one$formula, 
     upper=formula(Count ~ (Opinion + Age + Sex + Race)^3)), trace=1)
names(res)
res$anova




#This will do the same as above.  
library(MASS)
mod.fit.one<-glm(formula = Count ~ Opinion + Age + Sex + Race, data = abortion.opinion, 
          family = poisson(link = log), na.action = na.exclude, 
          control = list(epsilon = 0.0001, maxit = 50, trace = F))
res<-stepAIC(mod.fit.one, direction="forward", scope=list(lower=mod.fit.one$formula, 
     upper=formula(Count ~ (Opinion + Age + Sex + Race)^3)), trace=1)
names(res)
res$anova
