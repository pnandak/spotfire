#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-29-02                                                    #
# UPDATE: 12-13-03, 12-28-07                                        #
# Purpose: Find the logit model for the AZT and AIDS data on p. 112 #
#                                                                   #
# NOTES:                                                            #
#####################################################################

n.table<-array(c(14, 32, 93, 81, 11, 12, 52, 43), dim = c(2, 2, 2), 
        dimnames = list(AZT=c("Yes", "No"), 
        Symptoms=c("Yes", "No"), Race=c("White", "Black")))
n.table


#Convert data - examine the data sets at each step
#  Note: this is an explanatory variable pattern form of the data set (discussed in Chapter 5)
azt.aids.symp<-as.data.frame(as.table(n.table))
azt.aids.table<-xtabs(Freq ~ AZT + Race, data = azt.aids.symp)
azt.aids<-as.data.frame(azt.aids.table)
symp.yes<-azt.aids.symp[azt.aids.symp$Symptoms == "Yes",]
azt.aids2<-data.frame(azt.aids, Symptoms = symp.yes$Freq)
azt.aids2


mod.fit<-glm(formula = Symptoms/Freq ~ AZT + Race, data = azt.aids2, weight = Freq, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.000001, maxit = 50, trace = T))
summary(mod.fit)
data.frame(azt.aids2, obs.prop = round(azt.aids2$Symptoms/azt.aids2$Freq,4),
           pi.hat = round(predict(mod.fit, azt.aids2, type = "response"),4))
anova(mod.fit)
#Check what the X matrix looks like
model.matrix(mod.fit)

#Matches Table 4.5 on p. 112 after change reference levels 
azt.aids3<-data.frame(azt.aids2, AZT.new = relevel(azt.aids2$AZT, ref = "No"), Race.new = relevel(azt.aids2$Race, ref = "Black"))
mod.fit.tab4.5<-glm(formula = Symptoms/Freq ~ AZT.new + Race.new, data = azt.aids3, weight = Freq, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.000001, maxit = 50, trace = T))
summary(mod.fit.tab4.5)
anova(mod.fit.tab4.5)
model.matrix(mod.fit.tab4.5)


#################################################################
#  Change to sum to 0 option for the levels of each factor

#Run this code to change the coding of qualitative variables.  
N<-factor(c(0,1,2,4))

#Sum to zero option
options(contrasts=c("contr.sum", "contr.poly")) 
contrasts(N)

#match Agresti (1996) Table 5.6 on p. 121 (second column)
mod.fit<-glm(formula = Symptoms/Freq ~ AZT + Race, data = azt.aids2, weight = Freq, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.000001, maxit = 50, trace = T))
summary(mod.fit)
data.frame(azt.aids2, obs.prop = round(azt.aids2$Symptoms/azt.aids2$Freq,4),
           pi.hat = round(predict(mod.fit, azt.aids2, type = "response"),4))

#Check what the X matrix looks like
model.matrix(mod.fit)


#First level set to 0 - change back!
options(contrasts=c("contr.treatment", "contr.poly")) 
contrasts(N)




#################################################################
# Old way to fit a loglinear model - for historical reasons :)

#Fits logit model {AZT, Race} with Symptoms as the dependent variable 
#  using a loglinear model setup.  LRT and Pearson statistics match Agresti p. 120
loglin(n.table, list(c("AZT","Race"), c("AZT", "Symptoms"), c("Race", "Symptoms")), 
      param=T, fit=T)
