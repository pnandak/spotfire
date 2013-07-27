# --- Logistic and Log-linear Regression ---#
#     Death Penalty Example                 #
# ------------------------------------------#

DeathP =c("Yes","No")
Victim.race=c("White","Black")
Defend.race=c("White","Black")
deathpenalty = expand.grid(DeathP,Victim.race,Defend.race)
names(deathpenalty) =c("DeathP","Defendent","Victim")
deathpenalty=data.frame(deathpenalty,counts=c(19,132,11,52,0,9,6,97))
deathpenalty$DeathP = as.factor(deathpenalty$DeathP)
deathpenalty$Victim=as.factor(deathpenalty$Victim)
deathpenalty$Defendent=as.factor(deathpenalty$Defendent)
#xtabs(counts~DeathP+Defendent+Victim,deathpenalty)

fitDVP = glm(counts~DeathP+Defendent+Victim,data=deathpenalty, family=poisson)
summary(fitDVP)
fitP.DV = update(fitDVP,.~.+Defendent*Victim)
summary(fitP.DV)
fitVP.DV = update(fitDVP,.~.+DeathP*Victim+Defendent*Victim)
summary(fitVP.DV)
fitDP.VP.DV = update(fitDVP,.~DeathP*Victim+Defendent*Victim+DeathP*Defendent)
summary(fitDP.VP.DV)
fit.Saturated = glm(counts~DeathP*Defendent*Victim,data=deathpenalty,family=poisson)
anova(fitDVP,fitP.DV,fitVP.DV,fitDP.VP.DV,fit.Saturated,test="Chisq")
fitPD.PV = update(fitDVP,.~.+DeathP*Victim+DeathP*Defendent)

# ------------------------- #
# Using logistic regression
# ------------------------- #

attach(deathpenalty)
yes = deathpenalty[c(1,3,5,7),]$counts
no = deathpenalty[c(2,4,6,8),]$counts
Victim.2=Victim[c(2,4,6,8)]
Defendent.2 =Defendent[c(2,4,6,8)]

fitLogistic.V = glm(cbind(yes,no)~Victim.2,family=binomial(link="logit"))
fitLogistic.V.D = glm(cbind(yes,no)~Victim.2+Defendent.2,family=binomial(link="logit"))

 here is a handy function for computing confidence intervals.
confint(fitLogistic.V,"Victim.2Black")
confint(fitLogistic.V.D,c("Victim.2Black","Defendent.2Black"))

anova(fitLogistic.V,fitLogistic.V.D,test="Chisq")

