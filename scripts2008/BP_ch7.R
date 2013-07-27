#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  3-21-02                                                    #
# UPDATE: 1-10-03, 12-14-03 for R, 1-2-08                           #
# PURPOSE: Blood pressure example using loglinear models            #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#Create contingency table - notice the data is entered by columns
n.table<-array(c(716, 207, 79, 25, 819, 186, 67, 22), dim=c(2,2,2), 
        dimnames=list(Cholesterol=c("Normal", "High"), 
             BP=c("Normal", "High"), Personality=c("A", "B")))
n.table

#Convert data
BP.data.frame<-as.data.frame(as.table(n.table))
BP.data.frame

#Show how to convert back to a contingency table
xtabs(Freq ~ Cholesterol + BP + Personality, data = BP.data.frame)



####################################################################


options(contrasts=c("contr.treatment", "contr.poly")) 
#Model = (cholestrol*BP, cholestrol*personality, BP*personality)
#Note: Cholesterol + BP + Personality + Cholesterol:BP + Cholesterol:Personality + BP:Personality and
#      Cholesterol*BP + Cholesterol*Personality + BP*Personality are equivalent forms
mod.fit.Ha<-glm(formula = Freq ~ (Cholesterol + BP + Personality)^2, 
        data = BP.data.frame, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit.Ha)



#Model = (cholestrol*personality, BP*personality)
#Note: Cholesterol + BP + Personality + Cholesterol:Personality + BP:Personality and 
#      Cholesterol*Personality + BP*Personality are equivalent to
#      Cholesterol + Personality)^2 + (BP + Personality)^2
mod.fit.Ho<-glm(formula = Freq ~ (Cholesterol + Personality)^2 + (BP + Personality)^2, 
        data = BP.data.frame, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit.Ho)
model.matrix(mod.fit.Ho)

G.sq<-mod.fit.Ho$deviance-mod.fit.Ha$deviance
G.sq.df<-mod.fit.Ho$df.residual-mod.fit.Ha$df.residual
cat("G^2 =", round(G.sq,4), "with p-value =", round(1-pchisq(G.sq, G.sq.df),4), "\n")


#Put predictions and residuals in a contingency table format
mu.hat<-predict(mod.fit.Ho, type="response")
pearson<-residuals(mod.fit.Ho, type="pearson")
h<-lm.influence(model = mod.fit.Ho)$h  
standard.pearson<-pearson/sqrt(1-h)

all.BP<-data.frame(BP.data.frame, mu.hat = round(mu.hat,4), pearson = round(pearson,4), standard.pearson = round(standard.pearson,4))
xtabs(mu.hat ~ Cholesterol + BP + Personality, data = all.BP)
xtabs(pearson ~ Cholesterol + BP + Personality, data = all.BP)
xtabs(standard.pearson ~ Cholesterol + BP + Personality, data = all.BP)


#Calculate goodness-of-fit measures
pearson.stat<-sum(pearson^2)
cat("X^2 =", round(pearson.stat, 4), "with p-value =", round(1-pchisq(pearson.stat, mod.fit.Ho$df.residual),4))

dev<-mod.fit.Ho$deviance
cat("G^2 =", round(dev, 4), "with p-value =", round(1-pchisq(dev, mod.fit.Ho$df.residual),4))
