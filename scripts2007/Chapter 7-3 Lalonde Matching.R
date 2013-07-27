
#Load Libraries
library(Matching)
library(mgcv)

#Load the Data
data(lalonde)

#Estimate GLM Propensity Score
glm <- glm(treat ~ age +I(age^2) + educ + I(educ^2) + black +
             hisp + married + nodegr + re74  + I(re74^2) + re75 + I(re75^2) +
             u74 + u75, family=binomial, data=lalonde) 
             
#Estimate GAM Propensity Score           
gam <- gam(treat ~ s(age, fx=TRUE, k=10) +  s(educ) + black +
             hisp + married + nodegr + re74  + s(re74) + re75 + s(re75) +
             u74 + u75, family=binomial, data=lalonde)

#Likelihood Ratio Test 
1-pchisq((deviance(glm) - deviance(gam)), 7)
          
anova(glm, gam, test="Chisq")

#Now use in mathcing analysis
#First with GLM
X  <- glm$fitted
Y  <- lalonde$re78
Tr  <- lalonde$treat

# One-to-one matching with replacement
rr.1  <- Match(Y=Y,Tr=Tr,X=X,M=1);
summary(rr.1)
MatchBalance(treat~age + I(age^2) + educ + I(educ^2) + black +
             hisp + married + nodegr + re74  + I(re74^2) + re75 + I(re75^2) +
             u74 + u75 + I(re74*re75) + I(age*nodegr) + I(educ*re74) + I(educ*re75),
             data=lalonde, match.out=rr.1, nboots=1000)

#Now with GAM
X  <- gam$fitted

# One-to-one matching with replacement
rr.2  <- Match(Y=Y,Tr=Tr,X=X,M=1);
summary(rr.2)
MatchBalance(treat~age + I(age^2) + educ + I(educ^2) + black +
             hisp + married + nodegr + re74  + I(re74^2) + re75 + I(re75^2) +
             u74 + u75 + I(re74*re75) + I(age*nodegr) + I(educ*re74) + I(educ*re75),
             data=lalonde, match.out=rr.2, nboots=1000)

