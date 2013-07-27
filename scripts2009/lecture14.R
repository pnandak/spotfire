setwd("C:/Documents and Settings/xuanming/My Documents/Nancy Research/Teaching/Stat191_2008/lecture14 Logistic Regression/")

# ---- Flu data set -----

flu=read.table("Flu.txt",header=T)
attach(flu)
pairs(flu)

# look at the help files for these functions.
?glm
?family

flu.glm <- glm(Shot ~ Age + Health.Aware,family=binomial(link='logit'))
print(summary(flu.glm))

names(flu.glm)
plot(flu$Shot, flu.glm$fitted,col="blue",pch=15,cex=1.5)

plot(flu$Age, flu$Shot,col="blue",pch=12,cex=1.5)
points(flu$Age, flu.glm$fitted,col="red", pch=15,lwd=1.5)

plot(flu$Health, flu$Shot,col="blue",pch=12,cex=1.5)
points(flu$Health, flu.glm$fitted,col="red", pch=15,lwd=1.5)


# sequential anova tests using different criterion.
print(anova(flu.glm,test="Chisq"))


# get residuals
flu.glm.devres=residuals(flu.glm,type="deviance")
flu.glm.pearson=residuals(flu.glm,type="pearson")

# -----------------------------------------------
# lumber example for Poisson regression.
#    The Miller Lumber Company conducted an in-store customer survey. 
#    The researcher counted the number of customers who visited the 
#    store from each nearby census tract. The researcher also collected 
#    and subsequently retained five (quantitative) predictor variables 
#    for use in the Poisson Regression.  
#    Predictors: number of housing units in region, average household income, average
#    housing unit age in region, distance to nearest competitor, distance to store in miles.
#    Response: number of customers visiting store from census tract.  Each data 
#    point represents one census tract.
# --------------------------------------------
lumber <- read.table('Lumber.txt', header=T)
attach(lumber)

# Fit model using log link

lumber.glm <- glm(Customers ~ Housing + Income + Age + Competitor + Store, family=poisson())

# Check to see if Store and Competitor can be dropped

lumber.R.glm <- glm(Customers ~ Housing + Income + Age, family=poisson(link='log'))

# partial deviance test of full vs. reduced

print(anova(lumber.R.glm, lumber.glm,test="Chisq"))

# look at BIC
library(BMA)
lumber.bic = bic.glm(Customers ~ Housing + Income + Age + Competitor + Store, data=lumber, glm.family=poisson())
summary(lumber.bic)

