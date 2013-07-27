

library(nlme)
library(foreign)
library(mgcv)

#Set the Working Directory
setwd()
#Read in the Data
Bryk <- read.dta("schools.dta")
attach(Bryk)

#Generalized Additive Mixed Model
mixed.1 <- gamm(mathach ~ s(cses, bs="cr") + minority + female, random=list(school=~1), na.action = na.omit, data=Bryk)

#Summarize Model
summary(mixed.1$gam)
summary(mixed.1$lme)

#Plot Effect - Overall Linear Here
plot(mixed.1$gam, select=1, rug=FALSE, xlab="Family SES", ylab="Math Achievement", bty="l", shift=13.92)

#Generalized Additive Mixed Model with Interaction
mixed.2 <- gamm(mathach ~ s(cses, bs="cr", by=cath, fx=TRUE, k=6) + s(cses, bs="cr", by=public, fx=TRUE, k=6) + minority + female, random=list(school=~1), na.action = na.omit, data=Bryk)

summary(mixed.2$gam)
summary(mixed.2$lme)

#Figure 7.1
par(mfrow=c(1,2))
plot(mixed.2$gam, select=1, rug=FALSE, xlab="Family SES", ylab="Math Achievement", bty="l", main="Parochial Schools", shift=10.74)
plot(mixed.2$gam, select=2, rug=FALSE, xlab="Family SES", ylab="Math Achievement", bty="l", main="Public Schools", shift=10.74)

#Parametric Mixed Model
mixed.3<- gamm(mathach ~ cses + minority + female, random=list(school=~1), na.action = na.omit, data=Bryk)

#Test Against Linear Functional Form - Test Is Approximate
anova(mixed.3$lme, mixed.2$lme)



