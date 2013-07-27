
#Set The Working Directory
setwd()

library(foreign) 
library(mgcv)
fem.at <- read.dta("allwomen1986.dta")

#Women Only Post 1986
#Full Model

ols.1 <- gam(famresp ~ inlaborf + notinlbf + propinc + hours + spinlabo + spnotinl + widowed + married + nevmar +           			childs + age + educ + maeduc + white + south + srcbelt + cath + bapsect + jewish + noneoth + attend + 			polviews + year, data=fem.at)

summary(ols.1)

gam.1 <- gam(famresp ~ inlaborf + notinlbf + s(propinc, bs="cr", fx=TRUE, k=4) + s(hours, bs="cr") + spinlabo + spnotinl + widowed + married + nevmar + childs + s(age, bs="cr") + educ + maeduc + white + south + srcbelt + cath + bapsect + jewish + noneoth + attend + polviews + year, data=fem.at)

summary(gam.1)

#Figure 5.5
par(mfrow=c(1,3))
plot(gam.1, select=1, se=TRUE, shift=2.10, rug=FALSE, ylab="Feminist Attitudes", xlab="Proportion of Income Earned", bty="l")
plot(gam.1, select=2, se=TRUE, shift=2.10, rug=FALSE, ylab="Feminist Attitudes", xlab="Hours Worked", bty="l")
plot(gam.1, select=3, se=TRUE, shift=2.10, rug=FALSE, ylab="Feminist Attitudes", xlab="Age", bty="l")

anova(ols.1, gam.1, test="Chisq")

#Refit with Manual smoothing selection
gam.1 <- gam(famresp ~ inlaborf + notinlbf + s(propinc, bs="cr", fx=TRUE, k=6) + s(hours, bs="cr") + spinlabo + spnotinl + widowed + married + nevmar + childs + s(age, bs="cr") + educ + maeduc + white + south + srcbelt + cath + bapsect + jewish + noneoth + attend + polviews + year, data=fem.at)

summary(gam.1)

#Figure 5.6
plot(gam.1, select=1, se=TRUE, shift=2.10, rug=FALSE, ylab="Feminist Attitudes", xlab="Proportion of Income Earned", bty="l")

anova(ols.1, gam.1, test="Chisq")

#Retest Check Age 
gam.2 <- gam(famresp ~ inlaborf + notinlbf + s(propinc, bs="cr", fx=TRUE, k=6) + hours + spinlabo + spnotinl + widowed + married + nevmar + childs + age + educ + maeduc + white + south + srcbelt + cath + bapsect + jewish + noneoth + attend + polviews + year, data=fem.at)

#Test - We See to Retain Age as Nonlinear
anova(gam.2, gam.1, test="Chisq")



