
#Set the Working Directory
setwd()

library(foreign)
library(survival)

riot <- read.dta("repriot.dta")

mod.1 <- coxph(Surv(newend, cens) ~ lognwun + manuwage + unemrate + percfor + x15 + sumpi + lstwkall + lwka2 + d6768 + pasthist, data=riot)

summary(mod.1)

zph.mod.1 <- cox.zph(mod.1)

mod.base <- coxph(Surv(newend, cens) ~ pspline(nonwhtunemp, df=4) + pspline(manuwage, df=4) + pspline(unemrate, df=4) + pspline(percfor, df=4) + pspline(x15, df=4) + pspline(sumpi, df=4) + pspline(lstwkall, df=4) + d6768 + pasthist, data=riot)

summary(mod.base)

#Test Nonproportional Hazards
zph.mod.base <- cox.zph(mod.base)
zph.mod.base

termplot(mod.base, term=2, se=TRUE, rug=TRUE, ylab="Log Hazard", xlab="Manufacturing Wage")

#Fit Parsimonious Model
mod.trim <- coxph(Surv(newend, cens) ~ pspline(nonwhtunemp, df=4) + pspline(manuwage, df=4) + unemrate + percfor + pspline(x15, df=4) + pspline(sumpi, df=4) + pspline(lstwkall, df=4) + d6768 + pasthist, data=riot)

#Test Nonproportional Hazards
zph.mod.trim <- cox.zph(mod.trim)
zph.mod.trim

summary(mod.trim)

#Plot The Effects
par(mfrow=c(2,3))
termplot(mod.trim, term = 1, se = TRUE, ylab = "Log Hazard", rug=TRUE, xlab = "Non-White Unemployment") 

termplot(mod.trim, term = 2, se = TRUE, ylab = "Log Hazard", rug=TRUE, xlab = "Manufacturing Wage") 

termplot(mod.trim, term = 5, se = TRUE, ylab = "Log Hazard", rug=TRUE, xlab = "Non-White Unemployment x Percent Foreign Born") 

termplot(mod.trim, term = 6, se = TRUE, ylab = "Log Hazard", rug=TRUE, xlab = "Spatial Diffusion") 

termplot(mod.trim, term = 7, se = TRUE, ylab = "Log Hazard", rug=TRUE, xlab = "National Level Diffusion") 

#Figure 6.8
par(mfrow =c(1,2))
termplot(mod.trim, term = 2, se = TRUE, ylab = "Log Hazard", rug=FALSE, xlab = "Manufacturing Wage", bty="l") 

termplot(mod.trim, term = 1, se = TRUE, ylab = "Log Hazard", rug=FALSE, xlab = "Non-White Unemployment", bty="l") 

anova(mod.trim, mod.base, test="Chisq")

