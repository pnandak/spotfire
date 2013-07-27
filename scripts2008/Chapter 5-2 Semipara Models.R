library(foreign)

#Set the Working Directory
setwd()
#Read the data
cong <- read.dta("jacob.dta")
attach(cong)

library(mgcv)

#Baseline Model
ols <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend.raw + pres.vote + checks.raw + 
          marginal + partisan.redist + perotvote, data=cong) 
          
          
#Test Each Continuous Covariate
gam.1 <- gam(chal.vote ~ exp.chal + s(chal.spend.raw, bs="cr") + inc.spend.raw + pres.vote + checks.raw + marginal + partisan.redist + perotvote, data=cong) 

gam.2 <- gam(chal.vote ~ exp.chal + chal.spend.raw + s(inc.spend.raw, bs="cr") + pres.vote + checks.raw + marginal + partisan.redist + perotvote, data=cong)

gam.3 <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend.raw + s(pres.vote, bs="cr") + checks.raw + marginal + partisan.redist + perotvote, data=cong)

gam.4 <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend.raw + pres.vote + s(checks.raw, bs="cr") + marginal + partisan.redist + perotvote, data=cong)

gam.5 <- gam(chal.vote ~ exp.chal + chal.spend.raw+ inc.spend.raw + pres.vote + checks.raw + marginal + partisan.redist + s(perotvote, bs="cr"), data=cong)   

#Now Against Logged Fits
ols.1 <- gam(chal.vote ~ exp.chal + chal.spend + inc.spend.raw + pres.vote + checks.raw + 
          marginal + partisan.redist + perotvote, data=cong) 
          
ols.2 <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend + pres.vote + checks.raw + 
          marginal + partisan.redist + perotvote, data=cong) 
          
ols.3 <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend.raw + log(pres.vote) + checks.raw + 
          marginal + partisan.redist + perotvote, data=cong)     

ols.4 <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend.raw + pres.vote + logchecks1 + 
          marginal + partisan.redist + perotvote, data=cong)  

ols.5 <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend.raw + pres.vote + checks.raw + 
          marginal + partisan.redist + log.perot, data=cong)           
          
anova(ols.1, gam.1, test="Chisq")
anova(ols.2, gam.2, test="Chisq")
anova(ols.3, gam.3, test="Chisq")
anova(ols.4, gam.4, test="Chisq")
anova(ols.5, gam.5, test="Chisq")

ols.1 <- gam(chal.vote ~ exp.chal + chal.spend.raw + I(chal.spend.raw^2) + inc.spend.raw + pres.vote + checks.raw +  marginal + partisan.redist + perotvote, data=cong) 
          
ols.2 <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend.raw + I(inc.spend.raw^2) + pres.vote + checks.raw + marginal + partisan.redist + perotvote, data=cong) 
          
ols.3 <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend.raw + pres.vote + I(pres.vote^2) + checks.raw + marginal + partisan.redist + perotvote, data=cong)     

ols.4 <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend.raw + pres.vote + checks.raw + I(checks.raw^2) + marginal + partisan.redist + perotvote, data=cong)  

ols.5 <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend.raw + pres.vote + checks.raw + 
          marginal + partisan.redist + perotvote + I(perotvote^2), data=cong)               
          
anova(ols.1, gam.1, test="Chisq")
anova(ols.2, gam.2, test="Chisq")
anova(ols.3, gam.3, test="Chisq")
anova(ols.4, gam.4, test="Chisq")
anova(ols.5, gam.5, test="Chisq")

#Original Jacobson and Dimock Model
ols.4 <- gam(chal.vote ~ exp.chal + chal.spend + inc.spend + pres.vote + logchecks1 + 
          marginal + partisan.redist + perotvote, data=cong)
           
summary(ols.4)

gam.5 <- gam(chal.vote ~ exp.chal + s(chal.spend.raw, bs="cr") + s(inc.spend.raw, bs="cr") + s(pres.vote, bs= "cr") + logchecks1 + marginal + partisan.redist + s(perotvote, bs="cr"), data=cong)

gam.7 <- gam(chal.vote ~ exp.chal + s(chal.spend.raw, bs="cr") + s(inc.spend.raw, bs="cr") + s(pres.vote, bs= "cr") + logchecks1 + marginal + partisan.redist + perotvote, data=cong) 

summary(gam.5)  
summary(gam.7) 

anova(ols.4, gam.5, test="Chisq")

anova(gam.7, gam.5, test="Chisq")

#Figure 5.2
par(mfrow = c(2,2))
plot(gam.5, select=1, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", xlab="Challenger Spending", residual=FALSE, bty="l", shift=33.02)
points(chal.spend.raw, chal.vote, pch=".", cex=1.75)

plot(gam.5, select=2, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", xlab="Incumbent Spending", residual=FALSE, bty="l", shift=33.02)
points(inc.spend.raw, chal.vote, pch=".", cex=1.75)

plot(gam.5, select=3, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", xlab="Challenger's Party Pres. Vote", residual=FALSE, bty="l", shift=33.02)
points(pres.vote, chal.vote, pch=".", cex=1.75)

plot(gam.5, select=4, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", residual=FALSE, bty="l", shift=33.02)
points(perotvote, chal.vote, pch=".", cex=1.75)
dev.off()

#Interactions
detach(cong)
cong$nonmarg <- as.numeric(cong$marginal==0)
attach(cong)

gam.4 <- gam(chal.vote ~ s(perotvote, by=marginal) + s(perotvote, by=nonmarg) + s(checks.raw) + marginal, data=cong)
summary(gam.4)

#Figure 5.3
par(mfrow = c(1,2))
plot(gam.4, select=1, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", residual=FALSE, bty="l", shift=36.6672, main="Competitive Districts")
points(perotvote, chal.vote, pch=".", cex=1.75)

plot(gam.4, select=2, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", residual=FALSE, bty="l", shift=36.6672, main="Non-Competitive Districts")
points(perotvote, chal.vote, pch=".", cex=1.75)

#Nonlinear Interaction
gam.6 <- gam(chal.vote ~ exp.chal + s(chal.spend.raw, bs="cr") + s(inc.spend.raw, bs="cr") + s(pres.vote, bs= "cr") + logchecks1:chal.spend.raw + marginal + partisan.redist + perotvote, data=cong) 

gam.7 <- gam(chal.vote ~ exp.chal + s(chal.spend.raw, checks.raw) + s(inc.spend.raw, bs="cr") + s(pres.vote, bs= "cr")  + marginal + partisan.redist + s(perotvote, bs="cr"), data=cong) 

summary(gam.6)  

#Figure 5.4
vis.gam(gam.7, view=c("chal.spend.raw", "checks.raw"), theta=325, se=FALSE, xlab="Number of Overdrafts", ylab="Challenger Spending", color="bw", plot.type="persp", zlim=range(seq(20,60, by=10)), type="response", ticktype="simple")

anova(gam.6, gam.7, test="Chisq")
