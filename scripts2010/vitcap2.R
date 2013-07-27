###############################################
## Day 5: vital capacity in cadmium industry
###############################################
library(ISwR,lib.loc="~/02441")

data(vitcap2)
summary(vitcap2)
vitcap2$fGroup<-factor(vitcap2$group,labels=c("+10","0-10","Not"))
pairs(vitcap2,panel=panel.smooth)
lm1<-lm(vital.capacity~fGroup*age,vitcap2)
summary(lm1)
summary.aov(lm1)
par(mfrow=c(2,2))
plot(lm1)

par(mfrow=c(1,1))
plot(vitcap2$age,vitcap2$vital.capacity,pch=19,col=vitcap2$group,xlim=c(0,65),ylim=c(2.5,8.2))
co<-coef(lm1)
abline(a=co[1],b=co[4],lwd=3)
abline(a=co[1]+co[2],b=co[4]+co[5],lwd=3,col=2)
abline(a=co[1]+co[3],b=co[4]+co[6],lwd=3,col=3)
