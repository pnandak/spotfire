library(RWinEdt)
library(MASS)
library(epitools)
library(lattice)
options(width=75)


# School behaviour (Everitt, 1977, p 67)

n <- c(16,7,15,34,5,3,1,1,3,8,1,3)
behaviour <- c(rep("NonDeviant",6), rep("Deviant",6))
risk <- rep(c("NotAtRisk","AtRisk"),6)
adversity <- rep(c("Low","Low","Med","Med","High","High"),2)

behave <- data.frame(n=n, behaviour=behaviour, risk=risk, adversity=adversity)

behave.xyz <- glm(n ~ behaviour*risk*adversity, family=poisson(), data=behave)
behave.xy.xz.yz <- glm(n ~ (behaviour+risk+adversity)^2, family=poisson(), data=behave)
behave.xy.xz <- glm(n ~ behaviour*risk + behaviour*adversity, family=poisson(), data=behave)
behave.xy.yz <- glm(n ~ behaviour*risk + risk*adversity, family=poisson(), data=behave)
behave.xz.yz <- glm(n ~ behaviour*adversity + risk*adversity, family=poisson(), data=behave)
behave.xy.z <- glm(n ~ behaviour*risk + adversity, family=poisson(), data=behave)
behave.xz.y <- glm(n ~ behaviour*adversity + risk, family=poisson(), data=behave)
behave.yz.x <- glm(n ~ risk*adversity + behaviour, family=poisson(), data=behave)
behave.x.y.z <- glm(n ~ behaviour + risk + adversity, family=poisson(), data=behave)

summary(behave.xyz)
summary(behave.xy.xz.yz)
summary(behave.xy.yz)
summary(behave.xz.yz)
summary(behave.xy.z)
summary(behave.xz.y)
summary(behave.yzx)
summary(behave.x.y.z)

df <- c(df.residual(behave.xyz),
        df.residual(behave.xy.xz.yz),
        df.residual(behave.xy.xz),
        df.residual(behave.xy.yz),
        df.residual(behave.xz.yz),
        df.residual(behave.xy.z),
        df.residual(behave.xz.y),
        df.residual(behave.yz.x),
        df.residual(behave.x.y.z))
        
x2pear <- c(sum(resid(behave.xyz,type='pearson')^2),
        sum(resid(behave.xy.xz.yz,type='pearson')^2),
        sum(resid(behave.xy.xz,type='pearson')^2),
        sum(resid(behave.xy.yz,type='pearson')^2),
        sum(resid(behave.xz.yz,type='pearson')^2),
        sum(resid(behave.xy.z,type='pearson')^2),
        sum(resid(behave.xz.y,type='pearson')^2),
        sum(resid(behave.yz.x,type='pearson')^2),
        sum(resid(behave.x.y.z,type='pearson')^2))
        
x2 <- c(deviance(behave.xyz),
        deviance(behave.xy.xz.yz),
        deviance(behave.xy.xz),
        deviance(behave.xy.yz),
        deviance(behave.xz.yz),
        deviance(behave.xy.z),
        deviance(behave.xz.y),
        deviance(behave.yz.x),
        deviance(behave.x.y.z))
        
print(cbind(df,x2pear,x2, pchisq(x2,df,lower.tail=F))[2:9,],digits=2)

anova(behave.xy.xz.yz, behave.xy.yz, test='Chisq')
anova(behave.xy.xz.yz, behave.xz.yz, test='Chisq')
anova(behave.xy.xz.yz, behave.yz.x, test='Chisq')
anova(behave.xy.yz, behave.yz.x, test='Chisq')
anova(behave.xz.yz, behave.yz.x, test='Chisq')

behave.yz <- glm(n ~ risk*adversity, family=poisson(), data=behave)

summary(behave.yz)
anova(behave.yz, test='Chisq')

behave.y.z <- glm(n ~ risk + adversity, family=poisson(), data=behave)

summary(behave.y.z)
anova(behave.y.z, behave.yz, test='Chisq')

anova(behave.x.y.z, behave.yz.x, test='Chisq')
