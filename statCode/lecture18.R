library(RWinEdt)
library(MASS)
library(epitools)
library(lattice)
options(width=75)


# Business major

n <- c(68, 56, 91, 40, 5, 6, 61, 59)
major <- c("Accounting","Accounting","Administration","Administration",
   "Economics","Economics","Finance","Finance")
gender <- rep(c("Female","Male"),4)

business.tab <- matrix(n, ncol=2, byrow=T, 
  dimnames=list(major=unique(major), gender=c("Female","Male")))

business <- data.frame(n=n, major=major, gender=gender)

business.ind <- glm(n ~ major + gender, family=poisson(), data=business)

summary(business.ind)
anova(business.ind, test="Chisq")
pchisq(deviance(business.ind),df.residual(business.ind),lower.tail=F)

pearson.ind <- sum(resid(business.ind,type='pearson')^2)
pearson.ind
pchisq(pearson.ind, df.residual(business.ind), lower.tail=F)

business.tab
chisq.test(business.tab)

fitted.ind <- matrix(fitted(business.ind), ncol=2, byrow=T, 
  dimnames=list(major=unique(major), gender=c("Female","Male")))
  
devres.ind <- matrix(resid(business.ind), ncol=2, byrow=T, 
  dimnames=list(major=unique(major), gender=c("Female","Male")))

pres.ind <- matrix(resid(business.ind, type='pearson'), ncol=2, byrow=T, 
  dimnames=list(major=unique(major), gender=c("Female","Male")))

cbind(business.tab,fitted.ind)
print(cbind(devres.ind,pres.ind),digits=2)

business.int <- glm(n ~ major * gender, family=poisson(), data=business)

summary(business.int)
anova(business.int, test="Chisq")

options(contrasts=c("contr.sum","contr.poly"))
business.int2 <- glm(n ~ major * gender, family=poisson(), data=business)
options(contrasts=c("contr.treatment","contr.poly"))

summary(business.int2)
anova(business.int2, test="Chisq")

options(contrasts=c("contr.sum","contr.poly"))
business.int3 <- glm(n ~ major * gender - 1, family=poisson(), data=business)
options(contrasts=c("contr.treatment","contr.poly"))

summary(business.int3)


anova(business.ind, business.int, test="Chisq")

# Afterlife

na <- c(435, 147, 375, 134)
belief <- c("Yes","No","Yes","No")
gender <- c("Female","Female","Male","Male")
afterlife <- data.frame(n=na, belief=belief, gender=gender)

afterlife.tab <- matrix(na, ncol=2, byrow=F, 
  dimnames=list(belief=unique(belief), gender=c("Female","Male")))

afterlife.int <- glm(n ~ belief*gender, family=poisson(), data=afterlife)

summary(afterlife.int)
anova(afterlife.int, test="Chisq")

afterlife.ind <- glm(n ~ belief + gender, family=poisson(), data=afterlife)

after.fitted.ind <- matrix(fitted(afterlife.ind), ncol=2, byrow=F, 
  dimnames=list(belief=unique(belief), gender=c("Female","Male")))
  
after.devres.ind <- matrix(resid(afterlife.ind), ncol=2, byrow=F, 
  dimnames=list(belief=unique(belief), gender=c("Female","Male")))

after.pres.ind <- matrix(resid(afterlife.ind, type='pearson'), ncol=2, byrow=F, 
  dimnames=list(belief=unique(belief), gender=c("Female","Male")))

cbind(afterlife.tab,after.fitted.ind)
print(cbind(after.devres.ind,after.pres.ind),digits=2)



agree <- matrix(na,ncol=2,byrow=T)
gen <- c("Female","Male")

gen <- 1:2

afterlife.logit <- glm(agree ~ gen, family=binomial())
summary(afterlife.logit)
anova(afterlife.logit, test="Chisq")
