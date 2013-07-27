
library(car) 	# contains Anova()
library(MASS)	# contains snails data, fits glm.nb().
library(VGAM)	# contains vglm()

# Let's write the 2 datasets we're going to use to .csv files for use in SAS.
write.csv(InsectSprays,"PUT FILE PATH HERE/InsectSprays.csv",row.names=F)
write.csv(snails,"PUT FILE PATH HERE/snails.csv",row.names=F)

###

?glm

?InsectSprays

# Count data: try Poisson.

reg.spray <- glm(count~spray,data=InsectSprays,family=poisson(log))
summary(reg.spray)
Anova(reg.spray,type="III")

?snails 

# Make sure factor variable recognized as factor variables.
snails2 <- snails
snails2$Exposure <- as.factor(snails$Exposure)
snails2$Rel.Hum <- as.factor(snails$Rel.Hum)
snails2$Temp <- as.factor(snails$Temp)

# Grouped binary: try binomial.

reg.snails <- glm(cbind(Deaths,N-Deaths)~Species+Rel.Hum+Temp,data=snails2,family=binomial(logit))
summary(reg.snails)
Anova(reg.snails,type="III")

# Note: if not grouped code would look like this for Bernoulli:
# glm( Deaths~Species+Rel.Hum+Temp,data=snails2,family=binomial(logit))

# For overdispersed count data: use negative binomial.

?glm.nb
reg.spray.nb <- glm.nb(count~spray-1,data=InsectSprays)
summary(reg.spray.nb)
Anova(reg.spray.nb,type="III") 

# Could use quasi-Poisson, but I don't recommend it.
reg.spray.qp <- glm(count~spray-1,data=InsectSprays,family=quasipoisson(link="log"))
summary(reg.spray.qp)
Anova(reg.spray.qp,type="III")

# For overdispersed binary: use quasibinomial (like SAS).
reg.snails.qb <- glm(cbind(Deaths,N-Deaths)~Species+Rel.Hum+Temp,data=snails2,family=quasibinomial(logit))
summary(reg.snails.qb)
Anova(reg.snails.qb,type="III")

# Or try fitting beta-binomial (I'm skipping this since I don't know enough about it)
reg.snails.bb <- vglm(cbind(Deaths,N-Deaths)~Species+Rel.Hum+Temp,data=snails2,family=betabinomial)
summary(reg.snails.bb)

# We are overcomplicating things. Just fit different X variables.
reg.snails2 <- glm(cbind(Deaths,N-Deaths)~Species+Exposure,data=snails2,family=binomial(link=logit))
summary(reg.snails2)
Anova(reg.snails2,type="III")

# However there still might be issues with too many zeros, i.e. an inflated number of zeros.
hist(snails$Deaths/snails$N)

# Or try zero-inflated binomial (skipping this since I need to know more about it)
reg.snails.zib <- vglm(cbind(Deaths,N-Deaths)~Species+Rel.Hum+Temp,data=snails2,family=zibinomial)
summary(reg.snails.zib)

