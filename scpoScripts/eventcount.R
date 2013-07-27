## Event Count models in R ##

library(foreign)

CountData<- na.omit(read.dta("C:\\courses\\ps207\\week4\\week4.dta",  convert.factors=FALSE))



# Poisson model #

counteqn1 <- glm(jumps2564_m  ~ spcorr_all  + whitepct2564m + amindpct2564m + avgunemp + avgurban90 + totbexp + offset(log(totpop2564m)), family=poisson(), data=CountData, na.action=na.exclude)

install.packages("sandwich")    # for robust standard errors
library(sandwich)   

install.packqages("lmtest")
library(lmtest)

coeftest(counteqn1, vcov=sandwich)


library(MASS)

# Negative binomial model #

counteqn2 <- glm.nb(jumps2564_m  ~ spcorr_all  + whitepct2564m + amindpct2564m + avgunemp + avgurban90 + totbexp + offset(log(totpop2564m)), data=CountData, na.action=na.exclude)
coeftest(counteqn2, vcov=sandwich)

## Test for overdispersion -- added October 20

install.packages("pscl")
library(pscl)

odTest(counteqn2)



IRR <- exp(counteqn2$coefficients)
IRR

## predicted counts ##
# adding constant, coefficient for exposure #

coeffs <- c(counteqn2$coefficients,1)
meanvars <- c(1,colMeans(subset(CountData, select = c(spcorr_all,whitepct2564m,amindpct2564m,avgunemp,avgurban90, totbexp,totpop2564m))))
meanvars[8] <- log(meanvars[8])

# bridge = 0

meanvars[7] <- 0

predb0 <- exp(sum(meanvars * coeffs))

# bridge = 1

meanvars[7] <- 1

predb1 <- exp(sum(meanvars * coeffs))

predb0
predb1

################


CountData2 <- na.omit(read.dta("C:\\courses\\ps207\\week4\\biochem.dta",  convert.factors=FALSE))

# Zero-inflated models #

counteqn3 <- zeroinfl(articles ~ female + married + kid5 + phdprestige + mentor3 | mentor3, dist="poisson", link="logit", data=CountData2)
summary(counteqn3)

counteqn4 <- zeroinfl(articles ~ female + married + kid5 + phdprestige + mentor3 | mentor3, dist="negbin", link="logit", data=CountData2)
summary(counteqn4)

