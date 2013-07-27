## Heckman selection models in R ##

library(foreign)

SelData1 <- read.dta("C:\\courses\\ps207\\week2\\gssheckman.dta",  convert.factors=FALSE)

## Outcome equation without correction for selection ##

outeqn1 <- lm(polviews ~ partyid + region + union + age + educ + sex + income + relig, data=SelData1)
summary(outeqn1)

##  Correcting for selection ##

install.packages("sampleSelection")
library(sampleSelection)

selmodel1 <- selection(polvmiss ~ educ + sex + race + income + relig,  polviews ~ partyid + region + union + age + educ + sex + income + relig, data=SelData1, method="ml")
summary(selmodel1)

## Heckman model "by hand" ##
## Selection equation  -- the "na.exclude" command makes the IMR number of rows match the data ##

seleqn1 <- glm(polvmiss ~ educ + sex + race + income + relig, family=binomial(link="probit"), data=SelData1, na.action=na.exclude)
summary(seleqn1)

## Calculate inverse Mills ratio by hand ##

yhat <- qnorm(fitted(seleqn1))
SelData1$IMR <- dnorm(yhat)/pnorm(yhat)

## Outcome equation correcting for selection ##

outeqn2 <- lm(polviews ~ partyid + region + union + age + educ + sex + income + relig + IMR, data=SelData1)
summary(outeqn2)

## Note standard errors will not be correct.  One option is to redo the model with a bootstrap to correct.

BScorr <- matrix(0,100,10)

for (i in 1:100) {
   s <- SelData1[sample(nrow(SelData1), replace=TRUE),]
   seleqnBS <- glm(polvmiss ~ educ + sex + race + income + relig, family=binomial(link="probit"), data=s, na.action=na.exclude)
   yhatBS <- qnorm(fitted(seleqnBS))
   s$IMR <- dnorm(yhatBS)/pnorm(yhatBS)
   outeqnBS <- lm(polviews ~ partyid + region + union + age + educ + sex + income + relig + IMR, data=s)
   BScorr[i,] <- outeqnBS$coefficients
 }
 
 BScoeff <- apply(BScorr,2,mean)
 BSse <- apply(BScorr,2,sd)
 BSz <- BScoeff/BSse
 BSp <- 2*(1 - pnorm(abs(BSz)))
 BS <- cbind(BScoeff, BSse, BSz, BSp)

 colnames(BS) <- c("Coefficient","Std. Error")
 rownames(BS) <- c("Intercept", "partyid", "region", "union", "age", "educ", "sex", "income", "relig", "IMR")
 BS
 
