##################################################
## R demo of survival models
## 
## simon jackman, dept of polisci, su
## june 2005
##################################################

library(survival)     ## load the Therneau survival 
library(MASS)         ## load Venables & Ripley for data sets

## leukemia data from MASS, no survivors
plot(survfit(Surv(time) ~ ag,
             data = leuk),
     xlab="Weeks Since Diagnosis",
     lty = 2:3, col = 2:3,
     legend.text=levels(leuk$ag))

## if the log survivor plot linear?
## if so, consider exponential...
plot(survfit(Surv(time) ~ ag,
             data = leuk),
     log=T,
     xlab="Weeks Since Diagnosis",
     lty = 2:3, col = 2:3,
     legend.text=levels(leuk$ag))


expModel <- survreg(Surv(time) ~ ag*log(wbc),
                    data=leuk,
                    dist="exponential")

weibullModel <- survreg(Surv(time) ~ ag*log(wbc),
                        data=leuk,
                        dist="weibull")  ## weibull is default

## weibullModel has no significant improvement over exponential
anova(expModel,weibullModel)

loglogModel <- survreg(Surv(time) ~ ag*log(wbc),
                       data=leuk,
                       dist="loglogistic")

## different scale parameters, \sigma, for each group
## via strata argument
summary(survreg(Surv(time) ~ strata(ag) + log(wbc),
                data=leuk))

##########################################################
## gehan data, has right censoring
#########################################################
modelExp <- survreg(Surv(time,cens) ~ factor(pair) + treat,
                    data=gehan,
                    dist="exponential")

## drop fixed effects for pairs
modelExp2 <- survreg(Surv(time,cens) ~ treat,
                    data=gehan,
                    dist="exponential")

## weibull model
modelWei <- survreg(Surv(time,cens) ~ treat,
                    data=gehan)

anova(modelExp2,modelWei)

###################################################################
## motors data, p364, MASS
plot(survfit(Surv(time,cens) ~ factor(temp),
             data=motors),
     conf.int=F)
motor.wei <- survreg(Surv(time,cens) ~ temp,
                     data=motors)
summary(motor.wei)

## predicted mean survival times, conditional on temp, with se
predict(motor.wei,
        list(temp=130),
        se.fit=T)

## predicted quantiles of survival time distribution
## n.b., median survival time is different from expected value
predict(motor.wei,
        list(temp=130),
        type="quantile",
        p=c(.05,.10,.50,.90,.95))

## confidence intervals on median survival time
halfLife <- predict(motor.wei,
                    list(temp=130),
                    type="uquantile",  ## underlying log-time scale
                    p=c(.05,.10,.50,.90,.95),
                    se=T)
exp(cbind(LB=halfLife$fit - 1.96*halfLife$se,    ## lower bound
          UB=halfLife$fit + 1.96*halfLife$se))   ## upper bound

## cox models
tmp <- leuk
tmp$agDummy <- as.numeric(tmp$ag=="present")
leuk.cox <- coxph(Surv(time) ~ agDummy + log(wbc),
                  data=tmp)
summary(leuk.cox)

## some predicted survivor functions
par(mfrow=c(2,1))
newdata <- data.frame(agDummy=1,
                      wbc=mean(leuk$wbc))
sfit <- survfit(leuk.cox,
                newdata=newdata)
plot(sfit,xlab="Weeks Since Diagnosis",ylab="Expected Survival")
abline(h=.5)
title("Predicted Survival Function for AG=Present, mean WBC")

newdata <- data.frame(agDummy=0,
                      wbc=mean(leuk$wbc))
sfit <- survfit(leuk.cox,
                newdata=newdata)
plot(sfit,xlab="Weeks Since Diagnosis",ylab="Expected Survival")
title("Predicted Survival Function for AG=Absent, mean WBC")

## stratify for different baseline hazards 
leuk.coxs <- coxph(Surv(time) ~ strata(ag) + log(wbc),
                   data=leuk)

## and then have different effect for log(wbc), by ag
leuk.coxs1 <- coxph(Surv(time) ~ strata(ag) + log(wbc) + ag:log(wbc),
                    data=leuk)

## we prefer leuk.coxs model
## plot predicted survivor functions against Kaplan-Meier estimate
## heavier line is from Cox model
plot(survfit(Surv(time) ~ ag,
             data=leuk),
     lty=2:3,log=T)
lines(survfit(leuk.coxs),lty=2:3,lwd=3)
legend(80,0.8,
       c("ag absent","ag present"),
       lty=2:3)

             
