##################################################################################
## Jim Fearon data on civil war
##
## simon jackman, dept of political science, stanford univ
## may 2003
## may 2005
## may 2007
##################################################################################
## notes on the data from Jim Fearon:
##
## ccode (num country ccode)
## casename (string, name of war)
## cname (string of country name)
## year (year of civ war onset)
## durest (duration of war in years)
## ended (1 for wars that have ended by 2000, 0 for right censored cases)
##
## hence, s <- Surv(durest, ended)
##
## covariates:
##
## couprev (civ war begun by competing units of the armed forces,
##          or by popular revolution)
## eeurop (1 for eastern european cases)
## decol ( a war of decolonization)
## sos ("sons of soil" case, small indigenous minority on periphery of
##       state fighting farmers migrating in who belong to majority ethnic group)
## drugs (rebels get significant financing from contraband)
##
## Other possible covar's of interest:
## emgdpl (lagged gdp measured in year before "year" above)
## empop (total country pop in year of war start)
###################################################################################
library(foreign)
civilwar <- read.dta(file="civilwar.dta")   ## import Stata to R
attach(civilwar)

## soak and poke
table(couprev)
table(eeurop)
table(decol)
table(sos)
table(drugs)

boxplot(durest ~ couprev)
boxplot(durest ~ eeurop)
boxplot(durest ~ decol)
boxplot(durest ~ sos)
boxplot(durest ~ drugs)

## set up for survival
library(survival)
s <- Surv(durest,ended)   ## set up for survreg,
                          ## feeding lifetimes and right-censoring dummy

## look at some Kaplan-Meier curves
par(las=1)
sos.fit <- survfit(s ~ sos)
plot(sos.fit,
     yaxs="i",
     ylim=c(0,1.02),
     xlab="Duration of Civil War (Years)",
     ylab="survival",
     col=c("blue","red"),
     log=FALSE)
title("Kaplan-Meier Estimates, SOS vs non-SOS")
abline(h=.5,lty=1)
legend(x="topright",
       lty=rep(1,2),
       bty="n",
       col=c("blue","red"))

## couprev KM estimtes
couprev.fit <- survfit(s ~ couprev)
plot(couprev.fit,
     conf.int=FALSE,
     yaxs="i",
     ylim=c(0,1),
     xlab="Duration of Civil War (Years)",
     ylab="survival",
     col=c("blue","red"),
     log=F)
legend(x="topright",
       bty="n",
       lty=c(1,1),
       legend=c("Onset not by Coup/Revolution","Onset via Coup/Revolution"),
       col=c("blue","red"))
title("Kaplan-Meier Estimates, Onset via Coup/Revolution")
abline(h=.5,lty=1)

## lets fit some parametric survival models
s <- Surv(durest,ended)   ## set up for survreg,
                          ## feeding lifetimes and right-censoring dummy

model1 <- survreg(s ~ couprev + eeurop + decol + sos + drugs,
                  y=T,   ## store dep var with missings dumped in model object
                  dist="weibull")
model1e <- survreg(s ~ couprev + eeurop + decol + sos + drugs,
                   dist="exponential")
anova(model1e,model1)     ## compare restricted (exp) against unrestrict (weib)

## plot actual against predicted
yhat <- log(predict(model1))
plot(model1$y[,1] ~ yhat, ## get actual from fitted model (set y=T in model1)
     pch=model1$y[,2],    ## plotting symbol indicates censored or not
     xlab="Predicted Years (Log-Scale)",
     ylab="Actual Years (Log-Scale)",
     axes=F,
     xlim=c(log(1),log(100)),
     ylim=c(log(1),log(100)))
legend(0,4,
       legend=c("Censored","Not Censored"),
       pch=c(0,1))
axis(1,at=log(c(1,2,5,10,25,50,100)),
     c("1","2","5","10","25","50","100"))
axis(2,at=log(c(1,2,5,10,25,50,100)),
     c("1","2","5","10","25","50","100"))
abline(0,1)
box()

## a function for showing hazards
hfunc <- function(object,newdata,tseq=1:100,plot=T,save=F){
  if(class(object)!="survreg")
    stop("Error: model is not of class survreg")

  if(object$dist=="weibull"){
    yhat <- predict(object,newdata=newdata,type="lp")
    alpha <- 1/object$scale
    n <- length(yhat)
    m <- length(tseq)
    y <- matrix(NA,n,m)
    for(i in 1:n)
      y[i,] <- alpha*(tseq^(alpha-1)) * exp(alpha*yhat[i])
  }

  if(object$dist=="exponential"){
    yhat <- predict(object,newdata=newdata,type="lp")
    n <- length(yhat)
    m <- length(tseq)
    y <- matrix(NA,n,m)
    for(i in 1:n)
      y[i,] <- exp(yhat[i])
  }

  if(plot){
    plot(y[1,]~tseq,type="n",
         ylim=range(y),
         xlab="Time",ylab="Hazard")
    for(i in 1:n)
      lines(tseq,y[i,],lty=i)
  }
  
  if(save)
    y
  else
    invisible(NULL)
}

newdata <- expand.grid(list(couprev=0,eeurop=0,decol=0,sos=c(0,1),drugs=0))
hfunc(model1,
      newdata=newdata,
      tseq=seq(from=min(civilwar$durest),to=max(civilwar$durest),length=100))

legend(60,12,
       legend=c("SOS","Non-SOS"),
       lty=c(1,2))
title("Estimated Hazard Functions")

## predictions of median duration, with confidence bounds
yhat <- predict(model1,
                type="quantile",
                p=c(.25,.50,.75))

## quick and dirty, run regression on log survival time
## do this at home, not for publication (ignores right-censoring)
reg1 <- lm(log(durest) ~ couprev + eeurop + decol + sos + drugs,y=T)
plot(predict(reg1)~reg1$y)
abline(0,1)

##############################################################################
## Cox proportional hazards models
##############################################################################
cox1 <- coxph(Surv(durest,ended) ~ couprev + eeurop + decol + sos + drugs,
              data=civilwar)

## survfit(cox1) then plot, gives survivor curve for the mean subject, T&G, p270.
surv1 <- survfit(cox1)
plot(surv1,lwd=3)
surv2 <- survfit(Surv(durest,ended) ~ 1, data=civilwar)  ## Kaplan-Meier
lines(surv2,lty=1,mark.time=F)


## do cluster(id), save this discussion for panel weeks
