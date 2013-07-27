#############################################################
## final exam code fragments
## ps350c 2007
#############################################################

## q3: look at the Marinov data used on final exam
data <- dget(file="marinovhw4.dpt")
library(survival)
attach(data)


## Q2a: KM plots for usdummy
## note that I do this two ways (one long, one fast, see below)

###############################################################
## slow way
###############################################################
## create a reduced version of the data set
## so as to grind out KM plots 

## find where we have breaks in the endyear series
n <- dim(data)[1]
spellbreak <- rep(FALSE,n)
endyear.lag <- c(NA,endyear[-n])
spellbreak[endyear != (endyear.lag + 1)] <- TRUE
lastObs <- c(spellbreak[-1],TRUE)

data$id <- 1 + cumsum(spellbreak)

## collapse data so we have only one obs per spell
## for KM plots
newdata <- data[lastObs,]
newdata$startyear <- 0
km2a <- survfit(Surv(startyear,endyear,ended) ~ usdummy,
               data=newdata)
plot(km2a,
     legend.text=c("Not US Sanction","US Sanction"),
     col=2:3)

##########################################################
## fast way KM via Cox model...!?!
## this won't show the censored obs, see the bug warning
## in help(plot.survfit), which I fixed below
##########################################################
cox2a <- coxph(Surv(startyear,endyear,ended) ~ strata(usdummy),
               data=data)
kmCox2a <- survfit(cox2a,type="kaplan-m")

## compare this with summary(km2a)
summary(kmCox2a)
summary(km2a)

plot(kmCox2a,
     conf.int=TRUE,
     legend.text=c("Not US Sanction","US Sanction"),
     mark.time=data$endyear[lastObs & data$ended==0],
     col=2:3)

## even faster, and the way we did it to start with...!
## the confusion here arises from the bug in plot.survfit...
km2a.simple <-  survfit(Surv(startyear,endyear,ended) ~ usdummy,
                        data=data)
summary(km2a.simple)
table(unclass(summary(km2a.simple)$surv) == unclass(summary(km2a)$surv))


###############################################################
## Q2c
## now for demtarg, we might patch missing data via soaking and poking
## for example
newdata$demtarg[9] <- 1
###############################################################


##########################################################
## Q2d Cox model
cox2d <- coxph(Surv(startyear,endyear,ended) ~ usdummy + demtarg + align + multilat + gdppctarg,
               data=data)

## look at survivor function
plot(survfit(cox2d),
     conf.int=TRUE)

## hazards
foo <- basehaz(cox2d)
plot(foo$time,
     foo$hazard)

##########################################################
## q3
##########################################################

##########################################################
## q5
##########################################################
require(glmmML)
require(MASS)
data(bacteria)
numy <- as.numeric(bacteria$y=="y")
mod <- glmmML(numy ~ trt + as.factor(week),
              cluster=ID,
              data=bacteria,
              family=binomial)
x <- rbind(
           c(1,0,0,0,0,0,0),  ## placebo week zero
           c(1,0,0,1,0,0,0),  ## placebo week two
           c(1,0,0,0,1,0,0),  ## placebo week four
           c(1,0,0,0,0,1,0),  ## placebo week six
           c(1,0,0,0,0,0,1),  ## placebo week eleven
           c(1,1,0,0,0,0,0),  ## trtdrug week zero
           c(1,1,0,1,0,0,0),  ## trtdrug week two
           c(1,1,0,0,1,0,0),  ## trtdrug week four
           c(1,1,0,0,0,1,0),  ## trtdrug week six
           c(1,1,0,0,0,0,1),  ## trtdrug week eleven
           c(1,0,1,0,0,0,0),  ## trtdrug+ week zero
           c(1,0,1,1,0,0,0),  ## trtdrug+ week two
           c(1,0,1,0,1,0,0),  ## trtdrug+ week four
           c(1,0,1,0,0,1,0),  ## trtdrug+ week six
           c(1,0,1,0,0,0,1)   ## trtdrug+ week eleven
           )
b <- coef(mod)
yhat <- x%*%b
phat <- 1/(1+exp(-yhat))

vc <- mod$variance[-8,-8]  ## vc matrix of beta
v <- x%*%vc%*%t(x)         ## vc of predictions
se <- sqrt(diag(v))        ## std errors

bounds <- cbind(yhat - 1.96*se,
                yhat + 1.96*se)
phatBounds <- 1/(1+exp(-bounds))


## a plotting function we will use
plotFunc <- function(x,y,bounds,xlab){
  print(x)
  print(y)
  print(bounds)
  plot(x,y,
       xlab=xlab,
       ylab="Probability",
       ylim=c(0,1),
       axes=FALSE)
  axis(2)
  segments(x0=x,
           x1=x,
           y0=bounds[,1],
           y1=bounds[,2])
  invisible(NULL)
}

## look by treatment
par(mfrow=c(3,1),
    las=1)

plotFunc(x=c(0,2,4,6,11),
         y=phat[1:5],
         phatBounds[1:5,],
         xlab="Week")
axis(1)
title("Placebo")

plotFunc(x=c(0,2,4,6,11),
         y=phat[6:10],
         phatBounds[6:10,],
         xlab="Week")
axis(1)
title("Treatment")

plotFunc(x=c(0,2,4,6,11),
         y=phat[11:15],
         phatBounds[11:15,],
         xlab="Week")
axis(1)
title("Treatment Plus")

## look by index
index <- matrix(1:15,5,3,byrow=FALSE)
par(mfrow=c(5,1),las=1)
week <- c(0,2,4,6,11)
for(j in 1:5){
  plotFunc(x=1:3,
           y=phat[index[j,],],
           bounds=phatBounds[index[j,],],
           xlab="")
  axis(1,
       at=1:3,
       labels=c("Placebo","Treatment","Treatment Plus"))
  title(paste("Week",week[j]))
}


                            

