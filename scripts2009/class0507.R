## nick ayers' country year data
data <- read.csv("CountryYear.csv",
                 header=TRUE)
data$y <- as.numeric(substring(data$Year,2))
data$postIraq <- data$y > 2002
data$IraqpostIraq <- data$y > 2002 & data$Country == "Iraq"

library(car)
library(MASS)
library(pscl)

### look at the data
attach(data)
table(SA)
table(Country)
table(SA,Country)

## CL is repression code (freedom house, 1 thru 7)
## POL is Polity
## CB conventional bombings
## nafrme - North Africa and Middle East dummy var

boxplot(SA ~ CL,data=data)
boxplot(log(1+SA) ~ Year,data=data)

plot(SA ~ CB,data=data)
## label cases interactively
identify(data$CB,
         data$SA,
         paste(data$Country,data$y))
## cntrl-option mouse click to bail

y <- jitter(log(1+data$SA))
x <- jitter(log(1+data$CB))
plot(x,y)
## label cases interactively
identify(x,y,
         paste(data$Country,data$y))
## cntrl-option mouse click to bail

## simple Poisson model
mod1 <- glm(SA ~ CL + POL + CB + log(GDP) + y,
            data=data,
            family=poisson)

## negative binomial
mod1a <- glm.nb(SA ~ CL + POL + CB + log(GDP) + postIraq + nafrme,
                control=glm.control(maxit=1e3,eps=1e-14,trace=TRUE),
                data=data)
odTest(mod1a)

## look at predicted counts
newdata <- expand.grid(POL=0:20,
                       CB=5,
                       GDP=1604,
                       CL=5,
                       postIraq=FALSE,
                       nafrme=0)
yhat <- predict(mod1a,newdata,type="response",se.fit=TRUE)

###########################################################################
## Zero Inflate
zinb <- zeroinfl(SA~CL+POL+log(CB+1)+log(GDP)+nafrme + postIraq | CL+POL+log(CB+1)+log(GDP),
                  dist="negbin",
                 data=data,
                 EM=TRUE,
                 control=zeroinfl.control(trace=TRUE))
summary(zinb)

## predicted values
phat <- predict(zinb,type="response")
boxplot(phat ~ zinb$y)

## look at predictions for a user-chosen country over time
## versus actual
countryLook <- function(theCountry){
  tmp <- cbind(phat[data$Country==theCountry],
               data$SA[data$Country==theCountry],
               data$y[data$Country==theCountry])
  ## sort data for moron countries not sorted by year
  indx <- order(tmp[,3])
  tmp <- tmp[indx,]
  
  plot(y=tmp[,2],x=tmp[,3],
       type="b",
       xlab="Year",
       ylab="Number of Suicide Bombings",
       ylim=range(tmp[,1],tmp[,2]))
  lines(y=tmp[,1],x=tmp[,3],type="b",col="blue")
  legend(x="topleft",
         bty="n",
         col=c("black","blue"),
         lwd=2,lty=1,
         legend=c("Actual","Predicted"))
  title(theCountry)
  invisible(NULL)
}
countryLook("Israel")
countryLook("Iraq")
countryLook("Sri Lanka")
countryLook("France")
countryLook("Armenia")


##Basic Logit for Any SA in a given Year
LModel2=glm(SAZ~CL+POL+CB+Punishment+PercIslam+log(GDP)+nafrme,data=data2,family=binomial)
summary(LModel2)
