## panel data demos
library(Ecdat)
library(plm)    ## panel linear models

## from the plm vignette
data(Produc)

boxplot(log(gsp) ~ state,    ## x-sectional variation
        data=Produc)

library(lattice)
xyplot(log(gsp) ~ year | state,
       data=Produc)

boxplot(log(gsp) ~ year,
        data=Produc)    ## longitudinal variation

## overlay state-specific time trends on one another
plot(log(gsp) ~ year,
     type="n",
     data=Produc)
par(xpd=TRUE)
by(Produc,state,
   function(x){
     lines(x=x$year,
           y=log(x$gsp))
     text(x=x$year[1],
          y=log(x$gsp)[1],
          x$state,
          adj=1,
          cex=.75)
   }
   )

## how much variation is cross-sectional?
## answer: r^2 from the following regression
feState <- lm(log(gsp)~state,
              data=Produc)

## how much variation is longitudinal?
## answer: r^2 from the following regression
feTime <- lm(log(gsp) ~ as.factor(year),
             data=Produc)


## fixed effects for both state and time
## do not totally exhaust all variation in the data
## i.e., the r^2 from the following regression is not
## exactly 1.0
feBoth <- lm(log(gsp) ~ state + as.factor(year),
             data=Produc)

e <- resid(feBoth)
boxplot(e ~ state,
        data=Produc)
