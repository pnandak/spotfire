##
##  section7.R - interaction term goodness (msot code by mb)
##  oct 29, 2009
##  discrete case
##  note that the continous case is the same thing, but
##  you use continous var instead of a discrete var.

## the end result there is that you fit a lot of different models
## as opposed to just two.

## note: i'm not going to go over this in class, really.
## this is stuff you've had practice on already.

#############################################################

## generate some data (ignore this)
beers <- rpois(n=100,3)
water <- rbinom(n=100, 1,.4)
hangovers <- 4 + .5*beers - .4*water + (-.55)*beers*water + rnorm(100,0,.5)
 	## hangover length

## Run some OLS regressions

alcohol.model <- lm(hangovers ~ beers)
summary(alcohol.model)

water.model <- lm(hangovers ~ beers + water)
summary(water.model)

## Let's say that we have some reason to think that there 
## is an interaction between beers and water. We can specify
## the interaction model in R in two ways.

## First is the colon method ":". You can add an interaction
## term by including var1:var2 in the regression model.
## Note that you have to manually add the lower-order terms
## (var1 and var2) seperately. 

int.model <- lm(hangovers ~ beers + water + beers:water)
summary(int.model)

## Second is the asterick method. You can add an interaction
## term by including var1*var2 in the lm function call. This
## will add the interaction term and all of the lower order terms
## as well. 

int.model2 <- lm(hangovers ~ beers*water)
summary(int.model2)

## Now we'll do some plotting with these regressions to 
## demonstrate the interaction between the two variables

plot(x=beers, y=hangovers)

## ok, but this needs some labels

plot(x=beers, y=hangovers, xlab="beers consumed", ylab="length of hangovers",
     main="self-study of alcohol consumption")

## hm, well, we still don't know which points are water-drinking nights
## and which are not water drinking nights. so let's separate them by
## color:
     
plot(x=beers[water==0], y=hangovers[water==0], xlab="beers consumed", ylab="length of hangovers",
     main="self-study of alcohol consumption", col="orange")
points(x=beers[water==1], y=hangovers[water==1], col="olivedrab")     

## ok, awesome, but that's a little hard to see, especially if 
## someone prints this in b&w. let's add different plotting characters
## with the "pch" option:

plot(x=beers[water==0], y=hangovers[water==0], xlab="beers consumed", ylab="length of hangovers",
     main="self-study of alcohol consumption", col="orange", pch=19)
points(x=beers[water==1], y=hangovers[water==1], col="olivedrab", pch=17)  


## hm, i think the bottom of the graph is being cut off because
## we're plotting the no-water points first. we need to specify
## the range of y-axis. in order to do this smartly, we can 
## use the range() function:

range(hangovers)

## this gives us the vector c(min(hangovers), max(hangovers)). If we 
## put this in the "ylim" option in the plot() commmand, the window 
## will contain the full range of hangovers:

plot(x=beers[water==0], y=hangovers[water==0], xlab="beers consumed", ylab="length of hangovers",
     main="self-study of alcohol consumption", col="orange", pch=19,
     ylim=range(hangovers))
points(x=beers[water==1], y=hangovers[water==1], col="olivedrab", pch=17) 

## now that we have nicely plotted all the points, we need to add 
## the regression lines. remember our model:

summary(int.model)

## we're going to have to compute the slopes using the coefficients
## from the model, but we should do this in a smart way, without
## using hard-coded numbers. for this, we'll use the coef() function,
## which we can run on the output from an lm() call: 

coef(int.model)
coef(int.model)["(Intercept)"]
coef(int.model)["beers:water"]

## thus, if water == 0, we can easily draw the line:

abline(a=coef(int.model)["(Intercept)"], 
       b=coef(int.model)["beers"])
       
## if water==1, then the slope is slightly harder, so i'll break
## it up into 2 steps:

h2o.slope <- coef(int.model)["beers"] + coef(int.model)["beers:water"]
h2o.int <- coef(int.model)["(Intercept)"] + coef(int.model)["water"]

## now we'll plot that line:

abline(a=h2o.int, b=h2o.slope)

## whew, that was hard, but now we have a problem with the 
## color and style of the lines. someone reading in b&w
## won't be able to tell the difference between them. so,
## we'll use the "col" and "lyt" (line type) options in
## abline():


plot(x=beers[water==0], y=hangovers[water==0], xlab="beers consumed", ylab="length of hangovers",
     main="self-study of alcohol consumption", col="orange", pch=19,
     ylim=range(hangovers))
points(x=beers[water==1], y=hangovers[water==1], col="olivedrab", pch=17)
abline(a=coef(int.model)["(Intercept)"], 
       b=coef(int.model)["beers"], col="orange", lty=1)
abline(a=h2o.int, b=h2o.slope, col="olivedrab", lty=2)

## you can also make a nice legend: 

legend(x="topleft", legend=c("water before bed", "no water"), 
       col=c("olivedrab","orange"), pch=c(17,19), lty=c(2,1), bty="n")


## maybe you're thinking that two plotting commands for the 
## points is too much. we can do it in one step. first, let's 
## review how R handles the plotting options. note that if pch
## and col are vectors, R matches the first element of pch to the
## first plotting point and so on.
       
plot(x=c(1,2,3,4), y=c(1,2,3,4), pch=c(15,16,17,18,19), col=c("red","blue"),
     cex=3)


## thus, we could use the "ifelse" command to create a vector that
## has the same size as the data and contains the plotting options
## for that observation. 

ifelse(water==1,"olivedrab","orange")
head(cbind(water,ifelse(water==1,"olivedrab","orange")))
head(cbind(beers,water,ifelse(water==1,"olivedrab","orange")))

## using these commands in the "col" and "pch" options will save us lines
## of code:

plot(x=beers, y=hangovers, xlab="beers consumed", ylab="length of hangovers",
     main="self-study of alcohol consumption", 
     col=ifelse(water==1,"olivedrab","orange"), 
     pch=ifelse(water==1,17,19))

