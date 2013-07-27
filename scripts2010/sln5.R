##### Problem 1 ######

beef<-read.csv("beef.csv")
YES <- beef$YES
SIZE <- beef$SIZE
VAL <- beef$VAL

## (i)
plot(beef)
#  Check out the multicolinearity! This will lead to unstable estimates of the
#  independent size and log(val) effects (and, infact, make log(val) redundant).

## (ii)
reg <- lm(YES ~ SIZE + log(VAL)) # straightforward MLR
print(summary(reg))
# We see clear evidence of non-constant variance
plot(reg$fitted.values, reg$residuals, pch=20)

## (iii)
# Two possible regression models of interest:
reg1 <- lm(YES ~ SIZE) # simpler model with just size
print(summary(reg1))
reg2 <- lm(YES ~ SIZE*log(VAL)) # a size effect that depends on log(VAL)
print(summary(reg2))
# The interaction effect is significant.
# The residuals still look a bit sketchy...
plot(reg2$fitted.values,  reg2$residuals, pch=20)
# Thus: 
print(paste("The marginal effect of SIZE is ",
            round(summary(reg2)$coefficients[2,1],2), "+",
            round(summary(reg2)$coefficients[4,1],2), "*log(VAL)."))


##### Problem 2 #####

traffic <- read.csv("WSPTrafficStops.csv")
Stops <- traffic$Stops
Radar <- traffic$Radar

# (i)
par(mfrow=c(2,3))
plot(Radar, Stops, pch=20, main="Stops vs Radar") # obvious non-constant variance
##
# Clearly, this is a candidate for the log-log transform
radarlm <- lm(log(Stops) ~ log(Radar))
plot(log(Stops), log(Radar), pch=20, col=4, main="log(stops) vs log(radar)")
# The relationship appears linear, which fits with our proposition that
# Radar is providing a baseline for the "at risk of traffic stop" population.
plot(radarlm$fitted.values, radarlm$residuals, pch=20, col=4,
     xlab="Yhat", ylab = "e", main="fitted vs residuals") # things look much better
qqnorm(rstudent(radarlm), pch=20, col=4) # check normality
abline(a=0, b=1, lty=2)
hist(rstudent(radarlm), col=4)
# We can also check error independence for the (spatially ordered) APAs
plot(traffic$APA, radarlm$residuals, pch=20, col=4,
     xlab="APA", ylab = "e", main="Error by APA")
# It looks like we fail this assumption (perhaps lower numbers are urban,
# closer to seattle?), but that is a more advanced topic (time-series lecture).
##
# From the elasticity interpretation of beta1,
# the increased % in active Stops per 1% more radar stops is
confint(radarlm)[2,]
# An elasticity of 1 seems like a decent assumption, which is intuitive

# (ii)
# First, plot the data with colors to show race.
# Use relevel() to make white drivers the intercept for discrimination
par(mfrow=c(1,3))
Race <- relevel(traffic$Race, "White")
plot(log(Radar), log(Stops), pch=20, col=Race)
legend("topleft", legend=levels(Race), fill=1:6)
# We want to see if there are race effects beyond
# the number of stops predicted by our Radar baseline
summary(racelm <- lm(log(Stops) ~ log(Radar) + Race))
anova(radarlm, racelm) # Race Matters!
# Residuals still look fine 
plot(racelm$fitted.values, racelm$residuals, pch=20, col=Race,
     xlab="Yhat", ylab = "e", main="fitted vs residuals")
# To see what we fit, take a look at the prediction lines for each race:
plot(log(Radar), log(Stops), col=8, cex=.5)
abline(a=racelm$coef[1], b=racelm$coef[2])
for(i in 1:5){ abline(a=racelm$coef[1]+racelm$coef[2+i], b=racelm$coef[2], col=i+1) }
# The lines look basically the same, but it is hard to really tell...
# Use the partial F-test to see if the "racial bias" effect is significant
# H0: log(Stops) ~ log(Radar)
# H1: log(Stops) ~ log(Radar) + Race
anova(radarlm, racelm)
# So the race effect is significant at .05 level (even though
# none of the individual t-statistics are signif).
#
# In the plot, only Native Americans systymatically differed from the line.
# But in the regression ouput, Hispanics also looked fairly significant.
# So lets try with both
NatAm <- Race=="NativeAmerican"
Hisp <- Race=="Hispanic"
summary( biaslm <- lm(log(Stops) ~ log(Radar) +  NatAm + Hisp ) )
# Compare this to the previous model, to see if the "racial bias"
# only affects Native Americans, or if we need the full model
# H0: log(Stops) ~ log(Radar) + NatAm + Hisp
# H1: log(Stops) ~ log(Radar) + Race
anova(biaslm, racelm)
## Looks like the native american and hispanic regression (null H0) explains things
## We can't really assume discrimination, however,
## since those residuals look to be correlated in ADA...
## Elasticity is still about 1.

xx <- 1:10
par(mfrow=c(1,3))
plot(log(Radar)[Hisp], log(Stops)[Hisp], col=5, main="Hispanic Drivers")
lines(xx, .5795 + 1.01589*xx + .15057, col=5, lwd=2)
plot(log(Radar)[NatAm], log(Stops)[NatAm], col=6, main="Native American Drivers")
lines(xx, .5795 + 1.01589*xx + .24111, col=6, lwd=2)
plot(log(Radar)[!(Hisp|NatAm)], log(Stops)[!(Hisp|NatAm)],  main="Other Drivers")
lines(xx, .5795 + 1.01589*xx, col=1, lwd=2)
