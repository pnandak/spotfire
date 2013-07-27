names(crabs)
# If you haven't already done so, convert weight to kilograms and
# the color scores to 1-4, instead of 2-5:
crabs$weight <- crabs$weight/1000
crabs$color <- crabs$color - 1
crabs$colfac <- factor(crabs$color) # Create a factor version of color
levels(crabs$colfac)  # Level "1" is "first"
# To make levels 1-3 get the dummy variables (like SAS), move
# level 4 to the front (make it the "reference level") using the
# relevel() function:
crabs$colfac <- relevel(crabs$colfac,ref="4")
levels(crabs$colfac)  # Now level "4" is "first"
# Fit the model for part (a)
crabs.pll.a <- glm(satell ~ colfac + weight, family=poisson(), data=crabs)
summary(crabs.pll.a)
# For part (b), prediction for medium light crabs (color=1):
predict(crabs.pll.a, newdata=data.frame(colfac="1",weight=2.44), type="response")
# Or by hand (showing you all the steps involved, but only the
# last one is really needed):
coef(crabs.pll.a)
coef(crabs.pll.a) * c(1,1,0,0,2.44)
sum(coef(crabs.pll.a) * c(1,1,0,0,2.44))
exp(sum(coef(crabs.pll.a) * c(1,1,0,0,2.44)))
# For part (b), dark crabs (color=4):
predict(crabs.pll.a, newdata=data.frame(colfac="4",weight=2.44), type="response")
exp(sum(coef(crabs.pll.a) * c(1,0,0,0,2.44)))
# Do the test for part (c):
# First fit the reduced model without color:
crabs.pll.c <- glm(satell ~ weight, family=poisson(), data=crabs)
anova(crabs.pll.c,crabs.pll.a,test="Chisq")
# Fit the model for part (d):
crabs.pll.d <- glm(satell ~ color + weight, family=poisson(), data=crabs)
summary(crabs.pll.d)
# For part (d), prediction for medium light crabs (color=1):
predict(crabs.pll.d, newdata=data.frame(color=1,weight=2.44), type="response")
# Or by hand:
coef(crabs.pll.d)
exp(sum(coef(crabs.pll.d) * c(1,1,2.44)))
# Dark crabs:
predict(crabs.pll.d, newdata=data.frame(color=4,weight=2.44), type="response")
exp(sum(coef(crabs.pll.d) * c(1,4,2.44)))
# Test for effect of color in model:
anova(crabs.pll.c,crabs.pll.d,test="Chisq")
# Compare to model of part (a):
anova(crabs.pll.d,crabs.pll.a,test="Chisq")
# For part (e):
crabs.pll.e <- glm(satell ~ color + weight + width, family=poisson(), data=crabs)
anova(crabs.pll.e,test="Chisq")
anova(crabs.pll.d,crabs.pll.e,test="Chisq")
cor(crabs$weight,crabs$width) # Correlation of weight and width
