##########################################################
## fit a model to the Prussian horsekick data
##
## simon jackman, dept of political science,
## april 2003
## updated may 2007
##########################################################

library(pscl)   ## Jackman's R package
data(prussian)

p1 <- glm(y ~ 1, family="poisson",
          data=prussian, trace=TRUE)
require(MASS)
p1nb <- glm.nb(y ~ 1,data=prussian)

## compare log-likelihoods of two models
odTest(p1nb)

p2 <- glm(y ~ -1 + corp, family="poisson")  ## fixed effects for Corp
p3 <- glm(y ~ year, family="poisson")       ## linear time trend
p4 <- glm(y ~ -1 + as.factor(year),family="poisson") ## fixed effects for year

p5 <- glm(y ~ -1 + corp + as.factor(year),
          family="poisson")   ## fixed effects for year AND Corp
