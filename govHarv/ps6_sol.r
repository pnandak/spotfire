## Gov 2000
## Problem set 6 solutions

######################
## Problem 1
######################

## We know the data generating process, so we can generate one sample
 
## We make a vector of years

years <- 1976:2008

## And draw a vector of errors

set.seed(12345)
errors <- rnorm(length(years), mean = 0, sd = .1384)

## So our simulated temperatures are:

y.sim <- -14.8404 + .015148*years + errors

## Now we regress simulated temperature on year

out <- lm(y.sim ~ years)

## And create a table

library(xtable)
xtable(out)

## Now we plot the sample and add the regression line and the true
## regression line

pdf(file = "ps6_plot1.pdf")

plot(x = years, y = y.sim, main = "Global mean temperatures", 
 ylab = "Simulated Temperature", xlab = "Year")

abline(out, col = "red")
abline(a = -14.8404, b = .015148, lty = "dashed", col = "blue")
dev.off()

## Now we simulate 1000 datasets from the null distribution, regress
## the simulated temperatures on year, store the vector of slopes,
## and determine the proportion of the simulated slopes that are 
## larger in magnitude than our estimated slope coefficient (-22.23)

sims <- 1000
slopes.sim <- c()

for (i in 1:sims){
  
  years <- 1976:2008
  errors.sim <- rnorm(length(years), mean = 0, sd = .1384)
  temp.sim <- -14.8404 + errors.sim

  out.sim <- lm(temp.sim ~ years)
  slopes.sim[i] <- coef(out.sim)["years"]
}

## The proportion of slopes larger in magnitude than our estimated slopes
## is

prop <- sum(abs(slopes.sim) >= abs(coef(out)["years"]) )/length(slopes.sim)  
prop #returns 0



#########################
## Problem 2
#########################

## First we load the data
load("ps6_data.RData")


## We regress democracy on literacy

out.lit <- lm(Democracy ~ Literacy, data = Democ)
xtable(out.lit)

## find the 95% confidence interval

confint(out.lit)

## And democracy on GDPPC

out.gdp <- lm(Democracy ~ GDPPC, data = Democ)
xtable(out.gdp)

## find the 95% confidence interval

confint(out.gdp)

## Let's make an added variable plot to investigate the relationship
## between literacy and democracy, controlling for GDP per capita

## First we remove the influence of gdppc from literacy

out.av1 <- lm(Literacy ~ GDPPC, data = Democ)

## And store the residuals

resids.lit <- residuals(out.av1)

## And repeat for democracy

out.av2 <- lm(Democracy ~ GDPPC, data = Democ)
resids.democ <- residuals(out.av2)

## Now we regress the second set of residuals on the first

out.av <- lm(resids.democ ~ resids.lit)

## And create a plot

pdf(file = "ps6_plot2.pdf")

plot(x = resids.lit, y = resids.democ, main = "Added Variable Plot", 
  ylab = "Residuals from literacy on gdppc", xlab = "Residuals from democracy on gdppc")
abline(out.av, col = "purple")

dev.off()

## Now we run the same regression with lm()

out.mult <- lm(Democracy ~ Literacy + GDPPC, data = Democ)
xtable(out.mult)

## And we find the 95% confidence intervals

confint(out.mult)

## We know that high collinearity between independent variables
## inflates standard errors.  Let's have a look:

cor(Democ$Literacy, Democ$GDPPC)
## wow, a correlation of .97.  

## In the formula for standard errors that includes a variance
## inflation factor, we would plug in the R^2 from this regression:

summary(lm(Literacy ~ GDPPC, data = Democ))
## Which is .94, very high

pdf(file = "ps6_plot3.pdf")
plot(x = Democ$Literacy, y = Democ$GDPPC, main = "collinearity")
dev.off()











