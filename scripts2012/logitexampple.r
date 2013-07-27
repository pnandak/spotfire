The Raftery and Hout Irish education data can be obtained here as an S-PLUS/R data dump. To load it into R you can simply type source("irished.dmp") at the R command line.


# These examples were done in R 1.1.0 on an i686 running linux. I've
# checked most of the syntax. Everything should work under most
# versions of S-PLUS although the output is slightly different.
#
# I'll start by using the Irish education data from Raftery and Hout 1985.
# I already have a dataframe containing these data so I'll just attach
# it

> attach(irished)

# Now I'll fit a simple logistic regression model in which 
# the leaving certificate indicator is the response variable and
# the student's DVRT test result is the sole explanatory variable

> result1 <- glm(lvcert~DVRT, family=binomial(logit))
> summary(result1)

Call:
glm(formula = lvcert ~ DVRT, family = binomial(logit))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.9077  -0.9810  -0.4543   1.0307   2.1552  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -6.72498    0.77798  -8.644   <2e-16 ***
DVRT         0.06437    0.00759   8.481   <2e-16 ***
---
Signif. codes:  0  `***'  0.001  `**'  0.01  `*'  0.05  `.'  0.1  ` '  1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 686.86  on 499  degrees of freedom
Residual deviance: 593.77  on 498  degrees of freedom
AIC: 597.77

Number of Fisher Scoring iterations: 2

# Here we see a very significant bivariate association between 
# DVRT and the leaving certificate indicator.
# Is this magnitude of this association substantively large?
# Let's plot the fitted values against DVRT.
 
> plot(DVRT, fitted(result1))

# Here we see that the probability of a student at the low end of
# the distribution of DVRT scores has about a 0.10 probability of
# getting a leaving certificate while a student at the hight end of
# the distribution of DVRT scores has about a 0.90 probability of 
# getting a leaving certificate. This is sizable effect.

# Let's do a probit analysis of these same data

> result2 <- glm(lvcert~DVRT, family=binomial(probit))
> summary(result2)

Call:
glm(formula = lvcert ~ DVRT, family = binomial(probit))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.9126  -0.9887  -0.4363   1.0346   2.1904  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -4.051730   0.440535  -9.197   <2e-16 ***
DVRT         0.038799   0.004311   8.999   <2e-16 ***
---
Signif. codes:  0  `***'  0.001  `**'  0.01  `*'  0.05  `.'  0.1  ` '  1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 686.86  on 499  degrees of freedom
Residual deviance: 593.88  on 498  degrees of freedom
AIC: 597.88

Number of Fisher Scoring iterations: 2

# Once again, as we would expect, we see a very significant bivariate
# association between DVRT and the leaving certificate indicator
# Let's plot the probit fitted values against DVRT

> plot(DVRT, fitted(result2))

# As we would expect, this looks pretty similar to the plot of the 
# fitted values from the logit analysis

# Let's plot the the logit fitted values, the probit fitted values,
# and the actual values of the leaving certificate indicator against
# DVRT.

# first the raw data 

> plot(DVRT, jitter(lvcert, .4), ylim=c(-0.2,1.2), ylab="")

# Note that I added some jitter to the leaving certificate indicator
# so that the density of responses could be seen in the scatterplot.
# Also, note that I set the y axis to run from -0.2 to 1.2. It is 
# important to manually set the limits of the plot axes when
# overlaying multiple graphs. 

# now the logit fitted values

> par(new=TRUE)
> plot(DVRT, fitted(result1), ylim=c(-0.2,1.2), ylab="", col=2, pch=3)

# the par(new=TRUE) line tells R/S-PLUS not to overlay the new plot
# on the existing plot. In R col=2 specifies the plotting color to be
# red, and pch=3 specifies the plotting symbol to be a plus sign.

# now let's add the probit fitted values

> par(new=TRUE)
> plot(DVRT, fitted(result2), ylim=c(-0.2,1.2), ylab="", col=4, pch=0)

# In R the probit fitted values are displayed as blue squares. As we
# expect, the probit fitted values are almost identical to the logit
# fitted values. 

# Let's fit a slightly more realistic model in which sex and the
# prestige score of the father's education enter as covariates
# Missing values in the prestige of father's occupation are coded
# as 0s. To turn these into NAs we type:

> irished$fathocc[irished$fathocc==0] <- NA

# then to fit the new logistic regression model we type:


> result3 <- glm(lvcert~DVRT+sex+fathocc, na.omit(irished), family=binomial(logit))
> summary(result3)

Call:
glm(formula = lvcert ~ DVRT + sex + fathocc, family = binomial(logit), 
    data = na.omit(irished))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.1557  -0.9151  -0.4497   0.9175   2.2907  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -8.648069   0.983888  -8.790  < 2e-16 ***
DVRT         0.060585   0.008148   7.435 1.04e-13 ***
sex          0.516546   0.214930   2.403   0.0162 *  
fathocc      0.039139   0.007489   5.227 1.73e-07 ***
---
Signif. codes:  0  `***'  0.001  `**'  0.01  `*'  0.05  `.'  0.1  ` '  1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 651.39  on 473  degrees of freedom
Residual deviance: 530.63  on 470  degrees of freedom
AIC: 538.63

Number of Fisher Scoring iterations: 3

# Here we see that DVRT, sex, and fathocc exert significant effects on
# the probability of obtaining a leaving certificate. 

# If we are interested in testing whether the inclusion of these
# additional covariates improves the fit over the original logistic
# regression model with DVRT as the sole covariate we could perform a
# likelihood ratio test. However, to do this correctly we need to
# refit the original model to the dataset in which the observations 
# with missing values of fathocc have been dropped.


> result4 <- glm(lvcert~DVRT, na.omit(irished), family=binomial(logit))
> summary(result4)

Call:
glm(formula = lvcert ~ DVRT, family = binomial(logit), data = na.omit(irished))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8877  -0.9848  -0.4547   1.0447   2.1543  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -6.742867   0.805496  -8.371   <2e-16 ***
DVRT         0.064653   0.007869   8.216   <2e-16 ***
---
Signif. codes:  0  `***'  0.001  `**'  0.01  `*'  0.05  `.'  0.1  ` '  1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 651.39  on 473  degrees of freedom
Residual deviance: 564.09  on 472  degrees of freedom
AIC: 568.09

Number of Fisher Scoring iterations: 2

# the LR test statistic is 564.09 - 530.63
# the p-value is


> 1 - pchisq(564.09 - 530.63, 2)
[1] 5.423171e-08

# this is significant at any conventional level. 

# let's see what the difference in fitted probabilities is for men vs
# women with average test results and an average father's education

> logit.prob <- function(eta){exp(eta)/(1+exp(eta))}

> xbar.male <- apply(na.omit(cbind(DVRT, sex, fathocc)), 2, mean)
> xbar.male[2] <- 1
> xbar.male <- c(1, xbar.male)
> xbar.male
             DVRT      sex  fathocc 
  1.0000 100.0949   1.0000  38.9346 

> xbar.female <- xbar.male
> xbar.female[2] <- 2     
> xbar.female <- c(1, xbar.female)
> xbar.female
             DVRT      sex  fathocc 
  1.0000 100.0949   2.0000  38.9346 
> beta <- coef(result3)
> beta
(Intercept)        DVRT         sex     fathocc 
-8.64806880  0.06058488  0.51654593  0.03913933 


> prob.male <- logit.prob(xbar.male %*% beta)
> prob.female <- logit.prob(xbar.female %*% beta)
> prob.male
         [,1]
[1,] 0.367395
> prob.female
          [,1]
[1,] 0.4932847

# in other words, the probability of an average male gaining a leaving 
# certificate is about 74% the probability of an average female
# getting a leaving certificate.


