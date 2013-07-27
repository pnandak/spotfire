# This is commented fairly minimally. If you have questions please 
# let me know.

# load nlme3 library
> library(nlme3)   

# use treatment contrasts
> options(contrasts=c("contr.treatment", "contr.treatment"))

# load data
> school3 <- read.table("school3.dat", header=TRUE)

# turn data into groupedData object
> school3 <- groupedData(mathscore~pincome|school, data=school3)

# take look at first 3 rows
> school3[1:3,]
Grouped Data: mathscore ~ pincome | school
  school sex fathColgrad minority pincome mathscore 
1      1   1           0        0       5       533
2      1   1           0        0       4       565
3      1   1           0        0       3       519


# fit a pooled regression model
> lm.out <- lm(mathscore~sex+fathColgrad+minority+pincome, data=school3)

> summary(lm.out)

Call: lm(formula = mathscore ~ sex + fathColgrad + minority + pincome, data = school3
	)
Residuals:
    Min     1Q  Median    3Q   Max 
 -73.87 -16.16 -0.3882 16.12 72.11

Coefficients:
                Value Std. Error   t value  Pr(>|t|) 
(Intercept)  517.0169    3.3107   156.1660    0.0000
        sex   -1.7111    1.9955    -0.8575    0.3915
fathColgrad   13.5922    1.9881     6.8369    0.0000
   minority    0.1760    2.3444     0.0751    0.9402
    pincome    6.1330    0.3740    16.3981    0.0000

Residual standard error: 24.27 on 595 degrees of freedom
Multiple R-Squared: 0.3517 
F-statistic: 80.69 on 4 and 595 degrees of freedom, the p-value is 0 

Correlation of Coefficients:
            (Intercept)     sex fathColgrad minority 
        sex -0.3723                                 
fathColgrad -0.3085      0.0151                     
   minority -0.2037      0.1032 -0.0283             
    pincome -0.8195      0.0493 -0.0170      0.0164 


# check diagnostic plots
> plot(lm.out, ask=TRUE) 

# turn graphics device off 
> dev.off()

# plot residuals by school
> bwplot(getGroups(school3)~resid(lm.out))

# Seems to be quite a bit of structure in the residuals corresponding to 
# particular schools




# lets go to the other extreme and fit individual regression within each 
# cluster (school in this case)

> lmlist.out <- lmList(mathscore~sex+fathColgrad+minority+pincome|school,
                       data=school3)


> summary(lmlist.out)

# output ommitted here

> pairs(lmlist.out)

> intervals(lmlist.out)

> plot(intervals(lmlist.out))

# looks like the intercepts may vary by school-- hard to say about other 
# coefficients



# lets fit a random intercept model

> lme1.out <- lme(mathscore~sex+fathColgrad+minority+pincome, 
                random=~1|school, data=school3)

> summary(lme1.out)

Linear mixed-effects model fit by REML
 Data: school3 
       AIC      BIC    logLik 
  5331.118 5361.838 -2658.559

Random effects:
 Formula:  ~ 1 | school
        (Intercept) Residual 
StdDev:    14.54946 19.66747

Fixed effects: mathscore ~ sex + fathColgrad + minority + pincome 
                Value Std.Error  DF   t-value p-value 
(Intercept)  518.1082  4.236184 576  122.3054  <.0001
        sex   -2.1550  1.628981 576   -1.3229  0.1864
fathColgrad   13.6534  1.644205 576    8.3039  <.0001
   minority   -1.7303  1.934364 576   -0.8945  0.3714
    pincome    6.0708  0.306567 576   19.8024  <.0001
 Correlation: 
            (Intr)    sex fthClg minrty 
        sex -0.238                     
fathColgrad -0.201  0.025              
   minority -0.128  0.103 -0.041       
    pincome -0.525  0.047 -0.015  0.016

Standardized Within-Group Residuals:
       Min         Q1         Med        Q3      Max 
 -2.975121 -0.6289795 0.007439654 0.6412385 2.997292

Number of Observations: 600
Number of Groups: 20 

# Definitely school-specific heterogeneity here as evidenced by the size
# of the standard deviation of the random intercept vs. the standard deviation
# of the within group disturbances (the epsilons)

# Diagnostic plots

> bwplot(getGroups(school3)~resid(lme1.out, level=1))

> plot(lme1.out, school~resid(.), abline=0)

> plot(lme1.out, resid(.,type="p")~fitted(.)|sex)

> plot(compareFits(coef(lmlist.out), coef(lme1.out)))

> plot(comparePred(lmlist.out, lme1.out))


# lets fit a random coefficients model


> lme2.out <- lme(mathscore~sex+fathColgrad+minority+pincome, 
                random=~pincome|school, data=school3)


> summary(lme2.out)

Linear mixed-effects model fit by REML
 Data: school3 
       AIC      BIC    logLik 
  5331.172 5370.669 -2656.586

Random effects:
 Formula:  ~ pincome | school
 Structure: General positive-definite
               StdDev   Corr 
(Intercept) 12.120694 (Inter
    pincome  1.001641 0.072 
   Residual 19.498629       

Fixed effects: mathscore ~ sex + fathColgrad + minority + pincome 
                Value Std.Error  DF   t-value p-value 
(Intercept)  518.0347  3.836752 576  135.0191  <.0001
        sex   -2.1654  1.618593 576   -1.3378  0.1815
fathColgrad   13.3922  1.640927 576    8.1613  <.0001
   minority   -1.4775  1.926065 576   -0.7671  0.4433
    pincome    6.0907  0.380713 576   15.9982  <.0001
 Correlation: 
            (Intr)    sex fthClg minrty 
        sex -0.263                     
fathColgrad -0.223  0.024              
   minority -0.137  0.101 -0.043       
    pincome -0.441  0.040 -0.009  0.010

Standardized Within-Group Residuals:
       Min         Q1        Med        Q3      Max 
 -3.002872 -0.6304622 0.01036705 0.6724614 3.003783

Number of Observations: 600
Number of Groups: 20 

# diagnostic plots

> bwplot(getGroups(school3)~resid(lme2.out, level=1))
 
> plot(lme2.out, school~resid(.), abline=0)

> plot(lme2.out, resid(.,type="p")~fitted(.)|sex)

> plot(compareFits(coef(lmlist.out), coef(lme2.out)))

> plot(comparePred(lmlist.out, lme2.out))


# lets center pincome and reestimate the model

> mean(school3$pincome)
[1] 7.143333

> lme2.out <- lme(mathscore~sex+fathColgrad+minority+I(pincome-7.143), 
                random=~I(pincome-7.143)|school, data=school3)

> summary(lme2.out)

Linear mixed-effects model fit by REML
 Data: school3 
       AIC      BIC    logLik 
  5331.172 5370.669 -2656.586

Random effects:
 Formula:  ~ I(pincome - 7.143) | school
 Structure: General positive-definite
                       StdDev   Corr 
       (Intercept) 14.5198498 (Inter
I(pincome - 7.143)  0.9975857 0.558 
          Residual 19.4995762       

Fixed effects: mathscore ~ sex + fathColgrad + minority + I(pincome - 7.143) 
                       Value Std.Error  DF   t-value p-value 
       (Intercept)  561.5402  3.595914 576  156.1606  <.0001
               sex   -2.1643  1.618632 576   -1.3371  0.1817
       fathColgrad   13.3928  1.640883 576    8.1620  <.0001
          minority   -1.4787  1.926095 576   -0.7677  0.4430
I(pincome - 7.143)    6.0909  0.380162 576   16.0218  <.0001
 Correlation: 
                   (Intr)    sex fthClg minrty 
               sex -0.250                     
       fathColgrad -0.244  0.024              
          minority -0.139  0.101 -0.043       
I(pincome - 7.143)  0.288  0.040 -0.009  0.010

Standardized Within-Group Residuals:
       Min         Q1         Med        Q3      Max 
 -3.002606 -0.6307449 0.009513393 0.6723236 3.003616

Number of Observations: 600
Number of Groups: 20 
