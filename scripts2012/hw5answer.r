


# We want to look at changes in the rate of growth by both chick and diet. 
# We are really interested in the differences in growth rate by diet and can
# consider the chick-specific variability as a nuisance. The key here is that 
# we need to interact the diet indicators with time.
# 
# After plotting the raw data and fitting some simple models with just linear 
# and quadratic effects it seems like something similar to the following model
# is a pretty reasonable first cut.


> options(contrasts=c("contr.treatment", "contr.treatment"))

> lme4.out <- lme(weight~Diet*Time + Diet*I(Time^2) + I(Time^3), 
                  random=~1+Time+I(Time^2)+I(Time^3)|Chick, 
		  data=ChickWeight, 
		  control=lmeControl(maxIter=5000, niterEM=3000))

> summary(lme4.out)
Linear mixed-effects model fit by REML
 Data: ChickWeight 
       AIC      BIC    logLik 
  3913.083 4017.167 -1932.541

Random effects:
 Formula:  ~ 1 + Time + I(Time^2) + I(Time^3) | Chick
 Structure: General positive-definite
                StdDev   Corr               
(Intercept) 0.70500702 (Intr) Time   I(Tm^2
       Time 2.30579140 -0.975              
  I(Time^2) 0.55714460  0.778 -0.888       
  I(Time^3) 0.01907127 -0.577  0.740 -0.934
   Residual 4.17165482                     

Fixed effects: weight ~ Diet * Time + Diet * I(Time^2) + I(Time^3) 
                   Value Std.Error  DF   t-value p-value 
   (Intercept)  41.40495  0.824206 519  50.23617  <.0001
         Diet2  -1.13037  1.404140  46  -0.80503  0.4249
         Diet3   0.30780  1.404140  46   0.21921  0.8275
         Diet4  -2.11635  1.405067  46  -1.50623  0.1388
          Time   2.27415  0.525967 519   4.32375  <.0001
     I(Time^2)   0.37924  0.091227 519   4.15713  <.0001
     I(Time^3)  -0.00987  0.002861 519  -3.45001  0.0006
     Diet2Time   1.42654  0.747729 519   1.90783  0.0570
     Diet3Time   0.52249  0.747729 519   0.69877  0.4850
     Diet4Time   3.54862  0.748287 519   4.74233  <.0001
Diet2I(Time^2)   0.05317  0.079876 519   0.66562  0.5060
Diet3I(Time^2)   0.22324  0.079876 519   2.79488  0.0054
Diet4I(Time^2)  -0.02014  0.079905 519  -0.25199  0.8011
 Correlation: 
               (Intr)  Diet2  Diet3  Diet4   Time I(Tm^2 I(T^3) Dit2Tm Dit3Tm 
         Diet2 -0.572                                                        
         Diet3 -0.572  0.336                                                 
         Diet4 -0.571  0.335  0.335                                          
          Time -0.587  0.292  0.292  0.291                                   
     I(Time^2)  0.278 -0.081 -0.081 -0.080 -0.798                            
     I(Time^3) -0.161 -0.001 -0.001 -0.003  0.553 -0.855                     
     Diet2Time  0.350 -0.605 -0.206 -0.206 -0.486  0.225  0.004              
     Diet3Time  0.350 -0.206 -0.605 -0.206 -0.486  0.225  0.004  0.344       
     Diet4Time  0.349 -0.206 -0.206 -0.606 -0.485  0.223  0.006  0.344  0.344
Diet2I(Time^2) -0.157  0.273  0.094  0.093  0.364 -0.296 -0.013 -0.755 -0.261
Diet3I(Time^2) -0.157  0.094  0.273  0.093  0.364 -0.296 -0.013 -0.261 -0.755
Diet4I(Time^2) -0.157  0.093  0.093  0.272  0.365 -0.297 -0.012 -0.261 -0.261

               Dit4Tm D2I(T^2) D3I(T^2) 
         Diet2                         
         Diet3                         
         Diet4                         
          Time                         
     I(Time^2)                         
     I(Time^3)                         
     Diet2Time                         
     Diet3Time                         
     Diet4Time                         
Diet2I(Time^2) -0.261                  
Diet3I(Time^2) -0.261    0.351         
Diet4I(Time^2) -0.753    0.351    0.351

Standardized Within-Group Residuals:
       Min         Q1         Med        Q3      Max 
 -3.134583 -0.4908853 0.005309179 0.4721511 3.510561

Number of Observations: 578
Number of Groups: 50 



# Looking at the residuals on the fitted values and on time, we see that
# mean of the residuals is generally pretty close to 0 (aside from time 14) 
# but that the variability of the residuals seems to change a bit over 
# time and the fitted values. 

> plot(lme4.out, resid(.)~Time, abline=0)

> plot(lme4.out, resid(.)~fitted(.), abline=0)

# Note that the variance doesn't appear to by changing monotonically in 
# time and probably not in the fitted values, although this last point is
# less clear. 
# 
# One think we could try is to fit a separate within group error variance
# for each time period. We can do this using the varIdent() function.



> lme5.out <- update(lme4.out, weights=varIdent(form=~1|Time))

> summary(lme5.out)                                       
Linear mixed-effects model fit by REML
 Data: ChickWeight 
       AIC      BIC    logLik 
  3748.877 3900.666 -1839.439

Random effects:
 Formula:  ~ 1 + Time + I(Time^2) + I(Time^3) | Chick
 Structure: General positive-definite
                StdDev   Corr               
(Intercept) 0.49872464 (Intr) Time   I(Tm^2
       Time 1.80537029  0.639              
  I(Time^2) 0.52552250 -0.263 -0.908       
  I(Time^3) 0.01895165  0.084  0.777 -0.929
   Residual 1.00860070                     

Variance function:
 Structure: Different standard deviations per stratum
 Formula:  ~ 1 | Time 
 Parameter estimates:
 0        2        4        6        8       10       12      14      16 
 1 2.248471 1.546746 2.410575 3.037097 3.207866 5.445179 6.77682 7.63203

       18       20       21 
 7.002792 2.251407 6.371668
Fixed effects: weight ~ Diet * Time + Diet * I(Time^2) + I(Time^3) 
                   Value Std.Error  DF   t-value p-value 
   (Intercept)  41.42271 0.2480250 519  167.0102  <.0001
         Diet2  -0.75458 0.4293143  46   -1.7576  0.0855
         Diet3  -0.54845 0.4293143  46   -1.2775  0.2078
         Diet4  -0.51507 0.4293924  46   -1.1995  0.2365
          Time   2.06440 0.3535553 519    5.8390  <.0001
     I(Time^2)   0.43619 0.0844082 519    5.1677  <.0001
     I(Time^3)  -0.01176 0.0027939 519   -4.2107  <.0001
     Diet2Time   1.13943 0.4815280 519    2.3663  0.0183
     Diet3Time   0.98191 0.4815280 519    2.0392  0.0419
     Diet4Time   2.44966 0.4815484 519    5.0871  <.0001
Diet2I(Time^2)   0.05399 0.0768388 519    0.7026  0.4826
Diet3I(Time^2)   0.20231 0.0768388 519    2.6329  0.0087
Diet4I(Time^2)   0.01595 0.0769173 519    0.2074  0.8358
 Correlation: 
               (Intr)  Diet2  Diet3  Diet4   Time I(Tm^2 I(T^3) Dit2Tm Dit3Tm 
         Diet2 -0.578                                                        
         Diet3 -0.578  0.334                                                 
         Diet4 -0.578  0.334  0.334                                          
          Time  0.111 -0.068 -0.068 -0.069                                   
     I(Time^2) -0.079  0.051  0.051  0.052 -0.837                            
     I(Time^3) -0.015  0.002  0.002  0.001  0.605 -0.842                     
     Diet2Time -0.088  0.155  0.051  0.051 -0.461  0.233  0.008              
     Diet3Time -0.088  0.051  0.155  0.051 -0.461  0.233  0.008  0.342       
     Diet4Time -0.088  0.051  0.051  0.155 -0.461  0.233  0.008  0.342  0.342
Diet2I(Time^2)  0.100 -0.173 -0.058 -0.058  0.351 -0.307 -0.014 -0.765 -0.264
Diet3I(Time^2)  0.100 -0.058 -0.173 -0.058  0.351 -0.307 -0.014 -0.264 -0.765
Diet4I(Time^2)  0.100 -0.058 -0.058 -0.174  0.352 -0.309 -0.012 -0.264 -0.264

               Dit4Tm D2I(T^2) D3I(T^2) 
         Diet2                         
         Diet3                         
         Diet4                         
          Time                         
     I(Time^2)                         
     I(Time^3)                         
     Diet2Time                         
     Diet3Time                         
     Diet4Time                         
Diet2I(Time^2) -0.264                  
Diet3I(Time^2) -0.264    0.351         
Diet4I(Time^2) -0.764    0.350    0.350

Standardized Within-Group Residuals:
       Min         Q1         Med        Q3      Max 
 -2.748437 -0.5175538 -0.04104451 0.5196003 3.438615

Number of Observations: 578
Number of Groups: 50 


# based on AIC, BIC and a LR test this model is preferred over the previous
# model. 
# 
# Let's look at the residuals
#

> plot(lme5.out, resid(., type="n")~Time, abline=0)

> plot(lme5.out, resid(., type="n")~fitted(.), abline=0)

# Variance looks more stable so let's see if there is any serial correlation

> plot(ACF(lme5.out, resType="n", maxLag=5), alpha=0.01)      

> plot(Variogram(lme5.out, resType="n")) 

# Here we see some evidence of negative temporal dependence at lags 2, 3
# and 4. Given the short lengths of the individual time-series (12 periods) 
# and the small magnitudes of the autocorrelations I'm reluctant to fit a 
# complicated time-series model to these data. If we choose not to try to 
# model this serial correlation we should realize that our standard errors 
# aren't correct. 
#
# I will try to fit an AR(2) model to the data. 

> lme6.out <- update(lme5.out, correlation=corARMA(p=2,q=0))

> summary(lme6.out)
Linear mixed-effects model fit by REML
 Data: ChickWeight 
       AIC     BIC    logLik 
  3677.047 3837.51 -1801.524

Random effects:
 Formula:  ~ 1 + Time + I(Time^2) + I(Time^3) | Chick
 Structure: General positive-definite
               StdDev   Corr               
(Intercept) 0.2512794 (Intr) Time   I(Tm^2
       Time 1.3313141  0.999              
  I(Time^2) 0.4527088 -0.935 -0.916       
  I(Time^3) 0.0164247  0.803  0.777 -0.916
   Residual 1.3279440                     

Correlation Structure: ARMA(2,0)
 Formula:  ~ 1 | Chick 
 Parameter estimate(s):
      Phi1        Phi2 
 0.6943597 -0.03321157
Variance function:
 Structure: Different standard deviations per stratum
 Formula:  ~ 1 | Time 
 Parameter estimates:
 0        2        4        6        8       10       12       14       16 
 1 2.622099 1.859576 3.008756 3.765952 4.121177 7.249498 6.133454 7.060778

       18       20       21 
 7.304641 5.523663 2.827365
Fixed effects: weight ~ Diet * Time + Diet * I(Time^2) + I(Time^3) 
                   Value Std.Error  DF   t-value p-value 
   (Intercept)  41.45194 0.2658252 519  155.9368  <.0001
         Diet2  -1.02339 0.4562281  46   -2.2431  0.0297
         Diet3  -0.91221 0.4562281  46   -1.9995  0.0515
         Diet4  -0.73466 0.4562305  46   -1.6103  0.1142
          Time   2.25304 0.3120783 519    7.2195  <.0001
     I(Time^2)   0.39387 0.0772543 519    5.0983  <.0001
     I(Time^3)  -0.01050 0.0025581 519   -4.1027  <.0001
     Diet2Time   0.81479 0.4347495 519    1.8742  0.0615
     Diet3Time   0.90249 0.4347495 519    2.0759  0.0384
     Diet4Time   1.97346 0.4347866 519    4.5389  <.0001
Diet2I(Time^2)   0.07709 0.0724500 519    1.0640  0.2878
Diet3I(Time^2)   0.19995 0.0724500 519    2.7598  0.0060
Diet4I(Time^2)   0.04291 0.0726041 519    0.5911  0.5547
 Correlation: 
               (Intr)  Diet2  Diet3  Diet4   Time I(Tm^2 I(T^3) Dit2Tm Dit3Tm 
         Diet2 -0.574                                                        
         Diet3 -0.574  0.335                                                 
         Diet4 -0.574  0.335  0.335                                          
          Time  0.100 -0.016 -0.016 -0.016                                   
     I(Time^2) -0.157  0.031  0.031  0.031 -0.804                            
     I(Time^3)  0.121  0.002  0.002  0.002  0.579 -0.831                     
     Diet2Time -0.021  0.029  0.013  0.013 -0.472  0.225  0.008              
     Diet3Time -0.021  0.013  0.029  0.013 -0.472  0.225  0.008  0.343       
     Diet4Time -0.021  0.013  0.013  0.029 -0.473  0.226  0.007  0.342  0.342
Diet2I(Time^2)  0.059 -0.099 -0.035 -0.035  0.335 -0.318 -0.015 -0.711 -0.247
Diet3I(Time^2)  0.059 -0.035 -0.099 -0.035  0.335 -0.318 -0.015 -0.247 -0.711
Diet4I(Time^2)  0.059 -0.035 -0.035 -0.099  0.337 -0.320 -0.012 -0.247 -0.247

               Dit4Tm D2I(T^2) D3I(T^2) 
         Diet2                         
         Diet3                         
         Diet4                         
          Time                         
     I(Time^2)                         
     I(Time^3)                         
     Diet2Time                         
     Diet3Time                         
     Diet4Time                         
Diet2I(Time^2) -0.247                  
Diet3I(Time^2) -0.247    0.353         
Diet4I(Time^2) -0.711    0.352    0.352

Standardized Within-Group Residuals:
       Min         Q1       Med        Q3      Max 
 -2.982564 -0.2452056 0.2262529 0.6894788 2.947259

Number of Observations: 578
Number of Groups: 50 

# Based on any of the model selection criteria this model is preferred 
# to the earlier models. Further, looking at the residuals, things look 
# pretty good.

> plot(lme6.out, resid(., type="n")~Time, abline=0)     
> plot(lme6.out, resid(., type="n")~fitted(.), abline=0)
> plot(ACF(lme6.out, resType="n", maxLag=9), alpha=0.01)   

# Note that the ARMA correlation structure isn't technically appropriate
# for these data since the last time period is 1 day away from the second 
# to last time period while all the other time periods are 2 days apart.
# For now I'm ignoring this complication

# OK, now to assess whether there are significant differences in growth by 
# diet we can perform an F-test:

> anova(lme6.out, Terms=c(6,7))
F-test for: Diet:Time, Diet:I(Time^2) 
  numDF denDF  F-value p-value 
1     6   519 11.70283  <.0001

# This tells us that there is reason to reject the null hypothesis
# that chicks have the same growth rates under diets 2, 3, and 4 as
# under diet 1. In other words, there is significant variability in
# the effect of time on weight by diet.

# OK, let's plot the predicted population weight levels over time by diet. 

> predweight.d1 <- function(time){return(41.45 + time*2.25304 
                                  + time^2 * 0.39387 + time^3 * -0.01050)}

> predweight.d2 <- function(time){return( (41.45 + -1.02) 
			          +  time*(2.25304 + 0.81479)  
                                  + time^2 * (0.39387 + 0.07709) 
                                  +  time^3 * -0.01050) }

> predweight.d3 <- function(time){return( (41.45 + -0.91) 
                                  + time*(2.25304 + 0.90249)  
                                  + time^2 * (0.39387 + 0.19995) 
                                  +  time^3 * -0.01050)}

> predweight.d4 <- function(time){return( (41.45 + -0.73) 
                                  + time*(2.25304 + 1.97346)  
                                  + time^2 * (0.39387 + 0.04291) 
                                  +  time^3 * -0.01050)}

> time <- 0:21







> plot(time, predweight.d1(time), type="l", col=1, xlim=c(0,21),
        ylim=c(38,300))

> par(new=TRUE)

> plot(time, predweight.d2(time), type="l", col=2, xlim=c(0,21),
        ylim=c(38,300))

> par(new=TRUE)

> plot(time, predweight.d3(time), type="l", col=3, xlim=c(0,21),
        ylim=c(38,300))

> par(new=TRUE)

> plot(time, predweight.d4(time), type="l", col=4, xlim=c(0,21),
        ylim=c(38,300))


# from this we can see that diets 3 and 4 dominate diets 1 and 2. Diet 
# 4 is slightly preferred to diet 3 until about day 7 when diet 3 
# begins to produce heavier chicks. 

# We can also look at the growth rate rather than the level of growth 
# under each of the four diets. This can be done by calculating the 
# first derivatives of the growth curves with respect to time
# under each of the four diets.

> predrate.d1 <- function(time){return(2.25304 
                                  + 2* time * 0.39387 + 3*time^2 * -0.01050)}

> predrate.d2 <- function(time){return( (2.25304 + 0.81479)  
                                  + 2*time * (0.39387 + 0.07709) 
                                  +  3*time^2 * -0.01050) }

> predrate.d3 <- function(time){return( (2.25304 + 0.90249)  
                                  + 2*time * (0.39387 + 0.19995) 
                                  +  3*time^2 * -0.01050)}

> predrate.d4 <- function(time){return( (2.25304 + 1.97346)  
                                  + 2*time * (0.39387 + 0.04291) 
                                  +  3*time^2 * -0.01050)}

> plot(time, predrate.d1(time), type="l", col=1, xlim=c(0,21), ylim=c(0,18))

> par(new=TRUE)

> plot(time, predrate.d2(time), type="l", col=2, xlim=c(0,21), ylim=c(0,18))

> par(new=TRUE)                                                             

> plot(time, predrate.d3(time), type="l", col=3, xlim=c(0,21), ylim=c(0,18))

> par(new=TRUE)                                                             

> plot(time, predrate.d4(time), type="l", col=4, xlim=c(0,21), ylim=c(0,18))


# This is very informative b/c it shows us that the rate of growth
# (not the level) is always greater under diets 3 and 4 than under diets 1
# and 2. Once again, diet 4 is slightly better than diet 3 early (until about
# day 4) and then diet 3 really takes off. Chicks continue to grow very 
# rapidly under diet three even at day 21. At day 21, chicks are growing 
# almost three times faster under diet 3 than under diet 1. 




