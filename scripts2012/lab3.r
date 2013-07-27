> library(nlme3)

> attach(Spruce)
> plot(Spruce)


# piecewise linear growth with 1 knot at day 258

> days2 <- days-258                                                        
> days2[days<=258] <- 0  
> Spruce <- cbind(Spruce, days2)                                             
> dimnames(Spruce)[[2]][5] <- "days2"
> lme1.out <- lme(logSize~days+days2, random=~days+days2|Tree, data=Spruce)

> summary(lme1.out)
summary(lme1.out)
Linear mixed-effects model fit by REML
 Data: Spruce 
       AIC      BIC    logLik 
  30.34667 79.66138 -5.173333

Random effects:
 Formula:  ~ days + days2 | Tree
 Structure: General positive-definite
                 StdDev   Corr        
(Intercept) 0.750588365 (Intr) days  
       days 0.002560364 -0.577       
      days2 0.002521900  0.522 -0.978
   Residual 0.178916803              

Fixed effects: logSize ~ days + days2 
                Value  Std.Error  DF   t-value p-value 
(Intercept)  2.547849 0.09666386 946  26.35782  <.0001
       days  0.011135 0.00036514 946  30.49387  <.0001
      days2 -0.009224 0.00038658 946 -23.86006  <.0001
 Correlation: 
      (Intr)   days 
 days -0.691       
days2  0.650 -0.980

Standardized Within-Group Residuals:
       Min         Q1       Med        Q3      Max 
 -2.451582 -0.6381711 0.1437953 0.6676941 3.488346

Number of Observations: 1027
Number of Groups: 79 






# polynomial regression 
lme2.out <- lme(logSize~days+days^2, random=~days+days^2|Tree, data=Spruce) 

> summary(lme2.out)                                                           
Linear mixed-effects model fit by REML
 Data: Spruce 
       AIC      BIC    logLik 
  698.2267 747.5414 -339.1133

Random effects:
 Formula:  ~ days + days^2 | Tree
 Structure: General positive-definite
                  StdDev   Corr        
(Intercept) 5.614548e-01 (Intr) days  
       days 1.430438e-03  0.049       
  I(days^2) 1.193324e-06 -0.331 -0.959
   Residual 2.674770e-01              

Fixed effects: logSize ~ days + days^2 
                Value  Std.Error  DF   t-value p-value 
(Intercept)  3.607163 0.08372077 946  43.08564  <.0001
       days  0.006653 0.00036662 946  18.14764  <.0001
  I(days^2) -0.000004 0.00000043 946  -9.69345  <.0001
 Correlation: 
          (Intr)   days 
     days -0.552       
I(days^2)  0.499 -0.977

Standardized Within-Group Residuals:
       Min         Q1        Med        Q3     Max 
 -3.411861 -0.6715669 0.05345485 0.5474627 2.54547

Number of Observations: 1027
Number of Groups: 79 



# or using orthogonal polynomials
> lme3.out <- lme(logSize~poly(days, degree=2), 
             random=~poly(days, degree=2)|Tree, data=Spruce)

> summary(lme3.out)
Linear mixed-effects model fit by REML
 Data: Spruce 
       AIC      BIC    logLik 
  654.7594 704.0741 -317.3797

Random effects:
 Formula:  ~ poly(days, degree = 2) | Tree
 Structure: General positive-definite
                           StdDev   Corr          
            (Intercept) 0.6254975 (Intr) p(,d=2)1
poly(days, degree = 2)1 3.4947553  0.231         
poly(days, degree = 2)2 0.7033705 -0.839 -0.617  
               Residual 0.2675954                

Fixed effects: logSize ~ poly(days, degree = 2) 
                            Value Std.Error  DF   t-value p-value 
            (Intercept)   5.54842 0.0708676 946  78.29277  <.0001
poly(days, degree = 2)1  19.94362 0.4756114 946  41.93258  <.0001
poly(days, degree = 2)2  -2.73020 0.2790515 946  -9.78387  <.0001
 Correlation: 
                        (Intr) p(,d=2)1 
poly(days, degree = 2)1  0.189         
poly(days, degree = 2)2 -0.236   -0.145

Standardized Within-Group Residuals:
       Min         Q1        Med       Q3      Max 
 -3.455302 -0.6661111 0.05041506 0.543378 2.548427

Number of Observations: 1027
Number of Groups: 79 





# polynomial spline regression

>  lme4.out <- lme(logSize~bs(days, knots=258, degree=2),  
           random=~bs(days, knots=258, degree=2)|Tree, data=Spruce)  



> summary(lme4.out)
Linear mixed-effects model fit by REML
 Data: Spruce 
        AIC      BIC   logLik 
  -511.9424 -437.985 270.9712

Random effects:
 Formula:  ~ bs(days, knots = 258, degree = 2) | Tree
 Structure: General positive-definite
                                      StdDev   Corr                
                       (Intercept) 0.6418466 (Intr) b(,k=258,d=2)1
bs(days, knots = 258, degree = 2)1 0.3049543 -0.306               
bs(days, knots = 258, degree = 2)2 0.3627830 -0.096  0.884        
bs(days, knots = 258, degree = 2)3 0.4282567 -0.344  0.743        
                          Residual 0.1265519                      

                                                  
                       (Intercept) b(,k=258,d=2)2
bs(days, knots = 258, degree = 2)1               
bs(days, knots = 258, degree = 2)2               
bs(days, knots = 258, degree = 2)3  0.894        
                          Residual               

Fixed effects: logSize ~ bs(days, knots = 258, degree = 2) 
                                      Value  Std.Error  DF  t-value p-value 
                       (Intercept) 4.066296 0.07324758 945 55.51441  <.0001
bs(days, knots = 258, degree = 2)1 1.261438 0.03937248 945 32.03858  <.0001
bs(days, knots = 258, degree = 2)2 1.417731 0.04570427 945 31.01965  <.0001
bs(days, knots = 258, degree = 2)3 2.310093 0.05091111 945 45.37502  <.0001
 Correlation: 
                                   (Intr) b(,k=258,d=2)1 b(,k=258,d=2)2 
bs(days, knots = 258, degree = 2)1 -0.330                              
bs(days, knots = 258, degree = 2)2 -0.123          0.738               
bs(days, knots = 258, degree = 2)3 -0.363          0.724          0.762

Standardized Within-Group Residuals:
       Min         Q1        Med        Q3     Max 
 -2.574205 -0.6201736 0.04522573 0.6213707 3.46929

Number of Observations: 1027
Number of Groups: 79 


> plot(augPred(lme4.out))


# check for temporal autocorrelation

> plot(ACF(lme4.out), alpha=0.01)

>  lme5.out <- lme(logSize~bs(days, knots=258, degree=2),  
           random=~bs(days, knots=258, degree=2)|Tree, 
           correlation=corCAR1(0.8, form=~days|Tree), data=Spruce)  

> plot(ACF(lme5.out, resType="n"), alpha=0.01)

> anova(lme4.out, lme5.out)                   
         Model df       AIC       BIC   logLik   Test  L.Ratio p-value 
lme4.out     1 15 -511.9376 -437.9801 270.9688                        
lme5.out     2 16 -824.7800 -745.8920 428.3900 1 vs 2 314.8424  <.0001

# lme5 isn't perfect but it is a noticeable improvement over lme4