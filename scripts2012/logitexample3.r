
# let's use the low birthweight data again. 
# Taking a look at the first 3 rows we see:

> HLdata[1:3,]
  id low age lwt race smoke ptl ht ui ftv  bwt
1  4   1  28 120    3     1   1  0  1   0  709
2 10   1  29 130    1     0   0  0  1   2 1021
3 11   1  34 187    2     1   0  1  0   0 1135


# fit a logit model

> logit1.out <- glm(low~age+lwt+smoke+ht+ui, family=binomial, data=HLdata)
> summary(logit1.out)
Call:
glm(formula = low ~ age + lwt + smoke + ht + ui, family = binomial, 
    data = HLdata)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.6549  -0.8356  -0.6462   1.1251   1.9925  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)   
(Intercept)  1.399788   1.079770   1.296  0.19485   
age         -0.034073   0.033662  -1.012  0.31144   
lwt         -0.015447   0.006581  -2.347  0.01891 * 
smoke        0.647539   0.336548   1.924  0.05435 . 
ht           1.893272   0.683215   2.771  0.00559 **
ui           0.884606   0.443959   1.993  0.04631 * 
---
Signif. codes:  0 `***' 0.001 `**' 0q.01 `*' 0.05 `.' 0.1 ` ' 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 234.67  on 188  degrees of freedom
Residual deviance: 211.78  on 183  degrees of freedom
AIC: 223.78

Number of Fisher Scoring iterations: 3


# fit another logit model including race

> HLdata$AfrAm <- HLdata$race==2
> HLdata$othrace <- HLdata$race==3
> logit2.out <- glm(low~age+lwt+AfrAm+othrace+smoke+ht+ui, 
                    family=binomial, data=HLdata)
> summary(logit2.out		     )

Call:
glm(formula = low ~ age + lwt + AfrAm + othrace + smoke + ht + 
    ui, family = binomial, data = HLdata)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.7323  -0.8329  -0.5345   0.9869   2.1673  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)   
(Intercept)  0.437166   1.189626   0.367  0.71326   
age         -0.018254   0.035289  -0.517  0.60497   
lwt         -0.016284   0.006839  -2.381  0.01726 * 
AfrAmTRUE    1.280595   0.525871   2.435  0.01488 * 
othraceTRUE  0.901839   0.433378   2.081  0.03744 * 
smoke        1.027530   0.393022   2.614  0.00894 **
ht           1.857574   0.687974   2.700  0.00693 **
ui           0.895376   0.448023   1.999  0.04566 * 
---
Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 234.67  on 188  degrees of freedom
Residual deviance: 203.95  on 181  degrees of freedom
AIC: 219.95

Number of Fisher Scoring iterations: 3


# OK, let's conduct a likelihood ratio test of model 1 vs. model 2
# Here the constrained model is model 1 and the unconstrained model is 
# model 2. Since 2 constraints are applied, the test statistic under 
# the null follows a chi-square distribution with 2 degrees of freedom

> lr <- deviance(logit1.out)  - deviance(logit2.out)
> lr
[1] 7.829775
> 1 - pchisq(lr, 2)
[1] 0.01994279

# The p-value of 0.0199 indicates that there is reason to believe 
# (at the 0.02 level) that the constraints implied by model 1 do
# not hold. 


# We could also look at BIC to pick models. The AIC() function in R
# will return BIC values if the argument k is set to log(n)

> nrow(HLdata)
[1] 189
> bic1 <- AIC(logit1.out, k=log(189))
> bic2 <- AIC(logit2.out, k=log(189))
> bic1
[1] 243.2283
> bic2
[1] 245.8820
> bic2 - bic1
[1] 2.653719

# This indicates moderate support for model 1 over model 2. Nonetheless, 
# given that we have strong reason to believe that race should be in the 
# model we may well want to stick with model 2. 

# Let's conduct a Wald test of whether the coefficients on smoking and 
# hypertension are equal to each other in the second model

> Q <- matrix(c(0,0,0,0,0,1,-1,0), 1, 8)
> beta <- coef(logit2.out)
> r <- 0
> V <- vcov(logit2.out)
> W <- t(Q %*% beta - r) %*% solve(Q %*% V %*% t(Q)) %*% (Q %*% beta - r)
> W
         [,1]
[1,] 1.107668
> 1 - pchisq(W, 1)
          [,1]
[1,] 0.2925894

# the p-value of 0.293 suggests that there is no reason to believe that
# the null hypothesis (that the coefficients are equal) is not true. 


