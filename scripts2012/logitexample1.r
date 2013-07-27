# read data in
> HLdata <- read.table("lowbwt.dat", header=FALSE, col.names=c("id", 
"low", "age", "lwt", "race", "smoke", "ptl", "ht", "ui", "ftv", "bwt"))

# take a look at first 3 rows of dataset 
> HLdata[1:3,]  
id low age lwt race smoke ptl ht ui ftv  bwt
1  4   1  28 120    3     1   1  0  1   0  709
2 10   1  29 130    1     0   0  0  1   2 1021
3 11   1  34 187    2     1   0  1  0   0 1135

# fit a logit model with low as the dep. var. and age, lwt, and smoke
# as the covariates
> logit.out <- glm(low~age+lwt+smoke, family=binomial(link=logit), 
data=HLdata)


# fit a probit model with low as the dep. var. and age, lwt, and smoke
# as the covariates
> probit.out <- glm(low~age+lwt+smoke, family=binomial(link=probit), 
data=HLdata)


# take a look at the logit results
> summary(logit.out)

Call:
glm(formula = low ~ age + lwt + smoke, family = binomial(link = logit), 
    data = HLdata)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.2829  -0.8650  -0.6938   1.2624   2.0103  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)  
(Intercept)  1.368226   1.014106   1.349   0.1773  
age         -0.038995   0.032719  -1.192   0.2333  
lwt         -0.012139   0.006134  -1.979   0.0478 *
smoke        0.670763   0.325827   2.059   0.0395 *
---
Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 234.67  on 188  degrees of freedom
Residual deviance: 222.88  on 185  degrees of freedom
AIC: 230.88

Number of Fisher Scoring iterations: 3



# take a look at the probit results
> summary(probit.out)

Call:
glm(formula = low ~ age + lwt + smoke, family = binomial(link = probit), 
    data = HLdata)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.2785  -0.8714  -0.6891   1.2587   2.0285  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)  
(Intercept)  0.818464   0.596821   1.371   0.1703  
age         -0.024405   0.019424  -1.256   0.2090  
lwt         -0.007215   0.003538  -2.039   0.0414 *
smoke        0.416977   0.197264   2.114   0.0345 *
---
Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 234.67  on 188  degrees of freedom
Residual deviance: 222.67  on 185  degrees of freedom
AIC: 230.67

Number of Fisher Scoring iterations: 3


# extract just the coefficients from the logit output object
> coefficients(logit.out)
(Intercept)         age         lwt       smoke 
 1.36822568 -0.03899458 -0.01213854  0.67076320 

# put the logit coefficients in a new object called beta.logit
> beta.logit <- coefficients(logit.out)


# plot low on age adding some jitter to low
> plot(HLdata$age, jitter(HLdata$low, .1) )

# Now we're going to plot the predicted probabilities of a 120 lb. woman
# who is a smoker giving birth to a low birthweight child at different 
# ages. First we need to construct a new matrix of covariate values that 
# corresponds to our hypothetical women.
> X <- cbind(1, seq(from=14, to=45, by=1), 120, 1)

# multiply this matrix by out logit coefficients to get the value of the 
# linear predictor.
> Xb <- X %*% beta.logit

# Now use the logistic cdf to transform the linear predictor into 
# probabilities
> prob <- exp(Xb)/(1+exp(Xb))


# now plot these probabilities as a function of age on the pre-existing 
# graph of low on age
> lines(seq(from=14, to=45, by=1), prob, col="red")


# Now we're going to plot the predicted probabilities of a 120 lb. woman
# who is NOT a smoker giving birth to a low birthweight child at different 
# ages. First we need to construct a new matrix of covariate values that 
# corresponds to our hypothetical women.
> X <- cbind(1, seq(from=14, to=45, by=1), 120, 0)

# multiply this matrix by out logit coefficients to get the value of the 
# linear predictor.
> Xb <- X %*% beta.logit

# Now use the logistic cdf to transform the linear predictor into 
# probabilities
> prob <- exp(Xb)/(1+exp(Xb))


# now plot these probabilities as a function of age on the pre-existing 
# graph of low on age
> lines(seq(from=14, to=45, by=1), prob, col="blue")


# create a 3-d plot
>  weight <- seq(from=80, to=250, length=100)
>  age <- seq(from=14, to 45, length=100)
>  logit.prob.fun <- function(weight, age){
	exp(1.368226 -0.038995*age -0.012139*weight + 0.670763) /
	(1 + exp(1.368226 -0.038995*age -0.012139*weight + 0.670763))
}

> prob <- outer(weight, age, logit.prob.fun)

> persp(age, weight, prob, theta=30, phi=30, expand=0.5, col="lightblue")