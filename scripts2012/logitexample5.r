

# load the data
> nes96 <- read.table("http://www.stat.washington.edu/quinn/classes/536/data/nes96r.dat", header=TRUE)

# load the nnet library which has the multinom function
> library(nnet)
Loading required package: MASS 

# let's change the PID variable into a factor
> nes96$PID <- factor(nes96$PID, labels=c("Strong Democrat", "Weak 
Democrat", "Independent-Democrat", "Independent-Independent", 
"Independent-Republican", "Weak Republican", "Strong Republican"))

# fit a multinomial logit model where PID is the response variable,
# and the predictors are: log(popul+.1), selfLR, age, educ, and income

> multinom.out <- multinom(PID~log(popul+.1)+selfLR+age+educ+income, 
data=nes96) 
# weights:  49 (36 variable)
initial  value 1836.939181 
iter  10 value 1691.507857
iter  20 value 1603.709441
iter  30 value 1523.540117
iter  40 value 1461.935703
final  value 1461.922748 
converged

# let's take a look at the results
> summary(multinom.out, corr=FALSE)

Re-fitting to get Hessian

Call:
multinom(formula = PID ~ log(popul + 0.1) + selfLR + age + educ + 
    income, data = nes96)

Coefficients:
                        (Intercept) log(popul + 0.1)    selfLR          age
Weak Democrat            -0.3733563      -0.01153736 0.2976980 -0.024944529
Independent-Democrat     -2.2509348      -0.08875096 0.3916628 -0.022897526
Independent-Independent  -3.6659051      -0.10596768 0.5735134 -0.014851243
Independent-Republican   -7.6136944      -0.09155519 1.2787425 -0.008680754
Weak Republican          -7.0604314      -0.09328575 1.3469400 -0.017903442
Strong Republican       -12.1051935      -0.14087942 2.0699883 -0.009432601
                                educ      income
Weak Democrat            0.082487696 0.005195818
Independent-Democrat     0.181044184 0.047874118
Independent-Independent -0.007131611 0.057577321
Independent-Republican   0.199828063 0.084495215
Weak Republican          0.216938699 0.080958623
Strong Republican        0.321923127 0.108890412

Std. Errors:
                        (Intercept) log(popul + 0.1)     selfLR         age
Weak Democrat             0.6298384       0.03428246 0.09362654 0.006524873
Independent-Democrat      0.7631917       0.03916169 0.10823837 0.007914493
Independent-Independent   1.1565170       0.05703689 0.15854307 0.011331040
Independent-Republican    0.9575695       0.04379006 0.12889466 0.008418690
Weak Republican           0.8443601       0.03935158 0.11718480 0.007611003
Strong Republican         1.0599179       0.04213748 0.14340364 0.008133748
                              educ     income
Weak Democrat           0.07358680 0.01763372
Independent-Democrat    0.08528965 0.02228102
Independent-Independent 0.12628792 0.03361350
Independent-Republican  0.09412459 0.02619610
Weak Republican         0.08500687 0.02297606
Strong Republican       0.09109678 0.02530048

Residual Deviance: 2923.845 
AIC: 2995.845 

# the results above aren't too surprising-- for instance we see that
# more conservative respondents are more likely to be Republican identifiers
# than Democratic identifiers or independent identifiers. Income has a
# similar effect. Republican identifiers also tend to be better educated
# than other identifiers.
# 

# let's look at some fitted probabilities. To do this, we'll set all of 
# the covariates (except selfLR) equal to their median values and vary 
# selfLR from its low value to it's high value.

> beta <- coef(multinom.out)
> X <- cbind(1, 3.096, 1:7, 44, 4, 17)

> Xb1 <- X %*% beta[1,]
> Xb2 <- X %*% beta[2,]
> Xb3 <- X %*% beta[3,]
> Xb4 <- X %*% beta[4,]
> Xb5 <- X %*% beta[5,]
> Xb6 <- X %*% beta[6,]

> denomsum <- exp(Xb1) + exp(Xb2) + exp(Xb3) + exp(Xb4) + exp(Xb5) + 
exp(Xb6) 

> p0 <- 1/(1+denomsum)
> p1 <- exp(Xb1)/(1+denomsum)
> p2 <- exp(Xb2)/(1+denomsum)
> p3 <- exp(Xb3)/(1+denomsum)
> p4 <- exp(Xb4)/(1+denomsum)
> p5 <- exp(Xb5)/(1+denomsum)
> p6 <- exp(Xb6)/(1+denomsum)

> plot(0:6, p0, xlab="Self LR Placement", ylab="", ylim=c(0,1), type="l", 
col=1, pch=16)
> lines(0:6, p1, col=2)
> lines(0:6, p2, col=3)
> lines(0:6, p3, col=4)
> lines(0:6, p4, col=5)
> lines(0:6, p5, col=6)
> lines(0:6, p6, col=7)
> legend(0, 1, legend=c("Strong Dem.", "Weak Dem.", "Ind. Dem.", "Ind. 
Ind.", "Ind. Rep.", "Weak Rep.", "Strong Rep."), col=1:7, lty=1)



