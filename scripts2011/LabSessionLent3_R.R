# -------------------------------------------------------------------
# MPO1-A Advanced Quantitative Research Methods
# Thilo Klein
# Lab Session 3: Endogeneity, IV and SEM

# Libraries: lmtest, sandwich, AER, systemfit
  setwd("C:/Dokumente und Einstellungen/Thilo/Desktop/Lab_SessionsLT/Data")
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------



# --- Ex 1: IV regression, Sargan and Hausman test (1) -----------------------------
 bond <- read.csv("http://thiloklein.de/R/Lent/bonds.csv", header=T)
 str(bond)


# --- Ex 1: c)  ---

 data.frame(bond$paneldate, bond$dus3mt, c(NA, bond$dus3mt[-600]))
 bond$dus3mt_1 <- c(NA, bond$dus3mt[-600])
 bond$dus3mt_2 <- c(NA, NA, bond$dus3mt[c(-599, -600)])
 data.frame(bond$paneldate, bond$dus3mt, bond$dus3mt_1, bond$dus3mt_2)

 ## checking instrument relevance:
 lm1c <- lm(dus3mt ~ dus3mt_1 + dus3mt_2, data=bond); shccm(lm1c)


# --- Ex 1: e)  ---
# 2SLS

 ## convenience functions: HC0 and HC1 covariances
 hc0 <- function(x) vcovHC(x, type = "HC0")
 hc1 <- function(x) vcovHC(x, type = "HC1")

 lm1e.1 <- lm(dus3mt ~ dus3mt_1 + dus3mt_2, data=bond)
 library(lmtest)
 library(sandwich)
 coeftest(lm1e.1, vcov=hc0)
 linearHypothesis(lm1e.1, c("dus3mt_1=0","dus3mt_2=0"), vcov=hc0)
 lm1e.2 <- lm(daaa ~ lm1e.1$fitted, data=bond[is.na(bond$dus3mt_2)==FALSE,])
 coeftest(lm1e.2, vcov=hc0)

 library(AER)	
 # help("StockWatson2007", package="AER")
 # help("Greene2003")
 # help("Baltagi2002") 
 # help("CameronTrivedi1998")
 # help("Franses1998")
 # ?ivreg
 lm1e.iv <- ivreg(daaa ~ dus3mt | dus3mt_1 + dus3mt_2, data=bond)
 coeftest(lm1e.iv, vcov=hc0)	# errors are different for iv and manual 2SLS!
 # compare to OLS estimates:
 lm1e.ols <- lm(daaa ~ dus3mt, data=bond); coeftest(lm1e.ols, vcov=hc0)


# --- Ex 1: f)  ---
# Sargan test for overidentifying restrictions (validity of instruments)

 ## Sargan test from Stock and Watson (2007)
 lm1f <- lm(lm1e.iv$res ~ dus3mt_1 + dus3mt_2, data=bond[is.na(bond$dus3mt_2)==FALSE,])
 lm1f.or <- linearHypothesis(lm1f, c("(Intercept)=0", "dus3mt_1=0", "dus3mt_2=0"), test="Chisq", vcov=hc0)
 ## WARNING: df (and hence p-value) invalid above.
 ## correct df: no. instruments - no. endogenous variables
 pchisq(q=lm1f.or[2,3], lm1e.iv$df.res - lm1f$df.res, lower.tail = FALSE)

 ## Sargan test from handout, Lab Session 3
 ## Step 2. Auxiliary regression:
 lm.aux <- lm(lm1e.iv$res ~ dus3mt_1 + dus3mt_2, data=bond[is.na(bond$dus3mt_2)==FALSE,])
 R2 <- summary(lm.aux)$r.squared
 LM <- length(lm.aux$res) * R2 
 # LM is chi.sqr_{m-k} m = no. instruments = 3; k = no. regressors = 2
 pchisq(q = LM, 2 - 1, lower.tail = FALSE) 


# --- Ex 1: g)  ---
# Hausman tests for exogeneity of regressors

 ## Hausman test from Baltagi (2002)
 cf_diff <- coef(lm1e.iv) - coef(lm1e.ols)
 vc_diff <- vcov(lm1e.iv) - vcov(lm1e.ols)
 x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)	
 # is Chi^2 with degrees of freedom (df) = rank of matrix vc_diff
 pchisq(q = x2_diff, df = 2, lower.tail = FALSE)

 ## Durbin-Wu-Hausman test from handout, Lab Session 3
 ## Step 1.
 dwh1 <- lm(daaa ~ dus3mt, data=bond)
 bond$e <- dwh1$res
 ## Step 2.
 dwh2 <- lm(dus3mt ~ dus3mt_1 + dus3mt_2, data=bond)
 bond$v <- c(NA, NA, dwh2$res)
 ## Step 3.
 dwh3 <- lm(e ~ dus3mt + v, data=bond); shccm(dwh3)
 ## Lagrange Multiplier Statistic:
 LM <- length(dwh3$res) * summary(dwh3)$r.squared 
 # is Chi^2_{k-k0} with  
 # k = no. regressors = 2; 
 # k0 = no. regressors potentially endogenous = 1.
 # H0: All regressors are exogenous. p-value of the LM statistic:
 pchisq(q = LM, df = 1, lower.tail = FALSE)

 ## http://www.lsw.wiso.uni-erlangen.de/studium/grundstudium/07ss/statistik2/uebung/11_code.R
 lm1g <- lm(dus3mt ~ dus3mt_1 + dus3mt_2, data=bond)
 summary( lm(daaa ~ dus3mt + lm1g$res, data=bond[is.na(bond$dus3mt_2)==FALSE,]) )
 summary( lm(daaa ~ dus3mt + lm1g$res, data=bond[is.na(bond$dus3mt_2)==FALSE,]) )

# What did we do?
# 1. Made price exogenous by creating a new price variable. This variable is formed by 
# regressing all the exogenous variables on price, and taking the fitted value as the new 
# variable. The individual exogenous variables (as well as the new fitted value) are called 
# instrumental variables, or instruments. Since the new variable is created from exogenous 
# variables, it should not be correlated with the disturbance term, and can be considered 
# exogenous.
# 2. Ran a regression for the demand function in which you included the residual left over when 
# you created the new (exogenous) price variable. The Hausman test is simple: look at the 
# t-statistic on the residual's coefficient. Your null hypothesis is that there is no 
# endogeneity. If the t-statistic is high enough, you can reject the null hypothesis.




# --- Ex 2: IV regression, Sargan and Hausman test (2) -----------------------------
 mroz <- read.csv("http://thiloklein.de/R/Lent/mroz.csv", header=T)
 str(mroz)

# --- Ex 2: a)  ---
# Graph a histogram for wage and lwage, and comment on the shape of the distribution.

 table(ifelse(mroz$hours>0,1,0))
 # There are a total of 753 women in the sample of which 428 work.
 # Note that log-wages are missing for those women that don't work.

 par(mfrow=c(1,2))
 hist(mroz$wage)
 # The distribution of wages is clearly skewed to the right, a large tail of high 
 # wages, with a maximum of $25. Remember: the mean is $4.18.

 hist(mroz$lwage)
 # The distribution of log wages is more symmetric. Wages are often lognormally
 # distributed, which means that log wages are normally distributed. These are
 # typical patterns for wages.


# --- Ex 2: b)  ---
# For the remainder of this exercise, retain only the women that work in the data set.

 mroz.w <- subset(mroz, hours>0)


# --- Ex 2: c)  ---
# Estimate the model: lwage ~ educ + exper + I(exper^2). How would you interpret 
# the coefficients? Does estimated values make sense? Which kind of problems may 
# these present?

 lm2c.ols <- lm(lwage ~ educ + exper + I(exper^2), data=mroz.w); shccm(lm2c.ols)
 # The OLS coeffient on educ suggests an 11% return. The average return to log
 # wages of an extra year of experience is given by 0.042-(0.0008*2)*exper = 0.021 
 # at the mean of exper (=13.03), or a 2.5% increase in wages for an additional year 
 # of experience at that level.

 # Omitted ability is probably biasing the results upwards. Ability is correlated
 # with education but it is not in the regression (then is in the error term). Then 
 # educ is correlated with the error term. In other words educ is endogenous.


# --- Ex 2: d)  ---
# Estimate the model by manual 2SLS (do not use ivreg yet), using fatheduc and 
# motheduc as instruments for educ. How do the results differ from those obtained 
# with educ?

 ## 1st stage regression:
 lm2d.1 <- lm(educ ~ fatheduc + motheduc, data=mroz.w)
 # fatheduc and motheduc both explain educ, the reduced form coefficients are significant
 ## 2nd stage:
 # lm2d.1$fitted acts as an instrumental variable for educ.
 lm2d.2 <- lm(lwage ~ lm2d.1$fitted + exper + I(exper^2), data=mroz.w)
 # There is a reduction in the return to education to 6%. This was expected if the
 # effect was to collect the bias created by the omitted variable ability, as the 
 # bias with the OLS regression should be positive. Of course, other IVs could
 # have delivered different estimations of the rate of return. If this were the
 # case other explanations should be presented. These could be, for instance, that 
 # IV captures some error in measurement or others.

 coeftest(lm2d.2, vcov=hc0)
 # compare to 2SLS to OLS:
 coeftest(lm2c.ols,vcov=hc0)


# --- Ex 2: e)  ---
# Do fatheduc and motheduc explain educ? Are these good instruments?

 shccm(lm2d.1)


# --- Ex 2: f)  ---
# A problem with 2SLS is that the standard errors in the second stage are wrong. 
# The correct standard errors are provided by the ivreg command in R. 
# Comment on the differences.

 lm2f.iv <- ivreg(lwage ~ educ + exper + I(exper^2) 
	| exper + I(exper^2) + fatheduc + motheduc, data=mroz.w)
 coeftest(lm2f.iv, vcov=hc0)	# errors are different for ivreg and manual 2SLS!
 # compare to manual 2SLS estimates and OLS:
 coeftest(lm2d.2, vcov=hc0)
 coeftest(lm2c.ols,vcov=hc0)
 # There are only very minor changes in the standard errors.
 # Note that the se for educ is quite a bit larger than the OLS one.


# --- Ex 2: g)  ---
# Perform the test for overidentifying restrictions, i.e.: whether the instruments 
# are uncorrelated with u.

 # Sargan test from Stock and Watson (2007)
 lm2g.or <- lm(lm2f.iv$res ~ exper + I(exper^2) + fatheduc + motheduc, data=mroz.w)
 lm2g.or.test <- linearHypothesis(lm2g.or, c("fatheduc=0", "motheduc=0"), test="Chisq", vcov=hc0)
 ## WARNING: df (and hence p-value) invalid above.
 ## correct df: # instruments - # endogenous variables
 pchisq(q=lm2g.or.test[2,3], df = lm2f.iv$df.res - lm2g.or$df.res, lower.tail=F)

 ## Sargan test from handout, Lab Session 3
 ## Step 2. Auxiliary regression:
 lm.aux <- lm(lm2f.iv$res ~ exper + I(exper^2) + fatheduc + motheduc, data=mroz.w)
 R2 <- summary(lm.aux)$r.squared
 LM <- length(lm.aux$res) * R2 
 # LM is chi.sqr_{m-k} m = no. instruments = 5; k = no. regressors = 4
 pchisq(q = LM, df = 5-4, lower.tail=F) 

 # The p-values are 0.5088 and 0.5386 respectively. We don't reject that the 
 # instruments are valid in that they are not correlated with the errors.


# --- Ex 2: h)  ---
# Test for endogeneity of educ.

 ## Durbin-Wu-Hausman test from handout, Lab Session 3
 ## Step 1. Create OLS residuals. Already done! 
 e <- lm2c.ols$res
 ## Step 2. Regress every endogenous regressor (in this case only educ) 
 ## on z (constant, fatheduc, motheduc, exper, expersq) and obtain residuals. Done!
 v <- lm2h.1$res
 ## Step 3.
 dwh3 <- lm(e ~ educ + exper + expersq + v, data=mroz.w); shccm(dwh3)
 ## Lagrange Multiplier Statistic:
 LM <- length(dwh3$res) * summary(dwh3)$r.squared 
 # is Chi^2_{k-k0} with  
 # k = no. regressors = 2; 
 # k0 = no. regressors potentially endogenous = 1.
 # H0: All regressors are exogenous. p-value of the LM statistic:
 pchisq(q = LM, df = 1, lower.tail = F)

 ## Or:
 lm2h.1 <- lm(educ ~ fatheduc + motheduc + exper + I(exper^2), data=mroz.w)
 lm2h.2 <- lm(lwage ~ educ + exper + I(exper^2) + lm2h.1$res, data=mroz.w)
 coeftest(lm2h.2, vcov=hc0)
 # The coefficient on lm2h.i$res is not significant at 10%, and so endogeneity 
 # is debatable.

 ## Hausman test for exogeneity of regressors:
 dwh.test <- function(model.iv, model.ols){
	 cf_diff <- coef(model.iv) - coef(model.ols)
	 vc_diff <- vcovHC(model.iv, "HC0") - vcovHC(model.ols, "HC0")
	 x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
	 pchisq(q = x2_diff, df = dim(vc_diff)[1], lower.tail = F)
 }
 # execute dwh.test-function:
 dwh.test(lm2f.iv, lm2c.ols)

 # As they were designed the DWH test (p-value 0.09) and the Hausman test (0.16)
 # do not reject the hypothesis of exogeneity at 5%. Note, however, that also 
 # exper and expersq could be potentially endogenous. Other instruments should be 
 # added, should we consider them as such.




# --- Ex 3:  -----------------------------

 crime <- read.csv("http://thiloklein.de/R/Lent/crime.csv", header=T)
 str(crime)

# --- Ex 3: a)  ---
# Observe the data. Regress crime on a constant and police. Give a possible 
# explanation of the estimated positive effect. 

 plot(crime ~ police, data=crime)
 lm3a.ols <- lm(crime ~ police, data=crime); shccm(lm3a.ols)
 abline(lm3a.ols, col="red", lwd=2)


# --- Ex 3: d)  ---
# Use the data to estimate coefficients by instrumental variables, using z (and a 
# constant) as instruments. Check that the result of c) holds true. Provide an 
# interpretation of the resulting estimate.

 lm3e.iv <- ivreg(crime ~ police | election, data=crime)
 coeftest(lm3e.iv)

 # Applying property in c)
 y1 <- mean(crime$crime[crime$election==1])
 x1 <- mean(crime$police[crime$election==1])
 y0 <- mean(crime$crime[crime$election==0])
 x0 <- mean(crime$police[crime$election==0])
 b.iv <- (y1-y0)/(x1-x0)

 paste("IV estimation through formula for dummy IVs:", b.iv)
 # now the sign is as expected!


# --- Ex 3: e)  ---
# Perform the Hausman test on the exogeneity of the variable x.

 dwh.test(lm3e.iv, lm3a.ols)
 abline(lm3e.iv, col="blue", lwd=2)
 legend("topleft", legend=c("OLS","2SLS"), fill=c("red","blue"))




# --- Ex 4:  -----------------------------
 bw <- read.csv("http://thiloklein.de/R/Lent/brwght.csv", header=T)
 str(bw)

# --- Ex 4: a)  ---
# Estimate using OLS.

 lm4.ols <- lm(lbwght ~ packs, data=bw)
 coeftest(lm4.ols)


# --- Ex 4: d)  ---
# Estimate by IV using cigprice as an instrument.

 lm4.iv <- ivreg(lbwght ~ packs | cigprice, data=bw)
 coeftest(lm4.iv)

 lm4.1 <- lm(packs ~ cigprice, data=bw); coeftest(lm4.1)




# --- Simultaneous equation models (SEM) ---
# --- Ex 5:  -----------------------------
# --- Ex 5: c) ---
# Simulate n=100 observations from this model with a=0, b=0.5 (therefore the 
# multiplier is equal to 2), the perturbations are distributed as standard normals, 
# G is distributed normal with mean 10 and variance 1.  Applying the formula above: 
# plim(b) = 0.5 + 0.5/3 = 0.67, and the multiplier would be around 3. Note that 
# estimating the consumption equation by OLS would overestimate the real multiplier, 
# which is 2. (Try again with a sample of 50 and 10000 and note the differences with 
# the predicted values).

 epsilon1 <- rnorm(1000)
 epsilon2 <- rnorm(1000)
 g <- 10 + rnorm(1000)

 d <- (1/(1-0.5))*g + (epsilon1 + epsilon2)/(1-0.5)  
 # d was created using the reduced form.
 c <- 0.5*d + epsilon1

 lm5c <- lm(c ~ d)
 beta <- lm5c$coef[2]
 multiplier <- 1/(1-beta)
 paste("The multiplier estimated with OLS is equal to", multiplier)
 # spot on!


# --- Ex 5: d) ---
# Apply instrumental variables to obtain better estimators. Report the value for 
# the multiplier estimated.

 # As g is exogenous and we need two instruments, we use g and a constant as instruments.
 sim <- data.frame(c=c, d=d, g=g)
 lm5d <- ivreg(c ~ d | g, data=sim)
 beta <- lm5d$coef[2]
 multiplier <- 1/(1-beta)
 paste("The multiplier estimated with IV is equal to", multiplier)
 # spot on!




# --- Ex 6:  -----------------------------
 orange <- read.csv("http://thiloklein.de/R/Lent/oranges.csv", header=T)
 str(orange)


# --- Ex 6: a) ---
# Estimate the price equation using OLS. Test the null hypothesis that 
# price elasticity = -1.

 lm6a.ols <- lm(logpt ~ logqt + logrit, data=orange)
 linearHypothesis(lm6a.ols, "logqt = -1", vcov=hc0)
 # The p-value for the F-statistic is equal to 0.04, therefore the null is
 # rejected at 5%-level.


# --- Ex 6: b) ---
# Estimate the price equation also by IV, using log(ACt) and log(APt) as 
# instruments for log(Q). Test again the null hypothesis of unit price elasticity.

 lm6b.iv <- ivreg(logpt ~ logqt + logrit | logac + logap + logrit, data=orange)
 linearHypothesis(lm6b.iv, "logqt = -1", vcov=hc0)
 # The p-value for the F-statistic is now equal to 0.002, therefore the null is
 # rejected at 1% level of significance.


# --- Ex 6: c) ---
# Perform the Hausman test for the exogeneity of log(Qt) in the price equation.

 dwh.test(lm6b.iv, lm6a.ols)

 # by yourself:
 # Step 1.
 lm6c.i <- lm(logpt ~ logqt + logrit, data=orange) 
 ehat <- lm6c.i$res

 # Step 2.
 lm6c.ii <- lm(logqt ~ logrit + logac + logap, data=orange)
 vhat <- lm6c.ii$res

 # Step 3.
 lm6c.iii <- lm(ehat ~ logqt + logrit + vhat, data=orange)
 LM <- dim(model.matrix(lm6c.iii))[1] * summary(lm6c.ii)$r.sq

 paste("LM statistic is =", LM)
 paste("p-value of the LM statistic =", pchisq(q = LM, df=1, lower.tail=F) )

 # The results are not clear. With DWH test, we reject exogeneity at a 1% level
 # with Hausman test we do not reject. 


# --- Ex 6: d) ---
# Investigate the quality of the instruments, that is, whether they are 
# sufficiently correlated with log(Q_t) and uncorreated with the price shocks e_t 
# (take the IV residuals as estimates of the shocks).

 lm6d <- lm(logqt ~ logrit + logac + logap, data=orange)
 shccm(lm6d)
 # The R2 is .93 proof of a good correlation between endogenous variable and
 # instruments. Only past advertisements are insignificant.

 # To test for the exogeneity of the instruments we apply a Sargan test.
 lm6d.or <- lm(lm6b.iv$res ~ logac + logap + logrit, data=orange)
 lm6d.or.test <- linearHypothesis(lm6d.or, c("logac=0", "logap=0", "logrit"), test="Chisq", vcov=hc0)
 ## WARNING: df (and hence p-value) invalid above.
 ## correct df: # instruments - # endogenous variables
 pchisq(q=lm6d.or.test[2,3], df = lm6b.iv$df.res - lm6d.or$df.res, lower.tail=F)

 # Therefore the hypothesis of instruments being exogenous is rejected. One
 # possible explication is that there is some simultaneous determination of 
 # quantities and advertisement expenditures (example: high prices raising proffits 
 # and then advertisement and quantities supplied.
 
# --- Ex 6: e) ---
# Answer questions b), c) and d) also for the n=45 observations obtained by 
# excluding the data over the period 1942-6.

 # Left for the student, the conclusions are in line with what we observed so far.


# --- Ex 6: f) ---
# Is the demand equation identified? Estimate this equation by OLS, and motivate 
# your choice.

 # to answer this question we apply the order condition: k - k1 >= m1

 # where
 # k = total number of exogenous variables in the model;
 # k1 = total number of exogenous variables in the first equation;
 # m1 = number of endogenous variables in the same equation.

 # k = 2: the constant and the variable log(RI).
 # When we analyse the demand equation: the exogenous variables are the same, so 2 
 # again.
 # m1 = 1: log(Q). The order condition is not satisfied: 0<1. Then the demand
 # equation is not identified.

 # The estimation by OLS was already discussed in parts a, b, c. The equation 1
 # cannot be estimated as part of a
 # model as the SEM designed for parts f and g.


# --- Ex 6: g) ---
# Is the supply equation identified? Estimate this equation by a method that you 
# find most appropriate, and motivate your choice.

# The order condition is now satisfied. k = 2; k2 = 1; m2 = 1. Now: k - k2 = 1 = m2
# the reason to be satisfied is that it satisfies the exlcusion condition, as
# log(RI) is excluded.

# to estimate it within the scope of this model we should apply two-stage
# least squares using the constant and log(RI) as instrumets.

 lm6g.iv <- ivreg(logpt ~ logqt | logrit, data=orange)
 coeftest(lm6g.iv)

 # The slope is now negative, contrary to what it should have been expected,
 # and insignificant. So estimations in part c) (with more IVs) seem to be the 
 # best estimations for this model.




# --- Ex 7:  -----------------------------
 smoke <- read.csv("http://thiloklein.de/R/Lent/smoke.csv", header=T)
 str(smoke)


# --- Ex 7: d) ---
# Estimate the income equation by OLS and discuss the estimate of beta_1.

 lm7d <- lm(lincome ~ cigs + educ + age + agesq, data=smoke)
 shccm(lm7d)


# --- Ex 7: e) ---
# Estimate the reduced form for cigs. (Recall that this entails regressing cigs 
# on all exogenous variables.) Are log(cigpric) and restaurn significant in the 
# reduced form?

 lm7e <- lm(cigs ~ educ + age + agesq + lcigpric + restaurn, data=smoke)
 coeftest(lm7e)
 linearHypothesis(lm7e, c("lcigpric=0", "restaurn=0"), vcov=hc0)


# --- Ex 7: f) ---
# Now, estimate the income equation by 2SLS. Discuss how the estimate of beta_1
# compares with the OLS estimates.

 lm7f.iv <- ivreg(lincome ~ cigs + educ + age + agesq 
	| lcigpric + restaurn + educ + age + agesq, data=smoke)
 coeftest(lm7f.iv, vcov=hc0)




# --- Ex 8:  -----------------------------
 open <- read.csv("http://thiloklein.de/R/Lent/openness.csv", header=T)
 str(open)


# --- Ex 8: a) ---
# To confirm the last assertion, estimate the reduced form for open. Is the first 
# equation identified? Estimate it using a constant and log(land) as IVs (2SLS).

 lm8a.ols <- lm(open ~ lpcinc + lland, data=open); coeftest(lm8a.ols)
 lm8a.iv <- ivreg(inf ~ open + lpcinc | lland + lpcinc, data=open); coeftest(lm8a.iv)

 # Note that the coefficient on open is statistically significant and economically 
 # important, and the sign of alpha1 negative, as expected.


# --- Ex 8: b) ---
# Because log(pcinc) is insignificant in both estimations so far [CHECK THIS], 
# drop it from the analysis. Estimate by OLS and IV without log(pcinc). Do any 
# important conclusions change?

 lm8b.iv <- ivreg(inf ~ open | lland, data=open); coeftest(lm8b.iv)


# --- Ex 8: c) ---
# Still leaving log(pcinc) out of the analysis, is land or log(land) a better 
# instrument for open? (Hint: regress open on each of these separately and jointly).

 lm8c.i <- lm(open ~ land, data=open); coeftest(lm8c.i, vcov=hc0)
 lm8c.ii <- lm(open ~ lland, data=open); coeftest(lm8c.ii, vcov=hc0)
 lm8c.iii <- lm(open ~ land + lland, data=open); coeftest(lm8c.iii, vcov=hc0)


# --- Ex 8: d) ---
# Add the dummy variable oil (indicative of the country being an oil-producer) to 
# the original equation in a and treat it as exogenous. Estimate the equation by IV. 
# Does being an oil producer have a ceteris paribus effect on inflation?

 lm8d.iv <-ivreg(inf ~ open + lpcinc + oil | lland + lpcinc + oil, data=open)
 coeftest(lm8d.iv)



# --- Ex 9:  -----------------------------
 cement <- read.csv("http://thiloklein.de/R/Lent/cement.csv", header=T)
 str(cement)


# --- Ex 9: a) ---

 cement$month <- as.factor(cement$month)
 lm9a <- lm(gprc ~ gcem + gprcpet + month, data=cement); shccm(lm9a)


# --- Ex 9: b) ---

 lm9b <- lm(gcem ~ gdefs + gprcpet + month, data=cement)


# --- Ex 9: c) ---

 lm9c <- lm(gcem ~ gres + gnon + gprcpet + month, data=cement)


# --- Ex 9: d) ---

 lm9d.iv <- ivreg(gprc ~ gcem + gprcpet + month | gprcpet + month + gres + gnon, data=cement)




# --- Ex 10:  -----------------------------
 fish <- read.csv("http://thiloklein.de/R/Lent/fish.csv", header=T)
 str(fish)


# --- Ex 10: c) ---
 fish$weekday <- ifelse(fish$mon==1,"Mon", 
			ifelse(fish$tues==1,"Tue", 
			ifelse(fish$wed==1,"Wed",
			ifelse(fish$thurs==1,"Thu","Fri"))))
 fish$weekday <- as.factor(fish$weekday)
 levels(fish$weekday)

 lm10c <- lm(lavgprc ~ weekday + wave2 + wave3, data=fish); coeftest(lm10c, vcov=hc0)
 linearHypothesis(lm10c, c("wave2=0","wave3=0"), vcov=hc0)


# --- Ex 10: d) ---
# Now, estimate the demand equation by 2SLS. What is the 95% confidence interval for the 
# price elasticity of demand? Is the estimated elasticity reasonable?

 library(systemfit)
 ?systemfit

 ## Specify the system
 eqDemand <- ltotqty ~ lavgprc + weekday 
 eqSupply <- lavgprc ~ ltotqty + wave2 + wave3 
 system <- list( demand = eqDemand, supply = eqSupply )
 inst <- ~ wave2 + wave3 + weekday 

 ## OLS estimation
 lm10d.ols <- systemfit(system, data=fish)
 coeftest(lm10d.ols)

 ## 2SLS estimation
 lm10d.sem <- systemfit(system, "2SLS", inst=inst, data=fish) 
 coeftest(lm10d.sem)

 ## (Same results for demand equation with ivreg)
 lm10d.ols2 <- lm(ltotqty ~ lavgprc + weekday, data=fish)
 lm10d.iv2 <- ivreg(ltotqty ~ lavgprc + weekday | weekday + wave2 + wave3, data=fish)

 ## (2SLS estimation with different instruments in each equation)
 lm10d.sem$eq[[1]]; lm10d.sem$eq[[2]]
 inst1 <- ~ weekday
 inst2 <- ~ wave2 + wave3
 instlist <- list( inst1, inst2 ) 
 
 lm10d.sem2 <- systemfit(system, "2SLS", inst=instlist, data=fish) 
 coeftest(lm10d.sem2)

 # Note this is another way to estimate 2sls. Should we not have given the 2sls
 # option R would have estimated employing 3sls, this model first estimates a 2sls, 
 # then with the correlation between perturbations ACROSS equations it estimates
 # the covariance matrix of the perturbations across equations, which is used in a
 # third step to estimate the parameters.

 ## 3SLS estimation
 lm10d.3sls <- systemfit( system, "3SLS", inst = inst, data = fish, method3sls = "GMM" )
 coeftest(lm10d.3sls)
 hausman.systemfit(lm10d.sem, lm10d.3sls)
 # no significant difference between 2SLS and 3SLS.

 # In general, the results are more efficient, but it has two flaws: 1. it relies on all
 # equations being well modeled. errors in modelling any of them will affect estimations  
 # in all the rest of the equations (2sls is free from this problem);
 # 2. it relies in big datasets. as it has to estimate a large number of covariances.


# --- Digression: What about HC-standard errors?  ---
# based on Wooldridge (2002) "Econometric analysis of cross section and panel data", 
# Chapter 5, page 100, formula 5.34

 source("http://thiloklein.de/R/myfunctions.R")
 shccm.sysf(lm10d.sem)
 shccm.sysf(lm10d.3sls)


# --- Ex 10: e) ---

 lm10e <- lm(lavgprc ~ weekday + wave2 + wave3, data=fish); coeftest(lm10e, vcov=hc0)
 linearHypothesis(lm10e, c("weekdayMon=0","weekdayTue=0","weekdayWed=0","weekdayThu=0"), vcov=hc1)




# -------------------------------------------------------------------
# --- End of Session ------------------------------------------------

 save.image("EndOfSession.RData")
 q("yes")



