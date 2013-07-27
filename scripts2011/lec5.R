# -------------------------------------------------------------------
# MPO1-A Advanced Quantitative Research Methods
# Lecture 5: Endogeneity and IV

# Libraries: lmtest, sandwich, AER, systemfit
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------



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
 # wages of an extra year of experience is given by 0.042-(0.0008^2)*exper = 0.021 
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
 #lm2d.1 <- lm(educ ~ fatheduc + motheduc + exper + I(exper^2), data=mroz.w)
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

 library(lmtest)
 library(sandwich)
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

 library(AER)	
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
 #lm2g.or <- lm(lm2f.iv$res ~ exper + I(exper^2) + fatheduc + motheduc, data=mroz.w)
 #lm2g.or.test <- linearHypothesis(lm2g.or, c("fatheduc=0", "motheduc=0"), test="Chisq", vcov=hc0)
 ## WARNING: df (and hence p-value) invalid above.
 ## correct df: # instruments - # endogenous variables
 #pchisq(q=lm2g.or.test[2,3], df = lm2f.iv$df.res - lm2g.or$df.res, lower.tail=F)

 ## Sargan LM test from handout, Lab Session 3
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
 lm2h.1 <- lm(educ ~ fatheduc + motheduc + exper + I(exper^2), data=mroz.w)
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
 #lm2h.1 <- lm(educ ~ fatheduc + motheduc + exper + I(exper^2), data=mroz.w)
 #lm2h.2 <- lm(lwage ~ educ + exper + I(exper^2) + lm2h.1$res, data=mroz.w)
 #coeftest(lm2h.2, vcov=hc0)
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



