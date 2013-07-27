# -------------------------------------------------------------------
# Master in Finance Econometrics Module
# Thilo Klein
# Lab Session 3: The Generalized Linear Regression Model

# Required libraries: VGAM, zoo, timeDate, tseries
  setwd("C:/Dokumente und Einstellungen/Thilo/Desktop/LabSessions_MFin/Session3/Data")
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------




# --- Ex 1: Heteroskedasticity (1) ----------------
# --- Ex 1: a) ---
# Use the data in hprice1.csv to obtain heteroskedasticity-robust standard errors 
# and homoskedastic-only standard errors for equation: price ~ lotsize + sqrft + bdrms.
# Discuss any important difference with the usual homoskedasticity-only standard errors.

 house <- read.csv("http://thiloklein.de/R/hprice1", header=T)
 str(house)

 lm1a <- lm(price ~ lotsize + sqrft + bdrms, data=house)
 summary(lm1a)
 shccm(lm1a)


# --- Ex 1: b) ---
# Repeat part a) for log(price) ~ log(lotsize) + log(sqrft) + bdrms.

 lm1b <- lm(lprice ~ llotsize + lsqrft + bdrms, data=house)
 summary(lm1b)
 shccm(lm1b)


# --- Ex 1: c) ---
# What does this example suggest about heteroskedasticity and the transformation used 
# for the dependent variable?

 # refer to handout.


# --- Ex 1: d) ---
# Apply the full White-test for heteroskedasticity to part b). Which variables does it 
# apply? Using the chi-squared form of the statistic, obtain the p-value. What do you 
# conclude?

 house$lm1b.sqres <- lm1b$residuals^2
 lm1b.white.test <- lm(lm1b.sqres ~ llotsize*lsqrft*bdrms - llotsize:lsqrft:bdrms
	+ I(llotsize^2) + I(lsqrft^2) + I(bdrms^2), data=house); shccm(lm1b.white.test)
 T <- summary(lm1b.white.test)$r.squared * nrow(house)
 library(VGAM)
 pchisq(q=T, df=9, lower.tail=F)	# =0.39 -> little evidence against homoskedasticity assumption




# --- Ex 2: Heteroskedasticity (2) ----------------
 training <- read.csv("http://thiloklein.de/R/training", header=T)
 str(training)


# --- Ex 2: a) ---
# Consider the simple regression model log(scrap)=b_1 + b_2 grant + u, where scrap is 
# the firm scrap rate, and grant is a dummy variable indicating whether a firm received 
# a job training grant. Can you think of sum reasons why the unobserved factors in u 
# might be correlated in grant?

 lm2a <- lm(log(scrap) ~ grant, data=training); summary(lm2a)
 par(mfrow=c(2,2)) 
 plot(lm2a)


# --- Ex 2: b) ---
# Estimate a simple regression model. Does receiving a job-training grant significantly 
# lower a firm's scrap rate?

 lm2b <- lm(log(scrap) ~ grant, data=training); summary(lm2b)


# --- Ex 2: c) ---
# Now, add as an a explanatory variable log(scrap_1) (this variable is the scrap rate 
# of the previous year). How does this change the estimated effect of grant? Is it 
# statistically significant at the 5% level? 

 lm2c <- lm(log(scrap) ~ grant + log(scrap_1), data=training); summary(lm2c)
 plot(lm2c)

 
# --- Ex 2: d) ---
# Test the null hypothesis that the parameter on log(scrap_1) is 1 against the 
# two-sided alternative. Report the p-value for the test.

 linearHypothesis(model=lm2c, "log(scrap_1) = 1")


# --- Ex 2: e) ---
# Repeat parts c) and d) using heteroscedasticity-robust standard errors, and briefly 
# discuss any notable differences.

 shccm(lm2c)
 linearHypothesis(model=lm2c, "log(scrap_1) = 1", white.adjust=TRUE)
 # linearHypothesis(model=lm2c, "log(scrap_1) = 1", white.adjust="hc3")




# --- Ex 3: Autocorrelation ----------------
 bond <- read.csv("http://thiloklein.de/R/bonds", header=T)
 str(bond)


# --- Ex 3: a) ---
# Regress changes in AAA bond returns (daaa) on US Treasury Bill interest rates (dus3mt). 
# Plot the residuals. Are the residuals distributed evenly across time? 

 lm3a <- lm(daaa ~ dus3mt, data=bond); shccm(lm3a)

 library(zoo)
 bond$paneldate <- as.yearmon(bond$paneldate,format="%Ym%m")
 e <- lm3a$res
 plot(e ~ bond$paneldate, type="l")


# --- Ex 3: b) ---
# Investigate serial autocorrelation in residuals. Use the Breusch-Godfrey Serial 
# Correlation LM Test.

 N <- length(e)
 e1 <- c(NA, e[1:(N-1)])
 e2 <- c(NA, NA, e[1:(N-2)])
 head(data.frame(bond$paneldate, e, e1, e2))
 plot(e ~ e1)
 cor(e, e1, use="complete")
 abline(a=0, b=0.2761491, col="red", lwd=2)


# --- Breusch-Godfrey Serial Correlation LM Test: ---
# 1. fit auxiliary regression of estimated error terms against all independent variables and 
# lagged estimated residuals up to order p (here: p=2)

 lm3bBG <- lm(e ~ bond$dus3mt + e1 + e2); shccm(lm3bBG) 

# 2. under H0: "no autocorrelation in error terms up to order p" the test statistic n*R^2
# follows a Chi-squared distribution with p degrees of freedom.

 n <- length(e)
 R2 <- summary(lm3bBG)$r.sq
 n*R2			# test statistic
 qchisq(p=0.95,df=2)	# theoretical quantile of the Chi-squared distribution under H0

# or simply:

 library(lmtest)
 bgtest(daaa ~ dus3mt, data=bond, order=2, type="Chisq")


# --- Durbin-Watson Test for Autocorrelated Errors ---
 ?durbinWatsonTest
 durbinWatsonTest(lm3a, max.lag=1, alternative="positive")
 # or:
 dwtest(daaa ~ dus3mt, data=bond, alternative="greater")




# --- Ex 4: Non-linearity in variables ----------------
# --- Ex 4: a) ---
# Fit a regression model of volume on t (a time trend)
 nyse <- read.csv("http://thiloklein.de/R/nysevolume", header=T)
 str(nyse)
 
 lm4a <- lm(volume ~ t, data=nyse); shccm(lm4a)


# --- Ex 4: b) ---
# Examine the residuals. Assess the linearity of the relation between volume and 
# the time trend.

 par(mfrow=c(2,2))
 plot(lm4a)

 par(mfrow=c(1,1))
 plot(volume ~ t, data=nyse) 
 abline(lm4a, col="red", lwd=2)


# --- Ex 4: c) ---
# Generate a log transformation of the variable volume, call it logvol. Run the 
# regression in a. with this new variable. What happens now with the residual?

 lm4c <- lm(log(volume) ~ t, data=nyse); shccm(lm4c)
 par(mfrow=c(2,2))
 plot(lm4c)


# --- Ex 4: d) ---
# Now run the following model (and analyse again the residuals):
# log(volume) ~ t + t^2

 lm4d <- lm(log(volume) ~ t + I(t^2), data=nyse); shccm(lm4d)
 plot(lm4d)




# --- Ex 5: Normality ----------------
# --- Ex 5: a) ---
# Regress changes in AAA bond returns (daaa) on US Treasury Bill interest rates (dus3mt).  
# Obtain the histogram of the residuals. 

 bond <- read.csv("http://thiloklein.de/R/bonds", header=T)
 str(bond)

 lm5a <- lm(daaa ~ dus3mt, data=bond); shccm(lm5a)
 hist(lm5a$res, breaks=30)

 library(timeDate)
 sk <- skewness(lm5a$res, method="moment"); sk
 ku <- kurtosis(lm5a$res, method="moment"); ku


# --- Ex 5: b) ---
# Analyse the Jarque-Bera test of normality results.

 n <- length(lm5a$res)
 (n-2)/6 *( sk^2 + ((ku - 3)^2)/4 )	# Jarque-Bera test statistic (handout, p.7)
 qchisq(p=0.95, df=2) 			# theoretical distribution under the null

 # or simply:

 library(tseries)
 jarque.bera.test(lm5a$res)




# --- Ex 6: Outliers ----------------
 crime <- read.csv("http://thiloklein.de/R/crime", header=T)
 str(crime)


# --- Ex 6: a) ---
# A regression model for crime might have pctmetro, poverty, and single as independent 
# variables. Run this regression and plot the residuals.

 lm6a <- lm(crime ~ pctmetro + poverty + single, data=crime); shccm(lm6a)
 par(mfrow=c(2,2))
 plot(lm6a)


# --- Ex 6: b) ---
# Identify what is the observation that is an outlier.

 crime$rstudent <- rstudent(lm6a)
 crime <- crime[order(crime$rstudent), ]
 head(crime)
 tail(crime)


# --- Ex 6: c) ---
# Try to solve this problem adding to the regression an impulse dummy variable.

 crime$DC <- ifelse(crime$state=="dc", 1, 0)
 lm6c <- lm(crime ~ pctmetro + poverty + single + DC, data=crime); shccm(lm6c)
 lm6c <- lm(crime ~ pctmetro + poverty + single, data=subset(crime, state!="dc")); shccm(lm6c)




# --- Digression: Graphical Analysis of outlier ---
 plot(crime ~ single, data=crime, col="white"); text(x=crime$single, y=crime$crime,   labels=crime$state)
 m.pctmetro <- mean(crime$pctmetro)
 m.poverty <- mean(crime$poverty)
 r.single <- seq(min(crime$single),max(crime$single),.1)
 myReg <- function(x, model){
 	   coef(model)%*%c(1, m.pctmetro, m.poverty, x)
 }
 y <- sapply(r.single, myReg, model=lm6a)
 lines(x=r.single, y=y, col="red", lwd=2)
 y <- sapply(r.single, myReg, model=lm6c)
 lines(x=r.single, y=y, col="blue", lwd=2)
 legend("topleft", legend=c("with DC","without DC"), fill=c("red","blue"))





# -------------------------------------------------------------------
# --- End of Session ------------------------------------------------

 save.image("EndOfSession.RData")
 q("yes")


