# Lecture 7
 
 library(foreign)
 data = read.dta("http://thiloklein.de/R/data.dta")
 str(data)
 attach(data)

 data$discval = listprice * discount
 attach(data)

# summary statistics and figures
 d1 = data.frame(registrations, capvalue, listprice, discval, discount, modelage)
 summary(d1)

 lm1 = lm(registrations ~ capvalue + listprice + discval + modelage, data=subset(data, year == 2000))
 summary(lm1)

# observe change in R^2, Adj R^2, RMSE

# So far as before

# generate logs
 data$lnregistrations = log(registrations )
 data$lncapvalue  = log(capvalue )
 data$lnlistprice = log(listprice)
 data$lndiscval  = log(-discval)
 data$lnmodelage   = log(modelage)

 attach(data)


# double log regressions
 lm2 = lm(lnregistrations ~ lncapvalue + lnlistprice + lndiscval + lnmodelage, data=subset(data, year == 2000))
 summary(lm2)
 lm3 = lm(lnregistrations ~ lncapvalue + lnlistprice + lndiscval, data=subset(data, year == 2000))
 summary(lm3)
 lm4 = lm(lnregistrations ~ lnlistprice + lndiscval, data=subset(data, year == 2000))
 summary(lm4)


# building a dummy variable
 plot(registrations ~ month, data=subset(data, year==2000))

 data$letchdum = ifelse(month==3|month==9,1,0)
 attach(data)


# regression with intercept dummy
 lm5 = lm(lnregistrations ~ lnlistprice + lndiscval + letchdum, data=subset(data, year==2000))
 summary(lm5)


# generate slope dummies
 data$letchlnlistp = lnlistprice*letchdum
 data$letchlndiscval = lndiscval*letchdum
 attach(data)


# regression with intercept and slope dummies
 lm6 = lm(lnregistrations ~ lnlistprice + lndiscval + letchdum + letchlnlistp + letchlndiscval, data=subset(data, year==2000))
 summary(lm6)
 library(car)
 vif(lm6)


# multicollinearity

# separate regressions
 lm7 = lm(lnregistrations ~ lnlistprice + lndiscval, data=subset(data, year==2000 & (month==3|month==9)))
 summary(lm7)
 lm8 = lm(lnregistrations ~ lnlistprice + lndiscval, data=subset(data, year==2000 & (month==3|month==9)))
 summary(lm8)


# simplified model to illustrate heteroscedasticity - note, not in logs
 lm9 = lm(registrations ~ listprice + discval, data=subset(data, year==2000))
 summary(lm9)

 plot(lm9$resid ~ lm9$fitted)

 # Breusch-Pagan test
 library(lmtest)
 bptest(lm9, studentize=F)

 # ???
 bpagan listprice  discval
 bpagan listprice
 bpagan discval

# in logs
 lm10 = lm(lnregistrations ~ lnlistprice + lndiscval, data=subset(data, year==2000))
 summary(lm10)

 plot(lm10$resid ~ lm10$fitted)
 bptest(lm10, studentize=F)

# Without [varlist] and the "rhs" option, STATA will assume the variance 
# is thought to be a function of the fitted values.
 # bpagan lnlistprice  lndiscval
 # bpagan lnlistprice
 # bpagan lndiscval
 # If you identify heteroscedasticity this way, this can help to remedy 
 # through more general estimation methods.

# White-test
 lm11 = lm(I(lm10$resid^2) ~ lnlistprice*lndiscval + I(lnlistprice^2) + I(lndiscval^2), data=lm10$model)
 summary(lm11)
 # under the null that the coeffs are jointly 0, F dist; and reject the null at 1%

# robust regression
 source("http://thiloklein.de/R/myfunctions.R")
 lm12 = lm(lnregistrations ~ lnlistprice + lndiscval, data=subset(data, year==2000))
 shccm(lm12)


## Linearity ##

# the better model so far
 lm13 = lm(lnregistrations ~ lnlistprice + lndiscval + letchdum, data=subset(data, year==2000))
 summary(lm13)
 plot(lm13$resid ~ lm13$fitted)
 abline(h=0, col="red")

 par(mfrow=c(2,2))
 plot(lm13)

 # twoway (scatter r lnlistprice) (lfit r lnlistprice) (lowess r lnlistprice)
 # twoway (scatter r lndiscval) (lfit r lndiscval) (lowess r lndiscval)

 # Ramsey RESET test
 # H0: model has no omitted variables
 reset(lm13)

 # lm14 = lm(lnregistrations ~ lnlistprice + lndiscval + letchdum + lm13$fitted + I(lm13$fitted^2), data=lm13$model)
 # summary(lm14)


## Normality ##
 par(mfrow=c(2,2))
 plot(lm13)
# Note: skewness is not a problem, kurtosis is

# Analyse the Jarque-Bera test of normality results.
 n <- length(lm5a$res)
 (n-2)/6 *( sk^2 + ((ku - 3)^2)/4 )	# Jarque-Bera test statistic
 qchisq(p=0.95, df=2) 			# theoretical distribution under the null

 # or simply:

 library(tseries)
 jarque.bera.test(lm5a$res)
