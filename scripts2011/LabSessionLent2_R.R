# -------------------------------------------------------------------
# MPO1-A Advanced Quantitative Research Methods
# Thilo Klein
# Lab Session 2: ARIMA, ARCH and GARCH Models

# Libraries: tseries, ccgarch, FinTS, dynlm, forecast, rgarch
# http://cran.r-project.org/web/views/TimeSeries.html
  rm(list=ls())
  setwd("E:/LabSessions_MPO1A_LT/Data")
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------




# --- Ex 1: Estimation of a quarterly ARMA model of the US Producer Price Index (PPI) -----------------------------

 quart <- read.csv("http://thiloklein.de/R/Lent/quarterly.csv", h=T)
 str(quart)



# --- Ex 1: a) ---
# Plot the PPI in level and first difference. Are these sequences stationary?

 ## generate variables
 PPI <- ts(quart$PPI, start=c(1960, 1), freq=4)
 LPPI <- ts(log(PPI), start=c(1960, 1), freq=4)
 LPPI.d1 <- diff(LPPI, lag=1, differences=1)

 ## plot series
 par(mfrow=c(2,1))
 plot(LPPI); plot(LPPI.d1)



# --- Ex 1: b) ---
# Take logs and first difference of PPI (the resulting variable is a measure of what?). 
# Plot again and compute ACF and PACF and do ADF test. Why is it important to take logs? 
# Has the variable a unit root?

 # The autocorrelation and partial autocorrelation decay quickly: it seems to be a 
 # stationary series:

 par(mfrow=c(2,1))
 acf(LPPI.d1); pacf(LPPI.d1)
 acf(LPPI.d1, xlim=c(0.25,5.5)); pacf(LPPI.d1) # get rid of 0 lag autocorrelation in acf

 # ADF test: without trend!!
 library(tseries)
 adf.test.1(LPPI.d1, int=T, trend=F, k=2)
 # We reject the null hypothesis: the series is stationary.

 # test for remaining autocorrelation in the residuals
 lm1 <- adf.test.2(LPPI.d1, int=T, trend=F, k=2)
 acf(lm1$resid); pacf(lm1$resid)
 # install.packages("ccgarch")
 library(ccgarch) 
 ljung.box.test.1(lm1$resid, seq(1,15,1))



# --- Ex 1: d) ---
# Estimate an ARMA(p,q), for the following cases (p,q)=(1,0),  (2,0), (1,1),  (1,(1,4)), (2,1), 
# (1,(0,4)). For each of these models report SSR, AIC and Q(5), Q(10) and Q(15). Which is 
# the best model? Diagnostic tests: Normality, no autocorrelation, no ARCH terms?

 # --- best model: ---
 arma1d <- arma(LPPI.d1, lag=list(ar=1, ma=c(1,4)))
 summary(arma1d)
 sum(arma1d$resid^2, na.rm=T) # SSR

 # --- other models: ---
 arma1da <- arma(LPPI.d1, order=c(1,0)); summary(arma1da)
 nNA <- is.na(arma1d$res)==F
 ljung.box.test(arma1da$res[nNA])

 arma1db <- arma(LPPI.d1, order=c(2,0)); summary(arma1db)
 ljung.box.test(arma1db$res[nNA])

 arma1dc <- arma(LPPI.d1, order=c(1,1)); summary(arma1dc)
 ljung.box.test(arma1dc$res[nNA])

 arma1dd <- arma(LPPI.d1, order=c(2,1)); summary(arma1dd)
 ljung.box.test(arma1dd$res[nNA])

 arma1de <- arma(LPPI.d1, lag=list(ar=c(1),ma=c(1,4))); summary(arma1de)
 ljung.box.test(arma1de$res[nNA])

 arma1df <- arma(LPPI.d1, lag=list(ar=c(1),ma=c(4))); summary(arma1df)
 ljung.box.test(arma1df$res[nNA])



 # --- Diagnostic tests: ---

 ## Autocorrelation: Ljung-Box statistic
 # Is there any evidence of autocorrelation up to lag 5, 10 and 15? No, there isn’t!

 nNA <- is.na(arma1d$res)==F
 ljung.box.test.1(arma1d$res[nNA], seq(1,15,1))
 par(mfrow=c(2,1)); acf(arma1d$res[nNA]); pacf(arma1d$res[nNA])

 ## Normality: Jarque-Bera test for the null of normality
 # There is evidence of non-normality. We reject the null hypothesis of normal distributed 
 # residuals. We should improve our model!
 hist(arma1d$res, breaks=20, freq=F)
 grid.x <- seq(-.05,.05,.001)
 grid.y <- dnorm(grid.x, sd=sd(arma1d$res, na.rm=T))
 lines(grid.x, grid.y, col="blue", lwd=2)
 jarque.bera.test(arma1d$res[nNA])

 ## ARCH terms: ARCH LM test
 # We reject the hypothesis of no autocorrelation of the squared residuals: 
 # install.packages("FinTS")
 library(FinTS)
 ArchTest(c(arma1d$res), lags=2)
 plot(arma1d$res)

 # What did we just do? ARCH test manually:
 u.sqr <- arma1d$res^2
 library(dynlm)
 arch1d <- dynlm(u.sqr ~ L(u.sqr,1) + L(u.sqr,2))
 summary(arch1d)
 lht(arch1d, c("L(u.sqr, 1)", "L(u.sqr, 2)"))

 # Or alternatively: ARCH LM test manually:
 # Obs*R^2 is distributed chi-squared with 2 degrees of freedom
 n <- length(arch1d$res)
 R2 <- summary(arch1d)$r.sq
 LM <- n * R2; LM
 pchisq(LM, df=2, lower.tail=F)



# --- Ex 1: e) ---
# Estimate the ARMA models (p,q)=(1,1) and (1,(1,4)) over the period 1960.1-1989.3. Obtain 
# the one-step-ahead forecast and the one-step-ahead forecast error.

 LPPI.d1.w <- window(LPPI.d1, start=c(1960,1), end=c(1989,3))
 LPPI.d1

 arma1e.11 <- arima(LPPI.d1.w, order=c(1,0,1))
 arma1e.11

 arma1e.114 <- arima(LPPI.d1.w, order=c(1,0,4), fixed=c(NA, NA, 0, 0, NA, NA))
 arma1e.114
 
 # prediction from ARMA(1,1)
 arma11.pred <- predict(arma1e.11, n.ahead=1); arma11.pred
 # prediction from ARMA(1,(1,4))
 arma114.pred <- predict(arma1e.114, n.ahead=1); arma114.pred

 # Actual observation is closer to ARMA(1,(1,4))
 obs <- window(LPPI.d1, c(1989,4), c(1989,4))
 obs

 ## Forecast accuracy
 # Mean Square Error (MSE)
 (obs - arma11.pred$pred)^2; (obs - arma114.pred$pred)^2 
 # Mean Absolute Error (MAE)
 abs(obs - arma11.pred$pred); abs(obs - arma114.pred$pred) 
 # Mean Absolute Percentage Error (MAPE)
 abs((obs - arma11.pred$pred)/obs)*100; abs((obs - arma114.pred$pred)/obs)*100




 
# --- Ex 2: ARMA model selection -----------------------------
 aa <- read.csv("http://thiloklein.de/R/Lent/arima.csv", h=T)
 str(aa)
 
 for(i in 4:10){
   aa[,i] <- ts(aa[,i], start=1801, freq=1)
 }

 ## Box-Jenkins Procedure

 # 1. Is the time series stationary?
   # i) plot the data

   # ii) compute ACF and PACF

   # iii) perform ADF tests


 # 2. AR, MA, or ARMA?

   # Plot ACF and PACF


 # 3. Estimate the model, defining p and q
 # (guided by PACF and ACF)


 # 4. Verify that the ACF and PACF of residuals do not show
 # significant spikes after estimation


 # 5. If there are two or more competing models, use AIC or SIBC to decide
 # which is better. The lower AIC and/or SBIC the better the model

 # 6. In the final specification all parameters must be significant,
 # and residuals wel behaved:

   # No autocorrelation?

   # No heteroskedasticity?

   # Normal distribution?



 # --- automatic model seection based on aic criterion ---
 # install.packages("forecast")
 library(forecast)
 # ?auto.arima
 summary( auto.arima(aa$Y1, trace=T, d=0, ic="aic"))
 summary( auto.arima(aa$Y2, trace=T, d=0, ic="aic") )
 summary( auto.arima(aa$Y3, trace=T, d=0, ic="aic") )
 summary( auto.arima(aa$Y4, trace=T, d=0, ic="aic") )
 summary( auto.arima(aa$Y5, trace=T, d=0, ic="aic") )
 summary( auto.arima(aa$Y6, trace=T, d=0, ic="aic") )
 summary( auto.arima(aa$Y7, trace=T, d=0, ic="aic") )





# --- Ex 3: Seasonality and ARIMA modeling -----------------------------

 str(quart)

# --- Ex 3: a) ---
# Plot the M1NSA in level and first difference. Are these sequences stationary?

 M1NSA <- ts(quart$M1NSA, start=c(1960,1), freq=4)
 M1NSA.d1 <- diff(M1NSA, lag=1)
 par(mfrow=c(2,1))
 plot(M1NSA); plot(M1NSA.d1)

 adf.test(M1NSA)
 adf.test(M1NSA.d1)


# --- Ex 3: b) ---
# Take logs and first difference of MINSA (the resulting variable is a measure of money 
# growth). Plot again and compute ACF and PACF. What happens at lags 4, 8, 12, etc?

 LM1NSA <- log(M1NSA)
 LM1NSA.d1 <- diff(LM1NSA, lag=1)

 plot(LM1NSA); plot(LM1NSA.d1)
 acf(LM1NSA.d1); pacf(LM1NSA.d1) 


# --- Ex 3: c) ---
# Take a seasonal difference (over the first differenced series) and compute again ACF and 
# PACF, what can you observe now? 

 LM1NSA.d1.d4 <- diff(LM1NSA.d1, lag=4)
 acf(LM1NSA.d1.d4); pacf(LM1NSA.d1.d4)


# --- Ex 3: d) ---
# What kind of model suggests the ACF and PACF? 

 arma3d <- arma(LM1NSA.d1.d4, lag=list(ar=c(1),ma=c(4)))
 summary(arma3d)


# --- Ex 3: e) ---
# Estimate a SARIMA(1,1,0)(0,1,1) and a SARIMA(0,1,1)(0,1,1). What is the best model? 

 ## SARIMA(1,1,0)(0,1,1), AIC: -1465.47
 arima3e <- arima(LM1NSA.d1.d4, order=c(1,1,0), seasonal=list(order=c(0,1,1), period=4))
 summary(arima3e); coeftest(arima3e)

 ## SARIMA(0,1,1)(0,1,1), AIC: -1478.93
 arima3e2 <- arima(LM1NSA.d1.d4, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=4))
 summary(arima3e2); coeftest(arima3e2)

 ## The second model is the better one, but:
 auto.arima(LM1NSA.d1.d4, trace=T)





# --- Ex 4: GARCH model of US PPI -----------------------------

 str(quart)



# --- Ex 4: a) ---
# Formally test for ARCH errors (using the residuals from the ARIMA model).

 arma4a <- arma(LPPI.d1, lag=list(ar=1,ma=c(1,4))); summary(arma1d)
 plot(arma4a$res)

 ## ARCH terms: ARCH LM test with H0: No autocorrelation in RESID^2 up to order 4
 # We reject the hypothesis of no autocorrelation of the squared residuals: 
 library(FinTS)
 ArchTest(c(arma4a$res), lags=4)

 # ARCH test manually:
 u.sqr <- arma4a$res^2
 library(dynlm)
 arch4a <- dynlm(u.sqr ~ L(u.sqr,1) + L(u.sqr,2) + L(u.sqr,3) + L(u.sqr,4))
 summary(arch4a)
 lht(arch4a, c("L(u.sqr, 1)", "L(u.sqr, 2)", "L(u.sqr, 3)", "L(u.sqr, 4)"))

 # ARCH LM test manually:
 # Obs*R^2 is distributed chi-squared with 4 degrees of freedom
 LM <- length(arch4a$res) * summary(arch4a)$r.sq
 pchisq(LM, df=4, lower.tail=F)
 # We reject the null. Then proceed to estimate an ARCH model for the variance.



# --- Ex 4: b) ---
# Use the ACF and PACF of the squared residuals as an indication of the order of the GARCH 
# process. How many ARCH terms seem to be needed? Estimate the model. 

 par(mfrow=c(2,1))
 acf(arch4a$res^2); pacf(arch4a$res^2)

 # manually install rgarch and dependencies as described on
 # http://thiloklein.de/MPO1A.html
 library(rgarch)

 # Estimate ARCH(4) model with mean equation ARMA(1,(1,4))
 # ?ugarchspec
 spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel="GARCH", garchOrder = c(4,0)), 
   mean.model = list(armaOrder = c(1,4), include.mean=T), fixed.pars=list(ma2=0,ma3=0))
 # ?ugarchfit
 sgarch.fit <- ugarchfit(data=c(LPPI.d1), spec = spec)
 sgarch.fit



# --- Ex 4: c) ---
# Test again for remaining ARCH terms (and compute again ACF and PACF). What can you conclude? 
# Observe carefully the estimated coefficients, what problems do you identify?

 str(sgarch.fit)
 ArchTest(sgarch.fit@fit$resid, lags=4)

 # MA(1) term is not significant and ARCH effects still remain.



# --- Ex 4: d) ---
# Estimate now a GARCH(1,1), do you still have the same problems? Tabulate ACF and PACF and 
# test for autocorrelation up to lag 4 in squared residuals.

 # Estimate GARCH(1,1) model with mean equation ARMA(1,(1,4))
 spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel="GARCH", garchOrder = c(1,1)), 
   mean.model = list(armaOrder = c(1,4), include.mean=T), fixed.pars=list(ma2=0,ma3=0))
 sgarch.fit <- ugarchfit(data=c(LPPI.d1), spec = spec)
 sgarch.fit

 # Verify that the correlogram of squared residuals is clean. Do ARCH LM test again.

 ArchTest(sgarch.fit@fit$resid, lags=4)

 # ARCH test manually:
 u.sqr <- ts(sgarch.fit@fit$resid^2)
 arch4d <- dynlm(u.sqr ~ L(u.sqr,1) + L(u.sqr,2) + L(u.sqr,3) + L(u.sqr,4))
 summary(arch4d)
 lht(arch4d, c("L(u.sqr, 1)", "L(u.sqr, 2)", "L(u.sqr, 3)", "L(u.sqr, 4)"))

 # We reject the null of no autocorrelation in the squared residuals up to lag 4. 
 # We do not proceed further here.  



# --- Ex 4: e) ---
# Produce one-step-ahead forecast with this model.

 ?ugarchforecast
 forc <- ugarchforecast(sgarch.fit, n.ahead=100)
 forc
 plot(forc,which="all")

 # As time goes on, the forecast tends to the long run variance of the process.




# --- Ex 5:  -----------------------------


# --- Ex 5: a) ---
# Estimate an ARIMA model for the series ym (return on a portfolio) following 
# Box-Jenkins methodology. Why might someone conclude that the residuals appear 
# to be white noise?

 arch <- read.csv("http://thiloklein.de/R/Lent/arch.csv")
 str(arch)

 # Box-Jenkins methodology
 # -> Estimate a MA(3,6)

 arma5a <- arima(arch$ym, order=c(0,0,6), fixed=c(0, 0, NA, 0, 0, NA, NA))
 plot(arma5a$resid, type="l")


# --- Ex 5: b) ---
# Perform the LM test for ARCH errors.

 ArchTest(arma5a$resid, lags=4)


# --- Ex 5: c) ---
# Estimate an ARCH-M process (using the standard deviation in the mean equation), 
# with an ARCH(1) for the variance.

 spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel="GARCH", garchOrder = c(1,0)), 
   mean.model = list(armaOrder = c(0,6), garchInMean=T, inMeanType=1), 
   fixed.pars=list(ma1=0,ma2=0,ma4=0,ma5=0))
 sgarch.fit <- ugarchfit(data=arch$ym, spec = spec)
 sgarch.fit

 ArchTest(sgarch.fit@fit$resid, lags=4)


# --- Ex 5: d) ---
# Check the ACF and the PACF. Do they appear to be satisfactory? Try other formulations 
# for the ARCH-M process. Interpret the coefficient of the std. dev. in the mean equation.

 par(mfrow=c(2,1))
 acf(sgarch.fit@fit$resid); pacf(sgarch.fit@fit$resid)

 spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel="GARCH", garchOrder = c(1,1)), 
   mean.model = list(armaOrder = c(0,6), garchInMean=T, inMeanType=1), 
   fixed.pars=list(ma1=0,ma2=0,ma3=0,ma4=0,ma5=0))
 sgarch.fit <- ugarchfit(data=arch$ym, spec = spec)
 sgarch.fit

 ArchTest(sgarch.fit@fit$resid, lags=4)




# --- Ex 7:  -----------------------------
# The variables, Ret, Inf, dtbill, are the S&P Composite index return, the US inflation 
# rate, and the first difference of the three-month Treasury bill rate. The mean equation 
# has the following form Ret = c + b1*Ret(-1) + b2*Inf(-1) + b3*dtbill(-1) + u.

 garch <- read.csv("http://thiloklein.de/R/Lent/garch.csv")
 str(garch)
 
 ret <- ts(garch$ret, start=c(1954, 1), freq=12)
 inf <- ts(garch$inf, start=c(1954, 1), freq=12)
 dtbill <- ts(garch$dtbill, start=c(1954, 1), freq=12)
 garch.ts <- ts.union(ret, inf, dtbill)

 lm7 <- dynlm(ret ~ L(ret,1) + L(inf,1) + L(dtbill,1), data=garch.ts)
 summary(lm7)


# --- Ex 7: a) ---
# Test for ARCH terms in the squared residuals from this equation (and plot the ACF and PACF 
# of the squared residual).

 ArchTest(lm7$resid, lags=2)
 par(mfrow=c(2,1))
 acf(lm7$resid); pacf(lm7$resid) 
 

# --- Ex 7: b) ---
# Try to model the heteroskedasticity using: GARCH and TARCH models.

 ## ---
 # GARCH(1,1) probably is enough to take into account all the autocorrelation on squared residuals.

 # generate matrix of external regressors
 n <- dim(garch)[1]
 ret_1 <- c(NA, garch$ret[1:(n-1)])
 head(data.frame(garch$obs, garch$ret, garch$ret_1))
 inf_1 <- c(NA, garch$inf[1:(n-1)])
 dtbill_1 <- c(NA, garch$dtbill[1:(n-1)])
 ex.reg <- data.frame(ret_1, inf_1, dtbill_1)[2:n,]
 head(ex.reg)
 ex.reg <- as.matrix(ex.reg)

 spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel="GARCH", garchOrder = c(1,1)), 
   mean.model = list(armaOrder = c(0,0), external.regressors=ex.reg) )
 sgarch.fit <- ugarchfit(data=garch$ret[2:n], spec = spec)
 sgarch.fit

 par(mfrow=c(2,1))
 acf(sgarch.fit@fit$resid); pacf(sgarch.fit@fit$resid) 
 ArchTest(sgarch.fit@fit$resid, lags=2)
 
 ## ---
 # TARCH: Even though the previous model seems to be good, we can try to see if there are 
 # some asymmetric responses to negative and positive shocks.

 spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel="TGARCH", garchOrder = c(1,1)), 
   mean.model = list(armaOrder = c(0,0), external.regressors=ex.reg) )
 sgarch.fit <- ugarchfit(data=garch$ret[2:n], spec = spec)
 sgarch.fit








# -------------------------------------------------------------------
# --- End of Session ------------------------------------------------

 save.image("EndOfSession.RData")
 q("yes")

