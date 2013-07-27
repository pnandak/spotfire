# -------------------------------------------------------------------
# Lecture 4: Modelling volatility of S&P returns

# Required libraries: zoo, lmtest, TSA, rgarch, moments
  rm(list=ls())
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------


## S&p500 1988-2010
# When we study stock returns, the main target is to model volatility as stock price may follow a random walk
 sp <- read.csv("s&p500 daily.csv")
 str(sp)

 date <- as.Date(sp$Date, format="%d/%m/%Y")
 library(zoo)
 adj <- zoo(sp$Adj, as.Date(date))
 plot(adj)

 # As there is an extreme value of volatility in 1987, we select the sample from 1988 to 2010 to
 # estimate the Garch model.
 adj88 <- window(adj, start=as.Date("1988-01-01"))
 plot(adj88)


 ## Step 1:
  dlsp <- diff(log(adj88), lag=1)
 # Plot dlsp
  plot(dlsp)
 # Do unit root test
  dlsp.v <- as.vector(dlsp)
  adf.test.1(dlsp.v, int=F, trend=F, k=1)
  # -> DLSP is stationary


 ## Step2:
 # Fit a mean model and check whether the residuals have arch effects.
 # First look at correlogram of DLSP
  par(mfrow=c(2,1))
  acf(na.omit(dlsp.v)); pacf(na.omit(dlsp.v))
 # Fit arma model: dlsp = c ar(1) ar(2) ma(1) ma(2)
  arima202 <- arima(dlsp, order=c(2,0,2))
  library(lmtest)
  coeftest(arima202)
 # Correlogram of standardized residuals
  nNA <- which(is.na(arima202$resid)==F)
  # install.packages("TSA", dependencies=T)
  library(TSA)
  acf(rstandard(arima202)[nNA]); pacf(rstandard(arima202)[nNA])
  # -> Still correlations
 # Correlogram of sqared standardized residuals
  acf(rstandard(arima202)[nNA]^2); pacf(rstandard(arima202)[nNA]^2)
  # -> Still arch effects


 ## Step 3:
 # Fit one arch model to clean arch effects, start with the longest lags
 # dlsp c ar(1) ar(2) ma(1) ma(2) Arch(9)

 # manually install rgarch and dependencies as described on
 # http://thiloklein.de/Econometrics2.html
 library(rgarch)

 # ?ugarchspec
 spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel="GARCH",
   garchOrder = c(9,0)),
   mean.model = list(armaOrder = c(2,2), include.mean=T))
 # ?ugarchfit
 sgarch.fit <- ugarchfit(data=dlsp.v, spec = spec)
 sgarch.fit

 # Correlogram of residuals
  nNA <- which(is.na(sgarch.fit@fit$resid)==F)
  stdresid <- sgarch.fit@fit$residuals/sgarch.fit@fit$sigma
  acf(stdresid[nNA]); pacf(stdresid[nNA])
  ljung.box.test.1(stdresid[nNA], seq(1,15,1))
  # -> Still correlations

 # Correlogram of sqared residuals
  acf(stdresid[nNA]^2); pacf(stdresid[nNA]^2)
  ljung.box.test.1(stdresid[nNA]^2, seq(1,15,1))
  # -> No arch effects


 ## Step 4:
 # So we try different models to clean correlations among residuals and arch effects
 # and find

 # dlsp = ar(1) ma(1) ma(12) Garch(2,1)

  spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel="GARCH",
    garchOrder = c(2,1)),
    mean.model = list(armaOrder = c(1,12), include.mean=T),
    fixed.pars = list(ma2=0, ma3=0, ma4=0, ma5=0, ma6=0, ma7=0, ma8=0, ma9=0, ma10=0, ma11=0))
  sgarch.fit <- ugarchfit(data=dlsp, spec = spec)
  sgarch.fit

 # Has no problem with correlation in standardized residuals.
 # And all coefficients are significant.
  nNA <- which(is.na(sgarch.fit@fit$resid)==F)
  stdresid <- sgarch.fit@fit$residuals/sgarch.fit@fit$sigma
  acf(stdresid[nNA]); pacf(stdresid[nNA])
  ljung.box.test.1(stdresid[nNA], seq(1,15,1))


 ## Step 5:
 # Check normality of standardized residuals and do forecasting
 # Huge improvements in distributions in terms of Jarque-Bera statistics
  library(moments)
  jarque.test(stdresid)
 # This is the original distribution
  jarque.test(dlsp.v)

 # Static forecast: see the conditional variance changes
 #1
?ugarchforecast
 plot(ugarchforecast(sgarch.fit, n.ahead = 50))



 ## Step 6:
 # However there may be still arch mean effects and leverage effects.
 # So to improve our model, we may try TARCH model, IGARCH model , Garch_mean model,
 # EGARCH_mean model.

 # 1) TARCH model [OPTIONAL]
 ?ugarchspec
 spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel="TGARCH",
   garchOrder = c(2,1)),
   mean.model = list(armaOrder = c(1,12), include.mean=T),
   fixed.pars = list(ma2=0, ma3=0, ma4=0, ma5=0, ma6=0, ma7=0, ma8=0, ma9=0, ma10=0, ma11=0))
 sgarch.fit <- ugarchfit(data=dlsp.v, spec = spec)
 sgarch.fit

 # 2) IGARCH model [OPTIONAL]
 spec <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(2,1)),
   mean.model = list(armaOrder = c(1,12), include.mean=T),
   fixed.pars = list(ma2=0, ma3=0, ma4=0, ma5=0, ma6=0, ma7=0, ma8=0, ma9=0, ma10=0, ma11=0))
 sgarch.fit <- ugarchfit(data=dlsp.v, spec = spec)
 sgarch.fit
 # has problems with correlation in residuals

 # 3) EGRACH model [OPTIONAL]
 # After many trials to capture the arch mean effects and leverage effects,
 # We obtain a good model: dlsp = ar(1) ma(1) ma(12) EGARCH(2,1,1)
 spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(2,1)),
   mean.model = list(armaOrder = c(1,12), include.mean=T),
   fixed.pars = list(ma2=0, ma3=0, ma4=0, ma5=0, ma6=0, ma7=0, ma8=0, ma9=0, ma10=0, ma11=0))
 sgarch.fit <- ugarchfit(data=dlsp.v, spec = spec)
 sgarch.fit

 ## Interpretation of coefs!
 # The negative news RESID(-1) will increase the conditional volatility (ln(Garch) by 0.092359-0.000866.(-
 # c(8)+c(6)). The positive news RESID(-1) will decrease the conditional volatility (ln(Garch) by -0.092359-
 # 0.000866.( c(8)+c(6))

 ## 4) Garch in mean [USEFUL]
  spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel="GARCH", 
    garchOrder = c(2,1)),
    mean.model = list(armaOrder = c(1,12), include.mean=T, garchInMean=T),
    fixed.pars = list(ma2=0, ma3=0, ma4=0, ma5=0, ma6=0, ma7=0, ma8=0, ma9=0, ma10=0, ma11=0))
  sgarch.fit <- ugarchfit(data=dlsp, spec = spec)
  sgarch.fit
 # Interpretation: There is arch mean effects showing during the volatile periods, the risk premium rises as
 # the coefficient of garch is 0.066680 in the mean model. This is a time varying risk premium.

 # No correlation in standardized residuals:
  nNA <- which(is.na(sgarch.fit@fit$resid)==F)
  stdresid <- sgarch.fit@fit$residuals/sgarch.fit@fit$sigma
  ljung.box.test.1(stdresid[nNA], seq(1,15,1))
 # No arch effects in standardized residuals:
  ljung.box.test.1(stdresid[nNA]^2, seq(1,15,1))

 # Improvements in distributions in terms of Jarque-Bera statistics
  jarque.test(stdresid)

 # Static forecast: see the conditional variance changes
 ?ugarchforecast

 plot(ugarchforecast(sgarch.fit, n.ahead = 50))
