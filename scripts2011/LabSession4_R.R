# -------------------------------------------------------------------
# MPO1 Quantitative Research Methods
# Thilo Klein
# Lab Session 4: The Generalized Linear Regression Model

# Required libraries: car, VGAM, zoo, lmtest, graphics
  setwd("C:/Dokumente und Einstellungen/Thilo/Desktop/Lab_Sessions/Session4/Data")
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------




# --- Ex 1: Non-linear models. Production function. Multiple hypotheses. ------------------
# --- Ex 1: a) ---
# Load usmetal.txt with read.table

 metal <- read.table("http://thiloklein.de/R/usmetal.txt", header=T)
 str(metal)


# --- Ex 1: b) ---
# Generate new variables as logs of the old variables. Inspect the variables. 
# (summary and graph with histogram and scatter-plot)
 
 for(i in 1:3){
   metal[,i+3] <- log(metal[,i])
 }
 names(metal)[4:6] <- c("lK","lL","lY")
 str(metal)
 
 par(mfrow=c(3,2))
 for(i in 1:6){
   name <- names(metal)[i]
   hist(metal[,i], main=paste("Histogram of", name) , xlab=name)
 }


# --- Ex 1: c) ---
# Using a double log specification, estimate a production function. (This is the 
# Cobb-Douglas production function). Comment on the coefficients. 

 lm1c <- lm(lY ~ lK + lL, data=metal); summary(lm1c)
 par(mfrow=c(2,2)); plot(lm1c)


# --- Ex 1: d) ---
# Test the hypothesis that the coefficients are equal.

 library(car)
 linearHypothesis(lm1c, "lK=lL")


# --- Ex 1: e) ---
# [Optional] Test the hypothesis of constant returns to scale. 

 linearHypothesis(lm1c, "lK + lL = 1")


# --- Ex 1: f) ---
# Impose the CRS-restriction and re-estimate.

 lm1f <- lm(lY ~ I(lL - lK), offset=lK, data=metal); summary(lm1f)
 anova(lm1c, lm1f)	
 # same test as linearHypothesis for CRS above; models almost equal.




# --- Ex 2: Model selection ------------------
# --- Ex 2: a) ---
# Regress log(price) on growing-season temperatures, harvest-season rainfall, 
# off-season rainfall, and the age of a wine. Use the R2 to compute the F-statistic 
# to test the null hypothesis that none of the variables in the regression matter 
# for the price of wine.

 wine <- read.csv("http://thiloklein.de/R/wineweather1", header=T)
 str(wine)	# 38 obs. of 7 variables.
 lm2a <- lm(logprice ~ degrees + hrain + wrain + time_sv, data=wine); summary(lm2a)
 wine		# 11 NAs for variable logprice

 qf(p=0.95, df1=4, df2=22) # for the critical value.
 str(summary(lm2a))
 # n = number of observations = 27
 # k = number of coefficients = 5
 paste("R^2 is", summary(lm2a)$r.squared)
 paste("df residuals = n-k =", 27-5)
 paste("Restrictions = k–1 = df of the model =", 5-1)
 paste("F-stat =", summary(lm2a)$r.squared / (5-1) / ((1- summary(lm2a)$r.squared) / (27-5)) )
 # summary(lm2a)$fstatistic
 pf(q=summary(lm2a)$fstatistic[1], df1=summary(lm2a)$fstatistic[2], df2=summary(lm2a)$fstatistic[3], lower.tail=F)


# --- Ex 2: b) ---
# Test at the 5% significance level the null hypothesis that the intercept changes 
# in the decades after the 50s with respect to the corresponding one in the 50s.

 wine$vint
 wine$vint.dec <- cut(wine$vint, breaks=c(1950, 1959, 1969, 1979, 1990))
 levels(wine$vint.dec) <- c("50s","60s","70s","80s")
 table(wine$vint.dec)
 lm2b <- lm(logprice ~ degrees + hrain + wrain + vint.dec, data=wine); summary(lm2b)

 linearHypothesis(lm2b, c("vint.dec60s", "vint.dec70s", "vint.dec80s"))

 wine$fifties <- ifelse(wine$vint<1960, 1, 0)
 wine$sixties <- ifelse(wine$vint>1959 & wine$vint<1970, 1, 0)
 wine$seventies <- ifelse(wine$vint>1969 & wine$vint<1980, 1, 0)
 wine$eighties <- ifelse(wine$vint>1979, 1, 0)
 lm2b <- lm(logprice ~ degrees + hrain + wrain + sixties + seventies + eighties, data=wine); summary(lm2b)

 linearHypothesis(lm2b, c("sixties = 0", "seventies = 0", "eighties = 0"))


# --- Ex 2: c) ---
# The regression used for b) should EITHER include a dummy variable for each decade 
# and no constant OR a dummy for each of three decades and a constant. Explain.

 lm(logprice ~ degrees + hrain + wrain + sixties + seventies + eighties + fifties , data=wine)$coef
 lm(logprice ~ degrees + hrain + wrain + sixties + seventies + eighties, data=wine)$coef
 lm(logprice ~ -1 + degrees + hrain + wrain + fifties + sixties + seventies + eighties, data=wine)$coef


# --- Ex 2: d) ---
# Drop time_sv. Include lagged values (up to 2) for the average temperature in the 
# growing season. Select the best model with BIC and AIC criteria.

 wine <- wine[order(wine$vint, decreasing=T), ]
 wine$deglag1 <- c(wine$degrees[-1], NA) # pad with missing
 wine$deglag2 <- c(wine$degrees[-c(1,2)], rep(NA,2))
 # alternative: don't re-sort data: N <- length(x); xlag <- c(NA, x[1:(N-1)])

 lm2di <- lm(logprice ~ degrees + hrain + wrain + deglag1 + deglag2, data=wine); summary(lm2di)
 lm2dii <- lm(logprice ~ degrees + hrain + wrain + deglag1, data=wine); summary(lm2dii)
 lm2diii <- lm(logprice ~ degrees + hrain + wrain, data=wine); summary(lm2diii)

 myIC <- function(model){
   # -2*as.numeric(logLik(lm2di)) + k*length(lm2di$coef)
   print(model$call)
   print( paste("AIC:", AIC(model, k=2) ))				# Akaike's An IC
   print( paste("BIC:", AIC(model, k=log(length(model$res))) ))	# Schwarz's Bayes IC
   print( paste("R2 :", summary(model)$adj.r.squared ))		# adjusted R^2
 }
 myIC(lm2di); myIC(lm2dii); myIC(lm2diii)




# --- Ex 3: Heteroskedasticity ------------------
# --- Ex 3: a) ---
# Use the data in hprice1.csv to obtain heteroskedasticity-robust standard errors 
# and homoskedastic-only standard errors for equation: price ~ lotsize + sqrft + bdrms.
# Discuss any important difference with the usual homoskedasticity-only standard errors.

 house <- read.csv("http://thiloklein.de/R/hprice1", header=T)
 str(house)

 lm3a <- lm(price ~ lotsize + sqrft + bdrms, data=house)
 summary(lm3a)
 shccm(lm3a)


# --- Ex 3: b) ---
# Repeat part a) for log(price) ~ log(lotsize) + log(sqrft) + bdrms.
 lm3b <- lm(lprice ~ llotsize + lsqrft + bdrms, data=house)
 summary(lm3b)
 shccm(lm3b)


# --- Ex 3: c) ---
# What does this example suggest about heteroskedasticity and the transformation used 
# for the dependent variable?

 # refer to handout.


# --- Ex 3: d) ---
# Apply the full White-test for heteroskedasticity to part b). Which variables does it 
# apply? Using the chi-squared form of the statistic, obtain the p-value. What do you 
# conclude?

 house$lm3b.sqres <- lm3b$residuals^2
 lm3b.white.test <- lm(lm3b.sqres ~ llotsize*lsqrft*bdrms - llotsize:lsqrft:bdrms
	+ I(llotsize^2) + I(lsqrft^2) + I(bdrms^2), data=house); shccm(lm3b.white.test)
 T <- summary(lm3b.white.test)$r.squared * nrow(house)
 library(VGAM)
 pchisq(q=T, df=9, lower.tail=F)	# =0.39 -> little evidence against homoskedasticity assumption




# --- Ex 4: Autocorrelation  ------------------
 bond <- read.csv("http://thiloklein.de/R/bond_int_rates", header=T)
 str(bond)

 library(zoo)
 # data.frame(bond$year, bond$month, bond$month2, bond$paneldate)[1:100,]
 # bond$month2 <- c( as.yearmon(bond$month2[1:312],format="%y/%B"),
 #  as.yearmon(bond$month2[313:600],format="%y-%b") )
 bond$paneldate <- as.yearmon(bond$paneldate,format="%Ym%m")

 lm4 <- lm(daaa ~ dus3mt, data=bond); shccm(lm4)
 e <- lm4$res
 plot(e ~ bond$paneldate, type="l")

 N <- length(e)		# alternative: lags without re-sorting data
 e1 <- c(NA, e[1:(N-1)])
 head(data.frame(bond$paneldate, e, e1))
 plot(e ~ e1)
 cor(e, e1, use="complete")
 abline(a=0, b=0.2761491, col="red", lwd=2)

 ?durbinWatsonTest
 durbinWatsonTest(lm4, max.lag=1, alternative="positive")




# --- Ex 5: Linearity ------------------
# RESET (regression specification error test). 
# for: relevant omitted variables; non-linearities

 nations <- read.csv("http://thiloklein.de/R/nations", header=T)
 str(nations)

 lm5 <- lm(birth ~ gnpcap + urban, data=nations); shccm(lm5)
 e <- lm5$resid 
 par(mfrow=c(1,2))
 plot(e ~ lm5$model[,2], main="gnpcap"); lines(lowess(cbind(lm5$model[,2], e), f=1), col=2)
 plot(e ~ lm5$model[,3], main="urban"); lines(lowess(cbind(lm5$model[,3], e), f=1), col=2)

 library(lmtest)
 ?resettest
 resettest(lm5)

 plot(subset(nations, select=c("birth","gnpcap","urban")))
 summary(nations$gnpcap)

 plot(density(nations$gnpcap), col="green", lwd=2)
 grid.x <- seq(-10000, 20000, 1)
 grid.y <- dnorm(grid.x, sd=sd(nations$gnpcap))
 lines(grid.x, grid.y, col="blue", lwd=2) 
 legend("topright", legend=c("Density of gnpcap","Normal density"), fill=c("green","blue"))

 nations$lgnp <- log(nations$gnpcap) # log-transformation

 lm5b <- lm(birth ~ lgnp + urban, data=nations); shccm(lm5b)
 eb <- lm5b$resid 
 par(mfrow=c(1,2))
 plot(eb ~ lm5b$model[,2], main="lgnp"); lines(lowess(cbind(lm5b$model[,2], eb), f=1), col=2)
 plot(e ~ lm5$model[,2], main="gnpcap"); lines(lowess(cbind(lm5$model[,2], e), f=1), col=2)
 resettest(lm5b)




# --- Ex 6: Normality ------------------
 eaef21 <- read.csv("http://thiloklein.de/R/eaef21", header=T)
 str(eaef21)


# --- Ex 6: 1. ---
# Use the earnings eaef21 dataset from session 3 (see Annex 1 for variable 
# descriptions) and regress EARNINGS on S and ASVABC.

 lm6a <- lm(EARNINGS ~ S + ASVABC, data=eaef21); shccm(lm6a)

 
# --- Ex 6: 2. ---
# Use the model$res command to generate residuals.  
 
 e <- lm6a$res


# --- Ex 6: 3. ---
# Use the density command to produce a kernel density plot. Overlaid the plot with 
# a normal density.

 plot(density(e), col="green", lwd=2)
 grid.x <- seq(min(e),max(e),.1)
 grid.y <- dnorm(grid.x, sd=sd(e))
 lines(grid.x, grid.y, col="blue", lwd=2) 
 legend("topright", legend=c("Density of error terms","Normal density"), fill=c("green","blue"), )


# --- Ex 6: 4. ---
# Execute the command qqnorm and  assess  normality of residuals.

 library(graphics)
 qqnorm(e); qqline(e, col = 2)
 # y <- rt(200, df = 10)
 # qqnorm(y); qqline(y, col = 2)


# --- Ex 6: 5. ---
# Examine the results from density and qqnorm applied to the residuals after a 
# semi-log (log-level) regression 

 lm6b <- lm(log(EARNINGS) ~ S + ASVABC, data=eaef21); shccm(lm6b)
 e <- lm6b$res

 # for plots, use above functions.




# --- Ex 7: Outliers ------------------
 crime <- read.csv("http://thiloklein.de/R/crime", header=T)
 str(crime)

 plot(subset(crime, select=c("crime","pctmetro","poverty","single")))
 plot(crime ~ pctmetro, data=crime, col="white"); text(x=crime$pctmetro, y=crime$crime, labels=crime$state)
 plot(crime ~ poverty, data=crime, col="white"); text(x=crime$poverty, y=crime$crime, labels=crime$state)
 plot(crime ~ single, data=crime, col="white"); text(x=crime$single, y=crime$crime, labels=crime$state)

 lm7 <- lm(crime ~ pctmetro + poverty + single, data=crime); shccm(lm7)
 crime$rstudent <- rstudent(lm7)
 crime <- crime[order(crime$rstudent), ]
 head(crime)
 tail(crime)

 subset(crime, abs(rstudent) > 2)
 lm7b <- lm(crime ~ pctmetro + poverty + single, 
   data=subset(crime, state!="dc")); shccm(lm7b)


# --- Interpretation: ---
 plot(crime ~ single, data=crime, col="white"); text(x=crime$single, y=crime$crime, labels=crime$state)
 m.pctmetro <- mean(crime$pctmetro)
 m.poverty <- mean(crime$poverty)
 r.single <- seq(min(crime$single),max(crime$single),.1)
 
 myReg <- function(x, model){
   coef(model)%*%c(1, m.pctmetro, m.poverty, x) #mean(crime$single)
 }
 y <- sapply(r.single, myReg, model=lm7); lines(x=r.single, y=y, col="red", lwd=2)
 y <- sapply(r.single, myReg, model=lm7b); lines(x=r.single, y=y, col="blue", lwd=2)
 legend("topleft", legend=c("OLS with DC","OLS without DC"), fill=c("red","blue"))





# -------------------------------------------------------------------
# --- End of Session ------------------------------------------------

 save.image("workspace_EndOfSession.RData")
 q("yes")




