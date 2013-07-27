# -------------------------------------------------------------------
# Master in Finance Econometrics Module
# Thilo Klein
# Lab Session 2: Model Selection; Inference; Non-linear Models

# Required libraries: car
  setwd("C:/Dokumente und Einstellungen/Thilo/Desktop/LabSessions_MFin/Session2/Data")
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------




# --- Digression: Heteroskedasticity Consistent Covariance Matrix (HCCM) ---
 set.seed(123) # this way we will all have the same "random" variables
 x <- runif(1000)
 e <- rnorm(1000)
 par(mfrow=c(1,2))

 y.hom <- x + e	# homoscedasticity
 lm.hom <- lm(y.hom ~ x)
 plot(y.hom ~ x, ylim=c(-2,3)); abline(lm.hom, col="red", lwd=2)

 y.het <- x + e*x	# heteroscedasticity
 lm.het <- lm(y.het ~ x)
 plot(y.het ~ x, ylim=c(-2,3)); abline(lm.het, col="red", lwd=2)

 vcov(lm.het)
 ??hccm
 library(car)
 hccm(lm.het, type="hc0")	# White-adjusted errors are R's default setting
 hccm(lm.het, type="hc3")	# heteroscedasticity consistent errors used by Stata


# --- [load shccm function:] ---
 source("http://thiloklein.de/R/myfunctions.R")

 summary(lm.hom)		# similar results under homoscedasticity
 shccm(lm.hom) 

 summary(lm.het)		# different results under heteroscedasticity
 shccm(lm.het) 

 rm(x, e, y.hom, y.het, lm.hom, lm.het)




# --- Ex 1: Confidence intervals of regression coefficients ----------------
 oilprice1 <- read.csv("http://thiloklein.de/R/oilprice1",header=T,sep=",")
 str(oilprice1)

 plot(price ~ api, data=oilprice1)
 lm1 <- lm(price ~ api, data=oilprice1)
 summary(lm1)

 t <- qt(p=0.975, df=13-2)   		# quantile of t distribution

 str(lm1)
 b <- lm1$coef[2]		    	# coefficient

 str(summary(lm1))
 se_b <- summary(lm1)$coef[2,2]		# standard error of coefficient

 b + c(-1,1)*se_b*t			# confidence interval

 ls(); rm(b, t, se_b)			# remove objects if no longer needed!

 # or simply use:
 confint(object=lm1, level=0.95)


# --- Ex 2: Omitted variable bias and highly correlated regressors ---------------- 
 set.seed(123)
 epsilon <- rnorm(10000)
 omega   <- rnorm(10000)
 eta     <- rnorm(10000)
 zeta    <- rnorm(10000)

 x1 <- 5 + omega + 0.3* eta
 x2 <- 10 + omega
 x3 <- 5 + eta
 y  <- 20 + x1 + x2 + epsilon
 z  <- 30 + x2 + x3 + zeta


# --- Digression: visual test for normality ---
 hist(qnorm(runif(10000)), freq=F)
 grid.x <- seq(-4,4,.1)
 grid.y <- dnorm(grid.x)
 lines(grid.x, grid.y, col="blue", lwd=2)


# --- Ex 2: b) ---
 cor(cbind(x1, x2, x3))
 lm2b <- lm(y ~ x1 + x2)
 shccm(lm2b)
 vif(lm2b)		# VIF > 10, severe multicollinearity.


# --- Ex 2: c) ---
 lm2c <- lm(z ~ x2 + x3)
 shccm(lm2c)
 vif(lm2c)


# --- Ex 2: d) ---
 lm(y ~ x1)$coef		# cov(x1,x2)=1 -> OVB for b2; Intercept biased
 lm(y ~ x1 + x2)$coef
 lm(z ~ x2)$coef		# true model: z=30+x2+x3 -> Intercept biased
 lm(z ~ x2 + x3)$coef	




# --- Ex 4: Hypotheses testing in the Log-log model ----------------
# --- Ex 4: a) ---
# Run the previous regression. Interpret the coefficients for GDP and price of gasoline.

 gas <- read.csv("http://thiloklein.de/R/gasoline", header=T)
 str(gas)
 lm4 <- lm(log(gasoline) ~ log(gdp) + log(price), data=gas); summary(lm4)


# --- Ex 4: b) ---
# Is the "income" elasticity equal to one? Test this hypothesis.

 library(car)
 linearHypothesis(model=lm4, "log(gdp)=1")




# --- Ex 5: Non-linear models. Production function. Multiple hypotheses. ------------------
# --- Ex 5: a) ---
# Load usmetal.txt with read.table

 metal <- read.table("http://thiloklein.de/R/usmetal.txt", header=T)
 str(metal)


# --- Ex 5: b) ---
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


# --- Ex 5: c) ---
# Using a double log specification, estimate a production function. (This is the 
# Cobb-Douglas production function). Comment on the coefficients. 

 lm5c <- lm(lY ~ lK + lL, data=metal); summary(lm5c)
 par(mfrow=c(2,2)); plot(lm5c)


# --- Ex 5: d) ---
# Test the hypothesis that the coefficients are equal.

 linearHypothesis(lm5c, "lK=lL")


# --- Ex 5: e) ---
# [Optional] Test the hypothesis of constant returns to scale. 

 linearHypothesis(lm5c, "lK + lL = 1")


# --- Ex 5: f) ---
# Impose the CRS-restriction and re-estimate.

 lm5f <- lm(lY ~ I(lL - lK), offset=lK, data=metal); summary(lm5f)
 anova(lm5c, lm5f)	
 # same test as linearHypothesis for CRS above; models almost equal.


 

# --- Ex 6: Bank wages ----------------
 bank <- read.csv("http://thiloklein.de/R/bank", header=T)
 str(bank)


# --- Ex 6: i) ---
# Regress the log of salaries on a constant, education, the log of the 
# starting salary, and define a way to capture percentage differences 
# due to gender and belonging to a minority.

 bank$logsalbegin <- log(bank$salbegin)
 lm6i <- lm(logsal ~ educ + logsalbegin + gender + minority, data=bank); shccm(lm6i)


# --- Ex 6: ii) ---
 bank$female <- 1 - bank$male
 bank$femaleandminority <- bank$female * bank$minority				# either this
 lm5ii <- lm(logsal ~ educ + male + minority + femaleandminority, data=bank)	# ..
 lm(logsal ~ educ + male + minority + female:minority, data=bank)			# or that.


# --- Ex 6: iii) ---
 lm6iii <- lm(logsal ~ educ + female + minority , data=bank); shccm(lm6iii)
 linearHypothesis(lm6iii, "educ = 0.07")
 linearHypothesis(lm6iii, c("educ = 0.07", "female = minority"))





# -------------------------------------------------------------------
# --- End of Session ------------------------------------------------

 save.image("EndOfSession.RData")
 q("yes")


