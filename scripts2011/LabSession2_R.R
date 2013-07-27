# -------------------------------------------------------------------
# MPO1 Quantitative Research Methods
# Thilo Klein
# Lab Session 2: Linear Regression in R; Good Practice Guide

# Required libraries: .
  setwd("C:/Dokumente und Einstellungen/Thilo/Desktop/Lab_Sessions/Session2/Data")
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------




# --- Ex 1: Run Script-file from command line ---------------------------
 # print("hello, world")
 # setwd("C:/Dokumente und Einstellungen/Thilo/Desktop/Lab_Sessions/Session2/Data")
 # source("hello.R")


# --- Ex 2: The linear model ---------------------------
# --- Ex 2: a) ---
# Review the contents and regress employment growth on GDP growth. Provide an 
# interpretation of the results.

 growth <- read.csv("http://thiloklein.de/R/growth",header=T,sep=",")
 str(growth)
 lm2 <- lm(empgrow ~ GDPgrow, data=growth); summary(lm2)


# --- Ex 2: b) ---
# Visually inspect data and regression line. 

 plot(empgrow ~ GDPgrow, data=growth)
 abline(lm2, col="red")


# --- Ex 2: c) ---
# Are the coefficients significant? 

 # compare t-value of b2 (5.8) to 97.5% quantile of t-dist (2.07) and reject H0: b2=0.
 qt(p=1-0.025, df=23)
 qt(p=0.025, df=23, lower.tail=F) 


# --- Ex 2: f) ---
# Build a confidence interval for the slope. 

 b_1 <- lm2$coef[1]
 sd_b1 <- summary(lm2)$coef[1,2]
 t_0.975 <- qt(p=1-0.025, df=23)
 b_1 + c(-1,1)*sd_b1*t_0.975		# 95%-confidence interval for b_1




# --- Ex 3: The linear model with quadratic terms --------------------------------
 house <- read.csv("http://thiloklein.de/R/housing",header=T,sep=",")
 str(house)


# --- Ex 3: a) ---
# Estimate a model to test this, using total expenditure as a proxy for total income.

 lm3a <- lm(housing ~ total, data=house)
 summary(lm3a)


# --- Ex 3: b) ---
# Is a quadratic form more appropriate?

 house$totalsq <- house$total^2
 lm3b <- lm(housing ~ total + totalsq, data=house)
 summary(lm3b)




# --- Ex 4: Extrapolation and accuracy of least least squares --------------------------------

 eaef <- read.csv("http://thiloklein.de/R/eaef",header=T,sep=",")
 str(eaef)

 lm4 <- lm(weight ~ height, data=eaef)
 summary(lm4)




# --- Ex 5: Estimates for changing units of measurement --------------------------------

 eaef$weight_grams <- eaef$weight*454		# one pound is 454 grams
 eaef$height_metric <- eaef$height * 2.54		# one inch is 2.54 cm

 lm(weight_grams ~ height, data=eaef)
 5.562*454   # =2525.148

 lm(weight ~ height_metric, data=eaef)
 5.562496/2.54   # =2.189959




# --- Ex 6: Multiple linear regression --------------------------------

 hprice1 <- read.csv("http://thiloklein.de/R/hprice1",header=T,sep=",")
 str(hprice1)

 lm6 <- lm(price ~ sqrft + bdrms, data=hprice1)
 summary(lm6)




# --- Ex 7: Confidence intervals of regression coefficients (1) --------------------------------

 oilprice1 <- read.csv("http://thiloklein.de/R/oilprice1",header=T,sep=",")
 str(oilprice1)

 plot(price ~ api, data=oilprice1)
 lm7 <- lm(price ~ api, data=oilprice1)
 summary(lm7)

 t <- qt(p=0.975, df=13-2)   		# quantile of t distribution

 str(lm7)
 b <- lm7$coef[2]		    		# coefficient

 str(summary(lm7))
 se_b <- summary(lm7)$coef[2,2]	# standard error of coefficient

 b + c(-1,1)*se_b*t			# confidence interval

 ls(); rm(b, t, se_b)			# remove objects if no longer needed!




# --- Ex 8: Reversal of regressor and regressand --------------------------------

 lm(earnings ~ schooling, data=eaef)
 lm(schooling ~ earnings, data=eaef)

 ?cor
 cor(eaef$earnings, eaef$schooling, method="pearson")




# --- Ex 9: Regression against a constant (optional) --------------------------------

 summary(eaef$weight)
 lm(weight ~ 1, data=eaef)
 # lm(weight ~ -1 + c(rep(2,540)), data=eaef)




# --- Ex 10: Confidence intervals for regression coefficients (2) --------------------------------

 t5 <- qt(p=0.025, df=60-2, lower.tail=F)	# t-quantile 5%
 t1 <- qt(p=0.005, df=60-2, lower.tail=F) # t-quantile 1%

 myCI <- function(b, se, t){	# function to generate CI
   b + c(-1,1)*se*t
 }

 myCI(-.2, .07, t5)	# CI for beta=-0.2, se_beta=0.07; at 5% level
 myCI(-.12, .07, t5) 	# CI for beta=-0.12, se_beta=0.07; at 1% level





# -------------------------------------------------------------------
# --- End of Session ------------------------------------------------

 save.image("workspace_EndOfSession.RData")
 q("yes")




