# -------------------------------------------------------------------
# Master in Finance Econometrics Module
# Thilo Klein
# Lab Session 1: Descriptive Statistics and Linear Regression in R

# Required libraries: Rcmdr, timeDate
  setwd("C:/Dokumente und Einstellungen/Thilo/Desktop/LabSessions_MFin/Session1/Data")
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------




# --- Ex I-1: find help; R Commander; install/load packages -------------------
 ?memory.size		    # help if exact command is known
 help.search("memory size")  # help if exact command is not knowm
 install.packages("Rcmdr")   # install package Rcmdr
 library(Rcmdr)		    # load package




# --- Ex I-2: Load a dataset and save it with a different name ----------------
 eaef2 <- read.csv("http://thiloklein.de/R/eaef", header=T)  # read dataset from .csv file
 ls()   						# display active objects in workspace
 save("eaef2", file="eaef2.RData")		# save active object eaef2
 rm(eaef2) 						# clear object eaef2 from workspace
 rm(list=ls())					# clear workspace
 load("eaef2.RData")   				# load object eaef2




# --- Ex 1: Calculate summary statistics ----------------
 str(eaef2)
 summary(eaef2)
 attach(eaef2)

 hist(weight, col="grey", main="Histogram of Weight", xlab="weight in pounds")




# --- Ex 2: Calculate proportions of observations presenting a certain characteristic ----------------
# --- Ex 2: a) ---
# What is the proportion of observations with 3 siblings? 

 length(siblings[siblings==3])
 length(siblings)
 118/540

 # or:
 t <- table(siblings)
 round( t/sum(t), 2)*100


# --- Ex 2: b) ---
# What is the proportion of observations with weight less than 120?

 length(weight[weight<120])
 78/540

 # or:
 dim(subset(eaef2, weight<120))[1]/dim(eaef2)[1]




# --- Ex 3: Analysis of the frequency of discrete variables ----------------
# --- Ex 3: a) ---
# Tabulate the count, percentage, cumulative count and cumulative percentage for 
# every given number of siblings (from 0 to 13).

 table(siblings)				# count
 t <- table(siblings)			# percentage
 round( t/sum(t), 4 )*100
 cumsum(t)					# cumulative count
 round( cumsum(t/sum(t)), 4)*100	# cumulative percentage


# --- Ex 3: b) ---
# What is the proportion of observations with 3 siblings?

 t[4]/sum(t)




# --- Ex 4: Frequency of a combination of discrete variables (two-way tables) ----------------

 eaef2$agecut <- cut(age, breaks=quantile(age))
 attach(eaef2)
 table(agecut, siblings)




# --- Ex 5: Analysis of a variable conditioned on a discrete variable ----------------
# --- Ex 5: a) ---
# Evaluate the mean earnings conditioned on the four age-groups defined above.

 by(data=earnings, INDICES=agecut, FUN=mean)


# --- Ex 5: b) ---
# Plot histograms for earnings conditioned on the four age-groups.

 par(mfrow=c(2,2))
 by(data = earnings, INDICES = agecut, FUN = hist)




# --- Ex 6: Graphs ----------------

 plot(height ~ weight)
 hist(earnings)




# --- Ex 7: Generate linear transformation ----------------
 eaef2$ptearnings <- 2 + (earnings-2)*0.8
 eaef2$earnings <- log(earnings)




# --- Ex 8: T-tests ----------------
# --- Ex 8: b) ---
# Using R, perform this test and calculate the confidence interval at 99% 
# significance level.

 data <- scan("clipboard")
 mean(data) + c(-1,1)*sd(data)*qnorm(0.99)
 t.test(data, mu=60, alternative="less", conf.level=0.99)




# --- Ex 9: The linear model ----------------
# --- Ex 9: a) ---
# Review the contents and regress employment growth on GDP growth. Provide an 
# interpretation of the results.

 growth <- read.csv("http://thiloklein.de/R/growth",header=T,sep=",")
 str(growth)
 lm9 <- lm(empgrow ~ GDPgrow, data=growth); summary(lm9)


# --- Ex 9: b) ---
# Visually inspect data and regression line. 

 plot(empgrow ~ GDPgrow, data=growth)
 abline(lm, col="red")


# --- Ex 9: c) ---
# Are the coefficients significant? 

 # compare t-value of b2 (5.8) to 97.5% quantile of t-dist (2.07) and reject H0: b2=0.
 qt(p=1-0.025, df=23)
 qt(p=0.025, df=23, lower.tail=F) 


# --- Ex 9: f) ---
# Build a confidence interval for the slope. 

 b_1 <- lm9$coef[1]
 sd_b1 <- summary(lm9)$coef[1,2]
 t_0.975 <- qt(p=1-0.025, df=23)
 b_1 + c(-1,1)*sd_b1*t_0.975		# 95%-confidence interval for b_1




# --- Ex 10: The linear model with quadratic terms ----------------
 house <- read.csv("http://thiloklein.de/R/housing",header=T,sep=",")
 str(house)


# --- Ex 10: a) ---
# Estimate a model to test this, using total expenditure as a proxy for total income.

 lm10a <- lm(housing ~ total, data=house)
 summary(lm10a)


# --- Ex 10: b) ---
# Is a quadratic form more appropriate?

 house$totalsq <- house$total^2

 lm10b <- lm(housing ~ total + totalsq, data=house)
 summary(lm10b)




# --- Ex 11: Extrapolation and accuracy of least squares ----------------
 eaef <- read.csv("http://thiloklein.de/R/eaef",header=T,sep=",")
 str(eaef)

 lm11 <- lm(weight ~ height, data=eaef)
 summary(lm11)




# --- Ex 12: Estimates for changing units of measurement ----------------
 eaef$weight_grams <- eaef$weight*454		# one pound is 454 grams
 eaef$height_metric <- eaef$height * 2.54		# one inch is 2.54 cm

 lm(weight_grams ~ height, data=eaef)
 5.562*454   # =2525.148

 lm(weight ~ height_metric, data=eaef)
 5.562496/2.54   # =2.189959




# --- Ex 13: Multiple linear regression ----------------
 hprice1 <- read.csv("http://thiloklein.de/R/hprice1",header=T,sep=",")
 str(hprice1)

 lm13 <- lm(price ~ sqrft + bdrms, data=hprice1)
 summary(lm13)




# --- Ex 14: Reversal of regressor and regressand ----------------
 lm(earnings ~ schooling, data=eaef)
 lm(schooling ~ earnings, data=eaef)

 ?cor
 cor(eaef$earnings, eaef$schooling, method="pearson")




# --- Ex 15: Regression against a constant (optional) ----------------
 summary(eaef$weight)
 lm(weight ~ 1, data=eaef)
 # lm(weight ~ -1 + c(rep(2,540)), data=eaef)




# --- Ex 16: Confidence intervals for regression coefficients ----------------
 t5 <- qt(p=0.025, df=60-2, lower.tail=F)	# t-quantile 5%
 t1 <- qt(p=0.005, df=60-2, lower.tail=F) # t-quantile 1%

 myCI <- function(b, se, t){	# function to generate CI
   b + c(-1,1)*se*t
 }

 myCI(-.2, .07, t5)	# CI for beta=-0.2, se_beta=0.07; at 5% level
 myCI(-.12, .07, t5) 	# CI for beta=-0.12, se_beta=0.07; at 1% level




# --- Digression: my function to count NAs per variable ---
 showNAs
 showNAs(dataset2)





# -------------------------------------------------------------------
# --- End of Session ------------------------------------------------

 save.image("EndOfSession.RData")
 q("yes")




