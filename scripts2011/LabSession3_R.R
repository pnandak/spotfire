# -------------------------------------------------------------------
# MPO1 Quantitative Research Methods
# Thilo Klein 
# Lab Session 3: Model Selection; Inference; Dummy Variables

# Required libraries: car, lmtest, zoo, sandwich
  setwd("C:/Dokumente und Einstellungen/Thilo/Desktop/Lab_Sessions/Session3/Data")
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
 shccm(lm.hom, "hc0") 

 summary(lm.het)		# different results under heteroscedasticity
 shccm(lm.het) 

 rm(x, e, y.hom, y.het, lm.hom, lm.het)


 

# --- Ex 1: Multicollinearity; Omitted Variable Bias ------------------
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


# --- Ex 1: b) ---
 cor(cbind(x1, x2, x3))
 lm1b <- lm(y ~ x1 + x2)
 shccm(lm1b)
 vif(lm1b)		# VIF > 10, severe multicollinearity.


# --- Ex 1: c) ---
 lm1c <- lm(z ~ x2 + x3)
 shccm(lm1c)
 vif(lm1c)


# --- Ex 1: d) ---
 lm(y ~ x1)$coef		# cov(x1,x2)=1 -> OVB for b2; Intercept biased
 lm(y ~ x1 + x2)$coef
 lm(z ~ x2)$coef		# true model: z=30+x2+x3 -> Intercept biased
 lm(z ~ x2 + x3)$coef	




# --- Ex 2: Multicollinearity; Variance Inflation Factor ------------------
 salary <- read.table("http://thiloklein.de/R/salary.txt", header=T, sep="	")
 str(salary)
 lm2 <- lm(LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY, data=salary)
 shccm(lm2)
 vif(lm2)

 lm2ed <- lm(EDUC ~ LOGSALBEGIN + GENDER + MINORITY, data=salary)
 print( paste( "The R2 is",  summary(lm2ed)$r.squared ) )
 print( paste( "variance inflation factor is",  1/(1- summary(lm2ed)$r.squared) ) )

 cor(subset(salary, select=c("EDUC","LOGSALBEGIN","GENDER","MINORITY")))
 # Or:
 attach(salary)
 cor(cbind(EDUC, LOGSALBEGIN, GENDER, MINORITY))
 detach(salary)




# --- Ex 3: The effects of having highly correlated regressors ------------------
 eaef21 <- read.csv("http://thiloklein.de/R/eaef21", header=T)
 str(eaef21)
 lm3w <- lm( SIBLINGS ~ SM + SF, data = eaef21[eaef21$ETHWHITE==1, ] )
 lm3b <- lm( SIBLINGS ~ SM + SF, data = eaef21[eaef21$ETHBLACK==1, ] )
 lm3h <- lm( SIBLINGS ~ SM + SF, data = eaef21[eaef21$ETHHISP==1, ] )
 lm3w; lm3b; lm3h

 library(lmtest); library(zoo); library(sandwich)
 lm3w <- lm(SIBLINGS ~ SM + SF, data=eaef21[eaef21$ETHWHITE==1, ]); coeftest(lm3w, vcov=hc0)
 cor( cbind(eaef21$SM, eaef21$SF)[eaef21$ETHWHITE==1, ] )
 vif(lm3w)

 lm3b <- lm( SIBLINGS ~ SM + SF, data=subset(eaef21, ETHBLACK ==1) ); coeftest(lm3b, vcov=hc0)
 cor( subset(eaef21, ETHBLACK ==1, select=c(SM, SF)) )
 vif(lm3b)

 lm3h <- lm( SIBLINGS ~ SM + SF, data= subset(eaef21, ETHHISP==1) ); coeftest(lm3h, vcov=hc0)
 cor( subset(eaef21, ETHHISP ==1, select=c(SM, SF)) )
 vif(lm3h)


# --- Ex 3: b) ----
 lm3.1 <- lm(SIBLINGS ~ SM + SF, data=eaef21); shccm(lm3.1)

 linearHypothesis(model=lm3.1, "SM = SF", vcov=hc0)
 eaef21$SP <- eaef21$SF + eaef21$SM
 lm3.2 <- lm(SIBLINGS ~ SP, data=eaef21); shccm(lm3.2)




# --- Ex 4: OLS assumptions; dummy variables (optional) ------------------
 e <- rnorm(n=100, mean=1, sd=1)
 x <- runif(100)
 y <- 2*x + e
 OLS <- lm(y ~ x); shccm(OLS)
 par(mfrow=c(2,2))
 plot(OLS)




# --- Ex 5: Bank wages ------------------
 bank <- read.csv("http://thiloklein.de/R/bank", header=T)
 str(bank)

 bank$logsalbegin <- log(bank$salbegin)
 lm5i <- lm(logsal ~ educ + logsalbegin + gender + minority, data=bank); shccm(lm5i)


# --- Ex 5: ii) ---

 # 1="clerical"   2="custodial"  3="managerial"
 bank$jobcat.cler <- ifelse(bank$jobcat==1, 1, 0)
 bank$jobcat.cust <- ifelse(bank$jobcat==2, 1, 0)
 bank$jobcat.man <- ifelse(bank$jobcat==3, 1, 0)
 lm5iia <- lm(logsal ~ educ + male + minority + jobcat.cler + jobcat.cust, data=bank); shccm(lm5iia)

 bank$jobcat.fac <- factor(bank$jobcat)
 levels(bank$jobcat.fac) <- c("Clerical", "Custodial", "Managerial")
 lm5iib <- lm(logsal ~ educ + male + minority + jobcat.fac, data=bank); shccm(lm5iib)
 bank$jobcat.fac <- relevel(bank$jobcat.fac, ref="Managerial")	# change reference category/level

 linearHypothesis(model=lm5iia, "jobcat.cler = jobcat.cust", vcov=hc0)


# --- Digression: how to categorize a continuous variable ---
 bank$educ.cut <- cut(bank$educ, breaks=3)			# categorize variable in 3 categories/levels
 bank$educ.cut <- cut(bank$educ, breaks=c(0,12,16,Inf))	# alternatively: define categories yourself

 write.csv(bank, "bank_cat.csv")					# write data-frame to csv-file


# --- Ex 5: iii) ---
 table(bank$jobcat.fac)
 lm(logsal ~ educ + male + minority, data=subset(bank, jobcat==2) )
 lm(logsal ~ educ + male + minority, data=subset(bank, jobcat==3) )
 sum(bank$male[bank$jobcat==2])


# --- Ex 5: iv) ---
 bank$female <- 1 - bank$male
 bank$femaleandminority <- bank$female * bank$minority				# either this
 lm5iv <- lm(logsal ~ educ + male + minority + femaleandminority, data=bank)	# ..
 lm(logsal ~ educ + male + minority + female:minority, data=bank)			# or that.


# --- Ex 5: v) ---
 linearHypothesis(model=lm5iv, "male = minority", vcov=hc0)


# --- Ex 5: vi) ---
 lm5vi <- lm(logsal ~ educ + female + minority , data=bank); shccm(lm5vi)
 linearHypothesis(lm5vi, "educ = 0.07", vcov=hc0)
 linearHypothesis(lm5vi, c("educ = 0.07", "female = minority"), vcov=hc0)


# --- Ex 5: vii) ---
 lm(logsal ~ educ + female + minority + logsalbegin + logsalbegin:jobcat.man, data=bank)




# --- Ex 6: NO2 pollution ------------------
 no2poll <- read.csv("http://thiloklein.de/R/no2pollution", header=T)
 str(no2poll)

 lm6 <- lm(lno2 ~ lcars + temp + tchng23 + wndspd + wnddir + day, data=no2poll); shccm(lm6)
 linearHypothesis(lm6, "wndspd = wnddir", vcov=hc0)




# --- Ex 7: Programming in R ------------------
 data <- read.csv("http://thiloklein.de/R/dataset", header=T)
 str(data)

 # source("Rprogramming.R")





# -------------------------------------------------------------------
# --- End of Session ------------------------------------------------

 save.image("workspace_EndOfSession.RData")
 q("yes")




