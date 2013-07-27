# -------------------------------------------------------------------
# MPO1-A Advanced Quantitative Research Methods
# Thilo Klein
# Lab Session 4: Limited Dependent Variables and Panel Data

# Libraries: stats, VGAM, sampleSelection, sandwich, plm, Hmisc, corrgram, ellipse
  setwd("C:/Dokumente und Einstellungen/Thilo/Desktop/Lab_SessionsLT/Data")
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------


###############################
# Limited Dependent Variables #
###############################


# --- Ex 1: Logit model -----------------------------

 eaef <- read.csv("http://thiloklein.de/R/Lent/eaef21.csv", header=T)
 str(eaef)
 attach(eaef)


# --- Ex 1: a) ---
# Observe the distribution of ASVABC. ASVABC accounts for the result of an ability test.

 hist(ASVABC)
 summary(ASVABC)


# --- Ex 1: b) ---
# Investigate if the probability of a respondent obtaining a bachelor's degree 
# from a four-year college (BACH=1) is related to the respondent's score on ASVABC,
# by estimating a linear model and a logit model.

 BACH <- ifelse(S > 12, 1, 0)
 lm1b <- lm(BACH ~ ASVABC); shccm(lm1b)

 glm1b <- glm(BACH ~ ASVABC, family=binomial(link=logit))
 summary(glm1b)


# --- Ex 1: c) ---
# Plot the probabilities estimated. 

 par(mfrow=c(1,2))
 plot(lm1b$fitted ~ ASVABC)
 plot(glm1b$fitted ~ ASVABC)


# --- Ex 1: d) ---
# In order to interpret the logit results, estimate the marginal effects at the mean value 
# of ASVABC (default) and at values of 40, 55 and 70.

 M <- model.matrix(glm1b)
 str(data.frame(M))

 F.log <- function(x){
   1/(1 + exp(-x))
 }
 f.log <- function(x){
   F.log(x)*(1-F.log(x))
 }

 M[,2] <- 40; glm1b$coef[2] * f.log(mean(M%*%glm1b$coef))
 ## Or use R built in logistic function
 library(stats)
 M[,2] <- 55; glm1b$coef[2] * dlogis(mean(M%*%glm1b$coef))
 ## Or use the simple outcome equation instead
 glm1b$coef[2] * dlogis(c(1,70)%*%glm1b$coef)


# --- Ex 1: e) ---
# Give an interpretation of the OLS regression and explain why OLS is not a satisfactory 
# estimation method for this kind of model. 

 par(mfrow=c(1,2))
 plot(BACH ~ ASVABC, ylim=c(-1,2))
 lm1e <- lm(BACH ~ ASVABC)
 abline(lm1e, col="red")
 plot(lm1e$res ~ ASVABC) 
 abline(h=mean(lm1e$res), col="red")


# --- Ex 1: f) ---
# For the logit model, generate the pseudo-R2.

 glm1f.null <- glm(BACH ~ 1, family=binomial(link=logit))
 1 - logLik(glm1b)[1]/logLik(glm1f.null)[1]




# --- Ex 2: Probit model -----------------------------
 loan <- read.csv("http://thiloklein.de/R/Lent/loanapp.csv", header=T)
 str(loan)


#--- Ex 2: a) ---
# Find the estimated probability of loan approval for both whites and nonwhites. 
# How do these compare with the linear probability estimates? 

 glm2a <- glm(approve ~ white, family=binomial(link=probit), data=loan)
 summary(glm2a)
 # Prob. of loan approval
 pnorm(c(1,1)%*%glm2a$coef) # whites
 pnorm(c(1,0)%*%glm2a$coef) # non-whites

 lm2a <- lm(approve ~ white, data=loan)
 shccm(lm2a)

 c(1,1)%*%lm2a$coef # whites
 c(1,0)%*%lm2a$coef # non-whites

 # In this case, predicted probabilities form probit and LPM are the cell frequencies
 by(loan$approve, loan$white, mean)


#--- Ex 2: b) ---
# It might be that white people present characteristics which lead to higher approval 
# of loans.

 glm2b <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + married + dep 
		+ sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr
		, family=binomial(link=probit), data=loan)

 ## b1. Is there statistically significant evidence of discrimination against nonwhites? 

 summary(glm2b)


 ## b2. Is there statistically significant evidence of discrimination against family 
 ## status (married and dep)?

 # re-run the model without dep and marriage (making sure the sample is the same!)
 glm2b2 <- glm(approve ~ white + hrat + obrat + loanprc + unem + male  
		+ sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr
		, family=binomial(link=probit), data=subset(loan, is.na(dep)==F))
 # generate the LR statistic and obtain the p-value
 D <- 2*( logLik(glm2b)[1] - logLik(glm2b2)[1] )
 pchisq(q=D, df=2, lower.tail=F)


 ## b3. Estimate the same model, now reporting the marginal effects (use the dnorm command).
 ## How are these effects calculated for continuous and binary independent variables?

# --- Digression: Graphical model fit ---
 M<-model.matrix(glm2b)
 str(data.frame(M))
 x<-seq(0,3,0.01)
 # x<-seq(-10,10,0.01)
 y<-rep(NA,length(x))
 for(i in 1:length(x)){ 
	M[,5]<-x[i]
	y[i]<- pnorm(mean(M%*%coef(glm2b)))
 }
 plot(y~x,type="l",ylab="P(approve=1)",xlab="loanprc", col="red", lwd=2)
 points(rep(0.2, length(loan$loanprc)) ~ loan$loanprc, pch="|")


 # marginal effects (continuous variables): dep (number of dependents)
 glm2b$coef[9] * dnorm(mean(M%*%coef(glm2b)))

 # marginal effects (binary variables): white
 glm2b$coef[2] * dnorm(mean(M%*%coef(glm2b)))
 # -> not meaningful
 str(data.frame(M))
 M1 <- M0 <- M
 M1[,2] <- 1; M0[,2] <- 0
 pnorm(mean(M1%*%coef(glm2b))) - pnorm(mean(M0%*%coef(glm2b)))
 # -> more meaningful


 ## b4. Predict the events giving a success in the outcome as those cases with a 
 ## predicted probability higher than 0.5.

 success <- ifelse(glm2b$fitted > 0.5, "success", "reject")
 t <- table(success, glm2b$model$approve)
 t/sum(t)


#--- Ex 2: c) ---
# Estimate the model from part b) by logit. Compare the coefficient on white to the 
# probit estimate. 

 glm2c.logit <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + married + dep 
		+ sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr
		, family=binomial(link=logit), data=loan)
 glm2c.logit; glm2b
 glm2c.logit$coef[2]*0.625;  glm2b$coef[2]


#--- Ex 2: d) ---
# Compare the performance for all the models, consider actual and predicted values 
# for different categories ...

 ## 3 Models:
 # - Linear lm2d:
 lm2d <- lm(approve ~ white + hrat + obrat + loanprc + unem + male + married + dep 
		+ sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr, data=loan)
 # - Probit glm2b 
 # - Logit  glm2c.logit

 data <- data.frame(mean.approve=glm2b$model$approve, mean.app_LPM=lm2d$fitted, mean.app_probit=glm2b$fitted, mean.app_logit=glm2c.logit$fitted)
 INDICES <- data.frame(dependencies=glm2b$model$dep)
 FUN <- mean
 # the above is not what we want... did not yet get the following to work:
 # FUN <- function(x){ mean(ifelse(x>.5,1,0)) }
 by(data, INDICES, FUN)

 obrat.cat <- cut(glm2b$model$obrat, breaks=quantile(loan$obrat, seq(0,1,.1)))

 # complete this exercise in your own time.




# --- Ex 3: Tobit model -----------------------------

 fringe <- read.csv("http://thiloklein.de/R/Lent/FRINGE.csv", header=T)
 str(fringe)

# --- Ex 3: a) ---
# For what percentage of the workers in the sample is pension equal to zero? 
# What is the range of pension for workers with nonzero pension benefits? 
# Why is a Tobit model appropriate for modeling pension? 

 nopension <- ifelse(fringe$pension==0, 1, 0)
 table(nopension)
 summary(fringe$pension[nopension==0])


# --- Ex 3: b) ---
# Do whites and males have statistically significant higher expected pension benefits?

 lm3b <- lm(pension ~ exper + age + tenure + educ + depends + married + white + male
	, data=fringe)

 library(VGAM)
 vglm3b <- vglm(pension ~ exper + age + tenure + educ + depends + married + white + male
	, data=fringe, tobit(Lower=0), trace=TRUE)
 summary(vglm3b)

 vglm3b2 <- vglm(pension ~ exper + age + tenure + educ + depends + married 
	, data=fringe, tobit(Lower=0), trace=TRUE)
 # generate the LR statistic and obtain the p-value
 D <- 2*( logLik(vglm3b)[1] - logLik(vglm3b2)[1] )
 pchisq(q=D, df=2, lower.tail=F)


# --- Ex 3: c) ---
# Estimate the difference in expected pension benefits for a white male and a nonwhite 
# female, both of whom are 35 years old, single with no dependents, have 16 years of 
# education, and 10 years of experience. 

 # manual tobit-prediction for white males
 xb <- sum( vglm3b@coefficients[c(1,3:10)] * c(1, 10, 35, 10, 16, 0, 0, 1, 1) )
 sigma2 <- exp(vglm3b@coefficients[2])
 pnorm(xb/sigma2)*xb + sigma2*dnorm(xb/sigma2)

 # manual tobit-prediction for non-white females
 xb <- sum( vglm3b@coefficients[c(1,3:10)] * c(1, 10, 35, 10, 16, 0, 0, 0, 0) )
 sigma2 <- exp(vglm3b@coefficients[2])
 pnorm(xb/sigma2)*xb + sigma2*dnorm(xb/sigma2)
 
 # comparison with linear model:
 sum(lm3b$coef[8:9])

 # try predict.vglm ???
 # fringe.wm <- fringe.nwf <- data.frame(vglm3b@x)[1,]

 # exper = tenure = 10, age = 35, educ = 16, depends = 0, married = 0, white = 1, and male = 1
 # fringe.wm[,2:9] <- c(10, 35, 10, 16, 0, 0, 1, 1)
 # p.wm <- predict.vglm(vglm3b, newdata=fringe.wm)
 # exp(6.518521)	# = sigma^2

 # fringe.nwf[,2:9] <- c(10, 35, 10, 16, 0, 0, 0, 0)
 # p.nwf <- predict.vglm(vglm3b, newdata=fringe.nwf)
 # p.wm[1] - p.nwf[1]


# --- Ex 3: d) ---
# Add union to the Tobit model and comment on its significance. 

 vglm3d <- vglm(pension ~ exper + age + tenure + educ + depends + married + white + male
	+ union, data=fringe, tobit(Lower=0), trace=TRUE)
 summary(vglm3d)


# --- Ex 3: e) ---
# Apply the Tobit model from part d) but with peratio, the pension-earnings ratio, as 
the dependent variable. Does gender or race have an effect on the pension-earnings ratio?

 vglm3e <- vglm(peratio ~ exper + age + tenure + educ + depends + married + white + male
	+ union, data=fringe, tobit(Lower=0), trace=TRUE)
 summary(vglm3e)



# --- Ex 4: Heckman model (optional) -----------------------------
# Greene (2003): example 22.8, page 786
# Wooldridge (2003): example 17.5, page 590 

 mroz <- read.csv("http://thiloklein.de/R/Lent/mroz.csv", header=T)
 str(mroz)

# --- Ex 4: a) ---
# Using the 428 women who were in the workforce, estimate the return to education by OLS 

 mroz.w <- subset(mroz, inlf==1)
 lm4a <- lm(lwage ~ educ + exper + expersq + nwifeinc + age + kidslt6 + kidsge6, data=mroz.w)
 summary(lm4a)


# --- Ex 4: b) ---
# Estimate the return to education by heckit from library sampleSelection, where all exogenous 
# variables show up in the second-stage regression. 

 # install.packages("sampleSelection")
 library(sampleSelection)
 mroz$lfp <- as.logical(mroz$inlf)
 heck4b <- heckit(selection = lfp ~ exper + expersq + nwifeinc + age + kidslt6 + kidsge6
		, outcome = lwage ~ educ + exper + expersq + nwifeinc + age + kidslt6 + kidsge6
		, data=mroz, method="2step")
 summary(heck4b)


# --- Ex 4: c) ---
# Using only the 428 observations for working women, regress lambda_hat on 
# educ, exper, exper2, nwifeinc, age, kidslt6, and kidsge6. 
# How big is the R-squared? How does this help explain your findings from part b)?

 invMills <- heck4b$invMillsRatio
 lm4c <- lm(invMills[mroz$inlf==1] 
		~ educ + exper + expersq + nwifeinc + age + kidslt6 + kidsge6, data=mroz.w)
 summary(lm4c)


# --- Ex 4: d) ---
# Finally, estimate the return to education by heckit, where restrictions are applied in 
# the second stage. In other words, the regression is log(wage), on educ, exper, exper2 
# and lambda_hat. Calculate lambda_hat considering exper, exper2, nwifeinc, age, kidslt6, 
# and kidsge6 as explanatories.

 heck4d <- heckit(selection = lfp ~ exper + expersq + nwifeinc + age + kidslt6 + kidsge6
		, outcome = lwage ~ educ + exper + expersq #+ invMills
		, data=mroz, method="2step")
 summary(heck4d)





##############
# Panel Data #
##############

# --- Ex 1: Least Square Dummy Variable estimation, including dummies per time-period -----------------------------
 fert <- read.csv("http://thiloklein.de/R/Lent/FERTIL1.csv", header=T)
 str(fert)

# --- Ex 1: a) ---
# After controlling for all other observable factors, what has happened to fertility 
# rates over time?

 fert$year <- as.factor(fert$year)
 lm1a <- lm(kids ~ educ + age + agesq + black + east + northcen + west + farm + othrural 
	+ town + smcity + year, data=fert)
 shccm(lm1a)


# --- Ex 1: b) ---
# South is the base group. Test whether region of the country at age 16 has an effect 
# on fertility.

 hc0 <- function(x) vcovHC(x, type = "HC0")
 hc1 <- function(x) vcovHC(x, type = "HC1")
 library(sandwich)
 lht(lm1a, c("east=0","northcen=0","west=0"), vcov=hc0)


# --- Ex 1: c) ---
# Test whether other living environment characteristics at 16 have an effect on fertility.

 lht(lm1a, c("farm","othrural","town","smcity"), vcov=hc0)

# --- Ex 1: d) ---
# Heteroskedasticity. Has the variance of error u changed over time?

 lm1d <- lm(lm1a$res^2 ~ year, data=fert)
 shccm(lm1d)
 lht(lm1d, c("year74","year76","year78","year80","year82","year84"),vcov=hc0)


# --- Ex 1: e) ---
# Add interaction terms y74*educ,...,y84*educ to the model estimated. Explain what 
# these represent. Are they jointly significant?

 lm1e <- lm(kids ~ educ + age + agesq + black + east + northcen + west + farm + othrural 
	+ town + smcity + year + educ:year, data=fert)
 shccm(lm1e)
 lht(lm1e, c("educ:year74","educ:year76","educ:year78","educ:year80","educ:year82"
	,"educ:year84"),vcov=hc0)




# --- Ex 2: Pooled OLS (POLS), difference-equations and fixed effects -----------------------------
 murder <- read.csv("http://thiloklein.de/R/Lent/MURDER.csv", header=T)
 str(murder)


# --- Ex 2: b) ---
# Using just years 1990 and 1993, estimate the equation from part a) considering time 
effects. Do you find any evidence for a deterrent effect?

 murder$id <- as.factor(murder$id)
 murder$year <- as.factor(murder$year)
 murder.y <- subset(murder, d90==1 | d93==1)
 lm1b <- lm(mrdrte ~ d93 + exec + unem, data=murder.y)
 shccm(lm1b)
 

# --- Ex 2: c) ---
# Now, using 1990 and 1993, estimate the equation by FIXED EFFECTS.

 ## LSDV (Least Squares Dummy Variable estimator)
 lm2c.LSDV <- lm(mrdrte ~ d93 + exec + unem + id, data=murder.y)
 summary(lm2c.LSDV)

 ## Time-demeaning ("within" estimator)
 library(plm)
 plm2c.within <- plm(mrdrte ~ d93 + exec + unem, data=murder.y
			, model="within", effect="individual", index=c("id","year"))
 summary(plm1c.within)

 # First differencing ("first-differencing" estimator)
 plm2c.fd <- plm(mrdrte ~ d93 + exec + unem, data=murder.y
			, model="fd", effect="individual", index=c("id","year"))
 summary(plm2c.fd)

 ## Note: all models produce exactly the same results! 
 ## (for the "fd" model, the effect of d93 is lost due to differencing)


# --- Ex 2: d) ---
# Use the heteroscedasticity-robust standard error for the estimations in part c).

 coeftest(lm2c.LSDV, vcov=hc0)
 coeftest(plm2c.within, vcov=hc0) 
 # errors need to be corrected (Farnsworth, p. 20)?
 # plm2c.within gives same results for se as STATA.


# --- Ex 2: e) ---
# Find the state that has the largest number for the execution variable (exec) in 1993. 
# How much higher is this value from the next highest value?

 tail( murder[order(murder$year, murder$exec),] )


# --- Ex 2: f) ---
# Estimate the equation, dropping Texas from the analysis. Compute the usual and 
# heteroskedasticity-robust standard errors. What do you find? 

 plm2f.within <- plm(mrdrte ~ d93 + exec + unem, data=subset(murder.y, state!="TX")
			, model="within", effect="individual", index=c("id","year"))
 coeftest(plm2f.within); coeftest(plm2f.within, vcov=hc0)


# --- Ex 2: g) ---
# Finally, use all data. Estimate the two-way fixed effects with robust standard 
# errors and conclude.

 plm2g.within <- plm(mrdrte ~ exec + unem, data=murder
			, model="within", effect="twoways", index=c("id","year"))
 coeftest(plm2g.within)
 coeftest(plm2g.within, vcov=hc0)




# --- Ex 3: Panel Data, Random Effects, Fixed Effects and First Differences -----------------------------
 wage <- read.csv("http://thiloklein.de/R/Lent/wagepan.csv",header=T)
 str(wage)

 waget <- pdata.frame(wage, c("nr","year"))
 head(waget)
 summary(waget)


# --- Ex 3: a) ---
# Obtain summary statistics for lwage, educ, black, hisp, exper, married and union.

 wage$nr <- as.factor(wage$nr)
 wage$year <- as.factor(wage$year)

 wage.sub <- subset(wage, select=c("lwage","educ","black","hisp","exper","married","union"))
 for(i in 1:7){
   m <- mean(wage.sub[,i])
   s.o <- sd(wage.sub[,i])									# overall
   s.b <- sd(by(wage.sub[,i], wage$nr, mean))						# between
   s.w <- sd( unlist( by( wage.sub[,i], wage$nr, function(x) x - mean(x) ) ) )# within
   cat("------------------------------------------ \n")
   cat("Variable:", names(wage.sub)[i], "\n")
   print(round( data.frame(Mean=m, SE.overall=s.o, SE.between=s.b, SE.within=s.w), 3))
   cat("\n")
 }


# --- Ex 3: b) ---
# Estimate a wage equation. Use simple OLS. Comment on the results. In particular, 
# is this a panel data estimator?

 lm3b <- lm(lwage ~ year + educ + black + hisp + exper + married + union, data=wage)
 shccm(lm3b)


# --- Ex 3: c) ---
# In order to get an indication whether the errors are correlated over time, we will 
# look at the correlations of the residuals over time.

 e <- lm3b$res
 e_1 <- unlist( by(e, wage$nr, function(x) c(NA, x[-length(x)])) )
 e_2 <- unlist( by(e, wage$nr, function(x) c(NA, NA, x[-c(7:8)])) )
 e_3 <- unlist( by(e, wage$nr, function(x) c(NA, NA, NA, x[-c(6:8)])) )
 e_4 <- unlist( by(e, wage$nr, function(x) c(rep(NA,4), x[-c(5:8)])) )

[1] -0.01248641  0.06024635 -0.46856702  0.14015050 -0.48961525  0.20037236 -0.06485808 -0.39711911
[1]          NA -0.01248641  0.06024635 -0.46856702  0.14015050 -0.48961525  0.20037236 -0.06485808
[1]          NA          NA -0.01248641  0.06024635 -0.46856702  0.14015050 -0.48961525  0.20037236
...

 C <- cbind(e, e_1, e_2, e_3, e_4)
 library(Hmisc)
 # cor(C, method="pearson", use="complete")
 # cor(C, method="pearson", use="pairwise.complete")
 rcorr(C)

 ## or simply:
 acf(e)


# --- Digression: Correlation Matrix with significance levels ---
 corstars <- function(x){
   x <- as.matrix(x)
   R <- rcorr(x)$r
   p <- rcorr(x)$P
   mystars <- ifelse(p < .01, "***", ifelse(p < .05, "** ", ifelse(p < .1, "*  ", "   ")))
   R <- format(round(R, 3))
   Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
   rownames(Rnew) <- colnames(x)
   colnames(Rnew) <- colnames(x)
   Rnew[upper.tri(Rnew,diag=TRUE)] <- ""
   Rnew <- data.frame(Rnew)[-1,-length(colnames(x))]
   return(Rnew)
 } 
corstars(C)


# --- Digression: Graphical Representation of Correlation Matrix (CORRGRAM) ---
 # install.packages("corrgram")
 library(corrgram)
 corrgram(C, order=FALSE, lower.panel=panel.ellipse,
  upper.panel=panel.pts, text.panel=panel.txt,
  diag.panel=panel.minmax,
  main="Individual Level Part-Worths") 


# --- Digression: Graphical Representation of Correlation Matrix (PLOTCORR) ---
 library(ellipse)
 corC <- cor(C, use="complete")
 colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white",
            "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")   
 plotcorr(corC, col=colors[5*xc + 6], type = "lower")


# --- Ex 3: d) ---
# Adjust the standard errors for the correlation of the residuals over time per individual. 
# Are these standard errors different from the simple OLS ones? 

 library(lmtest)
 coeftest(lm3b)
 clx(lm3b, 1, wage$nr)

 # Alternatively:
 lm3d.ols <- plm(lwage ~ year + educ + black + hisp + exper + married + union
			, data=wage, model="pooling", index=c("nr","year"))
 coeftest(lm3d.ols) # -> same as coeftest(lm3b)
 # Method "arellano" also allows for general heteroskedasticity / serial correlation structure:
 # "Panel data econometrics in R: the plm package", page 31.
 coeftest(lm3d.ols, vcov=pvcovHC(lm3d.ols, method="arellano")) # -> same as clx(lm3b, 1, wage$nr)


# --- Ex 3: e) ---
# Estimate the model as in b), allowing for random and fixed unobserved individual effects.

 ## OLS:
 coeftest(lm3d.ols)

 ## Random Effects:
 lm3e.re <- plm(lwage ~ year + educ + black + hisp + exper + married + union
			, data=wage, model="random", effect="individual", index=c("nr","year"))
 summary(lm3e.re)

 ## Fixed Effects
 lm3e.fe <- plm(lwage ~ year + educ + black + hisp + exper + married + union
			, data=wage, model="within", effect="individual", index=c("nr","year"))
 coeftest(lm3e.fe)


# --- Ex 3: f) ---
# What do you conclude from the Hausman-test result? 

 # Hausman test
 phtest(lm3e.fe, lm3e.re)

 # Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
 plmtest(lm3e.ols, type="bp")
 # we reject the null and conclude that random effects is appropriate


# --- Ex 3: g) ---
# Estimate the model in first differences. Check the autocorrelation structure of the 
# residuals in the first-differenced model.

 lm3e.fd <- plm(lwage ~ year + educ + black + hisp + exper + married + union
			, data=wage, model="fd", effect="individual", index=c("nr","year"))
 coeftest(lm3e.fd) 

 e <- lm3e.fd$res
 acf(e)


# --- Ex 3: i) ---
# Estimate the between estimator. What does this estimator collect?

 lm3e.b <- plm(lwage ~ year + educ + black + hisp + exper + married + union
			, data=wage, model="between", effect="individual", index=c("nr","year"))
 coeftest(lm3e.b) 






# -------------------------------------------------------------------
# --- End of Session ------------------------------------------------

 save.image("EndOfSession.RData")
 q("yes")


