# Lecure 7: Binary dependent variable models

 source("http://thiloklein.de/R/myfunctions.R")

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
 plot(BACH ~ ASVABC, pch="|", main="OLS") 
 lines(sort(lm1b$fitted) ~ sort(ASVABC), lwd=2, col=2)
 plot(BACH ~ ASVABC, pch="|", main="Logit") 
 lines(sort(glm1b$fitted) ~ sort(ASVABC), lwd=2, col=2)


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
 plot(BACH ~ ASVABC, ylim=c(-.5,1.5))
 lines(sort(glm1b$fitted) ~ sort(ASVABC), col="blue", lwd=2, lty=2)
 abline(lm1b, col="red", lwd=2)
 legend("topright", c("OLS", "Logit"), col = c("red","blue"), lwd = 2, lty=1:2)
 plot(lm1e$res ~ ASVABC) 
 abline(h = 0)
 legend("topright", "OLS residuals")


# --- Ex 1: f) ---
# For the logit model, generate the pseudo-R2.

 glm1f.null <- glm(BACH ~ 1, family=binomial(link=logit))
 1 - logLik(glm1b)[1]/logLik(glm1f.null)[1]


# --- Digression: Logit vs Probit coefficients ---

 # logit: 
 glm1b

 # probit:
 glm1g <- glm(BACH ~ ASVABC, family=binomial(link=probit))
 glm1g

 par(mfrow=c(1,3))
 plot(BACH ~ ASVABC, pch="|", main="Logit vs. Probit")
 lines(sort(glm1b$fitted) ~ sort(ASVABC), col="blue", lwd=2, lty=2)
 lines(sort(glm1g$fitted) ~ sort(ASVABC), col=2, lwd=2, lty=2)
 legend(27,.95, c("Logit", "Probit"), col = c("blue","red"), lwd = 2, lty=2)

 plot(BACH ~ ASVABC, pch="|", ylim=c(0,.1), xlim=c(min(ASVABC),35), main="lower tail")
 lines(sort(glm1b$fitted) ~ sort(ASVABC), col="blue", lwd=2, lty=2)
 lines(sort(glm1g$fitted) ~ sort(ASVABC), col=2, lwd=2, lty=2)
 legend("topleft", c("Logit", "Probit"), col = c("blue","red"), lwd = 2, lty=2)

 plot(BACH ~ ASVABC, pch="|", ylim=c(.85,1), xlim=c(63,max(ASVABC)), main="upper tail")
 lines(sort(glm1b$fitted) ~ sort(ASVABC), col="blue", lwd=2, lty=2)
 lines(sort(glm1g$fitted) ~ sort(ASVABC), col=2, lwd=2, lty=2)
 legend("bottomright", c("Logit", "Probit"), col = c("blue","red"), lwd = 2, lty=2)
