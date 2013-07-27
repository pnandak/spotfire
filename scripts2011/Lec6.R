# -------------------------------------------------------------------
# MPO1-A Advanced Quantitative Research Methods
# Lec 6: SEM

# Libraries: lmtest, sandwich, AER, systemfit
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------




# --- Ex 10:  -----------------------------
 fish <- read.csv("http://thiloklein.de/R/Lent/fish.csv", header=T)
 str(fish)

# --- Ex 10: c) ---
 fish$weekday <- ifelse(fish$mon==1,"Mon", 
			ifelse(fish$tues==1,"Tue", 
			ifelse(fish$wed==1,"Wed",
			ifelse(fish$thurs==1,"Thu","Fri"))))
 fish$weekday <- as.factor(fish$weekday)
 levels(fish$weekday)

head(fish)

# Assume that the demand equation can be written in equilibrium for each period as:
# log(totqty_t) = const + alpha_1 log(avgprice) + week day dummy effects + error  
# Demand is allowed to differ across days  of the week. 
# Price variable is endogenous. 
# Q: What additional information 
# do we need to consistently estimate the demand-equation parameters?

# To estimate the demand equations, need at least one exogenous variable that 
# appears in the supply equation (equation for price = marginal cost).

# If we are to estimate the equation for price, and the variables we have are
# wave2t and wave3t - measures of ocean wave heights over past few days, 
# Q: what assumptions are needed to use wave2 and wave3 as IVs for price 
# in the demand equation?

# Two assumptions:  
# That wave2t and wave3t can be properly excluded from the demand equation.  
# Arguable, as wave heights are determined partly by weather, 
# and demand at a local fish  market could depend on weather. 
# Second assumption is that at least one of them appears in the supply equation.

# Check are the two variables are jointly significant in the 
# reduced form for log(avgprc_t)

 lm10c <- lm(lavgprc ~ weekday + wave2 + wave3, data=fish)
 #summary(lm10c) 
 coeftest(lm10c, vcov=hc0)
 linearHypothesis(lm10c, c("wave2=0","wave3=0"), vcov=hc0)

# The variables wave2t and wave3t are jointly significant.

# --- Ex 10: d) ---
# Now, estimate the demand equation by 2SLS. What is the 95% confidence interval for the 
# price elasticity of demand? Is the estimated elasticity reasonable?

 library(systemfit)
 #?systemfit

 ## Specify the system
 eqDemand <- ltotqty ~ lavgprc + weekday 
 eqSupply <- lavgprc ~ ltotqty + wave2 + wave3 
 system <- list( demand = eqDemand, supply = eqSupply )
 inst <- ~ wave2 + wave3 + weekday 

 ## 2SLS estimation
 lm10d.sem <- systemfit(system, "2SLS", inst=inst, data=fish) 
 coeftest(lm10d.sem)
 linearHypothesis(lm10d.sem, c("demand_weekdayMon=0","demand_weekdayTue=0","demand_weekdayWed=0","demand_weekdayThu=0"))

 ## OLS estimation
 lm10d.ols <- systemfit(system, data=fish)
 coeftest(lm10d.ols)

#The point estimate of demand elasticity  -0.82:  
#a 10 percent increase in price reduces  quantity demanded by about 8.2%.

#Given the supply equation evidently depends on the wave variables, 
#what assumptions would we need to make in order to estimate 
#the price elasticity of supply?

#have to assume that the day-of-the-week dummies do NOT appear in the supply equation, 
#AND they do appear in the demand  equation.  
#saw earlier that there are day-of-the-week effects in the demand function.
#So, in the reduced form equation for log(avgprc) 

#Q: are the day-of-the-week dummies jointly significant? 
# --- Ex 10: e) ---
 lm10e <- lm(lavgprc ~ weekday + wave2 + wave3, data=fish) 
 coeftest(lm10e, vcov=hc0)
 linearHypothesis(lm10e, c("weekdayMon=0","weekdayTue=0","weekdayWed=0","weekdayThu=0"), vcov=hc1)

#Conclusion about being able to estimate the supply elasticity?
#In the estimation of the reduced form for log(avgprct) variables 
#mon, tues, wed, and thurs are jointly insignificant  
#Need to examine the demand function (do these variables appear?} 

lm10d.iv <- ivreg(ltotqty ~ lavgprc + weekday | wave2 + wave3 + weekday, data=fish)
lm10d.iv
coeftest(lm10d.iv, vcov=hc0)
linearHypothesis(lm10d.iv, c("weekdayMon=0","weekdayTue=0","weekdayWed=0","weekdayThu=0"), vcov=hc1)

#The joint test rejects the null that mon, tues, wed, and thurs are jointly insignificant in the demand function
# so supply is identified


# --- based on paper: ---
# Testing for Imperfect Competition at the Fulton Fish Market 
# in Rand Journal of Economics, 1995, 26, 75-92.




