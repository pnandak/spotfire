#source("C:/Users/Iris/Documents/PS231A/lab11/lab11Rscript.r", echo=T)
sink("C:/Users/Iris/Documents/PS231A/lab11/Lab11outMay2.txt", type="output")
#LAB 11
#DETECTING OUTLIERS
  #GRAPHICAL/EYE BALL TEST
  #DIAGNOSTIC TESTS
  
#================
#Outliers
#================

set.seed(007)
gpa<-runif(100, 1, 4)
set.seed(777)
error<-rnorm(100, 0,50)
sat<-100*gpa+50*gpa^2+error    

newsat<-sat
newsat[5]<-50          #create extreme cases  
newsat[25]<-1600
newsat[35]<-1600

summary(gpa)
summary(newsat)


#==========================================
#PART I: "EYE-BALL" TEST---DETECT OUTLIER GRAPHICALLY
#==========================================
par(mfrow=c(1,2))

#~~~~~~~~~~~~~~~~~~~~~~~~~~
#method one: boxplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~
boxplot(sat, ylim=c(100,1600), main="No outlier")
boxplot(newsat, ylim=c(100,1600), main="Outlier")

#~~~~~~~~~~~~~~~~~~~~~~~~~~
#method two: plot (X,Y)
#~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(gpa,sat, ylim=c(100,1600), main="No outlier")
plot(gpa,newsat, ylim=c(100,1600), main="Outlier")


#~~~~~~~~~~~~~~~~~~~~~~~~~~
#we can use identify() to find out interactively which are the outliers
#~~~~~~~~~~~~~~~~~~~~~~~~~~
par(mfrow=c(1,1))
plot(gpa,newsat, ylim=c(100,1600), main="Outlier")
identify(gpa,newsat)      #hit 'escape' when done

#through the graph, i got observation 5,25,35

#~~~~~~~~~~~~~~~~~~~~~~~~~~                            
#fit a regression line to the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~
lm.out<-lm(newsat~gpa)


#~~~~~~~~~~~~~~~~~~~~~~~~~~
#method three: histogram of residuals after running regression
#~~~~~~~~~~~~~~~~~~~~~~~~~~

hist(residuals(lm.out), freq=FALSE, nclass=10)  # plot residuals
hist(rstudent(lm.out), freq=FALSE, nclass=10)   # plot studentized residuals



#==========================================
#PART II: MORE VIGOROUS DIAGNOSTIC TESTS
#==========================================
#BACKGROUND
#unusal data are problematic in linear models fit by least squares because they can influence the results of the analysis. 
#but what is an outlier? what are the criteria used to determine if a case is an outlier or not?

#a univariate outlier is a value of Y or X that is unconditionally unusal; such value may or may not be a regression outlier.

#in regression context, an outlier is an observation whose dependent-variable value is conditionally unusal given the value of the independent variable(s).

#conceptually, we can think of two distinct concepts related to outliers--- 'influence' and 'leverage'

#leverage assesses how far away a value of the independent variable is from the mean value: the further away the observation, the more leverage it has.

#observations whose inclusion or exclusion result in substantial changes in the fitted model (i.e. coefficients, fitted values) are said to be influential.

#outliers with respect to the predictors (a.k.a. independent variables) are called 'leverage points'. They can affect the regression model, though their corresponding response variables need NOT be outliers. These leverage point may or may not substantively change the regression model.

#to elaborate, let's consider the following cases:
#Case 1: an (univariate) outlier can have x-value near the center of the X distribution. as a result, deleting this outlier has little impact on the least squares fit, leaving the slope b unchanged and only affecting the intercept slightly. This case only exerts a low leverage and low influence on the regression fit.

x<-c(1,2.6,2,3,4)
y<-c(3,3.5,4,5,6)                  #the 2nd observation is an outlier

data1<-data.frame(cbind(x,y))      #delete the 2nd observation
data2<-data1[-c(2),]

summary(lm(y~x, data=data1))
summary(lm(y~x, data=data2))      #intercept & slope change slightly

#graphical illustration
plot(x,y, pch=8, cex=2)
abline(lsfit(data1$x,data1$y), col="red", lwd=2)
abline(lsfit(data2$x,data2$y), col="blue", lty=2, lwd=2)



#CASE 2: in contrast, another outlier can have an unusal x value (i.e.outside the range of X distribution--high leverage). Deleting this case would markedly change both slope and intercept. In other words, this case exerts a strong leverage and influence on the regression coefficients.


x2<-c(1,2,3,4,10)
y2<-c(3,4,5,6,7)

data1<-data.frame(cbind(x2,y2))
data2<-data1[-c(5),]                #delete the last observation that has high leverage and influence

summary(lm(y2~x2, data=data1))
summary(lm(y2~x2, data=data2))      #intercept and slope change significantly!

#graphical illustration
plot(x2,y2, pch=8, cex=2)
abline(lsfit(data1$x2,data1$y2), col="red", lwd=2)    #using all observations
abline(lsfit(data2$x2,data2$y2), col="blue", lty=2, lwd=2)     #deleting the extreme case


#Case 3: the outlier is far from the range of values for X and Y (i.e. high leverage). however, excluding this point does NOT change the regression coefficients. in other words, this outlier has high leverage but no influence. this point is in line with the rest of the data and hence it is NOT a regression outlier.


x3<-c(1,2,3,4,10)
y3<-c(11,12,13,14,20)

data1<-data.frame(cbind(x3,y3))
data2<-data1[-c(5),]                #delete the last observation that has high leverage and influence

summary(lm(y3~x3, data=data1))
summary(lm(y3~x3, data=data2))      #intercept and slope does not change 

#graphical illustration
plot(x3,y3, pch=8, cex=2)
abline(lsfit(data1$x3,data1$y3), col="red", lwd=2)    #using all observations
abline(lsfit(data2$x3,data2$y3), col="blue", lty=2, lwd=2)     #deleting the extreme case does NOT change the slope



#----------------------------
#MEASURING LEVERAGE
#----------------------------
#Standardized residual plots
plot(rstudent(lm.out))
abline(h=c(-1,1), lty=2)                  #we know 68% of data should be within 1 st-dev
abline(h=c(-2,2), lty=5, col="red")       #we know 95% of data should be within 2 st-dev
identify(rstudent(lm.out))                #let's see which cases fall beyond 2 st-dev?
                                          #hit 'escape' to leave


#----------------------------
#MEASURING INFLUENCE
#----------------------------
#use DFBETA test 
#~~~~~~~~~~~~~~~~~~~~~~~~~~                            

#dfbetas gives the CHANGE in the estimated parameters if an observation is excluded, relative to its standard error
#D_ij* = D_ij / (SE_{-i}(B_j))  
# the denominator contains the SE(B_j) calculated without observation i
 
## where:
##D_ij = B_j - B_{j(-i)}

#B_j is the regression coefficient obtained from using all data
#B_j-1 is the regression coefficient obtained from using all data EXCEPT the excluded observation
## Values can be positive or negative:  
## -Positive values indicate that deleting the observation yields a
## smaller coefficient
## -Negative values indicate that deleting the observation yields a
## larger coefficient

## abs(D_ij*) > 2/(sqrt(n)) are typically considered large

dfbetas.lm.out <- dfbetas(lm.out)
dfbetas.lm.out
plot(dfbetas.lm.out[,2])
abline(h=c(-2/sqrt(100), 2/sqrt(100)))
identify(dfbetas.lm.out[,2])                        
          
#deleting 35th observation would make the regression coefficient bigger     
#without 35th observation, the regression line will be pulled up to get closer to the 25th observation
#try the following if you don't believe:
datatemp<-data.frame(cbind(newsat, gpa))
lm.out
datatemp2<-datatemp[c(-35),]
summary(lm(newsat~gpa, data=datatemp2))


#deleting 25th observation would make the regression coefficient smaller    
#try the following if you don't believe:
lm.out
datatemp2<-datatemp[c(-25),]
summary(lm(newsat~gpa, data=datatemp2))


#References
#Wooldridge, Introductory Econometrics
#Fox, Applied Regression Analysis, linear Models and Related Methods
#http://www.people.fas.harvard.edu/~apost/web.section/gov2000sec9.R
#Nice, simple discussion
#http://www.davidson.edu/academic/economics/martin/jse.pdf



#=============================================
#APPENDIX ---- EXTRA STUFF 
#=============================================

#~~~~~~~~~~~~~~~~~~~~~~~~~~                            
#method TWO FOR MEASURING LEVERAGE
#use hat-values
#~~~~~~~~~~~~~~~~~~~~~~~~~~                            

#~~~~~~~~~~~~~~~~~~~~~~~~~~
#Recall that yhat=X(X'X)-1X'Y=Hy
#where H is the hat-matrix
#e-hat (i.e.epsilon-hat)=Y-Yhat
#hence ehat=(I-H)y
#large H(leverage) will make variance (ehat) small, i.e. fit would be closer
#for each case, we can compute the Mahalanobis distance from the regression line
#the larger the distance, the bigger the leverage 

## Hat values are one summary of the leverage, or the contribution of
## an observation Y_i, to the fitted value Yhat_j. Observations with
## unusual combinations of independent variable values have high hat
## values.  (in the two-D case, we can think of an observation with an
## x value far from xbar as having a high hat value.)  Hat values are
## bounded between 1/n and 1.  In simple regression, the formula is:

## h_i = 1/n + (X_i - Xbar)^2/ (sum(X_j - Xbar)^2)

## In the hat values plot, observations with hat values over 2 or even
## 3 times the mean hat value should stand out as potential problems
## in OLS estimation.

library(car)
plot(hatvalues(lm.out), ylim=c(-.1,.1))
abline(h= c(2,3)*mean(hatvalues(lm.out)))   #add horizontal lines to see 
                                            #which observations fall beyond 2 or 3 times the mean hat value
identify(1:100, hatvalues(lm.out))          #identify the outliers; hit 'escape' to exit identify()

#~~~~~~~~~~~~~~~~~~~~~~~~~~                            
#method TWO for MEASURING INFLUENCE
#use outlier test in "car" package
#perform Bonferroni Outlier Test
#~~~~~~~~~~~~~~~~~~~~~~~~~~                            
library(car)
outlier.test(lm.out)          #this tells me #35 is the biggest outlier 

#~~~~~~~~~~~~~~~~~~~~~~~~~~                            
#method THREE FOR MEASURING INFLUENCE:
#COOK'S DISTANCE
#~~~~~~~~~~~~~~~~~~~~~~~~~~                            

#cook's distance takes the unnormalized change in coefficients and use the norm defined by the estimated covariance matrix for b-hat, then divided by the number of coefficients

# This measure is calculated from standardized residuals (and is hence independent of the scale of the X variables) and a measure of leverage, the hat value associated with the particular observation.

# Cook's Distance for the i-th observation is based on the differences between the predicted responses from the model constructed from all of the data and the predicted responses from the model constructed by setting the i-th observation aside. 
# For each observation, the sum of squared residuals is divided by (p+1) times the Residual Mean Square from the full model. Some analysts suggest investigating observations for which Cook's distance is greater than 1. Others suggest looking at a dot plot to find extreme values.

# Cook's D-Statistic provides a summary index of the influence of particular observations on ALL of the regression coefficients. 
# D > 4/(n-k-1) is the accepted cut-off.

plot(cookd(lm.out))
identify(cookd(lm.out))

#~~~~~~~~~~~~~~~~~~~~~~~~~~                            
#method FOUR FOR MEASURING INFLUENCE:
#DFFITS
#~~~~~~~~~~~~~~~~~~~~~~~~~~                            

# A measure that is similar to Cook's D is "DFFITS"
# This measure is defined in terms of the hat value and studentized
# residual for a given observation.
#dffits expresses how much an observation affects the associated fitted value

#DFFITS_i = E_i* sqrt(h_i/(1-h_i))

# |DFFITS_i| > 2*sqrt(k/(n-k)) are considered large. 

# DFITSi is the scaled difference between the predicted responses from the model constructed from all of the data and the predicted responses from the model constructed by setting the i-th observation aside. 
# It is similar to Cook's distance. Unlike Cook's distance, it does not look at all of the predicted values with the i-th observation set aside. It looks only at the predicted values for the i- th observation. 
# Also, the scaling factor uses the standard error of the estimate with the i-th observation set aside. To see the effect of this, consider a dataset with one predictor in which all of the observations lie exactly on a straight line. The Residual Mean Square using all of the data will be positive. The standard errors of the estimate obained by setting one observation aside in turn will be positive except for the observation that does not lie on the line. When it is set aside, the standard error of the estimate will be 0 and DFITSi will be arbitrarily large. Some analysts suggest investigating observations for which |DFITSi| is greater than 2[(p+1)/(n-p-1)]. Others suggest looking at a dot plot to find extreme values.
  

dffits.out <- dffits(lm.out)
plot(dffits.out, ylim=c(-1.5,1.5))
abline(h=c(2*sqrt(2/100)), -2*sqrt(2/100))))
identify(dffits.out)


#FINAL NOTE
#you can get all these tests with one statement
library(stats)
influence.measures(lm.out)

