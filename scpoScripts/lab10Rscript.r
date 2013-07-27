#source("C:/Users/Iris/Documents/PS231A/lab10/lab10Rscript.r", echo=T)

#sink the console to destinated location
sink("C:/Users/Iris/Documents/PS231A/lab10/Lab10outApril25.txt", type="output")

#LAB 10 (REVISED VERSION)
#TOPICS COVERED

#REGRESSION DIAGNOSTICS: CHECKING ASSUMPTIONS
#     NON-LINEARITY
        # boxplot
        # qqplot
#     NON-CONSTANT VARIANCE (HOMOSKEDASTICITY/HETEROSKEDASTICITY)
        # Levene-test
        # breusch-pagan test     
        # white test
#APPENDIX: PARTIAL REGRESSION PLOT

#=========================================
#PART I: CHECK REGRESSION ASSUMPTIONS: LINEARLITY
#=========================================
#create make-up data

set.seed(007)
gpa<-runif(100, 1, 4)
set.seed(777)
error<-rnorm(100, 0,50)
sat<-100*gpa+50*gpa^2+error     #the true model: x and y are non-linear related 

summary(gpa)
summary(sat)


#OLS ONLY GIVES GOOD ESTIMATE OF THE RELATIONSHIP BETWEEN X AND Y IF THE RELATIONSHIP IS LINEAR
#FIRST CHECK WHETHER X AND Y RELATES LINEARLY

plot(gpa, sat)             #appear to be curve-linear

#fit a regression line
abline(lsfit(gpa, sat), col="red", lwd=3)  #not perfect fit

#pretend we don't know what's the true model
#try out different higher order term
sqout<-lm(sat~gpa)                    #run the simple model
sqout2<-lm(sat~gpa+I(gpa^2))          #put in quadratic term 
sqout3<-lm(sat~gpa+I(gpa^2)+I(gpa^3)) #put in cubic term 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#why do the quadratic and cubic terms appear to have simialr fit?
#a<-1:500
#quadratic function looks like this
#par(mfrow=c(1,2))
#plot(a^2)
#cubic function looks like this
#plot(a^3, col="red", lwd=3)
#cubic increases faster than quadratic
#but both are curvelinear
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(sqout)
summary(sqout2)
summary(sqout3)                       #hard to tell if quadratic or cubic has better fit

#Examine the fit visually
#method one
#compare GPA and SAT vs. GPA and predicted SAT
par(mfrow=c(1,3))
plot(gpa, sqout$fitted, col="red", lwd=2, lty=2, main="No higher order")
points(gpa, sat, col="black")

#quadratic term
plot(gpa, sqout2$fitted, col="red", lwd=2, lty=2, main="Quadratic")
points(gpa, sat, col="black")

#cubic fit looks similar 
plot(gpa, sqout3$fitted, col="red", lwd=2, lty=2, main="Cubic")
points(gpa, sat, col="black")


#method two
#check residuals
par(mfrow=c(1,3))
qqnorm(sqout$residuals, main="No Sq Term")
qqline(sqout$residuals, col="red", lwd=2)

qqnorm(sqout2$residuals, main="Quadratic")   #quadratic wins!
qqline(sqout2$residuals, col="red", lwd=2)

qqnorm(sqout3$residuals, main="Cubic")
qqline(sqout3$residuals, col="red", lwd=2)


#===========================
#PART IIA: Non-constant variance?
#What would the residual plots look like?
#Four Scenarios
#===========================
par(mfrow=c(2,2))

#constant variance
plot(1:50,rnorm(50), main="constant variance")
abline(h=0)

#strong non-constant variance
plot(1:50,(1:50)*rnorm(50), main="strong non-constant variance")
abline(h=0)

#mild non-constant variance
plot(1:50,sqrt((1:50))*rnorm(50), main="mild non-constant variance")
abline(h=0)

#non-linearity
plot(1:50,cos((1:50)*pi/25)+rnorm(50), main="non-linearity")
abline(h=0)

#=================================
#PART IIB: Diagnostic Test for non-constant variance in residuals
#=================================
#create an example with non-constant variance in residuals
par(mfrow=c(1,1))

set.seed(777)
attend<-(1:100)*runif(100, 0,1)      #create a new indep variable attend rate ranges(0-100%)
set.seed(999)
ncerror<-(1:100)*rnorm(100)
ncsat<-100*gpa+50*gpa^2+attend+ncerror

#check out residuals
par(mfrow=c(1,2))
ncreg<-lm(ncsat~gpa+I(gpa^2)+attend)
qqnorm(ncreg$residual)     #residuals don't follow normal distribution
qqline(ncreg$residual)

plot(ncreg$residual)  #clear sign of heteroskedasticity
abline(h=0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#method one:
#levene test on equal variance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#based on the predicted values(y-hat), classify the observations into e.g. 8 equal intervals (a.k.a.groups/ranges of values)
#conduct levene test to test if variance of the residuals for these intervals are the same
par(mfrow=c(1,1))
plot(ncreg$fit, ncreg$residuals)     

stres<-cut(ncreg$fit, 8)      #cut into 8 equal intervals; assign observations into groups
ncyhat<-ncreg$residual

#to get the var of the groups
varout<-tapply(ncyhat, stres, function (ncyhat){
var(ncyhat, na.rm=T)
})

#do levene test
#Levene's test is used to test if k samples have equal variances (homoskedasticity) 
#null=assume that variances are equal across groups or samples
library(car)
levene.test(ncyhat, factor(stres))  #can't reject null hypothesis
                                    #note:the test is sensitive to how/how many intervals are created
                                    
#Breusch-Pagan test (BP test)
#estimate the model by OLS: y=b0+b1X1+b2X2+e
#obtain the squared OLS residuals (one for each observation) (i.e. u^2=(uhat-ubar)^2)
#regress the squared OLS residuals on the regressors: u^2=d0+d1X1+d2X2+k
#keep the R-sq from above regression
#form F-stat and compute p-value
#if p-value is sufficiently small, below the chosen significance level, then we reject the null hypothesis of homoskedasticity


library(lmtest)
bptest.result<-bptest(ncsat~gpa+I(gpa^2)+attend)           
bptest.result

#sometimes bptest arrives at diff conclusion...
#article on bp test and its inconsistency
#http://129.3.20.41/eps/em/papers/0205/0205001.pdf

#can also use White test--
#a variation of bptest which includes ALL the regressors, squares and cross-product terms
whitetest.result<-bptest(ncsat~gpa+I(gpa^2)+attend+I(attend^2)+gpa*attend)          
whitetest.result


      
#==========================
#APPENDIX:
#PARTIAL REGRESSION PLOT
#============================
#if we run a bivariate regression, we can plot x and y
#and overlay the regression line on top
#what to do in multiple regression context where we have more than one X?
#say we have a model: Y~x1+x2+x3
#how do we plot the partial slope of x1 and y?
#there are two functions available

library(faraway)

par(mfrow=c(2,2))
ncreg
prplot(ncreg,1)   #x-axis is the range of value for X1, i.e.gpa
                  #y-axix is b1*gpa+residual
prplot(ncreg,3)   #x-axis is the range of value for X3, i.e.attend
                  #y-axix is b3*attend+residual

#less intuitive plot
library(car)
av.plots(ncreg)   #choose the indep variable of interest

#x-axis is the residuals generated when regress a particular independent variable x_j on the rest of the x variables
#y axis is the residuals generated when regress y on all independent variables except x_j