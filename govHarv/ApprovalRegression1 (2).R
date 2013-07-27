#You can run this file by typing 
#>source("http://jsekhon.fas.harvard.edu/gov1000/ApprovalRegression1.R")
#The output is attached below.

data.approval <- read.table(file="http://jsekhon.fas.harvard.edu/gov1000/approval.asc",header=T)

#what are the variables in the approval data set?
names(data.approval)

attach(data.approval)

#Regression model in Equation 234 (page 134 in the lecture notes)
eq234  <- lm(approval~unrate,data=data.approval)
#print our results
print(summary(eq234))

#Regression model in Equation 252 (page 145 in the lecture notes)
eq252  <- lm(approval~inflation + unrate,data=data.approval)
#print our results
print(summary(eq252))

#Regression model in Equation 253 (page 147 in the lecture notes)
eq253  <- lm(inflation~unrate,data=data.approval)
#print our results
print(summary(eq253))


#For HW Question 5
#This is the regression we ran
eq234  <- lm(approval~unrate,data=data.approval)

#the residuals are contained in the "eq234$residuals" object.  What is their mean:
a  <- mean(eq234$residuals)
cat("\n\n");
cat("Mean of eq234$residuals:",a,"\n\n")

#what is the covariance between the residuals and unemployment?
b  <- cov(eq234$residuals,unrate)
cat("Covariance between the residuals and unemployment:",b,"\n")


#This is what the output should look like if you type
#>source("http://jsekhon.fas.harvard.edu/gov1000/ApprovalRegression1.R")
#
#
#Call:
#lm(formula = approval ~ unrate, data = data.approval)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-33.7642  -6.0194   0.3938   7.8718  28.2875 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   69.303      1.892  36.625  < 2e-16 ***
#unrate        -2.280      0.315  -7.237 1.49e-12 ***
#---
#Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 
#
#Residual standard error: 11.48 on 568 degrees of freedom
#Multiple R-Squared: 0.08443,	Adjusted R-squared: 0.08281 
#F-statistic: 52.38 on 1 and 568 DF,  p-value: 1.494e-12 
#
#
#Call:
#lm(formula = approval ~ inflation + unrate, data = data.approval)
#
#Residuals:
#      Min        1Q    Median        3Q       Max 
#-24.00492  -6.09909   0.04854   5.92588  30.88987 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  69.7785     1.6279  42.864  < 2e-16 ***
#inflation    -2.1394     0.1510 -14.168  < 2e-16 ***
#unrate       -0.9258     0.2873  -3.222  0.00134 ** 
#---
#Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 
#
#Residual standard error: 9.872 on 567 degrees of freedom
#Multiple R-Squared: 0.3238,	Adjusted R-squared: 0.3214 
#F-statistic: 135.8 on 2 and 567 DF,  p-value: < 2.2e-16 
#
#
#Call:
#lm(formula = inflation ~ unrate, data = data.approval)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-4.9471 -1.9150 -0.6786  1.2089  9.4115 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.22236    0.45227   0.492    0.623    
#unrate       0.63288    0.07529   8.406 3.44e-16 ***
#---
#Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 
#
#Residual standard error: 2.743 on 568 degrees of freedom
#Multiple R-Squared: 0.1106,	Adjusted R-squared: 0.1091 
#F-statistic: 70.65 on 1 and 568 DF,  p-value: 3.437e-16 
#
#
#
#Mean of eq234$residuals: -7.052838e-16 
#
#Covariance between the residuals and unemployment: 1.307011e-15 
#> 
