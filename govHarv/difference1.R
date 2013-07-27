#
# Example code for the difference pathology
#

#set the random number seed so we get the same answer every time
set.seed(4232)

#Let's generate some data to replicate the pathology I talked about.

#the number of observations:
obs  <- 1000

#draw some random normal errors with mean 0 and standard deviation equal to 1
u <- rnorm(1000,0,1);

#y equals:
y <- 5 + u;

#let's create delta y
dely1 <- y[1:999] - y[2:1000]

#lets create the variables we are going to work with, only keep 950 of
#the obs, just to make sure we don't runoff the end of the index
dy1 <- dely1[1:950];
dy2 <- dely1[2:951];
dy3 <- dely1[3:952];
dy4 <- dely1[4:953];
dy5 <- dely1[5:954];
dy6 <- dely1[6:955];

print(summary(lm(dy1 ~ dy2 + dy3 +dy4 + dy5 + dy6)));

#Call:
#lm(formula = dy1 ~ dy2 + dy3 + dy4 + dy5 + dy6)
#
#Residuals:
#      Min        1Q    Median        3Q       Max 
#-3.541497 -0.692820  0.008343  0.737291  3.622813 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.0004232  0.0331207  -0.013     0.99    
#dy2         -0.8746884  0.0322578 -27.116  < 2e-16 ***
#dy3         -0.7120977  0.0417074 -17.074  < 2e-16 ***
#dy4         -0.4738585  0.0451525 -10.495  < 2e-16 ***
#dy5         -0.3290840  0.0417075  -7.890 8.32e-15 ***
#dy6         -0.1326036  0.0323032  -4.105 4.39e-05 ***

# WHAT'S GOING ON?!?!?!?!?!?!?!?
#

#let's drop one term

print(summary(lm(dy1 ~ dy2 + dy3+ dy5 + dy6)));

#Call:
#lm(formula = dy1 ~ dy2 + dy3 + dy5 + dy6)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-2.94908 -0.79864  0.04784  0.72391  3.64052 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.0006777  0.0349810  -0.019    0.985    
#dy2         -0.7147935  0.0300300 -23.803   <2e-16 ***
#dy3         -0.3919796  0.0300418 -13.048   <2e-16 ***
#dy5         -0.0091250  0.0300590  -0.304    0.762    
#dy6          0.0274591  0.0300753   0.913    0.361    
#---
#Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 
#
#Residual standard error: 1.078 on 945 degrees of freedom
#Multiple R-Squared: 0.3789,	Adjusted R-squared: 0.3762 
#F-statistic: 144.1 on 4 and 945 DF,  p-value: < 2.2e-16 
