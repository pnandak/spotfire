

> data(swiss)

# A Simple Bivariate Example
#
# let's take a look at the plot of fertility on education
 
> plot(swiss$Education, swiss$Fertility, xlab="Education", ylab="Fertility")

# looks like there is a negative, roughly linear relationship here
#
# Let's fit a linear least squares regression model to these data

> lm1.out <- lm(Fertility~Education, data=swiss)

# The R function used to fit least squares regression models is lm()
# for more information about this function you can get help by typing 
# ?lm
# You can also refer to the Fox R Companion
# The results have been assigned to the object called lm1.out. To get 
# a summary of these results we can type

> summary(lm1.out)

Call:
lm(formula = Fertility ~ Education, data = swiss)

Residuals:
    Min      1Q  Median      3Q     Max 
-17.036  -6.711  -1.011   9.526  19.689 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  79.6101     2.1041  37.836  < 2e-16 ***
Education    -0.8624     0.1448  -5.954 3.66e-07 ***
---
Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

Residual standard error: 9.446 on 45 degrees of freedom
Multiple R-Squared: 0.4406,     Adjusted R-squared: 0.4282 
F-statistic: 35.45 on 1 and 45 DF,  p-value: 3.659e-07 

# here we see that our estimate of the intercept is about 79.6 and the 
# slope estimate is about -0.86. As we expected there is a fairly strong
# negative relationship between education and fertility. We can 
# interpret these results as meaning that as education increases 1 unit
# say from 20% to 21% then Fertility will decrease about .86 units. When
# education is equal to 0 we would expect fertility to be about 79.6.
# 
# We can also superimpose the regression line based on our estimates
# on our plot by typing:
#

> abline(lm1.out, col="red")

# this line gives us the conditional expectation of fertility given
# education. 
#
#
# A 3 Variable Example
#
# OK, it seems reasonable that fertility covaries with things other than 
# education.  Percent Catholic would seem to have a potential relationship
# to fertility. Let's fit the model

> lm2.out <- lm(Fertility~Education+Catholic, data=swiss)

# Note that all we had to do was to add the additional variable (Catholic)
# to the right hand side of our formula. To see the results we can once
# again use the summary() function

> summary(lm2.out)

Call:
lm(formula = Fertility ~ Education + Catholic, data = swiss)

Residuals:
    Min      1Q  Median      3Q     Max 
-15.042  -6.578  -1.431   6.122  14.321 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 74.23369    2.35197  31.562  < 2e-16 ***
Education   -0.78833    0.12929  -6.097 2.43e-07 ***
Catholic     0.11092    0.02981   3.721  0.00056 ***
---
Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

Residual standard error: 8.331 on 44 degrees of freedom
Multiple R-Squared: 0.5745,     Adjusted R-squared: 0.5552 
F-statistic:  29.7 on 2 and 44 DF,  p-value: 6.849e-09 

# after accounting for % Catholic, education still has strong negative 
# effect. Holding Catholic fixed at some level we should expect a 1 unit
# increase in education to result in about a .79 unit decrease in 
# fertility. Fertility tends to be higher in more Catholic cantons after 
# accounting for the effect of Education. A one unit increase in the 
# Catholic variable (say from 45% to 46%) will increase fertility by 
# about 0.11 units. When both Catholic and education are equal to 0 we 
# would expect fertility to be about 74.
#
# In this case we can still visualize the conditional expection function
# which now takes the form of a plane in the space of education, Catholic
# and Fertility. This plane can be graphed using the code below. 

> library(lattice)
> cath <- seq(from=0, to=100, by=10)
> educ <- seq(from=0, to=100, by=10)
> g <- expand.grid(Education=educ, Catholic=cath)
> beta <- coef(lm2.out)
> g$Fertility <- beta[1] + g$Education*beta[2] +g$Catholic*beta[3]
> wireframe(Fertility~Education*Catholic, data=g, drape=TRUE, 
            scales=list(arrows=FALSE), screen=list(z=12, x=-70, y=2))

# Note that (as indicated by the size of the coefficients) the conditional
# expectation of Fertility varies more quickly over education levels than 
# levels of Catholic. 
#
#
# To fit a model with more than 2 independent variables we just add the
# names of the additional variables to the right hand side of the formula
# argument to lm(). For instance
#

> lm3.out <- lm(Fertility~Education+Catholic+Examination+Agriculture, 
                data=swiss)

# Once again, the summary() function displays the results

> summary(lm3.out)
Call:
lm(formula = Fertility ~ Education + Catholic + Examination + 
    Agriculture, data = swiss)

Residuals:
     Min       1Q   Median       3Q      Max 
-15.7813  -6.3308   0.8113   5.7205  15.5569 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 91.05542    6.94881  13.104  < 2e-16 ***
Education   -0.96161    0.19455  -4.943 1.28e-05 ***
Catholic     0.12442    0.03727   3.339  0.00177 ** 
Examination -0.26058    0.27411  -0.951  0.34722    
Agriculture -0.22065    0.07360  -2.998  0.00455 ** 
---
Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

Residual standard error: 7.736 on 42 degrees of freedom
Multiple R-Squared: 0.6498,     Adjusted R-squared: 0.6164 
F-statistic: 19.48 on 4 and 42 DF,  p-value: 3.95e-09 




