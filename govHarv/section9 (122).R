############################################

#   Goverment 1001
#   Section 9 - Tools for Multiple Regression
#                 
#   April 19, 2007
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################

#   Goals for today:

#   1) Discuss HW7/HW8

#   2) Hypotheses about multiple coefficients

#   3) Robust standard errors

#   4) Interactions/powers

#   5) Presenting results

#   6) Finding a reasonable model

############################################




#   1) Discuss HW7/HW8

#   What did you have questions about on HW7 or HW8?  
#   

#   2) Hypotheses about multiple coefficients

#   We know how to do a hypothesis test for a single coefficient in a 
#   multiple regression, but what if we want to test a hypothesis about 
#   a whole set of coefficients?  We can do this with an F test.

#   Let's say we are looking at the Robey data on fertility and contraceptive
#   use.  The model that we fit was 

library(car)
data(Robey)
attach(Robey)

lmout1 <- lm(tfr ~ contraceptors)
summary(lmout1)
plot(residuals(lmout1) ~ region)

#   In the homework, we saw that there appeared to be some dependence of the 
#   residuals on the region in which the country was located.  One way to 
#   deal with this is to add dummy variables for each region.  Since region
#   is a factor variable, you just have to include it in the lm function:

lmout2 <- lm(tfr ~ contraceptors + region)
summary(lmout2)
plot(residuals(lmout1) ~ region)

#   Whenever you add a multi-category factor variable to a model, you have 
#   to leave one of the categories out.  This is called the 'reference
#   category'.  In this case, the reference category is Africa.

#   None of the region dummies is significant at alpha=0.05 taken together.
#   But can we reject the null that they are all equal to zero?  This is an
#   F test.  

#   Remember that you can calculate an F statistic for comparing two nested 
#   models using the R^2 from each model.  The easiest way to do this, though,
#   is to use the anova() function in R:

anova(lmout1, lmout2)

#   So, we get an F statistic of 1.7764, which has a p-value of 0.1652. 
#   We can't reject the null that all of the region dummies have 
#   coefficients equal to 0.  On the other hand, it really doesn't affect 
#   the quantity of interest (the coefficient on contraceptors), so we might 
#   still report the results from the larger model.

#   Remember that an F-test comparing two models that differ by only one 
#   term is equivalent to a t-test for the coefficient on that variable:

lmout3 <- lm(tfr ~  region)
summary(lmout3)
anova(lmout2, lmout3)

#  3) Robust standard errors

#  Remember that the biggest problem with non-constant error variance is 
#  not that the point estimates that we get will be screwed up, but that
#  our estimated standard errors will be wrong.

#  Sometimes there are transformations that we can make to the data
#  to fix this problem; remember the Florida dataset:

data(Florida)
attach(Florida)

lmfl <- lm(GORE ~ BUSH)
plot(lmfl, which=3)

#  Taking the log of both sides fixes the problem; this is called a 
#  "variance stabilizing" transformation.

lmlogfl <- lm(log(GORE) ~ log(BUSH))
plot(lmlogfl, which=3)

#  Often, though, there is not an obvious way to transform the data; 
#  transforming it could screw up the linearity assumption, which is 
#  much more important, or it could make the results too difficult to 
#  interpret.

#  In these cases, we can attempt to fix up our standard errors after 
#  the fact.  The most common fix is called White robust standard errors.
#  We aren't going to talk about what is going on, but you should know
#  how to calculate them:

#  The hccm() function will give you a robust covariance matrix (don't
#  worry, you don't need to know what that means).

hccm.out <- hccm(lmfl)

#  You get the standard errors by taking the diagonal of the hccm object
#  and then taking the square root of that.

white.se <- sqrt(diag(hccm.out))
white.se

#  Let's compare these to the "normal" standard errors:

summary(lmfl)
cbind(summary(lmfl)$coef, white.se)

#  4) Interactions/powers

#  On the board
