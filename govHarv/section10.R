############################################

#   Goverment 50
#   Section 10 - Logistic Regression
#                 
#   April 24, 2008
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################

#   Goals for today:

#   1) Linear regression with factor variables

#   2) Linear regression with interaction terms

#   3) What are odds?  What is a logistic curve?

#   4) Logistic Regression

############################################


#   1) Linear regression with factor variables

#   We have already looked at linear regression models that include dummy
#   variables, but we have thought of those variables as having only
#   two categories.   What if we have a categorical predictor with more
#   than two categories?

#   We'll take a look at the Robey dataset, which has data on contraception
#   use and fertility rates in different countries.  It is in the car
#   library.

library(car)
data(Robey)
attach(Robey)
head(Robey)

#   Let's look at a plot with tfr (total fertility rate) as the outcome
#   and contraceptors as the predictor:

plot(tfr ~ contraceptors)
lmout <- lm(tfr ~ contraceptors)
abline(lmout)


#   Looks pretty good.  But is there dependence across regions?  We can 
#   look at a boxplot of the residuals:

boxplot(lmout$res ~ region)

#   It looks like there are some differences across regions.  We could try
#   to capture this by including a set of dummy variables:

#   region         africa      asia     latinamer    neareast    
#   africa           1          0           0           0 
#   africa           1          0           0           0 
#   asia             0          1           0           0 
#   asia             0          1           0           0 
#   africa           1          0           0           0 

#   Rather than constructing these dummy variables ourselves, R will do it 
#   for us.

lmout2 <- lm(tfr ~ contraceptors + region)
summary(lmout2)

#   Note that, even though there are four regions, there are only three
#   dummy variables reported in the output.  This is because we always
#   have to leave one of the categories out of the regression (this is why
#   we don't include variables for both men and women in the same model, 
#   for example).

#   How do we interpret the results?

#   The intercept is our prediction for the omited category (Africa in 
#   this case) when contraceptors = 0. 

#   regionAsia is the shift in the intercept from Africa to Asia

#   regionLatin.Amer is the shift in the intercept from Latin America
#   to Asia.

#   The t-statistics and p-values are generated under the hypothesis 
#   that the intercept is the same for the reference category and the
#   category of interest.

#   2) Linear regression with interaction terms

#   Recall that the basic linear regression terms have assumed additive
#   relationships, so that the effect of one variable does not depend
#   on the values of the other variable.  Interaction terms weaken
#   that assumption, allowing effects to vary based on the values of 
#   other variables.

#   For example, let's look at a dataset of weights and 
#   heights for men and women (the Davis dataset in car):

data(Davis)
attach(Davis)
head(Davis)
plot(weight ~ height)
lmout <- lm(weight ~ height)
abline(lmout)

#   Note that weight and height are reversed in row 12:

weight[12] <- 57
height[12] <- 166

plot(weight ~ height)
lmout <- lm(weight ~ height)
abline(lmout)

#   We might think that the relationship between weight
#   and height is different for men and women.  We can get a sense 
#   of this by plotting them in different colors

plot(weight ~ height)
points(weight[sex=="M"] ~ height[sex=="M"], col="red")
points(weight[sex=="F"] ~ height[sex=="F"], col="blue")

#   If we assume an additive model, then we are assuming that 
#   the prediction lines for men and women are parallel:

lmout2 <- lm(weight ~ height + sex)
summary(lmout2)

#   We can't plot the regression lines the way we usually
#   do, but we can still use the abline() function with 
#   two arguments: intercept and slope

#   for women
abline(-76.6167, 0.8106, col="blue", lwd=2)

#   for men

abline(-76.6167 + 8.2269, 0.8106, col="red", lwd=2)

#   This isn't bad, but we still might wonder if the slopes
#   of the lines are really the same.  To check this, we include 
#   an interaction term.  In R, we do this by including
#   height:sex as a term in the model:

lmout3 <- lm(weight ~ height + sex + height:sex)
summary(lmout3)

#   Note that the interaction is right at the boundary of significance
#   at the 0.05 level.  What do the lines look like?

plot(weight ~ height)
points(weight[sex=="M"] ~ height[sex=="M"], col="red")
points(weight[sex=="F"] ~ height[sex=="F"], col="blue")

#   for women
abline(-45.673, 0.623, col="blue", lwd=2)

#   for men
abline(-45.673 -55.657, 0.623 + 0.373, col="red", lwd=2)

#   We can include interaction terms for continuous variables in the 
#   same way.

#   3) What are odds?  What is a logistic curve?

#   What are odds?  You hear them a lot if you are interested in betting.
#   What does it mean if the odds are 2:1?  What about 5:4?  

#   http://www.paddypower.com/bet?action=go_disp_cat&disp_cat_id=31

#   What is an odds ratio?  It is just the ratio of two odds (surprise!).
#   So if the odds of Hillary being the democratic nominee are 11:4 and 
#   the odds of Obama being the democratic nominee are 2:9, what is the odds 
#   ratio?

#   What is true if the odds ratio = 1?  What is true if the log odds ratio 
#   is 0?

log(1)

#   We are going to write down a model where the log of the odds ratio is 
#   a linear function of some X variables.

#   Let's say logit(pi) = 2 + 3*XX

#   Here is a plot of the log odds as a function of XX:

XX <- rnorm(200)
XXa <- sort(XX)
plot(XXa, 2 + 3*XXa, type = "l")

#   And here are the odds:

plot(XXa, exp(2 + 3*XXa), type = "l")

#   And here is the probability:

plot(XXa, exp(2 + 3*XXa)/(1 + exp(2 + 3*XXa)), type = "l")



#   3) Logistic Regression

#   Fitting a logistic regression is easy; it is almost the same as
#   a linear regression, but we have to make two changes: use the glm() 
#   function instead of the lm() function, and add the family="binomial" 
#   argument inside the function.

load("C:/vote04.RData")
head(vote04)
attach(vote04)

#   This is simulated data based on a survey of voters in the 2004 
#   election.  We can start by plotting the data.
plot(bush ~ partyid)

#   We often need to jitter this type of data since it tends to 
#   stack up on top of each other:
plot(jitter(bush) ~ jitter(partyid))

#   What if we fit a linear regression?  
lmout <- lm(bush ~ partyid)
summary(lmout)
abline(lmout)

#   This isn't obviously horrible, but we do get impossible predictions
#   partyid = 1. 

#   Now we can fit a logit model:

glmout <- glm(bush ~ partyid, family = binomial)
summary(glmout)
e
#   The interpretation of much of the output is the same; we still have point 
#   estimates, standard errors, and p-values.  

#   We can interpret the sign and the significance directly, but not
#   the magnitude of the coefficients.  To do this, we need to look at
#   predicted probabilities.

#   Remember that we used the predict() function to get predicted 
#   probabilities last week.  We'll do the same here.  First, we 
#   need to create a fake dataset with the x values that we are 
#   interested in, using the data.frame() function

newvote <- data.frame(partyid = seq(0,1, by=.01))

#   The names of the variables in the new data frame should be 
#   the same as in the model you are making predictions from.

#   Next, use the predict() function.  We need to set
#   type="response" to get predictions on the probability scale

pred.out <- predict(glmout, newdata=newvote, type="response")
pred.out

#   Now, we can add the prediction line to our plot:

lines(newvote$partyid, pred.out, col="red", lwd=2)

#   To get confidence bands, we use predict() again but
#   add the argument se.fit=T

pred.out <- predict(glmout, newdata=newvote, type="response", se.fit=T)

#   Now, pred.out has two lists of numbers; the first is the predicted
#   probability and the second is the standard error of the predicted
#   probability.  To add a 95% confidence band, we just plot the predicted
#   probability plus or minus two times the standard error:

lines(newvote$partyid, pred.out$fit - 2*pred.out$se, col="red", lty=2)
lines(newvote$partyid, pred.out$fit + 2*pred.out$se, col="red", lty=2)
