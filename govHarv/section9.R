############################################

#   Government 50
#   Section 8 - Introduction to linear regression
#                 
#   April 10, 2008
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################

#   Goals for today:

#   1) Causes of an effect versus effects of a cause

#   2) Interpreting transformed variables

#   3) Diagnostics for multiple regression

#   4) Using the predict() function

############################################



#  1) Causes of an effect vs effects of a cause

#  [discussion]

#  2) Interpreting transformed variables:

#  We know how to descriptively interpret the results from 
#  untransformed linear regression:

library(car)
attach(Florida)
plot(BUSH ~ GORE)
lm(BUSH ~ GORE)

#  So, for this data, if two counties differ in the number of Gore 
#  votes by 1, we would expect them to differ in the number of Bush 
#  votes by 0.695.

#  Now, what if we take the square root of both variables?

plot(sqrt(BUSH) ~ sqrt(GORE))
lm(sqrt(BUSH) ~ sqrt(GORE))

#  How do we interpret the coefficient on sqrt(GORE)?  You can do the 
#  same thing, but now you have to remember that the units have changed:

#  If two counties differ in the square root of Gore 
#  votes by 1, we would expect them to differ in the 
#  square root of Bush votes by 0.848.

#  This isn't very intuitive, but there isn't much we can do with the 
#  square root transformation.

#  The same isn't true for the log transformation, however.

plot(log(BUSH) ~ log(GORE))
lm(log(BUSH) ~ log(GORE))

#  We can do the same thing as before, saying that if two counties 
#  differ in the log of Gore votes by 1, then we would expect them
#  to differ in the log of Bush votes by 0.887.

#  But, there are a couple of more intuitive ways to interpret these 
#  results.  If both the dependent and independent variables are logged,
#  then we can interpret the slope coefficient as the relationship between
#  a percentage unit change in X and a percentage unit change in Y.

#  So, if the coefficient on log(GORE) is 0.8869, then a 1% change 
#  in Gore votes is associated with a 0.89% change in Bush votes.

#  Note that this makes sense for small percentage changes in X; 
#  You probably wouldn't want to interpret a 1000% change in X as leading 
#  to a 887% change in Y. 

#  A more precise way to describe the relationship between the two variables 
#  is to think about the mathematics of the model

#  log(BUSH) = alpha + beta*log(GORE)
#  exp(log(BUSH)) = exp(alpha + beta*log(GORE))
#  BUSH = exp(alpha)*exp(log(GORE^beta))
#  BUSH = exp(alpha)*GORE^beta

#  So, if you multiply the number of Gore votes by a constant c, then 
#  the predicted number of Bush votes is multiplied by c^beta.
#  In other words, if beta = 2, and you double Gore votes, then you 
#  multiply the number of Bush votes by 2^2, or 4.


#  2)  Diagnostics for multiple regression

#  When we move to more explanatory variables, we still need to look at the 
#  assumptions of the model.  Some of these become a bit more difficult to 
#  examine when you have more variables (particularly the linearity 
#  assumption, which is the most important).  We can use some of the same
#  techniques that we used in the simple regression framework.

#  Remember that the assumption says that the relationship between 
#  the outcome variable and an explanatory variable is linear *after* you
#  control for the other variables in the model.

#  You can use the added variable plots to assess the linearity assumption
#  in some cases.

#  Remember that an added variable plot for variable XX is made by taking the 
#  residuals from the regression of YY on all of the other variables against
#  the residuals from the regression of XX on all of the other variables.



data(Anscombe)
attach(Anscombe)
head(Anscombe)

lmout <- lm(income ~ young + education)
summary(lmout)

#  Say that I want to construct an added variable plot with income as the 
#  outcome and young as the explanatory variable

lmoutY <- lm(income ~  education)
summary(lmoutY)

lmoutX <- lm(young ~  education)
summary(lmoutX)

plot(residuals(lmoutY) ~ residuals(lmoutX))
abline(lm(residuals(lmoutY) ~ residuals(lmoutX)))

#  You can do the same thing by using the av.plot() function in the
#  car library.

av.plot(lmout, variable="young")

av.plot(lmout, variable="education")

#  Remember that the slope of the line on an av plot is the same as 
#  the coefficient on the variable in the multiple regression.

#  An added variable plot can reveal a non-linear relationship between
#  one of the explanatory variables and the outcome variable, but there
#  is a catch: the relationships between the outcome and all of the other
#  variable has to be actually linear, and the relationships between 
#  the explanatory variable that you are looking at and all of the other
#  explanatory variables has to be actually linear.

#  So, the moral of the story is:  if you see a problem in an added 
#  variable plot, there is probably a problem.  If you don't see a 
#  problem, there may still be a problem, but you'll have to use other
#  tools.

#  The diagnostics that we use for the other modeling assumptions 
#  are easier to generalize to the multiple regression framework.

#  First, let's look at constant error variance.  We can still use the
#  spread-location plot, which has the fitted values from the regression
#  that we are checking; this is a way of combining all of the 
#  X values into one number.

plot(lmout, which=3)

#  We can also check the normality assumption the same way that we did 
#  before, using QQ plots.

plot(lmout, which=2)
qq.plot(lmout)

## 3) Using the predict() function

#  We may be interested in making predictions about hypothetical 
#  data points that we haven't observed.  This is pretty easy to 
#  do in linear regression - we just stick the values that we
#  are interested in into the prediction equation.  The predict() 
#  function in R will do this for us.  The first argument to the
#  predict function is a model object that we have stored.

#  If there are no other arguments, this just returns the fitted
#  values for the observed data points

predict(lmout)
lmout$fitted

#  We can also give the predict function new values to predict.
#  To do this, we need to give it a data frame with the same column
#  names as the original data.  Say we want to predict income for 
#  a state with young = 300 and education = 350.

newX <- data.frame(young = 300, education = 350)
predict(lmout, newdata=newX)

#  We can predict for many values simultaneously.  Let's look at
#  different values of education when young is held fixed at 
#  mean(young)

newX <- data.frame(young=mean(young), education = 300:400)
predict(lmout, newX)

#  We can then plot these predicted values:

plot(newX$education, predict(lmout, newX), type="l")

#  This gives us the regression line for education when young is 
#  held constant at the mean.  This is pretty simple now, but 
#  will be more important next week.
