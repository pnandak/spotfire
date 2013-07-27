## Expected and Predicted Values

## Expected and predicted values are both ways of using information about 
## the assumed functional form of the data and the observed data to draw 
## inferences about some outcome y

## Expected values are the values the outcome varable y is expected to take
## given some assumed values of the covariates X

## If we want to know what the weather tomorrow is expected to be, we want to
## know what the weather is expected to be like on a day like tomorrow.  We
## estimate this with data and make a guess which will inevitably be imperfect
## due to estimation uncertainty- we might not be able to measure weather
## and all other relevant attributes of tomorrow perfectly, and we may not be
## accounting for all relevant attributes of tomorrow perfectly.  

## We estimate the expected value by

## 1- obtaining MLEs for our model parameters

## 2- setting the covariates in some theoretically-motivated way
##    (for example holding all but one covariate at their observed values 
##     and fixing the value of one of them)

## 3- drawing one set of model parameters from the quadratic approximation
##    to the likelihood (i.e. one of each of the parameters)

## 4- plugging the drawn model parameters and the X from 2 into the 
##    stochastic component (in the linear regression case, this entails
##    obtaining the mean of the normal by multiplying X%*%beta)

## 5- drawing many draws from the stochastic component with the result from
##    4 as its parameter (in the linear regression case, this entails
##    drawing from the normal centered at the X%*%beta obtained from 4)
##    These draws will vary due to fundamental uncertainty accounted for
##    by the stochastic component

## 6- average the draws from 5

## There you have it- one expected value.  To estimate the uncertainty of 
## the expected value, you can repeat the steps above many times to approximate
## a distribution of expected values, and estimate the standard deviation
## and confidence intervals from that      


## The example in the section 6 code obtains MLE's using optim and draws
## parameters from the normal approximation to the likelihood centered
## at the MLE's

## To obtain one predicted value, you repeat steps 1-4, but then
## account for fundamental uncertainty by drawing one prediction of 
## y from the stochastic component.  In the case of linear regression,
## this entails drawing a single value from the normal centered at the X%*%beta
## from 4.  

## The predicted value is a guess or a prediction given the observed data and 
## the model for another value of the outcome variable



