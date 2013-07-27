

# load the data
> nes96 <- read.table("http://www.stat.washington.edu/quinn/classes/536/data/nes96r.dat", header=TRUE)

# check to see if the data were read in properly
> nes96[1:3,]
  popul TVnews selfLR ClinLR DoleLR PID age educ income vote reldist
1     0      7      7      1      6   6  36    3      1    1      -5
2   190      1      3      3      5   1  20    4      1    0       2
3    31      7      2      2      6   1  24    6      1    0       4


# OK, let's fit an ordered logit model where the response variable 
# is ClinLR-- in other words we're trying to explain the variation 
# in respondents perceptions of Clinton's conservatism
# 
#
# To fit the ordered logit model we'll need to load the MASS library.
# hopefully this is already installed with your distribution of R. 
# If the MASS library is already installed, the following command
# will load it:

> library(MASS)

# if this doesn't work, you'll need to follow the instruction for 
# installing R packages that is specific to your operating system. 
# Specifically, you will need to instally the VR bundle from the CRAN 
# website.
#
# assuming you've loaded the MASS library, you will now have access
# to the polr() function. This stands for proportional odds logistic 
# regression, which is just another name for the ordered logit model.
# 
# Let's fit a model where ClinLR is the response variable, and TVnews,
# selfLR, PID, age, educ, and income are the covariates. We won't
# worry about recoding any of the covariates for the purpose of this 
# example.


> polr.out <- polr(as.ordered(ClinLR)~TVnews+selfLR+PID+age+educ+income,
+ data=nes96)

# Note we had to turn our response variable (ClinLR) into an ordered
# factor (using the as.ordered() function) in order to use the 
# polr() function to fit our model. The response variable going into 
# polr always has to be an ordered factor.
#
# Let's look at the results:

> summary(polr.out)

Re-fitting to get Hessian

Call:
polr(formula = as.ordered(ClinLR) ~ TVnews + selfLR + PID + age + 
    educ + income, data = nes96)

Coefficients:
              Value  Std. Error     t value
TVnews -0.006300088 0.024551596  -0.2566060
selfLR  0.071312494 0.056837887   1.2546647
PID    -0.386311548 0.037330410 -10.3484412
age    -0.008159382 0.004064311  -2.0075681
educ   -0.122777844 0.041333157  -2.9704444
income -0.031093208 0.010924449  -2.8462036

Intercepts:
    Value    Std. Error t value 
1|2  -4.6250   0.3979   -11.6238
2|3  -2.5044   0.3744    -6.6893
3|4  -1.2554   0.3677    -3.4140
4|5  -0.0778   0.3699    -0.2104
5|6   0.8538   0.3826     2.2317
6|7   1.9979   0.4253     4.6978

Residual Deviance: 2902.732 
AIC: 2926.732 

# recall that there are ClinLR is a 7 point scale running from 1 
# (extremely liberal) to 7 (extremely conservative).
# 
# The section of results titled "Coefficients" gives us our point 
# estimates and measures of uncertainty for the coefficients on 
# our covariates. 
# We can see that there is a very significant and substantively large
# negative effect of PID on the perceived conservatism of Clinton. 
# Since PID is coded from 0 (Strong Democrat) to 6 (Strong Republican)
# this means that Republican identifiers are much more likely to 
# perceive Clinton to be very politically liberal than Democratic 
# identifiers. Indeed, looking at exp(-1*-0.386311548) = 1.471543 we can 
# say that a one unit increase in PID (PID becoming slightly more
# Republican) results in the odds of placing Clinton in category m or 
# lower to increase by the factor 1.47. We can also see that there are 
# significant effects of age, education, and income on perceptions of
# Clinton's conservatism. Older respondents are more likely to believe
# Clinton is very liberal than younger respondents, more educated 
# respondents are more likely to perceive Clinton is very liberal than 
# less well educated respondents, and wealthier respondents are also 
# more likely to perceive Clinton to be very liberal than less wealthy
# respondents.
# 
# The section of results title "Intercepts" gives the point estimates
# and measures of uncertainty for the cutpoints (the taus in the Long
# notation. Note that we did not include an constant term in our 
# beta vector so we have estimated all 6 cutpoints for our 7-category 
# dependent variable. 
# 
# Let's plot the probabilities of each of the outcome categories as 
# a function of PID for a someone who watches 3 days of TVnews a week 
# (this is the median value in our sample), considers themselves 
# politically moderate (selfLR = 4), is 44 years old (the median value
# in the sample), has more than 12 years of schooling but no higher
# degree (educ = 4), and has a family income of $35,000 to $39,999 
# dollars a year. 

> beta <- coef(polr.out)
> tau <- polr.out$zeta
> X <- cbind(3, 4, 0:6, 44, 4, 17)
> logit.prob <- function(eta){exp(eta)/(1+exp(eta))}
> p1 <- logit.prob(tau[1] - X %*% beta)
> p2 <- logit.prob(tau[2] - X %*% beta) - logit.prob(tau[1] - X %*% beta)
> p3 <- logit.prob(tau[3] - X %*% beta) - logit.prob(tau[2] - X %*% beta)
> p4 <- logit.prob(tau[4] - X %*% beta) - logit.prob(tau[3] - X %*% beta)
> p5 <- logit.prob(tau[5] - X %*% beta) - logit.prob(tau[4] - X %*% beta)
> p6 <- logit.prob(tau[6] - X %*% beta) - logit.prob(tau[5] - X %*% beta)
> p7 <- 1.0 - logit.prob(tau[6] - X %*% beta)
> plot(0:6, p1, type="l", col=1, ylim=c(0,1))
> lines(0:6, p2, col=2)
> lines(0:6, p3, col=3)
> lines(0:6, p4, col=4)
> lines(0:6, p5, col=5)
> lines(0:6, p6, col=6)
> lines(0:6, p7, col=7)
> legend(0, 1, legend=c("P(extremely liberal)", "P(liberal)", 
+ "P(slightly liberal)", "P(moderate)", "P(slightly conservative)",
+ "P(conservative)", "P(extremely conservative)"), col=1:7, lty=1)

# we could construct tons of other plots including plots of cumulative
# probabilities 


