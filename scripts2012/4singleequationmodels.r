#### "Single Equation Models"
#### Harvard Gov2001 Code Library
#### Instructor: Gary King
#### Maintainers: Margaret Roberts and Brandon Stewart
#### May 1, 2011
#### Version 0.5
#### Contact: bstewart@fas.harvard.edu

# Summary: This file covers single equation models.
#
# Table of Contents:
#    Show how the logit graph varies with xbeta
#    Generate data for probit via a latent variable
#    Why we simulate from the multivariate normal
#    Simulation of Predicted Values and Expected Values
#    Example of Simulating First Differences
#    Reparameterizing on an Unbounded Scale and First Differences
#    Graph on Confidence Intervals
#    Ternary Plots in R
#    Grouped uncorrelated binary variables model
#    Variance in Poisson Models
#    Logit vs. Probit
#    Exponential Models
#    Weibull Models
#    Cox Proportional Hazards Models
#    Zero-Inflated Negative Binomial Regression



#### Contributors: Ariel White, Ben Schneer, Michael Gill
#### April 20, 2011
#### Show how the logit graph varies with xbeta
#### Single-Equation Models: pg. 2

##########################################################################################
# Attribution: Thanks to Andrew Gelman:                                                  #
#              http://www.stat.columbia.edu/~gelman/bugs.course/logit/logit.R            #
#                                                                                        #
# In this exercise, we'll look at how the graph of the logit curve varies with xbeta.    #
# We'll generate different datasets, fit logit models to them,                           #
# and plot the corresponding curves.                                                     #
##########################################################################################
rm(list=ls(all=TRUE))
library(mvtnorm)
invlogit <- function(x){
  1 / (1 + exp(-x))
}#setting up a function that will help us generate data

## so, let's generate some data with some parameters:
x <- runif(100, -6, 6)
y <- rbinom (100, 1, invlogit(x))

pdf(file="Figs/logitXB.pdf")#this starts making a pdf of the figure
plot(x,y, col="white", main="Logit Curves") #this just sets up the plot; we'll add curves later

## now let's fit a logit to that, and plot the curve.
logit.R <- glm (y ~ x, family=binomial(link="logit"))
curve (invlogit (logit.R$coef[1] + logit.R$coef[2]*x),add=TRUE)

## if we do this with different values of x, we get different shapes:
x1 <- runif(100, -5, 5)
y1 <- rbinom (100, 1, invlogit(x1))
logit.1 <- glm (y1 ~ x1, family=binomial(link="logit"))
curve (invlogit (logit.1$coef[1] + logit.1$coef[2]*x),add=TRUE)


x2 <- runif(100, 0, 10)
y2 <- rbinom (100, 1, invlogit(x2))
logit.2 <- glm (y2 ~ x2, family=binomial(link="logit")) #Note this throws an error to make a point.
curve (invlogit (logit.2$coef[1] + logit.2$coef[2]*x),add=TRUE)
dev.off() #end pdf

## we can change x around, and see how this changes the curve.






#### Contributors: Ariel White, Ben Schneer, Michael Gill
#### April 20, 2011
#### Generate data for probit via a latent variable
#### Single-Equation Models: pg. 9

#################################################################################################
# Show how to generate binary data that could be modeled as a probit using y* as a              #
# continuous variable:                                                                          #
# Like the logit model, the probit model is used with binary outcome variables. In this example,# 
# our task is to simulate y* as a function of some covariate, then dichotomize it according to  #
# that story, and show that probit recovers the answer.                                         #
#                                                                                               #
# Imagine we had a dataset of 10000 random individuals with information about their height,     #
# whether they are of high or low income, but their true income is onobserved (y*). To show the #
# latent variable recovery of the probit, consider the following:                               #
# We generate a y* using simulated data with some covariates beta and some X's:                 #
#################################################################################################
rm(list=ls(all=TRUE))
N <- 10000

set.seed(02138)
height <- runif(N, 5.5, 6.25) #Sample of 10000 peoples' heights
hist(height, xlab="Height (in feet)", main="Frequency of Heights of Individuals in Sample (N=10000)") 

set.seed(02138)
epsilon <- rnorm(N, 0, 10000)

y.star <- 10000*height + epsilon #A person's income as a function of their height
hist(y.star)

highincome <- ifelse(y.star>60000,1,0) 
## Here we set a cutoff point according to if their income is higher than 60000 dollars a year.

## Our merged dataset:
data <- as.data.frame(cbind(y.star, highincome, height))
head(data)

probit.reg <- glm(highincome ~ height, data=data, family=binomial(link="probit")) 

summary(probit.reg)
## Note: We see the intercept returned is proportionate to the cut-off point we 
## set for high-income earners; 
## We also see that the coefficient on hight is proportionate to the true coefficient on height used to 
## generate y*.






#### Contributors: Ariel White, Ben Schneer, Michael Gill
#### April 20, 2011
#### Why we simulate from the multivariate normal
#### Single-Equation Models: pg. 18

############################################################################
# Suppose we ran a bivariate regression of y on x1 and x2 (w/ no constant) #
# and recovered the estimates beta                                         # 
# with covariance matrix var, estimated variance sigma2                    #
# and N = 200                                                              #
# We want to predict the outcome y when x1=1 and x2=1                      # 
############################################################################
rm(list=ls(all=TRUE))
library(MASS)

beta<-c(5,-10)
var<-matrix(c(10,.5,.5,4),nrow=2,ncol=2)
sigma2<-10
N<-200
S<-sigma2*(N-3)

## Part A. Simulating y the proper way
## Draws from multivariate normal
draws<-mvrnorm(1000,beta,sqrt(var))
sdraws<-sqrt(S/rchisq(1000, N-3, ncp=0))

X<-c(1,1)
sims<-draws%*%X

y<-sapply(sims,rnorm,n=1,sd=sdraws)

## Part B: Not using Multivariate normal
draws<-cbind(rnorm(1000,beta[1],var[1,1]),rnorm(1000,beta[2],var[2,2]))
sims<-draws%*%X

y2<-sapply(sims,rnorm,n=1,sd=sdraws)

## Comparison of predicted distribution of y
## Notice the inflated variance of the draws from the wrong distribution
## because of the omission of the covariances
pdf("Figs/SimulateMVNvsNorm.pdf",width=11,height=8.5)
plot(density(y),col="red",xlim=c(-30,30),main="Draws from MV Normal vs. Normal")
lines(density(y2),col="dodgerblue")
legend("topright",legend=c("MVN","N"),lty=c(1,1),col=c("red","dodgerblue"))
dev.off()






#### Contributors: Aditya Dasgupta and Sean Ingham
#### April 20, 2011
#### Simulation of Predicted Values and Expected Values
#### Single-Equation Models: pg. 20

############################################################################
# This example will use data from Besley, Persson and                      #
# Sturm (2010), "Political Competition, Policy and Growth", which          #
# examines the effect of political competition on economic                 #
# policies in US states over time.                                         #
############################################################################
rm(list=ls(all=TRUE))
## We will also need to simulate random draws from a multivariate 
## normal distribution
library(mvtnorm)

## Load Data 
load("Gov2001CodeLibrary.RData")
data <- BesleyEtAl

## Let's replicate column 1 from table 2 of the article. This is an
## OLS regression of state tax revenue as a share of state income
## on political competition, with 'state and year fixed effects' (dummy
## variables for each state and for each year, save a year and a state 
## reference category). Note that for the purposes of this example 
## we will use the ordinary OLS standard errors, as opposed to the SEs 
## adjusted for clustering, as in the article. 

lm1 <- lm(share_taxes_inc ~ compnorm + as.factor(state) + as.factor(year),
	data = data)

lm1$coefficients[2]; sqrt(vcov(lm1)[2,2])

## Note: the coefficient is the same as in the article, but the 
## standard error is different because we have not adjusted for clustering!
## This is to keep the example simple. But keep in mind that the decision 
## whether to to adjust standard errors or not impacts the predicted values 
## and expected values, since standard error adjustments affect the 
## variance-covariance matrix, and thus also our estimation uncertainty. 


## Simulate Predicted Values 
## Step 1) Since we have a large sample size, we simulate the uncertainty
## about our coefficient estimates by randomly drawing from a multivariate normal
## distribution with a mean equal to a vector of the coefficients estimates and 
## variance/covariance matrix equal to the variance-covariance matrix from our 
## fitted model.

set.seed(02138) ## Setting seed to ensure replicability

simbetas <- rmvnorm(1000, mean=lm1$coefficients, sigma=vcov(lm1))
help(rmvnorm)
## Step 2) We pick the explanatory variable values for which we want to generate
## predicted values of the outcome variable. Let's say we are interested in the 
## predicted state tax share in Arizona in 1970, if political competition were 
## quite low: -.3 (the domain of possible values is [-.5, 0]). Taking into account 
## the intercept, we want to create a vector of explanatory variable values where
## the Arizona state dummy variable is 1, the 1970 dummy variable is 1, the 
## political competition variable is -.3, and all other variables are 0.

predCovs <- c(  1, -.3, 1, c(rep(0,times=length(4:68))), 1,
	c(rep(0,times=length(70:100))) ) ## (intercept,compnorm,arizona,....,1970,...) 

## Step 3) For each vector of randomly drawn parameters, we generate a predicted
## value by multiplying it with the vector of explanatory variable values. Note
## that we also incorporate fundamental uncertainty for each predicted value
## by drawing from the stochastic component of our estimator - in the case of
## OLS, a normal distribution. 

set.seed(02138)

sim <- apply( simbetas, 1, function(x) {
	rnorm( 1, mean=predCovs%*%x, sd=sd(residuals(lm1)) )} ) # We apply the rnorm(...
										  # function by row, where
										  # each row corresponds
										  # to a vector of 
									        # coefficient estimates.
								
## Step 4) Finally, we plot the thousand predicted values we have simulated.

plot(density(sim), main="Predicted Value of Tax Share",
	xlab="Tax Share",col="blue")
abline(v=mean(sim),col="red")
savePlot(filename ="Figs/BesleyPredicted.pdf", type="pdf")

## Simulate Expected Values
## To simulate expected values, we simply average over the fundamental uncertainty
## associated with predicted values to yield one expected value for m simulations
## of predicted values. We do this repeatedly in order to simulate the distribution
## of expected values. 

set.seed(02138)

expectedsim <- apply( simbetas, 1, function(x) {
	mean(rnorm( 1000, mean=predCovs%*%x, sd=sd(residuals(lm1))) )} )
	
## This is the resulting simulated distribution:

plot(density(expectedsim), main="Expected Value of Tax Share",
	xlab="Tax Share",col="green")
abline(v=mean(expectedsim),col="yellow")
savePlot(filename ="Figs/BesleyExpected.pdf", type="pdf")
## Comparing Predicted and Expected Values
## Compare the simulated distributions of predicted and expected values:

plot(density(sim), main="Predicted and Expected Value of Tax Share",
	xlab="Tax Share",col="blue",ylim=c(0,3))
abline(v=mean(sim),col="red")
points(density(expectedsim), type='l',col="green")
abline(v=mean(expectedsim),col="yellow")
savePlot(filename ="Figs/BesleyCompare.pdf", type="pdf")

## Things to note:

## The predicted values are more widely dispersed, because, as we have seen,
## they incorporate both fundamental and estimation uncertainty, while
## expected values incorporate only estimation uncertainty and average over
## fundamental uncertainty. 

## The mean of both distributions is in expectation usually the same. 
## This is because both incorporate draws from the fixed/systematic 
## component of Y. 

## Which one matters depends on what question you are asking! If you
## are interested in population average properties, you would be more
## interested in the distribution of expected values. If you are 
## interested in causal effects, you would probably be interested in first
## differences in the distribution of expected values. If you are 
## interested in making a one-off prediction, then you are better off
## looking at the distribution of predicted values. 

## As a study's sample size approaches infinity, the distribution of 
## expected values collapses to a spike, due to decreased estimation 
## uncertainty. But the variance of predicted values does not 
## approach 0, since there is always fundamental uncertainty, which 
## has nothing to with how much data you have. This can be demonstrated
## by 'lying' to the computer and creating a much larger dataset by 
## randomly sampling from our current dataset with replacement. (Note: 
## we don't actually have any more information statistically, but we are 
## approximating an imaginary situation where we did). 

## Below is a function that creates a fake data set; you can choose the
## size. It repeats the procedure to simulate predicted and expected values
## of tax share, for explanatory variable values that correspond to Arizona 
## in 1970, with political competition at -.3. 

fake <- function(size){
	set.seed(02138)
	fakedata <- data[sample(1:nrow(data), size, replace = TRUE),] 
	lmfake <- lm(share_taxes_inc ~ compnorm + as.factor(state) + as.factor(year),
		data = fakedata)
	fakesimbetas <- rmvnorm(1000, mean=lmfake$coefficients, sigma=vcov(lmfake))
	fakepredCovs <- c(  1, -.3, 1, c(rep(0,times=length(4:68))), 1,
		c(rep(0,times=length(70:100))) )
	fakesim <- apply( fakesimbetas, 1, function(x) {
		rnorm( 1, mean=fakepredCovs%*%x, sd=sd(residuals(lmfake)) )} )
	fakeexpectedsim <- apply( fakesimbetas, 1, function(x) {
		mean(rnorm( 1000, mean=fakepredCovs%*%x, sd=sd(residuals(lmfake))) )} )
	plot(density(fakesim), main=as.character(size),
		xlab="Tax Share",col="blue",ylim=c(0,7))
	abline(v=mean(fakesim),col="red")
	points(density(fakeexpectedsim), type='l',col="green")
	abline(v=mean(fakeexpectedsim),col="yellow")
}

## Note that as this imaginary dataset gets bigger, the expected 
## value distribution collapses but the predicted value distribution 
## does not!

par(mfrow=c(2,2))
fake(1000)
fake(4000)
fake(16000)
fake(64000)
savePlot(filename ="Figs/ExpectedPredictedCollapse.pdf", type="pdf")
dev.off()





#### Contributors: Ana Catalano, Erru Yang, Yanfang Su
#### April 20, 2011
#### Example of Simulating First Differences
#### Single-Equation Models: pg. 24

############################################################################
# This code shows how to simulate first differences in two                 #
# different ways, manually and using Zelig.                                #
############################################################################
rm(list=ls(all=TRUE))

## First creating a dataset to work with:
female<-c(1, 0, 1, 0, 0, 0, 1, 1, 0, 0)
educ<-c(2, 1, 2, 2, 3, 3, 1, 2, 2, 1)
job<-c(1, 1, 1, 0, 0, 1, 1, 0, 1, 0)

data<-as.data.frame(cbind(female, educ, job))

X <- cbind(1, female, educ) #intercept and covariates
y<- job #dependent variable

## Running a probit model to predict job

library(Zelig)
out<-zelig(factor(job)~female + educ, model = "probit", data = data)

betas<-out$coef
varcov<-vcov(out)

## Simulating coefficients from a normal distribution

library(mvtnorm)
library(pscl)

set.seed(02138) #remember to set the seed to replicate later
simbetas <- rmvnorm(10000,betas, varcov) 
head(simbetas)

## Setting the covariates to their means

meanbetas <- c(apply(X, 2, FUN=mean))

## Setting the variable of interest to the values you care about 
## (for continuous or ordinal variables like education this may be a min and max)

means.female<-meanbetas
means.female[2]<-1 #[2] because female comes second in your X

means.male<-meanbetas
means.male[2]<-0

first.diffs<-pnorm(means.female%*%t(simbetas)) - pnorm(means.male%*%t(simbetas))

mean(first.diffs) #difference in expected values
 
quantile(first.diffs, .025); quantile(first.diffs, .975) #confidence interval


##Example of Zelig doing this for you
x.low <- setx(out, female = 0) 
x.high <- setx(out, female = 1)

s.out <- sim(out, x = x.low, x1 = x.high)
summary(s.out) #and you should get the same thing as the longer method, above.
plot(s.out)




#### Contributors: Leslie Finger and Adela Soliz 
#### April 20, 2011
#### Reparameterizing on an Unbounded Scale and First Differences
#### Single-Equation Models: Pg. 25

######################################################################################
# This code shows that by reparameterizing pi in the logistic model on an unbounded  #
# scale, so that pi=1/(1+exp(-Xbeta)) we are able to maximize the parameters more    #
# easily.  To sum up the steps, we first subset the data, then we write and optimize # 
# functions for the logistic model first on an unbounded scale and then on a bounded #
# scale.  Finally, we simulate first differences for certain interesting populations.#
######################################################################################
rm(list=ls(all=TRUE))
library(MASS)
library(Zelig)

## To demonstrate the use of reparameterizing on an unbounded scale, 
## let's look at how welfare determines whether kids go to school (scbase) using a logit model. 
load("Gov2001CodeLibrary.RData")
dat <- kids
head(dat)

## Subsets the data so it only includes data from 1998.  
## This data set also includes 1999 observations, so this deletes any kids in the data set for 
## both years. This data looks at a conditional cash transfer intervention starting in 98; by 
## looking at scbase, we are only looking at school in 97, prior to the conditional cash transfer 
## intervention. 
dat98 <- dat[dat$year==98,]
unique(dat98$year)

## Subsets the data to include just the variables we care about (the outcome, scbase, the explanatory variable,
## welfare, and the controls: child's gender, indigenous status, grade, the head of household's education, sex, 
## age, the number of family members, and the distance from secondary school).
dat1 <- subset(dat98, select=c(scbase, welfare, gender, indig, hohedu, hohage, 
                               hohsex, fam_n, dist_sec), 
               drop=TRUE)
head(dat1)

## This drops the na's from the dataset.  Ordinarily we would want to impute this missing data, 
## but for the purpose of this exercise, we took this shortcut.
dat2<-na.omit(dat1)
dim(dat2)
head(dat2)

## Functions and Optimization
## There are two ways to find the parameter values- using your own function or using Zelig.  

## Makes a function of the log likelihood of the logistic model.
logit.ll<-function(par, y, X){
	betas<-par
	X<-as.matrix(X)
	ll<-sum((-y*log(1+exp(-X %*% betas)))+((1-y)*log(1-1/(1+exp(-X %*% betas)))))
	return(ll)
	}

## Defines the variables in the function: y is the first column of the data matrix, 
## Xs are all the columns but the first one, and X is a matrix of the data plus a column of 1s 
## at the beginning for the intercept.
head(dat2)

Xs<-as.matrix(dat2[,-1])
y<-as.vector(dat2$scbase)
X <- cbind(1,Xs)
head(X)

## Optimizes the function to find the parameter values.  Our Zelig results are slightly different 
## from those produced by optim.  We believe this is because our function is badly behaved.
par <-rep(0,9)
opt.log<-optim(par, y=y, X=X, fn=logit.ll, method="BFGS", control=list(fnscale=-1), hessian=TRUE)

## Or you could optimize using zelig.
results <- zelig(scbase ~ welfare + gender + indig + hohedu + hohage + hohsex + 
                 fam_n + dist_sec, data=dat2, model = "logit")
results$coef
summary(results)
vcov(results)

## What if we do not put the model on an unbounded scale, but rather set pi=(-X %*% beta)? 
## Then our function becomes:
logit.llb<-function(par, y, X){
	betas<-par
	X<-as.matrix(X)
	llb<-sum(-y*(-X %*% betas)+(1-y)*(1-(-X %*% betas)))
	return(llb)
	}

## And we optimize using the L-BFGS-B method which allows us to set upper and lower bounds.  
## This is necessary because the parameters in the bernoulli model has to be between 0 and 1.	
opt.logb<-optim(par=rep(0,10), y=y, X=X, fn=logit.llb, method="L-BFGS-B", lower=rep(0,10), 
                upper=rep(1,10), control=list(fnscale=-1), hessian=TRUE)

## Putting pi on an bounded scale causes problems when we try to optimize.  
## That's why we put it on an unbounded scale.


## Simulations and First Differences

## Let's simulate the difference between the probability of school attendance changing 
## the gender variable for boys and girls (Male = 1/Fem = 0) and setting all other variables 
## at their medians.

set.seed(012345)  #Set the seed so you can reproduce the results.
draws <- mvrnorm(10000, mu=results$coef, Sigma=vcov(results))  
#Draw from a distribution defined by your optimization results.
head(draws)
nrow(draws)

meds <- apply(dat1[,-1], MARGIN=2, FUN=median) 
#Sets all variables except the outcome to their median values.
meds

medians <- c(1, meds) 
# Adds a 1 at the beginning of the vector of median values to account for the intercept.

## Makes a vector of all variables at their median values, except for gender which is set to 0 
## for a girl and 1 for a boy.
boys <- medians
girls <- medians
girls["gender"] <- 0
girls

## Calculates a simulated value of pi for all variables set to their medians except gender.
pi.boys <- 1/(1+ exp(-boys %*% t(draws)))
pi.girls <- 1/(1+ exp(-girls %*% t(draws)))

firstdif <- pi.boys- pi.girls
mean(firstdif)

## Boys have a higher probability of attending school.  This may be due to childbirth etc.
quantile(firstdif, .025); quantile(firstdif,.975)

## Finds quantiles by gender, to gauge certainty.
quantile(pi.boys, .025); quantile(pi.boys,.975)
quantile(pi.girls, .025); quantile(pi.girls,.975)


## We also calculated the expected value of pi for the whole model.  
## This is not as useful as the first differences, but it gives us some idea of the probability of school 
## enrollment for the average person in the dataset.
pi.model<-1/(1+exp(-medians%*%t(draws)))
mean(pi.model)

quantile(pi.model, .025); quantile(pi.model, .975)
## In sum, reparameterizing pi to an unbounded scale facilitates the optimization 
## and allows us to simulate quantities of interest.







#### Contributors: Chilton, Crouch, Lavie 
#### April 20, 2011
#### Graph on Confidence Intervals 
#### Single-Equation Models: Pg. 28

######################################################################################
# The goal: create a graph that looks similar to the one on p. 28 to the             #
# single equation slides with different data                                         #  
#                                                                                    # 
# the data is taken from                                                             #
# "Ethnicity, Insurgency and Civil War," by James Fearon and David Laitin.           # 
# The article uses a number of variables to predict the onset of civil conflict.     # 
#                                                                                    #
######################################################################################
rm(list=ls(all=TRUE))
library(Zelig)
library (MASS)

## load the data from the relevant library
## removing unnecessary variables and cleaning 
## the data
load("Gov2001CodeLibrary.RData")
vars.m1 <- c("onset","warl","gdpenl","lpopl1","lmtnest", "ncontig", "Oil",
             "nwstate","instab","polity2l","ethfrac","relfrac")
fearonclean<-drop(fearon[vars.m1])
head(fearonclean)
fearonclean<-na.omit(fearonclean)
fearonclean["onset"]<-replace(fearonclean["onset"],
fearonclean["onset"]==4,1)

## running the basic logit regression
output<- zelig(onset ~ warl + gdpenl + lpopl1 + lmtnest + ncontig +
Oil + nwstate + instab + polity2l+ ethfrac + relfrac,
data = fearonclean, model = "logit")

## simulating the betas
coef<-output$coefficients
vcov<-summary(output)$cov.unscaled
set.seed(02108)
betas <- mvrnorm(n = 1000, mu = coef, Sigma = vcov)
## betas, then, is a simulated matrix with possible values of the coefficients

## now turning on to create simulated data, in this case the median data
fearonclean1<-as.matrix(fearonclean)
data<-c(rep(0,ncol(fearonclean1)))
names(data)<-c(colnames(fearonclean1))
for (i in 1:length(data)){
  data[i]<-median(fearonclean1[,i])
}
data[1]<-1
names(data)[1]<-"Intercept"

## first differences- a country with 1 on nwstate (new states) parameter
datanwst<-data
datanwst["nwstate"]<-1
gdpseq <- seq(.25,7,.25)
## creating a matrix of non-new state countries where 
## the difference is the changing GDP
datamat <- matrix(rep(data), nrow = length(gdpseq),
ncol = length(data), byrow = TRUE)
datamat[,3] <- gdpseq
## and a parallel matrix for nwstate=1 countries
datamatnwst <- matrix(rep(datanwst), nrow = length(gdpseq),
ncol = length(datanwst), byrow = TRUE)
datamatnwst[,3] <- gdpseq
colnames(datamat)<-c(names(data))
colnames(datamatnwst)<-c(names(datanwst))

## simulating the expected values
holder<-holdernwst<-matrix(NA, nrow=nrow(datamatnwst), ncol=3, byrow=TRUE)
temp<-c(rep(0,nrow(betas)))

for (i in 1:nrow(datamat)){
  temp <-1/(1+exp(-datamat[i,]%*%t(betas)))
  holder[i,1] <-mean(temp)
## the confidence intervals are the 2.5% "tails"
  holder[i,2]<-sort(temp)[25]
  holder[i,3]<-sort(temp)[975]
## same for the new states 
  temp <-1/(1+exp(-datamatnwst[i,]%*%t(betas)))
  holdernwst[i,1] <-mean(temp)
  holdernwst[i,2]<-sort(temp)[25]
  holdernwst[i,3]<-sort(temp)[975]
}
pdf("Figs/ConfidenceIntervals.pdf")
plot(gdpseq, holder[,1], type = "l", ylim = c(0,.25), lwd = 2,
col = "darkblue", xlab = "GDP per capita (thousands of dollars)",
ylab = "Prob of Civil War Onset", cex = .1)

##creating the vertical confidence intervals
for (i in 1:length(gdpseq)){
  segments(gdpseq[i], holder[i,2],gdpseq[i], holder[i,3],col = "darkblue",
           lwd=1.2)
}
##now same for nwstates
lines(gdpseq, holdernwst[,1], col = "darkorange3", lwd = 2)
for (i in 1:length(gdpseq)){
   segments(gdpseq[i], holdernwst[i,2],gdpseq[i], holdernwst[i,3],col = 
   "darkorange", lwd=1.2)
}
legend("topleft", legend = c("Regular Countries","New States"),
lwd = c(2,2), col = c("darkblue","darkorange3"),
cex = .6)
dev.off()



#### Contributors: Nils Hagerdal and Dan Masterson
#### April 20, 2011
#### Ternary Plots in R
#### Single-Equation Models: Pg. 36

######################################################################
# The idea is to create plots that show the distribution             #
# of some variable for a range of subjects, where the                #
# variable is split between 3 possible values that all               #
# sum up to a constant: a + b + c = K                                #
#                                                                    #
# Eg, UK vote share: Tories + Labour + Liberals = 100%               #
# for each constituency.                                             # 
#                                                                    #
# Here, we use the distribution of employment in European            #
# countries: employment shares in the primary, secondary             #
# and tertiary sectors for each country sum to 100%.                 #
# We use the package "ade4" and the "euro123" data set,              # 
# with code borrowed from:                                           #
# http://addictedtor.free.fr/graphiques/RGraphGallery.php?graph=34   #
######################################################################
rm(list=ls(all=TRUE))
library(ade4)
data(euro123)
summary(euro123)
head(euro123)

colnames(euro123$in78) <- c("Primary", "Secondary", "Tertiary")

## triangle.plot() is the main command
pdf("Figs/TernaryPlot1.pdf")
triangle.plot(euro123$in78, clab = 0, cpoi = 0.9, addmean = TRUE, show = FALSE,
	sub = "Employment by sector, EU 1978", csub = 1, possub = "topleft")
dev.off()
pdf("Figs/TernaryPlot2.pdf")
triangle.plot(euro123$in78, label = row.names(euro123$in78), clab = 0.5,
	show = FALSE,
	sub = "Employment by sector, EU 1978", csub = 1, possub = "topleft") 
dev.off()
	
	
	
	
	
#### Contributors: Michael Hankinson & Jackelyn Hwang
#### April 20, 2011
#### fit and simulate quantities of interest from a grouped uncorrelated binary variables model
#### Single-Equation Models: Pg. 47

####################################################################################################
# In plain English: The Grouped Uncorrelated Binary Variable (GUBV) model is similar to a          #
# binary logit, but we are looking at i.i.d. (uncorrelated) groups (grouped) of Bernoulli          #
# trials (binary variables).  In other words, we may only observe a total count of the binary      #
# outcomes, not the outcome of each individual event.                                              #
#                                                                                                  #
# For instance, Gary uses the example of the number of times you voted out of the last 5           #
# elections.  However, instead of observing each individual turnout, we receive the total number   #
# of turnouts (coded as "1") as a single value.  Were to you to vote in 4 elections, our outcome   #
# would be "4".  Yet, we have no idea specifically in which of the past five elections you voted,  #
# but simply that you voted in four of them.                                                       #
#                                                                                                  #
# How to Simulate Quantities of Interest - (raw instructions as drawn from G. King slides).        #
# 1) Run optim and get beta-hat and the variance matrix                                            #
# 2) Draw many values of beta-tilde from the multivariate normal with mean vector beta-tilde and   #
#    the variance matrix that come from optim.                                                     #
# 3) Set X to your choice of Values, X-sub"c"                                                      #
# 4) Calculate simulations of the probability that any of the component binary vairables is a one. #
# 5) If pi is of interest, summarize with mean, SD, CI's or histogram as needed.                   #
# 6) If simulations of y are needed, go one more step and draw y-tilde from Binomial(y|pi).        #
####################################################################################################

rm(list=ls(all=TRUE))
## First, let's build some data to play with!

## Generate a true dataset of n=500 observations with 6 variables: Y, X1, X2, X3, X4, X5.
## The Xs will be drawn from a multivariate normal with means 0, variances 1, and the 
## following correlation matrix 
## Correlation matrix (1 -.12 -.1 .5 .1, -.12 1 .1 -.6 .1, -.1 .1 1 -.5 .1, .5 -.6 -.5 1 .1, 
## .1 .1 .1 .1 1). 
library(MASS)
set.seed(62788)

sigma <- matrix(c(1,-.12,-.1,.5,.1,-.12,1,.1,-.6,.1,-.1,.1,1,-.5,.1,.5,-.6,-.5,1,.1,.1,.1,.1,.1,1),5,5)
truedata <- mvrnorm(n=500,rep(0,5),sigma)
colnames(truedata) <- c("X1","X2","X3","X4","X5")
head(truedata)

## Now that we have our Xs, let's draw our Ys.  Because we are looking for integers, we'll pull 
## 5 draws from a binary distribution.  These 5 draws will be summed into a single variable, our 
## Y, representing the number of elections in which the individual voted.

Ys<-rbinom(500,5,.5)
Ys

## Great, now we cbind our Ys with our Xs.
GUBVdata1<-cbind(Ys,truedata)
head(GUBVdata1)

## Oh, and don't forget the intercept.
intercept<-matrix(c(rep(1,500)))
GUBVdata<-cbind(intercept,GUBVdata1)
head(GUBVdata)

## Fantastic!

## Let's try to understand Gary's instructions.
## 1) Run (optim) and get beta-hat and the variance matrix
## First, we need the function.  Below is the GUBV function as taken from Gary's slides.
## lnL(Beta|y) = sum(N-y)*log(1+exp(X %*% beta)-y*log(1+exp(-X %*% beta)))

## Because we are looking at 5 elections, our N will be 5, the maximum number of events in 
## which one can participate.

GUBV <- function(beta,y,X){
  xbeta<- X%*% beta
	out<- sum((5-y)*(log(1+exp(xbeta))-y*log(1+exp(-xbeta))))
	return(out)
	}

## Before running optim, we need to define our data components.
X<-as.matrix(GUBVdata1[,-1])
y<-as.vector(GUBVdata1[,1])

## And, now we'll run optim.
results<-optim(par=rep(0.1,5), y=y, X=X, fn=GUBV, method="BFGS", control=list(fnscale=-1), hessian=TRUE)

## 2) Draw many values of beta-tilde from the multivariate normal with mean vector beta-tilde and 
##    the variance matrix that come from optim.

## To get the mean vector beta-tilde and the variance matrix, we need to pull apart the optim results.

## Varaince Matrix
hessian <- as.matrix(results$hessian)
varcov <- solve(-hessian)

## Mean Vector
coef<-as.vector(unlist(results$par))

## Now, we'll draw our "many values of beta-tilde" from the multivariate normal.  How about 500?
draws<-mvrnorm(n=500, mu=coef, varcov)

## 3) Set X to your choice of Values, X-sub"c"

## Let's create an individual based on the mean of each covariate.  It may not occur in the real world, 
## but it should fit within our data.

MeanGUBV<-matrix(c(mean(GUBVdata1[,2]), mean(GUBVdata1[,3]), mean(GUBVdata1[,4]), 
                 mean(GUBVdata1[,5]), mean(GUBVdata1[,6])), nrow=1, ncol=5)

## 4) Calculate simulations of the probability that any of the component binary vairables is a one.

## Each of the 5 elections can register a 0 or 1, depending on whether our created individual decided to vote.  
## We can  apply our drawn values of beta-tilde to our created individual to calculate their probability of 
## participating in any individual election.

pi.one<- 1/(1+exp(-MeanGUBV %*% t(draws)))

## Now, we have 500 simulated probabilities of whether our created individual will turn out to vote.

## 5) If pi is of interest, summarize with mean, SD, CI's or histogram as needed.
## I love visuals.  Let's run some visual diagnostics.

## Mean
meanpi<-mean(pi.one)

## For ease, we'll make this a pretty vector.
tpi.one<-t(pi.one)
pi.one.vec<-as.vector(pi.one)

## Standard Deviation - always important
standev<-sd(as.vector(pi.one))

## Confidence Interval, with n=500
error<-qnorm(0.975)*standev/sqrt(500)
left<-meanpi-error
right<-meanpi+error
left
right

## Histogram, with Confidence Intervals
pdf(file="Figs/GroupedUnc.pdf")
hist(tpi.one, col="lavender", main="Histogram of Probabilities of One, With Confidence Interval", xlab="Probability of One")
abline(v=left, col="red")
abline(v=right, col="red")
dev.off()
## Wow, looks like a tight CI.  Alright!

## 6) If simulations of y are needed, go one more step and draw y-tilde from Binomial(y|pi).
## Let's take 1000 draws of y, based on our new probability of each binary component.  
## This will help simulate the number of elections in which our created individual will participate.

simy<-rbinom(1000, 5, meanpi)
median(simy)
## Three elections.  3 out of 5.  Better than the average American, right?

## Congratulations!  You've mastered Grouped Uncorrelated Binary Variable Modeling.  
## What a relief!  Now, back to Facebook.






#### Contributors: Frey and Walker
#### April 20, 2011
#### Variance in Poisson Models
#### Single-Equation Models: Pg. 55

#####################################################################################################
# We examine the variance assumptions of the Poisson count model.                                   #
##################################################################################################### 
rm(list=ls(all=TRUE))
load("Gov2001CodeLibrary.RData")

## Overdispersion
## Loading the Data
data<- Overdispersed
names(data)

## Altering "Treat"
data$treatf<-factor(data$treat)

## Generating a Poisson
PoissonOD<-glm(count ~ treatf + prior + age, family=poisson, data=data)
summary(PoissonOD)

## Calculating the Residuals of the Poisson Model
resid<-residuals.glm(PoissonOD, type="deviance")
fitted<-PoissonOD$fitted.values

## Calculating SSR: sum residuals^2/(#observations-#coefficients)
sum(resid^2)/(59-4)

## Comparing to Chi-Squared with df=#observations-#coefficients
1-pchisq(sum(resid^2),55)


## Plotting the Residuals
plot(fitted, resid, main="Residuals for Overdispersed Poisson Model", 
     xlab="Fitted Values", ylab="Deviance Residuals", ylim=c(-4,4))


##	Correct Variance

## Generating Data
var1<-factor(c('A','A','A','A','A','A','A','A','A','A','B','B','B','B','B','B','B','B','B','B'))
var2<-factor(c('Y','N','Y','Y','Y','Y','N','Y','N','Y','N','Y','N','Y','Y','Y','Y','N','N','N'))
var3<-c(10,5,9,4,6,14,2,3,8,10,11,15,4,5,7,9,12,6,5,8)
var4<-c(12,15,13,5,8,17,10,6,22,14,20,18,11,11,12,13,28,10,14,10)
data<-data.frame(Treat, var1, var2, var3)

## Generating Poisson
PoissonCorrect<-glm(var3 ~ var1 + var2, family=poisson, data=data)
summary(PoissonCorrect)

## Residuals and Fitted Values
residC<-residuals.glm(PoissonCorrect, type="deviance")
fittedC<-PoissonCorrect$fitted.values

## Calculating SSR: sum sq resid / #observations - #coefficients
sum(residC^2)/(20-3)

## Chi-Squared
1-pchisq(sum(residC^2), 17)

## Plotting
plot(fittedC, residC, main="Residuals for Overdispersed Poisson Model", 
     xlab="Fitted Values", ylab="Deviance Residuals")


## NOTE: If you offset the data, the dispersion appears to be even smaller 
## (the variance appears more correct)
## To see this, run the following:

PoissonOffset<-glm(var3 ~ var1 + var2, family=poisson, data=data, offset=log(var4))
residOffset<-residuals.glm(PoissonOffset, type="deviance")
fittedOffset<-PoissonOffset$fitted.values
1-pchisq(sum(residOffset^2), 17)
plot(fittedOffset, residOffset, 
     main="Residuals for Overdispersed Poisson Model", 
	 xlab="Fitted Values", ylab="Deviance Residuals")


## Underdispersed Poission

probs<-prop.table(dbinom(0:10, 10, .5) +
ifelse(0:10==5,1,0))
dataUD<-sample(0:10, 1000, pr=probs, repl=TRUE)

obs<-c(1:1000)
data.UD<-data.frame(obs=obs, dataUD=dataUD)

## Generating a Poisson
PoissonUD<-glm(obs ~ dataUD, family=poisson, data=data.UD)
summary(PoissonUD)

## Calculating the Residuals of the Poisson Model
residUD<-residuals.glm(PoissonUD, type="deviance")
fittedUD<-PoissonUD$fitted.values

## Calculating SSR: sum residuals^2/(#observations-#coefficients)
sum(residUD^2)/(998)

## Comparing to Chi-Squared with df=#observations-#coefficients
1-pchisq(sum(residUD^2),998)

## Plotting the Residuals
plot(fittedUD, residUD, main="Residuals for Overdispersed Poisson Model", 
     xlab="Fitted Values", ylab="Deviance Residuals", ylim=c(-4,4))





#### Contributors: James Conran, Jeremy Ferwerda, Yue Hou
#### April 20, 2011
#### Logit vs. Probit
#### Single-Equation Models: No pg. #

#####################################################################################################
# Show that the logit and probit do not offer substantively different quantities of interest        #
##################################################################################################### 
rm(list=ls(all=TRUE))
## We'll start by visualizing the probit and logit functions

## First, we will plot the probability densities using R's canned functions rnorm and rlogis

## Number of iterations
n <- 10^6

## Set up the plot space
par(mfrow=c(1,2))

## Graph densities
plot(density(rnorm(n)),main="Density",xlab="x") # probit
lines(density(rlogis(n)),col="blue") # logit
legend("topleft",legend=c("Probit","Logit"),bty="n",cex=.7,lty=1,col=c("black","blue"))

## input values
test <- seq(-5,5,by=.01)

## Second, let's plot the inverse link functions
link.logit <- 1/(1+exp(-test))
link.probit <- pnorm(test)

## Graph inverse link functions
plot(test,link.probit,type="l",xlim=c(-5,5),main="Inverse Link Function",xlab="XB",ylab="pi") #probit
lines(test,link.logit,col="blue",type="l",xlim=c(-5,5)) #logit
legend("topleft",legend=c("Probit","Logit"),bty="n",cex=.7,lty=1,col=c("black","blue"))
savePlot(filename = "Figs/LogitProbit1.pdf", type="pdf")

## It should be clear that logit and probit are very similar, except at the tails of the distribution. 
## As we will see in the next section, the difference in the tails does not result in substantially
## different estimates

## Concrete Example ##

## Use a default dataset that has a dichotomous variable
data(mtcars)
attach(mtcars)

## Let's test the probability that a car will have an automatic transmission (am),
## based upon its mpg rating and the number of cylinders.

## Create a matrix for the results and covariates
y <- as.matrix(am) # results - automatic transmission
X <- cbind(1, mpg, cyl) # covariates

## Define the log-likelihood functions. Although this can be done quickly using Zelig,
## these functions are included in order to show the log-likelihood of the inverse link functions.
ll.probit <- function(beta, y=y, X=X){
	xbeta = X %*% beta
	phi <- pnorm(xbeta, log = TRUE)
	phi2 <- pnorm(xbeta, log = TRUE, lower.tail = FALSE)
	result <- sum(y*phi + (1-y)*phi2)
	return(result)
}

ll.logit <- function(beta, y=y, X=X){
	xbeta = X %*% beta
	result <- -sum(log(1+exp((1-(2*y))*xbeta)))
	return(result)
}

## Use optim to find the results, inputting a vector of 0s as starting values
result_probit <- optim(par = c(rep(0,ncol(X))), fn = ll.probit, y=y, X=X, 
                       control = list(fnscale = -1),method = "BFGS", hessian = TRUE)

result_logit <- optim(par = c(rep(0,ncol(X))), fn = ll.logit, y=y, X=X, 
                      control = list(fnscale = -1),method = "BFGS", hessian = TRUE)

## Although the coefficients are different, the interpretation is not
result_probit$par
result_logit$par

## Calculate the probability of having an automatic transmission for each row in X
probit_o <- pnorm(X  %*%  result_probit$par)
logit_o <- 1/(1+exp(-X  %*%  result_logit$par))

## Table of predictions - both methods produce similar estimates
table <- cbind(probit_o,logit_o,am)
colnames(table) <- c("Probit Pr", "Logit Pr", "Automatic")
table

## Plot the resulting predictions
par(mfrow=c(1,1))
plot(density(probit_o),main="Logit vs Probit",xlab="",ylab="")
lines(density(logit_o),col="blue")
legend("topleft",legend=c("Probit","Logit"),bty="n",cex=.7,lty=1,col=c("black","blue"))
savePlot(filename = "Figs/LogitProbit2.pdf", type="pdf")

## As should be clear, the logit and probit result in very similar predictions

## Note that for a quick check, you can also follow Amemiya's (1981) and King's (1989) advice
## and multiply probit ceofficients by 1.6 in order to provide a rough comparison with the logit scale.
result_probit$par * 1.6
result_logit$par





#### Contributors: Sorapop Kiatpongsan and Slawa Rokicki
#### April 20, 2011
#### Exponential Models
#### Single-Equation Models: No pg. #

######################################################################################################
# We show how to estimate an exponential model in R and produce Hazard ratios and other quantities   #
# of interest (using Zelig)                                                                          #
# This code is to show how to fit the data into an exponential model in R and produce Hazard ratios  #
# and other quantities of interest.                                                                  #
# Data is from the course BIO 223 Applied Survival Analysis at Harvard School of Public Health.      #
# This is the data of patients with bladder tumor with 8 covariates.                                 #
#  Group 1 = Control, 2 = Treatment                                                                  #
#  futime = follow up time                                                                           #
#  number = number of tumor                                                                          #
#  size = size of largest tumor                                                                      #
#  t1 = time at the first recurrence                                                                 #
#  t2 = time at the second recurrence                                                                #
#  t3 = time at the third recurrence                                                                 #
#  t4 = time at the forth recurrence                                                                 #
# Note that NA means that the data is censored.                                                      #
# Using data "bladder tumor"                                                                         #
# Attached as txt file "bladder.tumor.noheader.txt"                                                  #
###################################################################################################### 
rm(list=ls(all=TRUE))
## Read Data
load("Gov2001CodeLibrary.RData")

## View the data
bladder

## Look at the distributions of baseline covariates
table(bladder$group)
table(bladder$number)
table(bladder$size)

## Now, using Zelig to estimate an exponential model
library(Zelig)

## Set random seed number
set.seed(02138) 

## Use the exponential duration regression model if you have a dependent variable representing
## a duration (time until an event). The model assumes a constant hazard rate for all events.
## The dependent variable may be censored (for observations have not yet been completed when
## data were collected). Ref. Zelig manual

## Creating censoring indicator c = o if t1 is censored otherwise c = 1
## "time" is time to first recurrence or end of follow-up;
## "c" = 1 if subject experienced a recurrence (not censored);
c <- as.numeric(is.na(bladder$t1)==F)
time <- pmin(bladder$futime, bladder$t1, na.rm=T)
bladder <- cbind(bladder, c, time)
head(bladder)

## first observation has 0 follow-up time, not used in any analysis, 
## drop first as it gives errors
bladder <- bladder[-1,]


## Now using Zelig to estimate and compare first recurrence time between treatment and control groups
z.out <- zelig(Surv(time, c) ~ group, model = "exp", data = bladder)
summary(z.out)


## Defining covariates for control and treated groups
x.control <- setx(z.out, group = 1)
x.treated <- setx(z.out, group = 2)

## To find a hazard ratio for an exponential model
## Note that the expected value = 1/hazard
## Then, to find hazard ratio is to find the ratio of the 1/expected value
s.out.control <- sim(z.out, x = x.control)
summary(s.out.control)
## E(Y/X) = 26.73509
## Hazard = 1/26.73509 = 0.03740403

s.out.treated <- sim(z.out, x = x.treated)
summary(s.out.treated)
## E(Y/X) = 45.64097
## Hazard = 1/45.64097 = 0.02191014

## Then, the hazard ratios of hazard of the control to the hazard of the 
## treated group = 0.03740403/0.02191014 = 1.707156

## Finding other quantity of interest: The difference between the time to first recurrence 
## between the control and treated groups
## Simulation to find expected values and first differences
s.out <- sim(z.out, x = x.control, x1 = x.treated)
summary(s.out)
plot(s.out)

## if you wanted to pull out the individual expected values:
expected.treated<-mean(s.out.treated$qi$ev)
expected.treated
expected.control<-mean(s.out.control$qi$ev)
expected.control

hazard.treated<-1/expected.treated
hazard.treated
hazard.control<-1/expected.control
hazard.control

Hazard.Ratio<-hazard.control/hazard.treated
Hazard.Ratio





#### Contributors: Maxwell Palmer & Andrew Hall
#### April 20, 2011
#### Weibull Models
#### Single-Equation Models: No pg. #

###############################################################################################
# Show how to estimate a weibull model in R and produce Hazard ratios and other quantities of #
# interest (using Zelig)                                                                      #
# Note that the code and example here comes straight from                                     #
# http://cran.r-project.org/web/packages/Zelig/vignettes/weibull.pdf                          #
# I merely add expository comments.                                                           #
###############################################################################################
rm(list=ls(all=TRUE))
library(Zelig)
## Use sample dataset from King, Alt, Burns, and Laver (1990)
data(coalition)

## Data description from original paper:
## "This data set contains survival data on government coalitions in parliamentary democracies 
## (Belgium, Canada, Denmark, Finland, France, Iceland, Ireland, Israel, Italy, Netherlands, Norway, 
## Portugal, Spain, Sweden, and the United Kingdom) for the period 1945-1987. For parsimony, country 
## indicator variables are omitted in the sample data."

## We want to estimate a model in which survival times of coalitions follow a Weibull distribution.

## Our dependent variable is the observed survival time of a coalition.  
## However, this is censored data because there is a known maximum duration 
## governed by the country's election laws.  These laws create what the authors call a 
## "constitutional interelection period (CIEP)" which induces right-censoring on 
## the duration observations.

## We can see how many right-censored observations we have:
table(coalition$ciep12)

## When ciep12 is 1, the coalition survived to the twelve months before the CIEP 
## and therefore the duration value is censored.  The true duration could be 
## larger than the observed.

## Therefore we need to modify our dependent variable using Surv()

## The call to Surv() creates the "survival object", which takes into account the 
## censored nature of the data. 
## You pass in your time variable (duration here) and your event variable (ciep12 here) 
## and you get back a Surv object you can use for survival models.

## Then you run zelig with the Surv object as the dependent variable. 
## Given the Weibull model, zelig will find MLE estimates for your explanatory variables 
## fract and numst2.  
## fract reflects the number/size of parties in the parliament, with higher values meaning 
## more fractionalization. numst2 is a dummy, 1 if majority government, 0 if minority government.
z.out <- zelig(Surv(duration, ciep12) ~ fract + numst2, model="weibull", data=coalition)

## Now we have our estimated model, so we can make simulations from it.

## For example, here we simulate to find the effect of being a majority government 
## vs. being a minority 
## government on the survival of the coalition.
x.low <- setx(z.out, numst2=0)
x.high <- setx(z.out, numst2=1)
s.out <- sim(z.out, x=x.low, x1=x.high)

summary(s.out)
plot(s.out)





#### Contributors: Colin Sullivan & Todd Kawakita 
#### April 20, 2011
#### Cox Proportional Hazards Models
#### Single-Equation Models: No pg. #

###############################################################################################
# This brief guide will demonstrate Cox proportional hazards model in Zelig.  We will use     #
# data from Zelig and some fancy footwork to estimate a hazards model and produce quantities  #
# of interest.  This draws heavily on examples in the Zelig manual (written by Patrick Lam    #
###############################################################################################
rm(list=ls(all=TRUE))
## Load the Zelig Package
library(Zelig)
    
## Load new data - 
## We will be looking at a dataset in Zelig called "coalition2" - Coalition Dissolution in 
## Parliamentary Democracies, Modified Version
data(coalition2)
  
## Let's have a look at the data:
head(coalition2)
summary(coalition2)

## Variables:
##  duration - The length of duration in months of the cabinet coalition
##  ciep12 - Binary variabel - did the coalition conclude its constitutional interelection 
##           period, or maximum length of duration?
##  invest - Binary - Is there a legal requirement for legislative investiture?
##  fract -  An index of the number and size of parties in parliament
##  polar -  Measures polarization - support for extremist parties
##  numst2 - Binary - Is this a majority government?
##  crisis - The length of crisis preceding government formation (in days)
##  country - No, this is not a trick.  Name of country

## So, how long do coalitions last?
plot(density(coalition2$duration, bw = 1.5), xlim=c(0, 70), main = "Duration Density", 
        xlab="Cabinet Duration in Months")
abline(v=mean(coalition2$duration))
  
## Estimating the model
## Let's start by looking at just one covariate:  invest
## The syntax:     z.out <- zelig(Surv(Y, C) ~ X1 + X2, model = "coxph", data = mydata)
##   where Y is the survival measure (duration) and C is the censoring mechanism (ciep12),
##   and X1 and X2 are your covariates
ex1 <- zelig(Surv(duration, ciep12) ~ invest, model = "coxph", data = coalition2)
summary(ex1)
    
## Note that if your censorship mechanism is missing, Zelig will assume all observations are censored!
ex1.b <- zelig(Surv(duration) ~ invest, model = "coxph", data = coalition2)
summary(ex1.b)  

## Running the model with several covariates
ex2 <- zelig(Surv(duration, ciep12) ~ invest + fract + polar + numst2 + crisis, 
             model = "coxph", data = coalition2)
summary(ex2)

## Generating quantities of interest
## Imagine you were interested in the impact of moving from the first to the third quartile
## of fractionalization. First, we find the quartiles.  Next, we assign those values to 
## the two sets of covariates, and hold all other covariates at their medians.
summary(coalition2$fract)
x.lowfrac <- setx(ex2, fract = 677)
x.highfrac <- setx(ex2, fract = 788)
  
## Next, we SIMULATE!!!
diff.frac <- sim(ex2, x = x.lowfrac, x1 = x.highfrac)
plot(diff.frac)
  
  
  
## VARIATIONS ON A THEME 
## Stratified Cox Proportional Hazards Model 
## What's the difference?  Baseline hazards vary, but coefficents are the same, across strata.
ex3.strat <- zelig(Surv(duration, ciep12) ~ invest + fract + polar + numst2 + 
                   strata(crisis), model = "coxph", data = coalition2)
x.lowstrat <- setx(ex3.strat, numst2 = 0, strata = "crisis=1")
x.highstrat <- setx(ex3.strat, numst2 = 1, strata = "crisis=1")
diff.strat <- sim(ex3.strat, x = x.lowstrat, x1 = x.highstrat)
summary(diff.strat)
   
## Cox Proportional Hazards Model with Time-Varying Covariates
## What's the difference?  Let's assume that right now, you are not wrestling a bear and
## you have some predicted survival duration.  If you someday find yourself wrestling a bear,
## your predicted survival time might decrease... slightly.  Zelig allows your model to 
## include variables whose value changes for a given observation over time.  
  
## Your data should be long - that is, you should have multiple observations for 
## each case with time-varying variables.  If you are using quarterly unemployment  
## to predict how long your goldfish survive, you will have one "q_unempl" variable and 
## one observation for every quarter that your golfish is still alive.  
## The intervals are not required to be regular (quarterly) - they can be any length, and 
## the start and stop times of each period must be defined for each observation.
## The function format is as follows:
## z.out <- zelig(Surv(start, stop, censoring) ~ X1 + X2 + ..., model="coxph", data=data)








#### Contributors: Jeff Javed and Noam Gidron
#### April 20, 2011
#### Zero-Inflated Negative Binomial Regression
#### Single-Equation Models: No pg. #

#################################################################################################
# We wish to show how to construct a zero-inflated negative binomial model from scratch. First, #
# this code constructs a function for the log-likelihood of the zero-inflated negative binomial #
# model. Next, it estimates the log-likelihood using a data set on fishing (Available at:       #
# http://www.ats.ucla.edu/stat/R/dae/fish.csv). Finally, it runs the same data through a canned # 
# zero-inflated negative binomial function from the pscl library to check for accuracy.         #
#                                                                                               # 
# Explanation of the Zero-Inflated Negative Binomial                                            #
# The zero-inflated negative binomial model is used for event counts where we are worried that  #
# we have overdispersion due to a large amount of structural zeroes in our dependent variable.  #
# Thus, if y=0, then there is a possibility pi that we will get a structural 0 and a possibility#  
# 1 - pi that the zero was drawn from a negative binomial distribution. Thus, the probability   #
# distribution for P(Y=y|lambda, theta, pi) is:                                                 #   
#  (pi + (1-pi)*negbinomial(0|lambda,theta))^d * (1-pi)*negbinomial(y|lambda, theta)^(1-d)      #
# where d = 1 when y = 0, and d = 0 when y > 0.                                                 # 
#                                                                                               #
# NB: This implementation may be a bit unstable at the gamma(theta) stage.  A production-quality#
# implementation would use the lgamma() function (as done in the pscl package).  Here we use a  #
# simpler coding for pedagogical purposes.                                                      #
#                                                                                               #
#################################################################################################
rm(list=ls(all=TRUE))
## Start a function that takes in parameters, a matrix of explanatory variables (X), 
## a matrix for the zero-inflated variables (Z), and the dependent variable (y). 
ll.zibinom <- function(par, X, Z, y){ 
  #Set beta, gamma, and theta. Beta will contain as many parameters as are in the X matrix, 
  #while gamma will contain as many parameters as are in the Z matrix. Theta is a single parameter.
  beta <- par[1:ncol(X)]
  theta <- par[(ncol(X) + 1)]
  gamma <- par[(ncol(X) + 2):length(par)]

  #Set lambda.
  lambda <- exp(X%*%beta)

  #Set pi.
  pie <- 1/(1+exp(-Z%*%gamma))

  #Create the probability density function for a negative binomial if y > 0.
  negbinom <- (gamma(theta+y)/((factorial(y))*gamma(theta)))*((lambda^(y))*(theta^(theta)))/
              ((lambda + theta)^(theta+y))

  #Create the probability density function for a negative binomial if y = 0.
  negbinom.zero <- (theta^(theta))/((lambda + theta)^(theta))

  #Set d so that d = 1 when y = 0, and d = 0 when y > 0. 
  #This will differentiate between instances where y = 0 and y > 0.
  d <- ifelse(y==0, 1, 0)

  #Write out the log-likelihood for the zero-inflated negative binomial function.
  sum((d)*log(pie + (1-pie)*(negbinom.zero)) + (1-d)*log((1-pie)*negbinom))
}

## Estimating the Model
## We use data about the probability that a group of people in a park caught a fish. 
## The explanatory variables are the number of children in the group, the number of people in the group, 
## and whether or not the group brought a camper to a park. The dependent variable is the number of 
## fish caught. The data and example are from UCLA Academic Technology Services. 
## Available at: http://www.ats.ucla.edu/stat/stata/dae/zinb.htm.

## Load and view data.
load("Gov2001CodeLibrary.RData")
data <- fish
head(data)

## The fish count is our dependent variable y.
y <- data$count

## The child and camper variables are the independent variables (X). We add a column of 1s 
## so that we can calculate an intercept.
X <- as.matrix(cbind(1, data$child, data$camper))

## The zero-inflated variable is number of people in a group. Again, we add a column of 1s 
## so that we can calculate an intercept.

Z <- as.matrix(cbind(1, data$persons))

## Optimize the function. Number of parameters will equal the number of columns in X and Z plus 1.
out <- optim(par=rep(0.01, ncol(X) + ncol(Z) + 1), ll.zibinom, Z=Z, X=X,y=y, method="BFGS", 
             control=list(fnscale=-1), hessian=TRUE)

## Extract parameters.
out$par

## Extract log-likelihood.
out$value

## Compare to the Canned Function
## Load the pscl library.
library(pscl)

## Run the zero-inflated binomial model. Place the Z matrix variables after the |.
zinb <- zeroinfl(count ~ child + camper | persons, dist = "negbin", EM=TRUE, data=data)

## Check and compare results. Note that zeroinfl provides the log(theta), while our function 
## provides an unlogged theta.
summary(zinb)
out$par
out$value



