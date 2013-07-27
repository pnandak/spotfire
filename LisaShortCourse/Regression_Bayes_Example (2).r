### SLR Example in bayesm package ###
#####################################

### Packages we'll need ###
library(bayesm)
library(coda)

### Look at the data ###
head(cars)
plot(cars$speed,cars$dist,ylab="Stopping Distance",xlab="Speed")

### We're going to use the runiregGibbs function to perform this analysis.
### It estimates the betas and the error variance in MLR assuming iid normal errors.
### It requires 3 arguments: Data, Prior, and Mcmc. All of which must be lists.

### The Data portion ###
y1 = as.vector(cars$dist)

# The X must be a design matrix. That means we must manually create a column of
# ones if we want an intercept.
X1 = cbind(matrix(1,50,1),cars$speed)

Data1 = list(y=y1,X=X1)

# This next portion is where we specify our prior information.
# runiregGibbs uses a normal prior on beta and an inverse chi-squared prior on sigma2

# I have manually set the prior information to the function default:
betabar1 = c(0,0) # prior mean
A1 = .01*diag(2) # prior precision matrix (precision = 1/variance)

nu1 = 3 # degrees of freedom
ssq1 = var(y1) # scale parameter

Prior1 = list(betabar=betabar1,A=A1,nu=nu1,ssq=ssq1)

# This portion specifies MCMC parameters
R1 = 5000 # This is how many iterations to run the Gibbs sampler 
keep1 = 1 # This is the thinning parameter. 1 means no thinning.

MCMC1 = list(R=R1,keep=keep1)

### Run the Gibbs sampler! ###
simOut = runiregGibbs(Data1,Prior1,MCMC1)

# Plot diagnostics
plot(simOut$betadraw)
plot(simOut$sigmasqdraw)

# Produces summaries of our estimates
summary(simOut$betadraw)
summary(simOut$sigmasqdraw)

# bayesm produces equal tail credible intervals.
# Below we compute HPD intervals. Needs library(coda)
HPDinterval(simOut$betadraw)
HPDinterval(simOut$sigmasqdraw)

### Let's compare to the classical estimates ###
fit = lm(dist ~ speed, data=cars)

# Check assumptions of our classical fit
par(mfrow=c(2,2))
plot(fit)

# Summarize our fit
summary(fit)

### Plot fits of both paradigms ###
win.graph()
plot(cars$speed,cars$dist,ylab="Stopping Distance",xlab="Speed")
abline(-11.8,3.6,col="Blue")
abline(fit,col="Red")
legend("topleft",c("Bayesian fit","Classical fit"),fill=c("Blue","Red"))

### Probit Regression ###

mydata <- read.csv(url("http://www.ats.ucla.edu/stat/r/dae/logit.csv"))

head(mydata)

y2 = mydata$admit
X2 = cbind(matrix(1,400,1), mydata$gre, mydata$gpa)

Data2 = list(y=y2,X=X2)

betabar2 = c(0,0,0)
A2 = .01*diag(3)

Prior2 = list(betabar=betabar2,A=A2)

R2 = 10000
keep2 = 1

Mcmc2 = list(R=R2,keep=keep2)

simOut2 = rbprobitGibbs(Data2,Prior2,Mcmc2)
plot(simOut2$betadraw)
summary(simOut2$betadraw)

fit2 = glm(admit ~ gre + gpa, data=mydata, family=binomial(probit))
summary(fit2)

par(mfrow=c(1,2))
plot(mydata$admit,pnorm(X2%*%matrix(c(-3.0019,.0017,.4499),3,1),0,1 ) ,xlab="y",ylab="yhat Bayesian")
plot(mydata$admit,fitted(fit2) ,xlab="y",ylab="yhat Classical")
