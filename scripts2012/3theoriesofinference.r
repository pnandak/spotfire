#### "Theories of Inference"
#### Harvard Gov2001 Code Library
#### Instructor: Gary King
#### Maintainers: Margaret Roberts and Brandon Stewart
#### May 1, 2011
#### Version 0.5
#### Contact: bstewart@fas.harvard.edu

# Summary: This file covers basic properties of MLE
#
# Table of Contents:
#    Asymptotic Properties of the MLE
#    Conducting a Likelihood Ratio Test
#    Quadratic Approximation to the Likelihood
#    Dependence in Observations








#### Contributors: Ana Catalano, Yanfang Su, Erru Yang
#### April 19 2011
#### Use simulation to show the asymptotic properties of the MLE
#### Inference: pg 24

#########################################################################################
#We will show the asymptotic properties of the MLE in a standard normal distribution.   #
#########################################################################################
rm(list=ls(all=TRUE))
## I will show the asymptotic prepertied of the MLE in a standard normal distribution.
mu <- 0
sigma <- 1
## n is the time of simulations, and T is the sample size in the distribution.##
## I do simulations of 100, 1000 and 10000 times##
n0<-10
n1<-100
n2<-1000
n3<-10000
T<-1000
set.seed(999)
sample0<-rnorm(n0*T,mu,sigma)
sample0<-matrix(sample0,nrow=T,ncol<-n0)
sample1<-rnorm(n1*T,mu,sigma)
sample1<-matrix(sample1,nrow=T,ncol<-n1)
sample2<-rnorm(n2*T,mu,sigma)
sample2<-matrix(sample2,nrow=T,ncol<-n2)
sample3<-rnorm(n3*T,mu,sigma)
sample3<-matrix(sample3,nrow=T,ncol<-n3)

## the MLE here are:#
xbar0<-apply(sample0,1,mean)
xbar1<-apply(sample1,1,mean)
xbar2<-apply(sample2,1,mean)
xbar3<-apply(sample3,1,mean)

## Use QQ plot to test normality##
qqnorm(xbar0)
qqnorm(xbar1)
qqnorm(xbar2)
qqnorm(xbar3)

## To show asymptotic normality
hist(xbar0)
hist(xbar1)
hist(xbar2)
hist(xbar3)

## Calculated the bias of the MLE
bias0=xbar0-mu
bias1=xbar1-mu
bias2=xbar2-mu
bias3=xbar3-mu

mean(bias0)
mean(bias1)
mean(bias2)
mean(bias3)

## The results show that the bigger the n the closer the mean biases are to 0. 
## This shows that MLE is asymptotically unbiased.
## It also shows that the MLE will converge in probability to the true value, so it's consistent.

## Now, I will show the variance of the MLE
## The bigger the n is, the closer of the variance of the MLE to the true variance. 
## This shows the efficiency of MLE.
sd(xbar0)
sd(xbar1)
sd(xbar2)
sd(xbar3)

sigma/sqrt(n0)
sigma/sqrt(n1)
sigma/sqrt(n2)
sigma/sqrt(n3)

sigma/sqrt(n0)-sd(xbar0)
sigma/sqrt(n1)-sd(xbar1)
sigma/sqrt(n2)-sd(xbar2)
sigma/sqrt(n3)-sd(xbar3)






#### Contributors: Chilton, Crouch, Lavie    
#### April 20, 2011
#### Conducting a Likelihood Ratio Test
#### Theories of Inference: pg. 26

#########################################################################################
# Attribution: This code and material draws on the Gov 2001                             #
# section 4 notes from Feb 16, 2001 and from answers developed                          #
# for questions 1 and 2 for Gov 2001 Pset 4.                                            #
#                                                                                       #
# Background: The Likelihood Ratio Test is for comparing two                            #
# models. The question is whether additional parameters in one                          #
# model are sufficiently useful to justify their presence in the model.                 #               
#########################################################################################
rm(list=ls(all=TRUE))
## Loading & Viewing Data
load("Gov2001CodeLibrary.RData")
data <- presidential
head(data)

## Likelihood Function  
## Note: Conducting a likelihood function. This example uses a 
## normal model, but any other models function could be
## substituted.

normal.test <- function(par, X, Y) {
	beta <- par[1:ncol(X)]
	sigma2 <- exp(par[ncol(X) +1])
	-1/2 * sum(log(sigma2) + ((Y-X %*% beta)^2)/sigma2)
	}
	
## Creating Variables

Y <- data[,1] 
X <- cbind(rep(1,nrow(data)), data[,2:20])  #Vector of variables for the unrestricted model
X2 <- cbind(rep(1,nrow(data)), data[,2:14]) #Vector of variables for the restrectied model

## Optimizing the Functions
## Note: Using optim, the next step is to find the MLE of both the 
## restricted and unrestricted versions of the model. 

unrestricted <- optim(par=rep(0, ncol(X) +1), normal.test, X=X, Y=Y, 
                control=list(fnscale=-1), method="BFGS", hessian=TRUE)
	
restricted <- optim(par=rep(0, ncol(X2) +1), normal.test, X=X2, Y=Y, 
                control=list(fnscale=-1), method="BFGS", hessian=TRUE)

## Creating a Test Statistic & Testing
## Note: This step requires finding the MLE values. 
## Then, under the null hypothesis that the restrictions are valid
## the test statistic wold be districbuted as X^2 witth one degree
## of freedom.  

unrestricted$value
restricted$value

r <- 2*(unrestricted$value - restricted$value)

1-pchisq(r,df=6)
## This shows that the probability of getting this test statistic 
## under the null is extremely small (essentially zero). As a 
## result we reject the null hypothesis, and know that it is okay 
## to use the unrestricted model. 






#### Contributors: Frey and Walker   
#### April 18, 2011
#### Quadratic Approximation to the Likelihood
#### Theories of Inference: pg. 31

##########################################################################################
# We demonstrate that the quadratic approximation is asymptotically correct for a        #
# logit log-likelihood although it is generally true                                     #
##########################################################################################
rm(list=ls(all=TRUE))

# Create Log Likelihood Function 
# (Use the Bernoulli to have an easy example which is not the normal)
bern.ll <- function(pie,data=data) {
   return(sum(dbinom(data,1,pie, log=TRUE)))
}

## We take a number of observations and a true parameter for pi
quad.approx <- function(n, true) {
   set.seed(02138)
   data <- rbinom(n, 1, true)
   est <- optim(data=data, par = .5, fn = bern.ll, 
                control = list(fnscale = -1), 
                method = "L-BFGS-B", lower=.Machine$double.eps, 
				upper=(1-.Machine$double.eps))$par #bound by machine epsilon
   sig2 <- est*(1-est)
   plot.fn <- function(x) bern.ll(x,data)
   plot.fn <- Vectorize(plot.fn)
   title <- paste("Normal Approx. n=", n, sep="")
   curve(plot.fn(x), from=0, to=1, main=title,
         xlab="Parameter", ylab="Log-Likelihood")
   adjust <- bern.ll(est, data) - dnorm(est, mean=est, sd=sqrt(sig2), log=T)*n
   curve((dnorm(x, mean=est, sd=sqrt(sig2), log=T)*n+adjust),
          add=T,col="red", lwd=2)  
}
par(mfrow=c(2,2))
quad.approx(3, true=.5)
quad.approx(10, true=.5)
quad.approx(100, true=.5)
quad.approx(10000, true=.5)
savePlot(filename="Figs/NormalApprox.pdf", type="pdf")
dev.off() #reset graphical parameters





#### Contributors: Julie Faller and Noah Nathan  
#### April 19, 2011
#### Dependence in Observations
#### Theories of Inference: pg. 35

############################################################################################
# In this section, we generate data to show that when observations are dependent, their    #
# MLE will not be unbiased.  We use OLS since it is a simple and familiar MLE for a linear # 
# model.                                                                                   #
############################################################################################
rm(list=ls(all=TRUE))
## Unbiased Example
## First, we sample some data from a uniform distribution to be our x's in an imaginary dataset
nums<-seq(-100,100,.5)
x<-sample(nums, 1000, replace=TRUE)
x<-as.data.frame(x)

##Create a dataframe to take our y values
y<-matrix(data=NA, nrow=1000, ncol=1)
y<-as.data.frame(y)

## Create unbiased data of imaginary Y's that are linear transformation of the X's 
## plus some random noise (thus linear regression is the appropriate model to explore 
## the effect of X on Y)

set.seed(02138)
for (i in 1:1000){
	y[i,1]<-.2*x[i,1] + 0.9*rnorm(1, mean = 0, sd = 1)
}
data<-as.data.frame(cbind(y, x))

## We then run an OLS. As expected, our MLE is close to the true value of the effect 
## of X on Y of 0.2. 
regression<-lm(data$V1~data$x)
summary(regression)

## Here is the plot
plot(data$x, data$V1)
abline(regression)


## Biased example
## Now we create a new dataset with dependence between the observations.

ydep<-matrix(data=NA, nrow=1000, ncol=1)
ydep<-as.data.frame(ydep)
set.seed(02138)
ydep[1,1]<-.2*x[1,1] + rnorm(1, mean = 0, sd = 1)

## Create a for loop to have the rest of the y's show dependence on other y values.
set.seed(02138)
for (i in 2:1000){
	ydep[i,1]<-.2*x[i,1] + rnorm(1, mean = 0, sd = 1)
		#The above row represents the true relationship
	ydep[i,1]<-ydep[i,1]+ .9*sample(ydep[i,1],1, replace=TRUE)
		#This row represents dependence among the observations. 
		#To see the effect of the bias, try playing with the size of the coefficient on sample. 
		#This means that each y observation is a linear transformation of X PLUS some transformation 
		#of another y observation in the same data set.
	}
datadep<-as.data.frame(cbind(ydep, x))

## We run an OLS and see that the estimated beta of x is biased.  
regressiondep<-lm(datadep$V1~datadep$x)
summary(regressiondep)
## NB: As the coefficient on sample (the term producing the dependence between the y's) increases, 
## the estimate of x's beta deviates further from its true value.

## Here's its plot
plot(datadep$x,datadep$V1, ylim=c(-100,100), xlim=c(-50,50))
abline(regressiondep, col="skyblue", lwd=3)



