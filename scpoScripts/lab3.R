### Goodness of Fit

#  Likelihood ratio test
#  Akaike Information Criterion
#  Bayesian Information Criterion
#  Average vs Predicted Plots
#  ROC plots
#  Cross Validation
#  Residual vs Leverage Plots


# Clear memory
rm(list=ls())


# Load libraries
library(MASS)
library(simcf) # download from Chris Adolph's website (software) -for simulation
library(tile) # download from Chris Adolph's website (software) - for visualization
library(RColorBrewer) # install from CRAN (for colors) 
library(WhatIf) #install from CRAN 
library(nlme)
library(verification)
library(ROCR)
library(Hmisc)
source("avp.r")  # Average vs Predicted plotting code. Download from Chris Adolph's website and save it in your working directory

file <- "nes00a.csv" # on Class website
data <- read.csv(file, header=TRUE)
summary(data) # shows descriptive statistics of the data
head(data) # shows first five rows of the data

attach(data)# if you "attach" data, you don't have to specify data name every time you call a variable from the data (ex. data$vote00). However, if you are working on several different datasets at the same time, attatching data might cause confusion. You can use detach(data) when you are done with the dataset


llk.logit <- function(param,y,x) {
  os <- rep(1,length(x[,1])) # constant 
  x <- cbind(os,x) # constant+covariates
  b <- param[ 1 : ncol(x) ] # number of parameters to be estimated equals number of columns in x (i.e, one for constant and one for each covariates : total 4)
  xb <- x%*%b  
  sum( y*log(1+exp(-xb)) + (1-y)*log(1+exp(xb))) # log-likelihood function for logit model (based on our choice of standard logistic cdf as the systematic component)
               # optim is a minimizer, so use -lnL here
}

# Construct variables and model objects

#response variable
     
y <- vote00   # you don't have to do data$vote00 since you attached the data

#Model 1:  Age, HS, College
#Model 2:  Age, HS, College, Married
     
#fit model 1     
x <- cbind(age,hsdeg,coldeg)
ls.result <- lm(y~x)  # use ls estimates as starting values
stval <- ls.result$coefficients  # initial guesses
logit.result <- optim(stval,llk.logit,method="BFGS",hessian=T,y=y,x=x)
                   # call minimizer procedure
pe.1 <- logit.result$par   # point estimates
vc.1 <- solve(logit.result$hessian)  # var-cov matrix
se.1 <- sqrt(diag(vc.1))    # standard errors
ll.1 <- -logit.result$value  # likelihood at maximum

# Fit model 2 (added covariate:  married)
x <- cbind(age,hsdeg,coldeg,marriedo)
ls.result <- lm(y~x)  # use ls estimates as starting values
stval <- ls.result$coefficients  # initial guesses
logit.result <- optim(stval,llk.logit,method="BFGS",hessian=T,y=y,x=x)
                   # call minimizer procedure
pe.2 <- logit.result$par   # point estimates
vc.2 <- solve(logit.result$hessian)  # var-cov matrix
se.2 <- sqrt(diag(vc.2))    # standard errors
ll.2 <- -logit.result$value  # likelihood at maximum

# GOF of added variable

# LR test
ll.1 #simpler
ll.2 # added covariate
lr.test <- 2*(ll.2 - ll.1) ## LR (or the difference between two deviances) follows chi-squared distribution with k-degrees of freedom 
lr.test.p <- pchisq(lr.test,df=1,lower.tail=FALSE) #so we can conduct chisq test to see if the difference is statistically meaningful
lr.test
lr.test.p # yes it is

# BIC
bic.test <- - 2*(ll.2 - ll.1) + 1*log(nrow(x)) #penalty on observations and parameters
bic.test

# AIC
aic.test <- -2*(ll.2 - ll.1) + 1*2 #penalty only on extra parameters (=1) typo in the original code
aic.test

# Deviance
deviance <- -2*(ll.2)


#### Act v Pred plot
avp

#first plot the avp of the model with married
avp(y,
    x=cbind(rep(1,nrow(x)),x),
    beta=pe.2,
    fnform="logit",
    cutpoints=seq(0,1,0.1),
    usr=c(0,1,0,1),
    sizefactor=.25,
    color = "blue",
    output = list(outfile="nes_logit_AvP.pdf", type="pdf",high=6,wide=5.5,epsi=FALSE),
    lab = list(x = .7, y=0.175, str="Model with married", col="blue", cex=1),
    ylab = "Actual Voting Rate, by bins of predicted",
    xlab = "Predicted Voting Rate, binned",
    closeplot=F)  ## don't close the plot here because we will add addtional points

# now plot the avp of the model without married (simpler model)
x <- cbind(age,hsdeg,coldeg)
avp(y,
    x=cbind(rep(1,nrow(x)),x),
    beta=pe.1,
    fnform="logit",
    cutpoints=seq(0,1,0.1),
    usr=c(0,1,0,1),
    sizefactor=.25,
    color = "red",
    output = list(outfile="nes_logit_AvP.pdf", type="pdf", high=6,wide=5.5,epsi=FALSE),
    lab = list(x = .7, y=0.1, str="Model w/o married", col="red", cex=1),
    addtoplot=T,
    closeplot=T)


#avp function also calcultes and prints pcp: pcp = sum( ya*(yp>.5) + (1-ya)*(yp<.5) ) /length(yp)  
#compare the pcp : 0.6988 vs. 0.6994 very similar, simpler model is a little better. 


#### Receiver Operating Characteristics (ROC) Plot
yhat.1 <- 1/(1+exp(-cbind(rep(1,nrow(x)),x)%*%pe.1)) #the formula for systematic component pi : this gives predicted value
roc.plot(y,cbind(yhat.1), legend=TRUE) # display AUC by setting legend=TRUE

yhat.2 <- 1/(1+exp(-cbind(rep(1,nrow(x)),x,marriedo)%*%pe.2))
roc.plot(y,cbind(yhat.1,yhat.2), legend=TRUE)

# compare the AUC: 0.708 vs. 0.716 very similar again, this time model with married variable performs a little better.

# there is another package doing ROC plot called ROCR
                                       
library(ROCR)

model1 <- glm(y~age + hsdeg + coldeg + marriedo,family="binomial") #run glm

pred.object<-prediction(model1$fitted.values,model1$y) #gives you information about prediction based on model 1

perf.object<-performance(pred.object,"tpr","fpr")  # Evaluate the performance of the model using the pred.object and calcultes true positive rate (P(Yhat = + | Y = +))and false positive rate ( P(Yhat = + | Y = -) )

plot(perf.object, 
   box.lty=7, box.lwd=2,
  box.col="black", lwd=2, colorkey.relwidth=0.5, xaxis.cex.axis=1,
   yaxis.cex.axis=1, cex.lab=1.5, cex.main=2, print.cutoffs.at=c(0.2,0.4,0.6,0.8))
abline(0,1)
# now let's calculate the area under the curve (AUC).  
library(Hmisc)
somers2(model1$fitted.values,model1$y)["C"] # the first element of the output "C" is the area under the curve. In this case, we get 0.716
legend(0.4,0.2, "AUC:0.716" , lwd=2, col=1) # let's put AUC on the plot


#### Cross-validation:  let's see how well one set of estimated coefficients from a subset explain another subset of the data

data$obs<-seq(1,1783,1)
head(data)
data_1<-subset(data, obs<900) # our traning set
data_2<-subset(data, obs>=900) # our test set

model2 <- glm(vote00~age + hsdeg + coldeg + marriedo,family="binomial",data=data_1) 

# At this stage, we have two options for obtaining the phats:
  
	# Using predict.glm()
	yhats<-predict.glm(model2, newdata=data_2, type="response")
	
	
	# or analytically calculate them

    # get the betas for that model:
		betas<-model2$coefficients
		# construct a matrix of covariate values obs >=900
		X<-as.matrix(cbind(1, subset(data_2, select=names(betas)[-1])))
		
		# Now calculate the yhats using the logit formula:
		yhats<-1/(1+exp(-X %*% betas))
		
	
# Now get the data on actual voting 
actuals<-data_2$vote00 

# Now let's make ROC plot 

pred.object<-prediction(yhats, actuals)
perf.object<-performance(pred.object,"tpr","fpr")
plot(perf.object, lwd=2, col=2, add=T) # add to the existing ROC plot
somers2(yhats,actuals)["C"] # the first element of the output "C" is the area under the curve. In this case, we get 0.699
legend(0.4,0.1, "AUC:0.699" , lwd=2, col=1)  # so a little worse


### leave one out cross validation

glm.result <- glm(vote00~age + hsdeg + coldeg + marriedo,family="binomial",data=data)
leave.one.out<- cv.glm(data,glm.result, K = nrow(data))
leave.one.out$delta #estimated prediction error is 0.198 when cost function is not specified
cost <- function(y, pi) mean(abs(y-pi) > 0.5) 
leave.one.out<- cv.glm(data,glm.result, cost, K = nrow(data)) # prediction erorr is 0.302 using the cost function

## now let's compare it with our original model 

sum((glm.result$y-glm.result$fitted.values)^2)/nrow(data) # predction error for the original model was 0.196
mean(abs(glm.result$y-glm.result$fitted.values)>0.5) # prediction error using the equivalent cost function was 0.301

### Residuals, Hat Values, Influential Statistics

hatscore <- hatvalues(glm.result)/mean(hatvalues(glm.result)) #how far away a value of the independent variable value is from the mean value, the farther away the observation the more leverage it has
rstu <- rstudent(glm.result)  #observed residual divided by the standard deviation

plot.new()
usr <- c(0,10,-3,3)
par(usr=usr,tcl=-0.1,mgp=c(2,0.35,0))
axis(2,labels = T, tick = T, line = 0, outer = F, font = NA, fg = "black",las=1);
par(usr=usr,tcl=-0.1,mgp=c(2,0.15,0))
axis(1,at=c(0,1,2,3,4,5,6,7,8,9,10),labels = T, tick = T, line = 0, outer = F, font = NA, fg = "black");

title(xlab="Standardized hat-values",ylab="Studentized residuals")
points(hatscore,rstu,col = "red")
lines(c(usr[1],usr[2]),c(-2,-2),lty="dashed")
lines(c(usr[1],usr[2]),c(2,2),lty="dashed")
lines(c(3,3),c(usr[3],usr[4]),lty="dashed")

inf<-influence(glm.result)  # gives list of informative statistics
names(inf)
inf$hat[1:10] #hatvalues
head(inf$coefficients)#change in the estimated coefficients which results when the i-th case is dropped from the regression