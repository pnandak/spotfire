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

#setwd("~/Documents/FALL 2011/510") - set working directory 

############FITTING AND INTERPRETING LOGIT MODEL###################

# Load data
file <- "nes00a.csv" # on Class website
data <- read.csv(file, header=TRUE)
summary(data) # shows descriptive statistics of the data
head(data) # shows first five rows of the data

# Estimate logit model using optim()
# Construct variables and model objects
y <- data$vote00 # this is our response variable
x <- cbind(data$age,data$hsdeg,data$coldeg) # column-bind covariates

# Likelihood function for logit
llk.logit <- function(param,y,x) {
  os <- rep(1,length(x[,1])) # constant 
  x <- cbind(os,x) # constant+covariates
  b <- param[ 1 : ncol(x) ] # number of parameters to be estimated equals number of columns in x (i.e, one for constant and one for each covariates )
  xb <- x%*%b  
  sum( y*log(1+exp(-xb)) + (1-y)*log(1+exp(xb))) # log-likelihood function for logit model (based on our choice of standard logistic cdf as the systematic component)
               # optim is a minimizer, so use -lnL here
}

# Fit logit model using optim
ls.result <- lm(y~x)  # Run lm just to use its parameter estimates as starting values
stval <- ls.result$coefficients  # so these are our initial guesses for parameters
logit.result.opt <- optim(stval,llk.logit,method="BFGS",hessian=T,y=y,x=x)
                   # call minimizer procedure
pe.opt <- logit.result.opt$par   # point estimates
vc.opt <- solve(logit.result.opt$hessian)  # var-cov matrix
se.opt <- sqrt(diag(vc.opt))    # standard errors
ll.opt <- -logit.result.opt$value  # likelihood at maximum

# Estimate logit model using glm()
# Set up model formula and model specific data frame
model <- vote00 ~ age + hsdeg + coldeg
mdata <- extractdata(model, data, na.rm=TRUE)
dim(data)
dim(mdata)

# Run logit & extract results
logit.result <- glm(model, family=binomial, data=mdata) # family bonomial calls logit in glm
summary(logit.result) ## almost same results

# now lets add age^2 to capture non-linear effect of age on voting 
model <- vote00 ~ age + I(age^2)+hsdeg + coldeg
mdata <- extractdata(model, data, na.rm=TRUE)
dim(data)
dim(mdata)

# Run logit & extract results
logit.result <- glm(model, family=binomial, data=mdata) # family bonomial calls logit in glm
summary(logit.result)

pe <- logit.result$coefficients  # extract point estimates
vc <- vcov(logit.result)         # extract var-cov matrix

# Simulate parameter distributions
sims <- 10000
simbetas <- mvrnorm(sims, pe, vc) # draw 10000 sets of parameter values from a multivariate normal distribution with mean pe and variance-covariance vc

# Now let's plan counterfactuals: We will have three sets of counterfactuals based on education lavel (less than hs edu, hs edu, college or higher edu), and for each set we will make age varies between 18 years old and 97 years old. 

# Now let's set up senarios as we planned. 
xhyp <- seq(18,97,1) # creat age vector 
nscen<-length(xhyp) # we will have total 80 differnt age scenarios for each education level

nohsScen <- hsScen <- collScen <- cfMake(model, mdata, nscen) #this is just to initialze 80 scenarios for each education level. As default, all covaraite values are set at the mean.

# Creat three sets of education counterfactuals

for (i in 1:nscen) {
  # no High school scenarios (loop over each age, total 80 scenarios)
  nohsScen <- cfChange(nohsScen, "age", x = xhyp[i], scen = i)
  nohsScen <- cfChange(nohsScen, "hsdeg", x = 0, scen = i) # no hs degree
  nohsScen <- cfChange(nohsScen, "coldeg", x = 0, scen = i) # no college degree

  # HS grad scenarios (loop over each age,total 80 scenarios)
  hsScen <- cfChange(hsScen, "age", x = xhyp[i], scen = i)
  hsScen <- cfChange(hsScen, "hsdeg", x = 1, scen = i)
  hsScen <- cfChange(hsScen, "coldeg", x = 0, scen = i)

  # College grad scenarios (loop over each age, total 80 scenarios)
  collScen <- cfChange(collScen, "age", x = xhyp[i], scen = i)
  collScen <- cfChange(collScen, "hsdeg", x = 1, scen = i)
  collScen <- cfChange(collScen, "coldeg", x = 1, scen = i)
}

# Now given the counterfactual covariates (nohsScen/hsScen/collScen) and simulated parameters (simbetas), we can calculate expected value of the response. In this case, expected probability of voting!
  
nohsSims <- logitsimev(nohsScen, simbetas, ci=0.95) 
hsSims <- logitsimev(hsScen, simbetas, ci=0.95)
collSims <- logitsimev(collScen, simbetas, ci=0.95)

names(nohsSims) # gives you lower and upper confidence intervals as well asexpected probability
  
# Get 3 nice colors for traces
col <- brewer.pal(3,"Dark2")

# Set up lineplot traces of expected probabilities

# no hs 
nohsTrace <- lineplot(x=xhyp, # age on x-axis
                      y=nohsSims$pe, #expected probability on y-axis
                      lower=nohsSims$lower, # lower confidence interval
                      upper=nohsSims$upper, #upper confidence interval
                      col=col[1], #color choice 
                      extrapolate=list(data=mdata[,2:ncol(mdata)],  #acutal covariates
                                       cfact=nohsScen$x[,2:ncol(hsScen$x)], #counterfactual covariates
                                       omit.extrapolated=TRUE), #don't show extrapolated values 
                      plot=1)
# hs
hsTrace <- lineplot(x=xhyp,
                    y=hsSims$pe,
                    lower=hsSims$lower,
                    upper=hsSims$upper,
                    col=col[2],
                    extrapolate=list(data=mdata[,2:ncol(mdata)],
                                       cfact=hsScen$x[,2:ncol(hsScen$x)],
                                       omit.extrapolated=TRUE),
                    plot=1)

#college                                        
collTrace <- lineplot(x=xhyp,
                      y=collSims$pe,
                      lower=collSims$lower,
                      upper=collSims$upper,
                      col=col[3],
                      extrapolate=list(data=mdata[,2:ncol(mdata)],
                                       cfact=collScen$x[,2:ncol(hsScen$x)],
                                       omit.extrapolated=FALSE), #if extrapolation is allowed, we will see extrapolated portion of the plot in a ligher color
                      plot=1)

# Set up traces with labels and legend
labelTrace <- textTile(labels=c("Less than HS", "High School", "College"),
                       x=c( 55,    49,     30),
                       y=c( 0.26,  0.56,   0.87),
                       col=col,
                       plot=1)

legendTrace <- textTile(labels=c("Logit estimates:", "95% confidence", "interval is shaded"),
                        x=c(82, 82, 82),
                        y=c(0.2, 0.16, 0.12),
                        plot=1)

# Plot traces using tile
tc<-tile(nohsTrace,
     hsTrace,
     collTrace,
     labelTrace,
     legendTrace,
     width=list(null=5),
     limits=c(18,94,0,1),
     xaxis=list(at=c(20,30,40,50,60,70,80,90)),
     xaxistitle=list(labels="Age of Respondent"),
     yaxistitle=list(labels="Probability of Voting"),
     frame=TRUE,
     output=list(file="education and voting")
     )
 
############# COMPARING AND SELCTING LOGIT MODELS#####################
     
     
library(nlme)
library(verification)
source("avp.r")  # Average vs Predicted plotting code. Download from Chris Adolph's website and save it in your working directory

attach(data)

# Construct variables and model objects

#response variable
     
y <- vote00

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
lr.test <- 2*(ll.2 - ll.1)
lr.test.p <- pchisq(lr.test,df=1,lower.tail=FALSE)

# BIC
bic.test <- - 2*(ll.2 - ll.1) + 1*log(nrow(x))

# AIC
aic.test <- 2*(ll.2 - ll.1) - 1*2

# Deviance
deviance <- -2*(ll.2)


# Act v Pred plot
avp(y,
    x=cbind(rep(1,nrow(x)),x),
    beta=pe.2,
    fnform="logit",
    cutpoints=seq(0,1,0.1),
    usr=c(0,1,0,1),
    sizefactor=.25,
    color = "blue",
    output = list(outfile="nes_logit_AvP.pdf",high=6,wide=5.5,epsi=FALSE),
    lab = list(x = .7, y=0.175, str="Model with married", col="blue", cex=1),
    ylab = "Actual Voting Rate, by bins of predicted",
    xlab = "Predicted Voting Rate, binned",
    closeplot=F)

x <- cbind(age,hsdeg,coldeg)
avp(y,
    x=cbind(rep(1,nrow(x)),x),
    beta=pe.1,
    fnform="logit",
    cutpoints=seq(0,1,0.1),
    usr=c(0,1,0,1),
    sizefactor=.25,
    color = "red",
    output = list(outfile="nes_logit_AvP.pdf",high=6,wide=5.5,epsi=FALSE),
    lab = list(x = .7, y=0.1, str="Model w/o married", col="red", cex=1),
    addtoplot=T,
    closeplot=T)

# ROC
yhat.1 <- 1/(1+exp(-cbind(rep(1,nrow(x)),x)%*%pe.1))
postscript("roc_m1.eps",paper="letter",pointsize = 14,width=6,height=5.5,horizontal = FALSE, onefile = TRUE)
roc.plot(y,cbind(yhat.1))
dev.off()

yhat.2 <- 1/(1+exp(-cbind(rep(1,nrow(x)),x,marriedo)%*%pe.2))
postscript("roc_m1m2.eps",paper="letter",pointsize = 14,width=6,height=5.5,horizontal = FALSE, onefile = TRUE)
roc.plot(y,cbind(yhat.1,yhat.2))
dev.off()


###  Residuals
glm.result <- glm(y~age + hsdeg + coldeg + marriedo,family="binomial")
summary.glm(glm.result)
hatscore <- hatvalues(glm.result)/mean(hatvalues(glm.result))
rstu <- rstudent(glm.result)

#x11()
#plot(hatscore,rstu)

usr <- c(0,10,-3,3)

postscript("nes_resid1.eps",paper="letter",pointsize = 14,width=5.5,height=5,horizontal = FALSE, onefile = TRUE);
plot.new()
par(usr=usr,tcl=-0.1,mgp=c(2,0.35,0))
axis(2,labels = T, tick = T, line = 0, outer = F, font = NA, fg = "black",las=1);
par(usr=usr,tcl=-0.1,mgp=c(2,0.15,0))
axis(1,at=c(0,1,2,3,4,5,6,7,8,9,10),labels = T, tick = T, line = 0, outer = F, font = NA, fg = "black");

title(xlab="Standardized hat-values",ylab="Studentized residuals")
points(hatscore,rstu,col = "red")
lines(c(usr[1],usr[2]),c(-2,-2),lty="dashed")
lines(c(usr[1],usr[2]),c(2,2),lty="dashed")
lines(c(3,3),c(usr[3],usr[4]),lty="dashed")

dev.off()


