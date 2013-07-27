######## CSSS536 Problem Set 2
#### Due on February 4, 2005

## Problem 1

# reading in the dataset cyyoun.csv

cyyoung <- read.csv("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/homework 2/cyyoung.csv")
attach(cyyoung)

## part (a): fit a logistic regression model to the variable cy using era and winpct as the only covariates

y <- cy
x1 <- cbind(era,winpct)

# Fit logit model using optim

llk.logit <- function(param,y,x) {
  os <- rep(1, nrow(x))
  x <- cbind(os,x)
  b <- param[ 1 : ncol(x) ]
  xb <- x%*%b
  sum( y*log(1+exp(-xb)) + (1-y)*log(1+exp(xb)));
}

ls.result <- lm(y~x1)  # use ls estimates as starting values
stval <- ls.result$coefficients  # initial guesses

logit.result <- optim(stval,llk.logit,method="BFGS",hessian=T,y=y,x=x1)
 
pe.1 <- logit.result$par   # point estimates
vc <- solve(logit.result$hessian)  # var-cov matrix
se <- sqrt(diag(vc))    # standard errors
cbind(pe.1, se)
#                   pe        se
#(Intercept)  1.342274 3.2899222
#xera        -2.112188 0.5129597
#xwinpct      6.179792 3.9118619

ll.1 <- -logit.result$value  # likelihood at maximum
#[1] -46.22437



# fit logit model using glm

logit.result2 <- glm(cy ~ era + winpct, family = binomial)

summary(logit.result2)
#glm(formula = cy ~ era + winpct, family = binomial)

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-1.9753  -0.7925  -0.3689   0.8480   2.0726  

#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.3417     3.2891   0.408    0.683    
#era          -2.1098     0.5126  -4.116 3.86e-05 ***
#winpct        6.1707     3.9103   1.578    0.115    

#    Null deviance: 124.742  on 91  degrees of freedom
#Residual deviance:  92.449  on 89  degrees of freedom
#AIC: 98.449



## part (b): calculate the probability a pitcher receives the CyYoung award given era = {1.5, 1.75 . . . 5} with winpct (1) at it's mean, (2) at .5, (3) at .9

erahat <- seq(1.5, 5, .25)
mu1 <- rep(0, length(erahat))
mu2 <- rep(0, length(erahat))
mu3 <- rep(0, length(erahat))

for (i in 1:length(erahat)){
mu1[i] <- pe.1%*%rbind(1, erahat[i], mean(winpct))
y1 <- 1/(1+exp(-mu1)) 
}

for (i in 1:length(erahat)){
mu2[i] <- pe.1%*%rbind(1, erahat[i], .5)
y2 <- 1/(1+exp(-mu2)) 
}

for (i in 1:length(erahat)){
mu3[i] <- pe.1%*%rbind(1, erahat[i], .9)
y3 <- 1/(1+exp(-mu3)) 
}

plot(erahat, y1, pch=20, col="blue")
points(erahat, y2, pch=21, col="green")
points(erahat, y3, pch=22, col="purple")
legend(x=4.5, y=.8, legend=c("mean", ".5", ".9"), col=c("blue", "green", "purple"), pch=c(20, 21, 22))



## part (c): simulate 95% confidence intervals for each of the probabilities plotted in b
library(MASS)
# for winpct = mean(winpct)

sims <- 1000
simbetas <- mvrnorm(sims,pe.1,vc) # draw parameters
erahat <- seq(1.5,5,.25)
simy1 <- NULL
simx1 <- NULL
for (i in 1:length(erahat)) {
  simmu <- simbetas%*%rbind(1,erahat[i],mean(winpct))
  simy01 <- 1/(1+exp(-simmu))
  simy1 <- rbind(simy1,simy01)
  simx1 <- rbind(simx1,t(t(rep(erahat[i],sims))))
}

# for winpct = .5
sims <- 1000
simbetas <- mvrnorm(sims,pe.1,vc) # draw parameters
erahat <- seq(1.5,5,.25)
simy2 <- NULL
simx2 <- NULL
for (i in 1:length(erahat)) {
  simmu <- simbetas%*%rbind(1,erahat[i],.5)
  simy02 <- 1/(1+exp(-simmu))
  simy2 <- rbind(simy2,simy02)
  simx2 <- rbind(simx2,t(t(rep(erahat[i],sims))))
}

# for winpct = .9
sims <- 1000
simbetas <- mvrnorm(sims,pe.1,vc) # draw parameters
erahat <- seq(1.5,5,.25)
simy3 <- NULL
simx3 <- NULL
for (i in 1:length(erahat)) {
  simmu <- simbetas%*%rbind(1,erahat[i],.9)
  simy03 <- 1/(1+exp(-simmu))
  simy3 <- rbind(simy3,simy03)
  simx3 <- rbind(simx3,t(t(rep(erahat[i],sims))))
}

source("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/chris' code/plot.simulates.r")

# Plot simulated results (all on same graph - but it's pretty messy)
par(mfrow=c(1,1))
plot.simulates(simx1,
               simy1,
               ci.range=c(0.95),
               ci.mark=c("dashed"),
               thin=0.0,
               usr=c(1.5,5,0,1),
               closeplot=FALSE,
               lcollist="blue"
               )

plot.simulates(simx2,
               simy2,
               ci.range=c(0.95),
               ci.mark=c("dashed"),
               thin=0.0,
               usr=c(1.5,5,0,1),
               addtoplot=TRUE,
               lcollist="gray",
               closeplot=FALSE
               )

plot.simulates(simx3,
               simy3,
               ci.range=c(0.95),
               ci.mark=c("dashed"),
               thin=0.0,
               usr=c(1.5,5,0,1),
               lcollist="purple",
               addtoplot=TRUE
               )
               
# Plot them on different graphs              
par(mfrow=c(2,2))
plot.simulates(simx1,
               simy1,
               ci.range=c(0.67, 0.95),
               ci.mark=c("poly", "dashed"),
               thin=0.0,
               usr=c(1.5,5,0,1),
               collist="light blue"
               )
text(3.5, .6, "era = mean")

plot.simulates(simx2,
               simy2,
               ci.range=c(0.67, 0.95),
               ci.mark=c("poly", "dashed"),
               thin=0.0,
               usr=c(1.5,5,0,1),
               collist="gray",
               )
text(3.5, .6, "era=.5")

plot.simulates(simx3,
               simy3,
               ci.range=c(0.67, 0.95),
               ci.mark=c("poly", "dashed"),
               thin=0.0,
               usr=c(1.5,5,0,1),
               collist="purple",
               )
text(4.5, .6, "era=.9")               

## part (d): find a "better model" of cy and show whether it improved using (1) an LR test, (2) ROC curves, (3) AvP plots, and (4) some other method


x2 <- cbind(era,wins,natleag*era)

ls.result <- lm(y~x2)  # use ls estimates as starting values

stval <- ls.result$coefficients  # initial guesses

logit.result <- optim(stval,llk.logit,method="BFGS",hessian=T,y=y,x=x2)     # call minimizer procedure

pe.2 <- logit.result$par   # point estimates
vc.2 <- solve(logit.result$hessian)  # var-cov matrix
se.2 <- sqrt(diag(vc.2))    # standard errors

ll.2 <- -logit.result$value  # likelihood at maximum


## Goodness of Fit of added variable


# Likelihood Ratio test

lr.test <- 2*(ll.2 - ll.1)
lr.test.p <- pchisq(lr.test,df=1,lower.tail=F)
lr.test.p

# BIC

bic.test <- - 2*(ll.2 - ll.1) + (ncol(x2)-ncol(x1))*log(nrow(x2))
bic.test

# AIC

aic.test <- - 2*(ll.2 - ll) + (ncol(x2)-ncol(x1))*2
aic.test

# Deviance
deviance <- -2*(ll.2)
deviance


source("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/chris' code/avp.r")
# Actual v Predicted values plot
# these will wind up in your working directory, remember
x11()
avp(y,x=cbind(rep(1,nrow(x1)),x1),beta=pe.1,fnform="logit",cutpoints=seq(0,1,0.1),
    usr=c(0,1,0,1),sizefactor=.8,color = "blue",
    #output = list(outfile="cy_logit_AvP.eps",high=6,wide=5.5,epsi=FALSE),              unless you comment this option out
    lab = list(x = .7, y=0.175, str="restricted model", col="blue", cex=1),
    ylab = "Actual Award Rate, by bins of predicted",
    xlab = "Predicted Award Rate, binned",
    closeplot=F)



avp(y,x=cbind(rep(1,nrow(x2)),x2),beta=pe.2,fnform="logit",cutpoints=seq(0,1,0.1),
    usr=c(0,1,0,1),sizefactor=.8,color = "red",
    #output = list(outfile="cy_logit_AvP.eps",high=6,wide=5.5,epsi=FALSE),
    lab = list(x = .7, y=0.1, str="expansive model", col="red", cex=1),
    addtoplot=T,closeplot=T)


library(verification)
# ROC
x11()
yhat.1 <- 1/(1+exp(-cbind(rep(1,nrow(x1)),x1)%*%pe.1))

yhat.2 <- 1/(1+exp(-cbind(rep(1,nrow(x2)),x2)%*%pe.2))

#postscript("cyroc_m1m2.eps",paper="letter",pointsize = 14,width=6,height=5.5,horizontal = FALSE, onefile = TRUE)
#i just turned this off because i wanted graphic to show up in R
roc.plot(y,cbind(yhat.1,yhat.2))

#dev.off()      if you leave it on, however, you need this command to close the postscript file


## part (e): calculate first differences

erahat <- seq(1.5, 5, .25)
mu1 <- rep(0, length(erahat))
mu2 <- rep(0, length(erahat))
mu3 <- rep(0, length(erahat))

for (i in 1:length(erahat)){
mu1[i] <- pe.1%*%rbind(1, erahat[i], .4)
y1 <- 1/(1+exp(-mu1)) 
}

for (i in 1:length(erahat)){
mu2[i] <- pe.1%*%rbind(1, erahat[i], .5)
y2 <- 1/(1+exp(-mu2)) 
}

for (i in 1:length(erahat)){
mu3[i] <- pe.1%*%rbind(1, erahat[i], .6)
y3 <- 1/(1+exp(-mu3)) 
}

fd1 <- y2-y1
fd2 <- y3-y2

#plot of difference in the probability of y when winpct changes from .4 to .5 and then from .5 to .6 across era values
plot(erahat, fd1, pch=20, ylim=c(0, .15), col="blue", ylab="change in probability of winning Cy Young award")
points(erahat, fd2, pch=19, col="green")
legend(x=3.5, y=.1, legend=c("winpct from .4 to .5", "winpct from .5 to .6"), col=c("blue", "green"), pch=c(20, 19))

mu4 <- rep(0, length(erahat))
mu5 <- rep(0, length(erahat))

for (i in 1:length(erahat)){
mu4[i] <- pe.1%*%rbind(1, erahat[i], min(winpct))
y4 <- 1/(1+exp(-mu4)) 
}

for (i in 1:length(erahat)){
mu5[i] <- pe.1%*%rbind(1, erahat[i], median(winpct))
y5 <- 1/(1+exp(-mu2)) 
}

fd3 <- y5-y4

plot(erahat, fd3, pch=20)


## part (f)
# There is no independence of observations in the cy data.  
# If one person in a league/year wins a cy, no one else can. So each observation of whether or not an individual player won the award 
#is dependent upon whether or not another player has already won. 

## part (g)
# If pitchers considered "unlikely" to win the award were ommitted from the dataset, 
# it would essentially result in selection on the dependent variable (or in this case, the latent DV), 
# which should suppress the effects of our covariates.
