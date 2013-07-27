###########################
# R code for logit                     #
# 2010 Essex Summer School #
###########################

## This line reads in the data.  Change what is in the quotation marks to your data filepath and name.
## This data is in ASCII format.

logitdata <- read.table("C:\\courses\\Essex\\ANES2004.txt", header=T, sep="\t")

## Alternatively, we can read data from a URL.

logitdata <- read.table("http://www.polsci.ucsb.edu/faculty/glasgow/ANES2004.txt", header=T, sep="\t")

## We can also read data in other formats, including Stata format.

library(foreign)
logitdata <- read.dta("http://www.polsci.ucsb.edu/faculty/glasgow/ANES2004.dta", convert.factors=FALSE)

## This line eliminates any missing data.  Some commands in R will crash if you feed it missing data.
## The code for missing data in R is "NA"

logitdata <- na.omit(subset(logitdata,select=c(yrseduc, voted2004)))

##  Plot our dependent and independent variables

plot(logitdata$yrseduc,logitdata$voted2004)

## A nicer looking plot

stripchart(logitdata$yrseduc ~ logitdata$voted2004, method="jitter", jitter=0.05, pch=1, xlab="Years of Education", ylab="Voted (0=no, 1=yes)")

## The indvars line picks out the variable "yrseduc" as the independent variable in the data.
## You can change this name, and/or add more variables, separated by commas

indvars <- subset(logitdata,select=c(yrseduc))
constant <- 1
X <- as.matrix(cbind(constant,indvars))

## This picks out the dependent variable, "voted2004"

y <- as.vector(subset(logitdata,select=c(voted2004)))


## Counts number of columns in X so we know how many independent variables we have

K <- as.numeric(ncol(X))

## Grab variable names from X for our output

varnames <- colnames(X)


## start values for estimation ##

startv <- matrix(0,nrow=K,ncol=1)

## These functions are what we maximize to estimate our logit ##

## logit log-likelihood function ##

logit.lf <- function(b, y, X) {

beta <- b[1:K]

exb <- exp(X%*%beta) 
prob1 <- exb/(1+exb) 

logexb <- log(prob1)

y0 <- 1 - y
logexb0 <- log(1 - prob1)

yt <- t(y)
y0t <- t(y0)

logl <- -sum(yt%*%logexb + y0t%*%logexb0)

return(logl)

}



## logit gradient function ##

logit.gr <- function(b, y, X) {

beta <- b[1:K]
grad <- beta*0


exb <- exp(X%*%beta) 
prob1 <- exb/(1+exb) 

for (k in 1:K) { 
  grad[k] <- sum(X[,k] * (y - prob1))
  }

return(-grad)

}

## This block of code estimates the logit and presents the results ##

logitmodel <- optim(startv, logit.lf, gr=logit.gr, method="BFGS", 
control=list(trace=TRUE, REPORT=1), hessian=TRUE, y=y, X=X)
coeffs <- logitmodel$par
covmat <- solve(logitmodel$hessian)
stderr <- sqrt(diag(covmat))
zscore <- coeffs/stderr
pvalue <- 2*(1 - pnorm(abs(zscore)))
results <- cbind(coeffs,stderr,zscore,pvalue)
colnames(results) <- c("Coeff.", "Std. Err.", "z", "p value")
rownames(results) <- varnames
print(results)  

## Generate and plot predicted probabilities ##

probsL <- exp(X%*%coeffs)/(1 + exp(X%*%coeffs))
plot(logitdata$yrseduc,probsL)

## The built-in linear regression routine

regmodel <- lm(voted2004 ~ yrseduc, data=logitdata)
summary(regmodel)

## The built-in routine to estimate logits and probits (and other models) in R

logitmodel2 <- glm(voted2004 ~ yrseduc, family=binomial(link="logit"), data=logitdata)
summary(logitmodel2)

## Plot predicted probabilities ##

plot(sort(logitdata$yrseduc),sort(logitmodel2$fitted.values), type="l")


