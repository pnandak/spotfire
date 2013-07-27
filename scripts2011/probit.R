###########################
# R code for probit                   #
# 2010 Essex Summer School #
###########################

## This line reads in the data.  Change what is in the quotation marks to your data filepath and name.
## This data is in ASCII format.

probitdata <- read.table("C:\\courses\\Essex\\ANES2004.txt", header=T, sep="\t")

## This line eliminates any missing data.  R will crash if you feed it missing data.
## The code for missing data in R is "NA"

probitdata <- na.omit(subset(probitdata,select=c(yrseduc, voted2004)))

## The indvars line picks out the variable "yrseduc" as the independent variable in the data.
## You can change this name, and/or add more variables, separated by commas

indvars <- subset(probitdata,select=c(yrseduc))
constant <- 1
X <- as.matrix(cbind(constant,indvars))

## This picks out the dependent variable, "voted2004"

y <- as.vector(subset(probitdata,select=c(voted2004)))

## Counts number of columns in X so we know how many independent variables we have

K <- as.numeric(ncol(X))

## Grab variable names from X for our output

varnames <- colnames(X)


## probit log-likelihood function ##

probit.lf <- function(b, y, X) {

beta <- b[1:K]

cdfn <- pnorm(X%*%beta) 

logcdfn <- log(cdfn)

y0 <- 1 - y
logcdfn0 <- log(1 - cdfn)

yt <- t(y)
y0t <- t(y0)

logl <- -sum(yt%*%logcdfn + y0t%*%logcdfn0)

return(logl)

}



## probit gradient function ##

probit.gr <- function(b, y, X) {

beta <- b[1:K]
grad <- beta*0


cdfn <- pnorm(X%*%beta) 

for (k in 1:K) { 
  grad[k] <- sum(X[,k] * (y - cdfn))
  }

return(-grad)

}

## This block of code estimates the probit and presents the results ##

probitmodel <- optim(startv, probit.lf, gr=probit.gr, method="BFGS", 
control=list(trace=TRUE, REPORT=1), hessian=TRUE, y=y, X=X)
coeffs <- probitmodel$par
covmat <- solve(probitmodel$hessian)
stderr <- sqrt(diag(covmat))
zscore <- coeffs/stderr
pvalue <- 2*(1 - pnorm(abs(zscore)))
results <- cbind(coeffs,stderr,zscore,pvalue)
colnames(results) <- c("Coeff.", "Std. Err.", "z", "p value")
rownames(results) <- varnames
print(results)  

## Generate and plot predicted probabilities ##

probsP <- pnorm(X%*%coeffs)
plot(probitdata$yrseduc,probsP, type="l")

## The built-in routine to estimate logits and probits (and other models) in R

probitmodel2 <- glm(voted2004 ~ yrseduc, family=binomial(link="probit"), data=probitdata)
summary(probitmodel2)

## Plot predicted probabilities ##

plot(sort(probitdata$yrseduc),sort(probitmodel2$fitted.values), type="l")
