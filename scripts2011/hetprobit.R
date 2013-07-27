###########################
# R code for heteroskedastic probit                   #
# 2010 Essex Summer School #
###########################

## This line reads in the data.  Change what is in the quotation marks to your data filepath and name.
## This data is in ASCII format.

library(foreign)
read.probitdata <- read.dta("C:\\courses\\Essex\\ANES2004.dta", convert.factors=FALSE)

## This line eliminates any missing data.  R will crash if you feed it missing data.
## The code for missing data in R is "NA"

probitdata <- as.matrix(subset(read.probitdata,select=c(voted2004,yrseduc, age, GWBtherm,IraqCost)))

probitdata <- na.omit(probitdata)

## The indvars line picks out the variable "yrseduc" as the independent variable in the data.
## You can change this name, and/or add more variables, separated by commas

indvars <- subset(probitdata,select=c(yrseduc, age, GWBtherm))
constant <- 1
X <- as.matrix(cbind(constant,indvars))

Z <- as.matrix(subset(probitdata,select=c(IraqCost)))

XZ <- cbind(X,Z)

## This picks out the dependent variable, "voted2004"

y <- as.vector(subset(probitdata,select=c(voted2004)))

## Counts number of columns in X so we know how many independent variables we have

K <- as.numeric(ncol(X))
G <- K+1
KG <- as.numeric(ncol(XZ))

## Grab variable names from X for our output

varnames <- c(colnames(XZ))

## start values for estimation ##

startv <- matrix(0,nrow=KG,ncol=1)



## hprobit log-likelihood function ##

hprobit.lf <- function(b, y, XZ) {

beta <- b[1:KG]

xb <- XZ[,1:K]%*%beta[1:K]
if ((KG-G)>0) zg <- exp(XZ[,G:KG]%*%beta[G:KG]) else zg <- exp(XZ[,KG]*beta[KG])

cdfn <- pnorm(xb/zg)

logcdfn <- log(cdfn)

y0 <- 1 - y
logcdfn0 <- log(1 - cdfn)

yt <- t(y)
y0t <- t(y0)

logl <- -sum(yt%*%logcdfn + y0t%*%logcdfn0)

return(logl)

}

## This block of code estimates the heteroskedastic probit and presents the results ##

hprobitmodel <- optim(startv, hprobit.lf, method="BFGS", 
control=list(trace=TRUE, REPORT=1), hessian=TRUE, y=y, XZ=XZ)
coeffs <- hprobitmodel$par
covmat <- solve(hprobitmodel$hessian)
stderr <- sqrt(diag(covmat))
zscore <- coeffs/stderr
pvalue <- 2*(1 - pnorm(abs(zscore)))
results <- cbind(coeffs,stderr,zscore,pvalue)
colnames(results) <- c("Coeff.", "Std. Err.", "z", "p value")
rownames(results) <- varnames
print(results)  

