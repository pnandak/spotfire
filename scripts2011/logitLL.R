###########################
# R code for logit                        #
# 2010 Essex Summer School  #
###########################

logitdata <- read.table("http://www.polsci.ucsb.edu/faculty/glasgow/ANES2004.txt", header=T, sep="\t")

logitdata <- na.omit(subset(logitdata,select=c(yrseduc, voted2004)))

indvars <- subset(logitdata,select=c(yrseduc))
constant <- 1
X <- as.matrix(cbind(constant,indvars))

y <- as.vector(subset(logitdata,select=c(voted2004)))

K <- as.numeric(ncol(X))

varnames <- colnames(X)

startv <- matrix(0,nrow=K,ncol=1)

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



## graph logit log-likelihood function for yrseduc ##

logl <- NULL
beta.ed <- NULL

for (i in 1:2000) {

betatemp <- (i-50)/2000

beta <- c(-2.51, betatemp)

exb <- exp(X%*%beta) 
prob1 <- exb/(1+exb) 

logexb <- log(prob1)

y0 <- 1 - y
logexb0 <- log(1 - prob1)

yt <- t(y)
y0t <- t(y0)

logl <- rbind(logl,sum(yt%*%logexb + y0t%*%logexb0))
beta.ed <- rbind(beta.ed,betatemp)

}

plot(beta.ed,logl, type="l")
abline(v=0.287)


## graph logit gradient function ##


gradl <- NULL
beta.ed <- NULL

for (i in 1:2000) {

betatemp <- (i-50)/2000

beta <- c(-2.51, betatemp)

exb <- exp(X%*%beta) 
prob1 <- exb/(1+exb) 

gradl <- rbind(gradl,sum(X[,2] * (y - prob1)))
beta.ed <- rbind(beta.ed,betatemp)

}

plot(beta.ed,gradl, type="l")
abline(v=0.287)

