#
# R code for difference in differences 
#

logitdata <- read.table("http://www.polsci.ucsb.edu/faculty/glasgow/ANES2004.txt", header=T, sep="\t")

logitdata <- na.omit(subset(logitdata,select=c(age,yrseduc,income,nonwhite, voted2004)))

indvars <- subset(logitdata,select=c(age,yrseduc,income,nonwhite))
constant <- 1
X <- as.matrix(cbind(constant,indvars))
y <- as.matrix(subset(logitdata,select=c(voted2004)))

K <- as.numeric(ncol(X))
varnames <- colnames(X)


## start values for estimation ##

startv <- rep(0, times=K)

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

#
# difference in differences
#

logit_coeffs <- coeffs
logit_covmat <- covmat
ndraws <- 1000


# comment out if already installed 
install.packages("MASS")
library(MASS)

betadraw <- mvrnorm(ndraws, logit_coeffs, logit_covmat)

# our 4 hypothetical individuals

hypind1 <- c(1,40,12,15,0)
hypind2 <- c(1,40,12,15,1)
hypind3 <- c(1,40,16,15,0)
hypind4 <- c(1,40,16,15,1)

cdfargs1 <- betadraw%*%hypind1
cdfargs2 <- betadraw%*%hypind2
cdfargs3 <- betadraw%*%hypind3
cdfargs4 <- betadraw%*%hypind4

pnw0ed12 <- exp(cdfargs1)/(1 + exp(cdfargs1))
pnw1ed12 <- exp(cdfargs2)/(1 + exp(cdfargs2))
pnw0ed16 <- exp(cdfargs3)/(1 + exp(cdfargs3))
pnw1ed16 <- exp(cdfargs4)/(1 + exp(cdfargs4))

# differences

diffnw0ed <- pnw0ed12 - pnw0ed16;
diffnw1ed <- pnw1ed12 - pnw1ed16;

# difference in differences 

diffnwed <- diffnw0ed - diffnw1ed;

# display results

means <- cbind(mean(diffnw0ed),mean(diffnw1ed),mean(diffnwed))
sds <- cbind(sd(diffnw0ed),sd(diffnw1ed),sd(diffnwed))
zs <- means/sds
ps <- 1 - pnorm(abs(zs))
presults <- rbind(means,sds,zs,ps)
colnames(presults) <- c("diffnw0ed","diffnw1ed","diffnwed")
rownames(presults) <- c("Mean","SD","Z","P")

print(presults)