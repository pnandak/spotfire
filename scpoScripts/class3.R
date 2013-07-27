## MLE, PS 206 Class 3

logitdata <- read.table("PPIC_class1.txt", header=T, sep="\t")

logitdata <- na.omit(logitdata)

logitdata <- as.data.frame(logitdata)
attach(logitdata)

# The built-in routine to estimate logits and probits (and other models) in R -- estimation method is IRLS

logitmodel2 <- glm(votemail ~  tenureres+youngkid+strongpart+age, family=binomial(link="logit"), data=logitdata)
summary(logitmodel2)




## writing our own likelihood function

constant <- 1

X <- cbind(constant, tenureres, youngkid, strongpart, age)

y <- votemail


## Counts number of columns in X so we know how many independent variables we have

K <- as.numeric(ncol(X))

## Grab variable names from X for our output

varnames <- colnames(X)

## start values for estimation ##

startv <- matrix(0,nrow=K,ncol=1)

## here's a good trick for better start values with a constant ##
## analytically solve for constant only model ##

conlysv <- log(mean(y)) - log(1 - mean(y))

startv2 <- rbind(conlysv, matrix(0, nrow=(K-1), ncol=1))

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

logitmodel <- optim(startv2, logit.lf, gr=logit.gr, method="BFGS", 
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


## Graph likelihood for age coefficient, holding all else at maximum

agecoeff <- seq(-0.5,0.5,0.01)
coeff2 <- coeffs
LL <- NULL

# LL
for (m in agecoeff) {
  coeff2[5] <- m
  LLt <- sum(y*log(exp(X%*%coeff2)/(1 + exp(X%*%coeff2))) + (1-y)*log(1/(1 + exp(X%*%coeff2))))
  LL <- rbind(LL,LLt)
}

plot(agecoeff,LL, type="l", xlab="Age Coeff.", ylab="LL")

abline(v=coeffs[5])
abline(h=-(logitmodel$value))





## Trying different optimization routines

#install.packages("maxLik")
library(maxLik)


logit.lf <- function(beta) y*log(exp(X%*%beta)/(1 + exp(X%*%beta))) + (1-y)*log(1/(1 + exp(X%*%beta)))


## BFGS estimation

logitmodel.1 <- maxBFGS(logit.lf, start=startv2, print.level=2)
summary(logitmodel.1)

## NR estimation

logitmodel.2 <- maxNR(logit.lf, start=startv2, print.level=2)
summary(logitmodel.2)

## BHHH estimation

logitmodel.3 <- maxBHHH(logit.lf, start=startv2, print.level=2)
summary(logitmodel.3)

## Compare the results

coeffs <- logitmodel.1$estimate
covmat <- solve(-(logitmodel.1$hessian))   ### note we're using the negative of the hessian here (not working with negative as in optim)
stderr <- sqrt(diag(covmat))
zscore <- coeffs/stderr
pvalue <- 2*(1 - pnorm(abs(zscore)))
results.1 <- cbind(coeffs,stderr,zscore,pvalue)
colnames(results.1) <- c("Coeff.", "Std. Err.", "z", "p value")
rownames(results.1) <- varnames

coeffs <- logitmodel.2$estimate
covmat <- solve(-(logitmodel.2$hessian))   ### note we're using the negative of the hessian here (not working with negative as in optim)
stderr <- sqrt(diag(covmat))
zscore <- coeffs/stderr
pvalue <- 2*(1 - pnorm(abs(zscore)))
results.2 <- cbind(coeffs,stderr,zscore,pvalue)
colnames(results.2) <- c("Coeff.", "Std. Err.", "z", "p value")
rownames(results.2) <- varnames 

coeffs <- logitmodel.3$estimate
covmat <- solve(-(logitmodel.3$hessian))   ### note we're using the negative of the hessian here (not working with negative as in optim)
stderr <- sqrt(diag(covmat))
zscore <- coeffs/stderr
pvalue <- 2*(1 - pnorm(abs(zscore)))
results.3 <- cbind(coeffs,stderr,zscore,pvalue)
colnames(results.3) <- c("Coeff.", "Std. Err.", "z", "p value")
rownames(results.3) <- varnames

print(results.1) # BFGS #
print(results.2) # NR #
print(results.3) # BHHH #


## A probit model

probit.lf <- function(beta) y*log(pnorm(X%*%beta)) + (1-y)*log(1 - pnorm(X%*%beta))

probitmodel.1 <- maxBHHH(probit.lf, start=startv, print.level=2)
summary(probitmodel.1)

probitmodel2 <- glm(votemail ~  tenureres+youngkid+strongpart+age, family=binomial(link="probit"), data=logitdata)
summary(probitmodel2)

#### Try different start values, estimation routines, graphing likelihoods

