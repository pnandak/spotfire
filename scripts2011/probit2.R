################################
# R code for probit            #
################################

probitdata <- read.table("http://www.polsci.ucsb.edu/faculty/glasgow/BES2010.txt", header=T, sep="\t")

probitdata <- na.omit(subset(probitdata,select=c(app_Brown,affect_fc,MP_resign,gender,age,app_Afgh)))


indvars <- subset(probitdata,select=c(app_Brown,affect_fc,MP_resign,gender,age))
constant <- 1
X <- as.matrix(cbind(constant,indvars))
y <- as.matrix(subset(probitdata,select=c(app_Afgh)))

K <- as.numeric(ncol(X))
varnames <- colnames(X)


## start values ##

startv <- matrix(0,nrow=K,ncol=1)

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

####################################
### Hypothetical Individual code ###
####################################

# grab coefficients #

probit_coeffs <- t(coeffs)

# set values of independent variables #
# right now constant=1, app_Brown=5, affect_fc=2, MP_resign=1, gender=0, age=40 #
# modify as necessary #

hypind <- c(1, 5, 2, 1, 0, 40)

# calculate probability #

hypprob <- pnorm(probit_coeffs%*%hypind)

# display results #

print(hypprob)

#####################################
# simulation to get standard errors #
#####################################

install.packages("MASS")
library(MASS)

# number of draws #

ndraws <- 1000

# grab covariance matrix #

probit_covmat <- covmat

# draw from MVN #

betadraw <- mvrnorm(ndraws, probit_coeffs, probit_covmat)

# multiply to get arguments for CDF #

cdfargs <- betadraw%*%hypind

# calculate probabilities #

hypprobs <- pnorm(cdfargs)

# get mean and standard deviation #

meanprob <- mean(hypprobs)
sdprob <- sd(hypprobs)

# display results #

print(meanprob)
print(sdprob)

## We can do the same thing with the GLM command ##

probitmodel2 <- glm(app_Afgh ~ app_Brown+affect_fc+MP_resign+gender+age, family=binomial(link="probit"), data=probitdata)
summary(probitmodel2)

probit_coeffs <- probitmodel2$coefficients
probit_covmat <- vcov(probitmodel2)

betadraw <- mvrnorm(ndraws, probit_coeffs, probit_covmat)

# multiply to get arguments for CDF #

cdfargs <- betadraw%*%hypind

# calculate probabilities #

hypprobs <- pnorm(cdfargs)

# get mean and standard deviation #

meanprob <- mean(hypprobs)
sdprob <- sd(hypprobs)

# display results #

print(meanprob)
print(sdprob)


## The Zelig package makes this very convenient ##

install.packages("Zelig")
install.packages("VGAM")

library(Zelig)

probitmodel3 <- zelig(app_Afgh ~ app_Brown+affect_fc+MP_resign+gender+age, model="probit", data=probitdata)
summary(probitmodel3)

hypind <- setx(probitmodel3, app_Brown=5, affect_fc=2, MP_resign=1, gender=0, age=40)

hypprobs <- sim(probitmodel3, x = hypind)

summary(hypprobs)

plot(hypprobs)
