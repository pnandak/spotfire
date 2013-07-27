############################################
### R code to estimate a mixed logit     ###
### Garrett Glasgow, 10/06/08            ###
### Based on GAUSS code by Kenneth Train ###
### http://elsa.berkeley.edu/~train/     ###
############################################

### Insert the data file name and path between the quotation marks ###
### It is assumed that each row in the data is one alternative for one observation ###
### and that variable names are at the top of each column ###

MXLData <- read.table("C:\\MXL.asc", header=T)

### Insert the names of the variables with fixed coefficients ###
### inside the second set of parentheses, separated by commas ###
### If you want a random coefficient, enter the variable here ###
### and in one of the distribution lines below.               ###
### If you want an error component, just enter the variable   ###
### in one of the distribution lines below.                   ###

fixed_coeff <- subset(MXLData,select=c(discand,Bush,Clinton,bpid,cpid,bage,cage,bunion,cunion))

### Insert the names of the variables with normal distributions here ###

normal_coeff <- subset(MXLData,select=c(bunion,cunion))

### Insert the names of the variables with uniform distributions here ###

uniform_coeff <- subset(MXLData,select=c())

### Insert the names of the variables with triangular distributions here ###

triangular_coeff <- subset(MXLData,select=c())

### Insert the name of the variable that indicates observation number here ###

obs_id <- subset(MXLData,select=c(id))

### Insert the name of the variable that indicates choice here ###

depvar <- subset(MXLData,select=c(votec))

### Insert the name of the variable that indicates the choice alternative here ###

altnum <- subset(MXLData,select=c(choice))

### Change this number to change the number of Halton draws used in estimation ###

R <- 50


###################################################################
### The user should not need to change anything below this line ###
###################################################################



### future additions -- new distributions.  Random draws? ###
### forecasted shares, weights ###

nobs <- as.numeric(nrow(MXLData))

group <- as.vector(t(obs_id))

y <- as.vector(t(depvar))

nfixed <- as.numeric(ncol(fixed_coeff))
nnorm <- as.numeric(ncol(normal_coeff))
nuni <- as.numeric(ncol(uniform_coeff))
ntri <- as.numeric(ncol(triangular_coeff))

nrand <- nnorm+nuni+ntri

X <- data.matrix(fixed_coeff)

Z <- data.matrix(cbind(normal_coeff,uniform_coeff,triangular_coeff))

fixnames <- colnames(X)
randnames <- colnames(Z)
sdnames <- paste("sd_",randnames,sep="")

varnames <- c(fixnames,sdnames)

K <- as.numeric(nfixed + nrand)
SK <- as.numeric(nfixed)

### obtain start values from MNL ###

svmnl <- matrix(data=0,nrow=SK,ncol=1)

clogit.lf <- function(b, X, y, group) {

beta <- b[1:SK]

exb <- exp(X%*%beta)

denom <- tapply(exb,group,sum)

exby <- exb * y
numer <- tapply(exby,group,sum)

frac <- numer/denom

logfrac <- log(frac)

logl <- -sum(logfrac)

return(logl)

}


clogit.gr <- function(b, X, y, group) {

beta <- b[1:SK]
grad <- beta*0

exb <- exp(X%*%beta)

denom <- tapply(exb,group,sum)

exby <- exb * y
numer <- tapply(exby,group,sum)

frac <- numer/denom 

expandd <- as.vector(by(group,group,length))
  
denomex <- rep(denom,expandd)
 
probc <- exb/denomex  

  for (k in 1:SK) {
    part <- (y - probc) * X[,k]
    spart <- tapply(part,group,sum)
    grad[k] <- sum(spart)
  }

return(-grad)

}   

startv <- optim(svmnl, clogit.lf, gr=clogit.gr, method="BFGS", control=list(trace=TRUE), 
hessian=F, X=X, y=y, group=group)

svrand <- matrix(data=0.5,nrow=nrand,ncol=1)

svmxl <- as.numeric(rbind(startv$par,svrand))

### Halton draws ###

primev <- c(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,
            61,67,71,73,79,83,89,97,101,103,107,109,113)

# prevents error if no random terms #

np <- as.vector(unique(group))

nobsp <- length(np)

nrdraws <- as.numeric(nobsp*nrand)

rdraws <- NULL
hdraws <- NULL

for (h in 1:nrand) {

  s <- primev[h]                       # prime number for Halton sequence
  hlength <- (R*nobsp)+10
  k <- floor(log(hlength+1) / log(s))
  phi <- 0

  for (i in 1:k) {
    hx <- phi
    for (j in 1:(s-1)) {
      hy <- phi + (j/(s^i))
      hx <- rbind(hx,hy)
    }
  phi <- hx

  }

  hx <- phi
  for (j in 1:(s-1)) {
    while (nrow(hx) < (hlength+1)) {
      hy <- phi + (j/(s^k))
      hx <- rbind(hx,hy)
    }
  }

  phi <- as.matrix(hx[11:hlength+1,1])

# normal draws #

  if(h<=nnorm) invdraws <- qnorm(phi)  
  
# uniform draws #

  if(h>nnorm & h<=(nnorm+nuni)) invdraws <- (phi*2) - 1
  
# triangular draws #

  if(h>(nnorm+nuni) & h<=(nnorm+nuni+ntri)) invdraws <- (sqrt(phi*2) - 1)*(phi<=0.5) + (1 - sqrt((1-phi)*2))*(phi>0.5)
  
  hdraws <- matrix(invdraws,nrow=nobsp,ncol=R,byrow=TRUE)
  
  rdraws <- rbind(rdraws,hdraws)  # append new matrix to bottom of rdraws #

}

### likelihood function ###

mxl.lf <- function(b, X, Z, y, rdraws, group) {

beta <- b[1:K]
Sfrac <- NULL

for (r in 1:R) {

  err <- matrix(rdraws[,r],nrow=nobsp,ncol=nrand,byrow=FALSE)
  
  # expand random draws from people to peopleXalts #
  
  expdraw <- as.matrix(by(group,group,length))
  Zdraw <- NULL
  
  for (i in 1:nrand) {
  
    experr <- rep(err[,i],expdraw)
    errX <- as.matrix(Z[,i]*experr)
    Zdraw <- as.matrix(cbind(Zdraw,errX))
  }
  
  Xall <- as.matrix(cbind(X,Zdraw))


  exb <- exp(Xall%*%beta)

  denom <- tapply(exb,group,sum)
  
  exby <- exb * y
  numer <- tapply(exby,group,sum)

  frac <- numer/denom

  Sfrac <- cbind(Sfrac, frac)

}

IndLL <- (rowSums(Sfrac))/R

logl <- -sum(log(IndLL))

return(logl)

}


### gradient function ###

mxl.gr <- function(b, X, Z, y, rdraws, group) {

beta <- b[1:K]
Sfrac <- NULL
ngrad <- matrix(data=0,nrow=nobsp,ncol=K)

for (r in 1:R) {

  err <- matrix(rdraws[,r],nrow=nobsp,ncol=nrand,byrow=FALSE)
  
  # expand random draws from people to peopleXalts #
  
  expdraw <- as.vector(by(group,group,length))
  Zdraw <- NULL
  
  for (i in 1:nrand) {
  
    experr <- rep(err[,i],expdraw)
    errX <- as.matrix(Z[,i]*experr)
    Zdraw <- as.matrix(cbind(Zdraw,errX))
  }
  
  Xall <- as.matrix(cbind(X,Zdraw))

  exb <- exp(Xall%*%beta)

  denom <- tapply(exb,group,sum)
  
  exby <- exb * y
  
  numer <- tapply(exby,group,sum)

  frac <- numer/denom

  Sfrac <- cbind(Sfrac, frac)
  
  denomex <- rep(denom,expdraw)
 
  probc <- exb/denomex  
  proby <- exby/denomex

  for (k in 1:K) {
    part <- (y - probc) * Xall[,k]
    spart <- tapply(part,group,sum)
    pspart <- spart * frac
    ngrad[,k] <- ngrad[,k] + pspart
    }
    
  }  

sumprob <- rowSums(Sfrac)
wgrad <- ngrad / sumprob

grad <- colSums(wgrad)

return(-grad)

}   


system.time(repmod <- optim(svmxl, mxl.lf, gr=mxl.gr, method="BFGS", control=list(trace=TRUE), 
hessian=T, X=X, Z=Z, y=y, rdraws=rdraws, group=group))
inverted <- solve(repmod$hessian)
results <- cbind(repmod$par, sqrt(diag(inverted)), repmod$par/sqrt(diag(inverted)))
colnames(results) <- c("Coefficient", "Std. Err.", "z")
rownames(results) <- varnames
print(results)  







################ don't run the code below this line yet ####################




## forecasted shares ##

####### this is calculating the right probabilities, but I need to keep track of 
## each option for each obs to add them up correctly ##
## after that switching this to a hypothetical case will be easy ###
## change fX to a vector ... I think ##
## censored alternatives should be handled automatically ##

fX <- X

fbeta <- repmod$par
fprob <- NULL

for (r in 1:R) {

  err <- matrix(rdraws[,r],nrow=nobsp,ncol=nrand,byrow=FALSE)
  
  # expand random draws from people to peopleXalts #
  
  expdraw <- as.matrix(by(group,group,length))
  Zdraw <- NULL
  
  for (i in 1:nrand) {
  
    experr <- rep(err[,i],expdraw)
    errX <- as.matrix(Z[,i]*experr)
    Zdraw <- as.matrix(cbind(Zdraw,errX))
  }
  
  Xall <- as.matrix(cbind(fX,Zdraw))

  exb <- exp(Xall%*%fbeta)

  denom <- tapply(exb,group,sum)

  denomex <- rep(denom,expdraw)
 
  probv <- exb/denomex  

  fprob <- cbind(fprob,probv)

}

sfshares <- (rowSums(fprob))/R
fshares <- tapply(sfshares,altnum,mean)
print(fshares)
