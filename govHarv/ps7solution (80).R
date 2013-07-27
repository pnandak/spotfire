rm(list=ls())

# Q1
snct <- read.table("snct.dat",header=T)

# a
ord.probit.llh <- function(parms,X,Y){

    ###Pre-process Y
    ###Convert Y to n x m categories matrix
    n <-length(Y); y0<-sort(unique(Y)); m <- length(y0);
    Z <- matrix(NA,n,m)
    for (j in 1:m){
        Z[,j] <- Y == y0[j]
    }

    ###Set up parameters
    ###Thresholds must be constrained
    ###to be larger than previous thresholds
    k <- ncol(X)
    taucut <- cumsum(c(0,exp(parms[-(1:k)])))

    ###Latent Variable
    b <- parms[1:k]
    ystar <- as.matrix(X)%*%b

    ###Initialize matrices to store probabilities
    cprobs <- probs <- matrix(NA,n,m)

    ###Create matrix of cumulative probabilities
    for (j in 1:(m-1)){
        cprobs[,j] <- pnorm(taucut[j]-ystar)
    }

    ###use cumulative probabilities to extract
    # fill top category
    probs[,m] <- 1-cprobs[,m-1]
    # fill bottom catgeory
    probs[,1] <- cprobs[,1]
    # fill middle categories
    for (j in 2:(m-1)){
        probs[,j] <- cprobs[,j]-cprobs[,j-1]
    }
    ###Use matrix of dependent variable to extract correct probabilities
    score <- log(probs[Z])
    llh.sum <- sum(score)
}

#
Y <- snct$RES
snct$TARGET.COOP <- snct$TARGET*snct$COOP
X <- cbind(1, snct[,c("IMPORT","COST","TARGET","COOP","TARGET.COOP")])

start.val <- rep(0,8)

op.out <- optim(start.val,fn=ord.probit.llh,
               method="BFGS",
               hessian=T,
               control=list(fnscale=-1),Y=Y,X=X)

#the coefficients
op.out$par[1:ncol(X)]

# unexp the cutpoints
k <- ncol(X)
b <- op.out$par[1:k]
taucut <- exp(op.out$par[-(1:k)])
taucut <- c(0,taucut)
taucut <- cumsum(taucut)
taucut

#the SEs - note the SEs for the cutpoints are still on the exp() scale
sqrt(diag(solve(-op.out$hessian)))

# chck with zelig
library(Zelig)
z.out <- zelig(formula(paste("as.factor(RES)~",paste(names(X)[-1],collapse="+"))),model="oprobit",data=snct)
summary(z.out)

#
tab <- cbind(c(op.out$par[2:ncol(X)],taucut[-1]),
             c(sqrt(diag(solve(-op.out$hessian)))[-1]))
colnames(tab) <- c("Coef","SE")
rownames(tab) <- c(names(X)[-1],"tau1","tau2")
xtable(tab,digits=2)


# b simulate counterfactuals
library(MASS)
betas <- mvrnorm(1000, op.out$par, solve(-op.out$hessian))

#function to simulate expected values
ev <- function(betas, X) {
  ####beta parameters
  ifelse(is.vector(X),k<-length(X),k <- ncol(X))
  b <- betas[,1:k]
  mu <- t(X)%*%t(b)

  taucut <- cbind(0,exp(betas[,-(1:k)]))
  taucut <- t(apply(taucut,1,cumsum))

  prob1 <- c(pnorm(taucut[,1], mu))
  prob2 <- c(pnorm(taucut[,2], mu) - pnorm(taucut[,1], mu))
  prob3 <- c(pnorm(taucut[,3], mu) - pnorm(taucut[,2], mu))
  prob4 <- 1-apply(cbind(prob1,prob2,prob3),1,sum)
  probs <- cbind(prob1,prob2,prob3,prob4)
}

qi <- array(NA, c(1000, 12,2))

#base X matrix
xbase <- apply(X,2,median)

#scenario 1
X0a <- X1a <- xbase
X0a[c("COOP")] <- 1
X0a[c("TARGET.COOP")] <- X0a[c("COOP")] * X0a[c("TARGET")]
X1a[c("COOP")] <- 2
X1a[c("TARGET.COOP")] <- X1a[c("COOP")] * X1a[c("TARGET")]
qi[,1:4,1] <- ev(betas=betas, X=X0a)
qi[,5:8,1] <- ev(betas=betas, X=X1a)
qi[,9:12,1] <- qi[,5:8,1]-qi[,1:4,1]

#scenario 2
X0b <- X1b <- xbase
X0b[c("IMPORT")] <- 0
X0b[c("TARGET.COOP")] <- X0b[c("COOP")] * X0a[c("TARGET")]
X1b[c("IMPORT")] <- 1
X1b[c("TARGET.COOP")] <- X1b[c("COOP")] * X1a[c("TARGET")]

qi[,1:4,2]<- ev(betas=betas, X=X0b)
qi[,5:8,2] <- ev(betas=betas, X=X1b)
qi[,9:12,2] <- qi[,5:8,2]-qi[,1:4,2]

#scenario 1
mean.sc1 <- apply(qi[,,1],2,mean)
CI.sc1   <- apply(qi[,,1],2,quantile,probs=c(.025, .975))
tab <- cbind(mean.sc1, CI.sc1[1,], CI.sc1[2,])
tab <- rbind(t(tab[1:4,]),t(tab[5:8,]),t(tab[9:12,]))
xtable(tab,digits=2)

#scenario 2
mean.sc2 <- apply(qi[,,2],2,mean)
CI.sc2 <- apply(qi[,,2],2,quantile,probs=c(.025, .975))
tab <- cbind(mean.sc2, CI.sc2[1,], CI.sc2[2,])
tab <- rbind(t(tab[1:4,]),t(tab[5:8,]),t(tab[9:12,]))
xtable(tab,digits=2)

# check in zelig
library(Zelig)
z.out <- zelig(as.factor(RES)~IMPORT+COST+TARGET*COOP,model="oprobit",data=snct)
summary(z.out)

# (scenario 1)
X.low <- setx(z.out,IMPORT=0,COST=2,TARGET=2,COOP=1)
X.high <- setx(z.out,IMPORT=0,COST=2,TARGET=2,COOP=2)
s.out <- sim(z.out, x = X.low, x1 = X.high, num=1000)
summary(s.out)

# (scenario 2)
X.low <- setx(z.out,IMPORT=0,COST=2,TARGET=2,COOP=1)
X.high <- setx(z.out,IMPORT=1,COST=2,TARGET=2,COOP=2)
s.out <- sim(z.out, x = X.low, x1 = X.high, num=1000)
summary(s.out)
sol <- s.out

## simulate from zelig output
#mu <- summary(z.out)$coef[,1]
#betas <- mvrnorm(1000, mu, solve(z.out$Hessian))
#
##function to simulate expected values
#ev <- function(betas, X) {
#  ####beta parameters
#  ifelse(is.vector(X),k<-length(X),k <- ncol(X))
#  b <- betas[,1:k]
##  mu <- X%*%t(b)
#  mu  <- as.matrix(X)%*%t(b)
##  taucut <- cbind(0,exp(betas[,-(1:k)]))
#  taucut <- betas[,-(1:k)]
#  taucut <- apply(taucut,1,cumsum)
##  taucut <- apply(taucut,1,sort,decreasing=T)
#  mu <- t(mu)
#  taucut <- t(taucut)
#  prob1 <- c(pnorm(taucut[,1], mu))
#  prob2 <- c(pnorm(taucut[,2], mu) - pnorm(taucut[,1], mu))
#  prob3 <- c(pnorm(taucut[,3], mu) - pnorm(taucut[,2], mu))
#  prob4 <- 1-apply(cbind(prob1,prob2,prob3),1,sum)
#  probs <- cbind(prob1,prob2,prob3,prob4)
#}
#
#qi <- array(NA, c(1000, 12,2))
#
##scenario 2
#X.low <- setx(z.out,IMPORT=0,COST=2,TARGET=2,COOP=1,RELAT=2)
#X.low <- X.low[,-1]
#X.high <- setx(z.out,IMPORT=1,COST=2,TARGET=2,COOP=1,RELAT=2)
#X.high <- X.high[-1]
#out.l <- ev(betas=betas, X=X.low)
#out.h <- ev(betas=betas, X=X.high)
#apply(out.l,2,mean)
#apply(out.h,2,mean)
#
#X.low <- setx(z.out,IMPORT=0,COST=2,TARGET=2,COOP=1,RELAT=2)
#s.out <- sim(z.out, x = X.low, num=1000)
#apply(s.out$qi$ev,2,mean)
#

# c
# factual
fact <- snct[,c("IMPORT", "COST", "TARGET", "COOP", "RELAT")]

# counterfactual
counterfact <- fact
counterfact$IMPORT <- ifelse(fact$IMPORT==0,1,0)

library(WhatIf)
whif <- whatif(data=fact, cfact=counterfact)

#% in hull
sum(whif$in.hull)/nrow(fact)
#geometric variability
whif$geom.var
#counterfactuals less than geom.var away
mean(whif$sum.stat)

# Q2
rm(list=ls())
load("ussu.rdata")

# Poisson
Y <- ussu$conflu
X <- as.matrix(cbind(1,ussu[,c("sumil","ppid")]))

##alright, writing down the likelihood
lik.pois<- function(par, X, Y){
beta<- par[1:ncol(X)]
mu<- X%*%beta
lambda<- exp(mu)
sum(-lambda + Y*log(lambda))
}


## let's try out the log-likelihood
out<- optim(rep(0,ncol(X)), lik.pois, method="Nelder-Mead",
		control=list(fnscale=-1, trace=10), X=X, Y=Y, hessian=T)

test <- zelig(conflu~sumil+ppid, data=ussu,model="poisson")

# on point
summary(test)
out$par
sqrt(diag(solve(-out$hessian)))

# store
po.out <- summary(test)$coef[,1:2]

# Q2
## negative binomial
lik.bin<- function(par, X, Y){
beta<- par[1:ncol(X)]
theta<- par[ncol(X) + 1]
#theta<- exp(theta)+1
mu<- X%*%beta
lambda<- exp(mu)
out<- lgamma(theta + Y) - lgamma(theta) + Y*log(lambda) + theta *log(theta) -
		(theta + Y)*log(lambda + theta)
sum(out)
}

# with box constrraints
out2<- optim(c(rep(0,ncol(X)),1), lik.bin, method="L-BFGS-B",
			control=list(fnscale=-1, trace=10), X=X, Y=Y,hessian=T,
      lower=c(rep(-Inf,ncol(X)),1),
      upper=c(rep(Inf,ncol(X)),10))

test2 <- zelig(conflu~sumil+ppid, data=ussu,model="negbin",maxit=1000)
summary(test2)
X0 <- setx(test2,ppid=0)
X1 <- setx(test2,ppid=1)
test2.out <- sim(test2,x=X0,x1=X1)


b <- out2$par
se <- sqrt(diag(solve(-out2$hessian)))

# almost on point with zelig
# store
nb.out <- cbind(b,se)
tab <- cbind(rbind(po.out,c(0,0)),nb.out)
xtable(tab,digits=2)

# store
po.out <- summary(test)$coef[,1:2]

# simulate
X1 <- X0<- apply(X, 2, mean)
X0["ppid"] <- 0
X1["ppid"] <- 1

# Negative Binomial
coef.negbin<- mvrnorm(1000, mu=c(out2$par), Sigma=solve(-out2$hessian))
mu.neg0<- exp(coef.negbin[,-ncol(coef.negbin)]%*%as.matrix(X0))
mu.neg1<- exp(coef.negbin[,-ncol(coef.negbin)]%*%as.matrix(X1))
t1 <- c(mean(mu.neg1),mean(mu.neg0),mean(mu.neg1-mu.neg0),c(quantile(mu.neg1-mu.neg0,probs=c(.025,.975))))

# Possion
coef.p<- mvrnorm(1000, mu=c(out$par), Sigma=solve(-out$hessian))
mu.p0<- exp(coef.p%*%as.matrix(X0))
mu.p1<- exp(coef.p%*%as.matrix(X1))

t2 <- c(mean(mu.p1),mean(mu.p0),mean(mu.p1-mu.p0),c(quantile(mu.p1-mu.p0,probs=c(.025,.975))))

xtable(rbind(t2,t1),digits=1)

