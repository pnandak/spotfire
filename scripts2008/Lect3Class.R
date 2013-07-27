Lecture3 <- function()
{
 library(stats4)

 # Part1()
 # Part2()
 # Part3()
 # Part5() 
 # Part6()

}

# ==========================================================================================================================================

ll1 <- function(x)
{
 obj <- (x-0.3)^2
 return(obj)
}

Part1 <- function()
{
 ret1 <- mle(ll1,start=list(x=1))
 print(summary(ret1))
 print(attributes(ret1))
 print(confint(ret1))
}

# =============================================================================================================================================

LL2 <- function(p) {
 obj <- -1*30*log(p)-20*log(1-p)
 return(obj)
  }
 
Part2 <- function()
{
 ret2 <- mle(LL2,start=list(p=0.1),lower=c(0.00001),upper=c(0.999999),method="L-BFGS-B")
 print(summary(ret2))
 Best <- -1*logLik(ret2)[1]

 pp <- seq(from=0.05,to=0.95,by=0.05)
 vals <- LL2(pp)
 par(mfrow=c(2,2))
 plot(pp,vals-Best,lty=1,lwd=2,type='l')
 abline(h=1.84)

}

# =============================================================================================================================================

Part3 <- function()
{

 # negative log-likelihood
 LL3 <- function(lam1,lam2,p,Blows)
  {
   LikeOut <- p*dpois(Blows,lam1)+(1-p)*dpois(Blows,lam2)
   NegLogLike <- -1*sum(log(LikeOut))
   # cat(NegLogLike,p,lam1,lam2,"\n")
   return(NegLogLike)  
  }

 TheData <- matrix(scan("C:\\Courses\\R Class\\Lectures\\Lect2a.dat",skip=1),byrow=T,ncol=2)
 Blows <- TheData[,2]

 # fit the model
 ret3 <- mle(LL3,start=list(lam1=10,lam2=10,p=0.4),fixed=list(Blows=Blows),lower=c(0.01,0.01,0.00001),upper=c(Inf,Inf,0.99999),method="L-BFGS-B")
 print(summary(ret3))

 # profile (not used)
 par(mfrow=c(2,2))
 profv <- NULL; loglik <- NULL
 for (ii in 1:49)
  {
    ret3a <- mle(LL3,start=list(lam1=10,lam2=10),fixed=list(p=ii*0.01,Blows=Blows),lower=c(0.01,0.01,0.00001),upper=c(Inf,Inf,0.99999),method="L-BFGS-B")
    profv <- c(profv,ii*0.01)
    loglik <- c(loglik,-1*logLik(ret3a)[1])
 }
 Outs <- cbind(profv,loglik)
 print(Outs)
 plot(profv,loglik,type='l',lty=1,lwd=2) 

}

# =============================================================================================================================================

Part5 <- function()
{
 TheData <- matrix(scan("C:\\Courses\\R Class\\Lectures\\Lect2a.dat",skip=1),byrow=T,ncol=2)
 Blows <- TheData[,2]

 # negative log likelihood
 LL3 <- function(x)
  {
   p <- x[3]
   lam1 <- x[1]
   lam2 <- x[2]
   LikeOut <- p*dpois(Blows,lam1)+(1-p)*dpois(Blows,lam2)
   NegLogLike <- -1*sum(log(LikeOut))
   return(NegLogLike)  
  }

 # fit the model using optim
 ret5 <- optim(par=c(10,10,0.4),fn = LL3,lower=c(0.01,0.01,0.00001),upper=c(Inf,Inf,0.99999),method="L-BFGS-B")
 print(ret5)

}


# =============================================================================================================================================

Part6 <- function()
{
 Years <- c(1950,1955,1960,1965,1970,1975,1980)
 Est <-    c(885,958,989,1132,1594,1027,959)
 
 par(mfrow=c(2,2))
 plot(Years,Est,xlab="Year",ylab="Estimates",ylim=c(0,max(Est)*1.05))
 
 # negative log-likelihood (efficient)
 LL6 <- function(N1950,rate,Sigma,Years,Est)
  {
   cat(N1950,rate,Sigma,"\n")
   Pred <- N1950*rate^(Years-1950)
   Like <- rep(0,length(Years))
   for (ii in 1:length(Years))
    Like[ii] <- 0.5*log(Sigma)+(log(Est[ii])-log(Pred[ii]))^2.0/(2.0*Sigma^2)
   LikeOut <- sum(Like)
   return(LikeOut)
  }

 # Fit the model
 ret6 <- mle(LL6,start=list(N1950=885,rate=1.01,Sigma=0.1),fixed=list(Years=Years,Est=Est),lower=c(0.01,-Inf,0.01),upper=c(Inf,1.05,0.3),method="L-BFGS-B")
 print(summary(ret6))
 ret6a <- lm(log(Est)~I(Years-1950))
 print(exp(coefficients(ret6a)[1]))
 print(coefficients(ret6a)[2])
  
 # negative log-likelihood (time-series version)  
 LL6B <- function(N1950,rate,Sigma,Years,Est)
  {
   ymax <- max(Years)
   Preds <- rep(0,length(ymax-1950+1))
   Preds[1] <- N1950
   for (ii in 1951:ymax)
    Preds[ii-1950+1] <-  Preds[ii-1950] * rate
   Like <- rep(0,length(Years))
   for (ii in 1:length(Years))
    Like[ii] <- 0.5*log(Sigma)+(log(Est[ii])-log(Preds[Years[ii]-1950+1]))^2.0/(2.0*Sigma^2)
   LikeOut <- sum(Like)
   return(LikeOut)
  }

 # fit the model
 ret6 <- mle(LL6B,start=list(N1950=885,rate=1.01,Sigma=0.1),fixed=list(Years=Years,Est=Est),lower=c(0.01,-Inf,0.01),upper=c(Inf,1.05,0.3),method="L-BFGS-B")
 print(summary(ret6))

}


# =============================================================================================================================================
Lecture3()
