library(stats4)

Lecture4 <- function()
{

# Part1()
# Part2()
# Part3()
# Part4()
# Part5()

}

# =========================================================================================

Part1 <- function()
{
 # Plot the fit
 PlotRes <- function(Linf,Kappa,Sigma,Age,Length)
  {
   Age2 <- seq(from=0,to=30,by=0.2)
   Pred <- Linf*(1-exp(-Kappa*Age2))
   ymax <- max(Pred,Length)*1.05
   par(mfrow=c(2,2))
   plot(Age,Length,pch=16,ylab="Length",xlab="Age",ylim=c(0,ymax))
   lines(Age2,Pred,lty=1,lwd=2)
  }

 # Negative log-likelihood
 LL4a <- function(Linf,Kappa,Sigma,Age,Length)
 {
  Pred <- Linf*(1-exp(-Kappa*Age))
  Like <- dnorm(Length,Pred,Sigma)
  LikeOut <- -1*sum(log(Like))
  return(LikeOut)
 }

# use uniroot 
LL4b <- function(Linf,Age,Length,LBest,target)
 {
  ret4a <- mle(LL4a,start=list(Kappa=0.2,Sigma=10),fixed=list(Linf=Linf,Age=Age,Length=Length),lower=c(0.01,0.01),upper=c(Inf,Inf),method="L-BFGS-B")
  LikeOut <- -1*logLik(ret4a)[1]
  return(LikeOut-LBest-target)
 }

 # Extract the data
 TheData <- matrix(scan("C:\\Courses\\R Class\\Lectures\\Lect2b.dat",skip=1),byrow=T,ncol=3)
 Sex <- TheData[,1]
 Use <- Sex == 2
 Age <- TheData[Use,2]
 Length <- TheData[Use,3]

 # Fit the model, record the negative log-likelihood, and plot the fit
 ret4 <- mle(LL4a,start=list(Linf=100,Kappa=0.2,Sigma=10),fixed=list(Age=Age,Length=Length),lower=c(0.01,0.01,0.01),upper=c(Inf,Inf,Inf),method="L-BFGS-B")
 BestL <- -1*logLik(ret4)[1]
 print(summary(ret4))
 ret4 <- coef(ret4)
 PlotRes(ret4[1],ret4[2],ret4[3],Age,Length)
 
 # Loop across L-infinity and record the negative log-likelihood
 Linfs <- seq(from=75,to=90,by=0.5)
 Likes <- NULL
 for (ii in 1:length(Linfs))
  {
   ret4a <- mle(LL4a,start=list(Kappa=0.2,Sigma=10),fixed=list(Linf=Linfs[ii],Age=Age,Length=Length),lower=c(0.01,0.01),upper=c(Inf,Inf),method="L-BFGS-B")
   Likes <- c(Likes,-1*logLik(ret4a)[1])
  } 
 ymax <- max(Likes-BestL)*1.05
 plot(Linfs,Likes-BestL,xlab="Linf",ylab="Negative Log-likelihood",ylim=c(0,ymax),type='l',lty=1,lwd=2) 

 # Plot!
 par(mfrow=c(2,2))
 plot(Linfs,Likes-BestL,xlab="Linf",ylab="Negative Log-likelihood",ylim=c(0,ymax),type='l',lty=1,lwd=2) 
 abline(h=1.92,lty=3,lwd=2)
 
 # Compute the CI using uniroot. 
 resa <- uniroot(LL4b,lower=40,upper=ret4[1],Age=Age,Length=Length,target=1.92,LBest=BestL)
 part1 <- resa$root
 resa <- uniroot(LL4b,lower=ret4[1],upper=90,Age=Age,Length=Length,target=1.92,LBest=BestL)
 part2 <- resa$root
 arrows(part1,4,part1,1.92,length=0.1,lwd=3)
 arrows(part2,4,part2,1.92,length=0.1,lwd=3)

}

# =========================================================================================

Part2 <- function()
{
 func <- function(x,a) return(x-a)

resa <- uniroot(func,lower=-1,upper=10,a=5)
print(resa)
}

# =========================================================================================

Part3 <- function()
{

 # Specify the data
 Years <- c(1950,1955,1960,1965,1970,1975,1980)
 Est <-    c(885,958,989,1132,1594,1027,959)

 # Plot observed and predicted
 PlotRes <- function(N1950,rate,Sigma,Years,Est)
  {
   ymax <- max(Years)
   Preds <- rep(0,length(ymax-1950+1))
   Preds[1] <- N1950
   for (ii in 1951:ymax)
    Preds[ii-1950+1] <-  Preds[ii-1950] * rate
   ymaxx <- max(Preds,Est)
   plot(seq(from=1950,to=ymax),Preds,xlab="Year",ylab="Population size",type='l',lwd=2,lty=1,ylim=c(0,ymaxx*1.05))
   points(Years,Est)
  }

 # Compute the likelihood
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

 # Function to be solved to find a confidence limit
 LikeP <- function(N1950,Years,Est,LBest,target)
  {
   ret6a <- mle(LL6B,start=list(rate=1.01,Sigma=0.3),fixed=list(N1950=N1950,Years=Years,Est=Est),lower=c(-Inf,0.01),upper=c(1.05,0.3),method="L-BFGS-B")
   LikeOut <- -1*logLik(ret6a)[1]
   return(LikeOut-LBest-target)
  }

 # Find the MLE and plot the fit
 ret6 <- mle(LL6B,start=list(N1950=885,rate=1.01,Sigma=0.1),fixed=list(Years=Years,Est=Est),lower=c(0.01,-Inf,0.01),upper=c(Inf,1.05,0.3),method="L-BFGS-B")
 BestL <- -1*logLik(ret6)[1]
 print(summary(ret6))
 Ret6c <- coef(ret6)

 par(mfrow=c(2,2))
 PlotRes(Ret6c[1],Ret6c[2],Ret6c[3],Years,Est)
 
 # Create the profile
 Vals <- seq(from=400,to=2000,by=100)
 Likes <- NULL
 for (ii in 1:length(Vals))
  {
   ret6a <- mle(LL6B,start=list(rate=1.01,Sigma=0.1),fixed=list(N1950=Vals[ii],Years=Years,Est=Est),lower=c(-Inf,0.01),upper=c(1.05,0.3),method="L-BFGS-B")
   Likes <- c(Likes,-1*logLik(ret6a)[1])
  } 
 ymax <- max(Likes-BestL)*1.05

 # Plot the profile
 plot(Vals,Likes-BestL,xlab="N1950",ylab="Negative Log-likelihood",ylim=c(0,ymax),type='l',lty=1,lwd=2) 
 abline(h=1.92,lty=3,lwd=2)

 # Compute the confidence intervals
 resa <- uniroot(LikeP,lower=400,upper=Ret6c[1],Years=Years,Est=Est,target=1.92,LBest=BestL)
 part1 <- resa$root
 resa <- uniroot(LikeP,lower=Ret6c[1],upper=2000,Years=Years,Est=Est,target=1.92,LBest=BestL)
 part2 <- resa$root
 cat("95% CI ",part1,part2,"\n")
 arrows(part1,4,part1,1.92,length=0.1,lwd=3)
 arrows(part2,4,part2,1.92,length=0.1,lwd=3)

}

# =========================================================================================

Part4 <- function()
{
 P1hat <- 30/50
 P2hat <- 10/50
 LogLike1 <- 30*log(P1hat)+20*log(1-P1hat)+10*log(P2hat)+40*log(1-P2hat)
 print(LogLike1)
 P1hat <- 40/100
 P2hat <- 40/100
 LogLike2 <- 30*log(P1hat)+20*log(1-P1hat)+10*log(P2hat)+40*log(1-P2hat)
 print(LogLike2)
 print(LogLike1-LogLike2)

}
# =========================================================================================

Part5 <- function()
{

 # negative log-likelihood for the model in which the parameters are sex-specific
 LLB <- function(Linf1,Kappa1,Linf2,Kappa2,Sigma,Age,Length,Sex)
 {
  Obs1 <- Length[Sex==1]
  Pred1 <- Linf1*(1-exp(-Kappa1*Age[Sex==1]))
  Obs2 <- Length[Sex==2]
  Pred2 <- Linf2*(1-exp(-Kappa2*Age[Sex==2]))
  Like1 <- dnorm(Obs1,Pred1,Sigma)
  Like2 <- dnorm(Obs2,Pred2,Sigma)
  LikeOut <- -1*(sum(log(Like1))+sum(log(Like2)))
  return(LikeOut)
 }
 
 # negative log-likelihood for the model in which the parameters are independent of sex
 LLA <- function(Linf,Kappa,Sigma,Age,Length,Sex)
 {
  Obs1 <- Length[Sex==1]
  Pred1 <- Linf*(1-exp(-Kappa*Age[Sex==1]))
  Obs2 <- Length[Sex==2]
  Pred2 <- Linf*(1-exp(-Kappa*Age[Sex==2]))
  Like1 <- dnorm(Obs1,Pred1,Sigma)
  Like2 <- dnorm(Obs2,Pred2,Sigma)
  LikeOut <- -1*(sum(log(Like1))+sum(log(Like2)))
  return(LikeOut)
 }

 # extract the data
 TheData <- matrix(scan("C:\\Courses\\R Class\\Lectures\\Lect2b.dat",skip=1),byrow=T,ncol=3)
 Sex <- TheData[,1]
 Age <- TheData[,2]
 Length <- TheData[,3]

 # fit the two models
 modelB <- mle(LLB,start=list(Linf1=100,Kappa1=0.2,Linf2=100,Kappa2=0.2,Sigma=10),fixed=list(Age=Age,Length=Length,Sex=Sex),lower=c(0.01,0.01,0.01,0.01,0.01),upper=c(Inf,Inf,Inf,Inf,Inf),method="L-BFGS-B")
 print(summary(modelB))
 print(-logLik(modelB)[1])
 modelA <- mle(LLA,start=list(Linf=100,Kappa=0.2,Sigma=10),fixed=list(Age=Age,Length=Length,Sex=Sex),lower=c(0.01,0.01,0.01,0.01,0.01),upper=c(Inf,Inf,Inf,Inf,Inf),method="L-BFGS-B")
 print(summary(modelA))
 print(-logLik(modelA)[1])

}

# =========================================================================================
Lecture4()
