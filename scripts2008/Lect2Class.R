Lecture2 <- function()
{
 # Part1()
 # Part2()


}

# =============================================================================================

Part1 <- function()
{
 Probs <- seq(from=0.01,0.99,by=0.01)
 
 
 par(mfrow=c(2,2))
 Like <- Probs^30*(1-Probs)^20
 plot(Probs,Like,type='l',lty=1,lwd=2,xlab="p",ylab="Likelihood",ylim=c(0,max(Like)*1.05),yaxs="i",xaxs="i")
 
 Ibest <- which(Like==max(Like),arr.ind=T)
 print(Probs[Ibest])
 
 par(mfrow=c(2,2))
 Like <- -30*log(Probs)-20*log(1-Probs)
 plot(Probs,Like,type='l',lty=1,lwd=2,xlab="p",ylab="Negative log-Likelihood",ylim=c(0,max(Like)*1.05),yaxs="i",xaxs="i")
 
 Ibest <- which(Like==min(Like),arr.ind=T)
 print(Probs[Ibest])
 
}

# =============================================================================================

Part2a <- function(Propn,Plot=F)
{
 TheData <- matrix(scan("C:\\Courses\\R Class\\Lectures\\Lect2a.dat",skip=1),byrow=T,ncol=2)
 TheData <- TheData[,2]

 Blow1 <- seq(from=1,to=40,by=1)
 Blow2 <- seq(from=1,to=60,by=1)
 Likes <- matrix(0,nrow=length(Blow1),ncol=length(Blow2))
 for (ii in 1:length(Blow1))
  for (jj in 1:length(Blow2))
   {
   LikeOut <- Propn*dpois(TheData,Blow1[ii])+(1-Propn)*dpois(TheData,Blow2[jj])
   Likes[ii,jj] <- -1*sum(log(LikeOut))
  }
 if (Plot==T)
  {
   par(mfrow=c(1,1))
   persp(x=Blow1,y=Blow2,z=Likes)
  } 
 Ibest <- which(Likes==min(Likes),arr.ind=T)

 cat(Propn,Blow1[Ibest[1]],Blow2[Ibest[2]],min(Likes),"\n")
 return(min(Likes))

}

# ============================================================================================

Part2 <- function()
{
 Part2a(0.5,Plot=T)

 Propns <- seq(from=0.05,to=0.5,by=0.025)
 Values <- rep(0,length(Propns))
 for (ii in 1:length(Propns))
  Values[ii] <- Part2a(Propns[ii],Plot=F)
  
 par(mfrow=c(2,2))
 plot(Propns,Values,xlab="Fraction",ylab="Negative log-likelihood",type='b',pch=16,lty=1,lwd=2)
 Ibest <- which(Values==min(Values),arr.ind=T)
 print(cbind(Propns,Values))
 print(Propns[Ibest])

}

# =============================================================================================

Lecture2()
