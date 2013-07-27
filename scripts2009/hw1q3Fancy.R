####################################################################
## ps150c/350c, 2007, hw1
## R commands to draw the picture required for Q3
##	$Id: hw1q3Fancy.R 98 2007-04-13 01:28:36Z jackman $
##
####################################################################

profilellhfunc <- function(mu,sigma,y){
  sum(dnorm(y,mean=mu,sd=sigma,log=TRUE))
}

## read pvote
data <- read.table(file="pvote00.asc",header=TRUE,sep=",")
n <- length(data$pvote00)
mles <- c(mean(data$pvote00),
          var(data$pvote00)*(n-1)/n)

maxLogLike <- profilellhfunc(mu=mles[1],
                             sigma=sqrt(mles[2]),
                             y=data$pvote00)
LogLikeCrit <- maxLogLike - 2*qchisq(.95,df=1)
objFunc <- function(sigma,mu,y,k){
  profilellhfunc(mu=mu,sigma=sqrt(sigma),y=y)-k
}
bounds <- rep(NA,2)
bounds[1] <- uniroot(objFunc,
                     lower=.015,
                     upper=.020,
                     mu=mles[1],
                     y=data$pvote00,
                     k=LogLikeCrit,
                     tol=.Machine$double.eps^(3/4))$root
bounds[2] <- uniroot(objFunc,
                     lower=.025,
                     upper=.030,
                     mu=mles[1],
                     y=data$pvote00,
                     k=LogLikeCrit,
                     tol=.Machine$double.eps^(3/4))$root


sigmaSeq <- seq(bounds[1]-.002,
                bounds[2]+.003,
                length=501)
m <- length(sigmaSeq)

llh <- rep(NA,m)       ## we'll write values of the log-likelihood into this
## loop over the grid
for(j in 1:m){
  llh[j] <- profilellhfunc(mu=mles[1],  ## mean
                           sigma=sqrt(sigmaSeq[j]),  ## standard deviation
                           y=data$pvote00)
}

pdf(file="hw1q3.pdf")       ## make a graph as pdf
par(las=1)
plot(sigmaSeq,
     llh,
     ylim=c(min(llh),maxLogLike),
     xlab=expression(sigma^2),
     ylab="conditional log-likelihood",
     type="l",
     lwd=3,
     axes=FALSE)
axis(1)
yTicks <- sort(c(pretty(llh,n=7),
                 LogLikeCrit,maxLogLike))     
axis(2,
     cex.axis=.65,
     at=c(yTicks),
     labels=as.character(round(yTicks,2)))
abline(v=mles[2])
abline(h=maxLogLike)
abline(h=LogLikeCrit,lty=2)
abline(v=bounds,lty=2)
dev.off()                    ## close the pdf graph file
