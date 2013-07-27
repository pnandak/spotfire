profilellhfunc <- function(mu,sigma,y){
  sum(dnorm(y,mean=mu,sd=sigma,log=TRUE))
}

## read pvote
data <- read.table(file="pvote00.asc",header=TRUE,sep=",")
n <- length(data$pvote00)
mles <- c(mean(data$pvote00),
          var(data$pvote00)*(n-1)/n)

sigmaSeq <- seq(.015,.035,length=501)
m <- length(sigmaSeq)

maxLogLike <- profilellhfunc(mu=mles[1],
                             sigma=sqrt(mles[2]),
                             y=data$pvote00)

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
     xlab=expression(sigma^2),
     ylab="conditional log-likelihood",
     type="l")
abline(v=mles[2])
abline(h=maxLogLike[2])
LogLikeCrit <- maxLogLike - 2*qchisq(.95,df=1)
abline(h=LogLikeCrit,lty=2)

dev.off()                    ## close the pdf graph file
