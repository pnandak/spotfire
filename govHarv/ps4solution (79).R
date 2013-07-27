rm(list=ls())
library(foreign)
dat <- read.dta("leader.dta")

# q1

# liklihood function
lnfc <- function(lam,y){
-length(y)*log(lam)-((length(y)*mean(y))/lam)
}

# score function
score <- function(lam,y){
-(length(y)/lam)+ ((length(y)*mean(y))/lam^2)
}

# fisher information
fish <- function(lam,y){
-((length(y)/lam^2) - ((2*length(y)*mean(y))/lam^3))
}

# data
y <- dat$yearsinoffice

# optimize
out <- out<- optim(0.1, lnfc, y=y, method="L-BFGS-B",lower=0.01,
				control=list(fnscale=-1, trace=1), hessian=T)

# MLE
out$par

# Fisher
fish(lam=out$par,y=y)

# SE
se <- sqrt(solve(fish(lam=out$par,y=y))); se

# plot lnf
store <- c()
ruler <- seq(2,10,0.01)
for(i in 1:length(ruler)){
store[i] <- lnfc(lam=ruler[i],y=y)
}

pdf("fig1.pdf")
plot(ruler,store,xlab="lambda",ylab="Log L",type="l")
points(out$par,out$value,col="red",pch=19)

# quadratic approximation
store <- c()
ruler <- seq(2,10,0.01)
for(i in 1:length(ruler)){
store[i] <- lnfc(lam=out$par,y=y)-1/2*fish(lam=out$par,y=y)*(ruler[i]-out$par)^2
}
lines(ruler,store,col="blue",lty="dashed")
legend("bottomright",legend=c("true log L","quadratic approximation"),lty=c("solid","dashed"))
dev.off()

# wald ci
wald.ci <- c(out$par-1.96*se,out$par+1.96*se)

# liklihood based ci
wilks <- function(l){3.84-2*(lnfc(lam=out$par,y=y)-lnfc(lam=l,y=y))}
wilks.ci <- c(uniroot(wilks,interval=c(out$par-2,out$par))$root,
              uniroot(wilks,interval=c(out$par,out$par+2))$root)

# boostrap
M <- 1000
# boostrap samples
store <- replicate(M,y[sample(1:length(y),size=length(y),replace=T)])
# compute MLE in each sample: here binomial: # of successed / # elements
get.est <- function(x){
optim(0.1, lnfc, y=x, method="L-BFGS-B",lower=0.01,
				control=list(fnscale=-1))$par
}
# get ci
boot.ci <- quantile(apply(store,2,get.est),probs=c(0.025,0.975))

# plot
pdf("fig2.pdf")
hist(apply(store,2,get.est),breaks=50,xlab="theta",main=paste("Bootstrap CI ", round(boot.ci,3),"\n"))
abline(v=boot.ci,col="red")
dev.off()

# Zelig/Clarify confidence intervals
draws    <- rnorm(M,mean=out$par,sd=sqrt(solve(-1*out$hessian)))
zelig.ci <- quantile(draws,probs=c(0.025,0.975))

# plot distriobution of theta and ci
pdf("fig3.pdf")
hist(draws,breaks=50,xlab="theta",main=paste("Zelig Method CI ", round(zelig.ci,3),sep=""))
abline(v=zelig.ci,col="red")
dev.off()

# table summary
xtable(rbind(wald.ci,wilks.ci,boot.ci,zelig.ci),digits=3)


# approx
pdf("fig5.pdf")
x <- rexp(length(y),rate=1/out$par)
plot(ecdf(y),col.points="red",do.point=T,main="Ecdf and Model based CDF",xlab="lambda",pch=19)
plot(ecdf(x),add=T,col.points="blue",pch=".")
legend("bottomright",legend=c("ECDF","Model Based CDF"),pch=c(19,NA),lty=c(NA,"solid"),col=c("red","blue"))
dev.off()