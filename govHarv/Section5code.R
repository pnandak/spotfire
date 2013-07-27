setwd("C:/Documents and Settings/Jank/My Documents/Uniraps/Gov 2001 TF/Section5")


# normal example large and small curvature

# take two samples from standart normal
x.small  <- mvrnorm (4, 0, 1,empirical=T)
x.large  <- mvrnorm (100, 0, 1,empirical=T)

# log likelihood
lnfn<-function(mu,x){-1/2*sum((x - mu)^2)}
# score function
scorefn<-function(mu,x){-sum(x - mu)}
# fisher information
fishinfo<-function(mu,x){length(x)}

# evaluate the log likelihood at different mu candiadte values
l.small <- l.large <- c()
ruler<- seq(-1, 1, by=0.01)
for(i in 1:length(ruler)){
l.small[i]<- lnfn(ruler[i], x=x.small)
l.large[i]<- lnfn(ruler[i], x=x.large)
}

# Ml Estimates
mu.hat.s <- ruler[which(l.small==max(l.small))]
mu.hat.l <- ruler[which(l.large==max(l.large))]

# fisher information at MLE
fishinfo(mu.hat.s,x=x.small)
fishinfo(mu.hat.l,x=x.large)

# plot log lik hoods
pdf("fig1.pdf")
plot(l.small~ruler, type="l", col="blue", lwd=2,ylab="Ln L",xlab="theta",lty="solid",main=c("ln L for Two Stylized Normal Samples"))
lines(I(l.large + abs(max(l.small) - max(l.large)))~ruler, col="red", lwd=2,lty="solid")
legend("topright",legend=c("n=4, I(mu)=4","n=100, I(mu)=100"),lty=c("solid","solid"),col=c("blue","red"),cex=.7)
dev.off()

# binomial example
x.small <- c(rep(1,8),rep(0,2))
x.large <- c(rep(1,80),rep(0,20))

# binomial ll
lnfn<-function(mu,x){sum(x)*log(mu)+(length(x)-sum(x))*(log(1-mu))}
# fisher information
fishinfo<-function(mu,x){length(x)/(mu*(1-mu))}

# evaluate the log likelihood at different mu candiadte values
l.small <- l.large <- i.small <- i.large <- c()
ruler<- seq(0, 1, by=0.01)
for(i in 1:length(ruler)){
l.small[i]<- lnfn(ruler[i], x=x.small)
l.large[i]<- lnfn(ruler[i], x=x.large)
i.small[i]<- -1/2*fishinfo(mu=0.8, x=x.small)*(ruler[i]-0.8)^2 # This evaluates the quadratic approximation
i.large[i]<- -1/2*fishinfo(mu=0.8, x=x.large)*(ruler[i]-0.8)^2
}

pdf("fig2.pdf")
plot(l.small~ruler, type="l", col="blue", lwd=2,ylab="Ln L",xlab="theta",lty="solid",main="Two Binomial Samples, theta=.8")
lines(I(l.large + abs(max(l.small) - max(l.large)))~ruler, col="red", lwd=2,lty="solid")
lines((max(l.small)+i.small)~ruler,col="blue",lty="dashed")
lines((max(l.large)+i.large+ abs(max(l.small) - max(l.large)))~ruler,col="red",lty="dashed")
legend("topleft",legend=c("n=4: true ln L","n=100: true ln L","n=4: Quad Approx","n=100: Quad. Approx"),lty=c("solid","solid","dashed","dashed"),col=c("blue","red"),cex=.8)
dev.off()

# bootstrap confidence intervals

# number of boostraps
M <- 5000
# boostrap samples
store.s <- replicate(M,x.small[sample(1:length(x.small),size=length(x.small),replace=T)])
store.l <- replicate(M,x.large[sample(1:length(x.large),size=length(x.large),replace=T)])
# compute MLE in each sample: here binomial: # of successed / # elements
est.s <- apply(store.l,2,sum)/length(x.small)
est.l <- apply(store.l,2,sum)/length(x.large)

# get confidence intervals
ci.s <- quantile(est.s,probs=c(0.025,0.975)); ci.s
ci.l <- quantile(est.l,probs=c(0.025,0.975)); ci.l

# plot
pdf("fig3.pdf")
hist(apply(store.l,2,sum)/length(x.large),breaks=50,xlab="theta",main="Bootstrap CI=[.71;.87]; Binomial n=100")
abline(v=ci.l,col="red")
dev.off()

##########################################
# Zelig/Clarify confidence intervals

# example 1 (no parametrization)

# binomal data
x <- c(rep(1,80),rep(0,20))
# binomial log lik function
lnfn<-function(mu,x){
sum(x)*log(mu)+(length(x)-sum(x))*(log(1-mu))
}
# optimize (search space constrained)
out <- optim(0.5,lnfn,x=x,method="L-BFGS-B",
             control=list(fnscale=-1,REPORT=1,trace=T),
             hessian=T,lower=0.01,upper=.99)

# MLE
theta.hat <- out$par; theta.hat

# Extract Fisher Information Matrix
FI <- -1*out$hessian; FI

# Invert FI to get Variance-Covariance Matrix
FI <- solve(FI)

# Draw M times theta from the normal approximation to the log lik.
M <- 1000
draws <- rnorm(M,mean=theta.hat,sd=sqrt(FI)) # notice we take sqrt here because rnorm takes sd not var

# confidence intervals
ci <- quantile(draws,probs=c(0.025,0.975)); ci

# plot distriobution of theta and ci
pdf("fig4.pdf")
hist(draws,breaks=50,xlab="theta",main="Zelig CI=[.72;.88]; Binomial n=100")
abline(v=ci,col="red")
dev.off()

# example 2: with re-parametrization
lnfn<-function(mu,x){
mu <- pnorm(mu) # reparametrisation to map mu to be between 0 and 1
out <- sum(x)*log(mu)+(length(x)-sum(x))*(log(1-mu))
}
# optimize
out <- optim(0,lnfn,x=x,method="BFGS",
             control=list(fnscale=-1,REPORT=1,trace=T),hessian=T)

# MLE
theta.hat <- out$par; theta.hat

# Draw M times theta from the normal approximation to the log lik.
draws <- rnorm(M,mean=theta.hat,sd=sqrt(solve(-out$hessian))) # notice we take sqrt here because rnorm takes sd not var

# now undo reparametrization
theta = pnorm(draws)

# correct confidence intervals
ci <- quantile(theta,probs=c(0.025,0.975)); ci

# don't do:
quantile(draws,probs=c(0.025,0.975))
# because you care about mu not pnorm(mu)

