##### 85% of the following R code is from Peter Hoff's code for Gibbs sampling in the Bayesian class:
##### we want to use this to illustrate the basic idea of Gibbs sampling in the context of hierarchical model.
dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec8") 
setwd(dd)


###
# the simple example for infection rate:
# pdf("beta222.pdf",family="Times",height=4,width=8)
x<-seq(0, 1, length=1000)
y<-dbeta(x, 2, 20)
plot(x, y, type="l", xlab=expression(theta), ylab=expression(p(theta)), col="gray")
# dev.off()

cat("the mass under the density curve is between 3% and 15% is ", pbeta(.15, 2, 20)-pbeta(.03, 2, 20), "\n")

# pdf("beta450.pdf",family="Times",height=4,width=8)
x<-seq(0, 1, length=1000)
y<-dbeta(x, 4, 50)
plot(x, y, type="l", xlab=expression(theta), ylab=expression(p(theta)))
lines(x, dbeta(x, 2, 20), col="gray")
legend(.5, 8, legend=c("Prior distribution", "Posterior distribution"), col=c("gray", "black"), lty=1)
# dev.off()


# pdf("beta450b.pdf",family="Times",height=4,width=8)
x<-seq(0, 1, length=1000)
y<-dbeta(x, 4, 50)
plot(x, y, type="l", xlab=expression(theta), ylab=expression(p(theta)))
lines(x, dbeta(x, 2, 20), col="gray")
lines(x, dbeta(x, 12, 40), col="red")
legend(.5, 8, legend=c("Prior distribution", "Posterior distribution when y=2",
       "Posterior distribution when y=10"), col=c("gray", "black", "red"), lty=1)
# dev.off()



#####
##### a diagram for multilevel model: 
# pdf("fig8_2.pdf",family="Times",height=4,width=8)#, horizontal=F)
par(mar=c(0,0,0,0),mgp=c(0,0,0));par(mfrow=c(1,1))
plot(c(0,1),c(0,1),type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")

text( .5,.8 , expression(paste( mu ,",", tau )) )
text(  c(.2,.3,.5,.7,.8),  rep(.6,5),
        expression( theta[1],theta[2],".....",theta[J-1],theta[J]) )
segments(  rep(.5,4),rep(.75,4), c(.2,.3,.7,.8),rep(.65,4) )
text( c(.2,.3,.5,.7,.8), rep(.4,5),
       expression( bar(y)[1], bar(y)[2],".....",bar(y)[J-1],bar(y)[J]) )
segments(  c(.2,.3,.7,.8),rep(.55,4), c(.2,.3,.7,.8),rep(.45,4) )
segments(  rep(.5,4),rep(.25,4), c(.2,.3,.7,.8),rep(.35,4) )
text( .5,.2,expression(sigma))
# dev.off()
#####


##### the Gibbs sampler example for today: 
#####
# pdf("fig8_3.pdf",family="Times",height=6,width=6)
par(mar=c(0,0,0,0),mgp=c(0,0,0))

library(MASS)
dat<-nlschools
J<-length(unique(dat$class))
classes<-sort(unique(dat$class))
class<-match( dat$class, classes)

Y<-list()
s2<-n<-ybar<-rep(0,J)
for(j in 1:J) {
Y[[j]]<- dat$IQ [ class==j ]
ybar[j]<-mean(Y[[j]])
n[j]<-sum(class==j)
s2[j]<-var( Y[[j]])
                }

##
## some visual summary of the data:
layout( matrix( c(1,1,1,2,3,4),2,3,byrow=T) )
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(c(1,J),range(Y) ,type="n",xlab="school",ylab="verbal IQ")
for(l in 1:J)  {
j<-order(ybar)[l]
points( rep(l,n[j]), Y[[j]],col=j,pch=1 )
points( l, ybar[j],cex=1.5,col=j,pch=16)
segments( l,min(Y[[j]]),l,max(Y[[j]]),col=j)
 }
abline(h=sum(ybar*n)/sum(n))
abline(h=mean(ybar))

hist(ybar,prob=T,nclass=15,main="",xlab=expression(bar(y)))
hist(sqrt(s2), prob=T, nclass=15, main="",xlab="s")
plot(table(n)/length(n),type="h", ylab="p(n)")
# dev.off()
#####

##### 
# pdf("fig8_4.pdf",family="Times",height=3,width=7)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(n,ybar,ylab=expression(bar(y)))
plot(n,sqrt(s2),ylab="s")
# dev.off()
#####


##### MCMC analysis for Netherlands schools data

## empirical Bayes-type priors
nu0<-2  ; s20<-mean(s2)
k0<-1   ; mu0<-mean(ybar) 
eta0<-2 ; t20<-var(ybar)
##


## starting values
theta<-ybar
sigma<-sqrt(s20)
tau<-sqrt(t20)
mu<-mean(ybar)
##

## Gibbs sampling 
set.seed(1)
nmc<-5000
THETA<-matrix( nrow=nmc,ncol=J)
MTS<-matrix( nrow=nmc,ncol=3)

for(ns in 1:nmc) {

# sample new values of thetas
for(j in 1:J) {
theta.hat.j<- (ybar[j]*n[j]/sigma^2 + mu/tau^2)/(n[j]/sigma^2+1/tau^2)
sigma.j<- 1/sqrt(n[j]/sigma^2+1/tau^2)
theta[j]<-rnorm(1,theta.hat.j,sigma.j)
               }

# sample new value of sigma
nun<- nu0+sum(n)
ss<- nu0*s20
for(j in 1:J) { ss<-ss+ sum( (Y[[j]]-theta[j])^2 ) }
sigma<-1/sqrt(rgamma(1,nun/2,ss/2))

# sample a new value of mu
mu<-rnorm(1, (J*mean(theta) + k0*mu0 )/(J+k0), tau/sqrt(J+k0) )

# sample a new value of tau
etaJ<-eta0+1+J
ss<- eta0*t20 + k0*(mu-mu0)^2 + sum( (theta-mu)^2 )
tau<-1/sqrt(rgamma(1,etaJ/2,ss/2))

# store results
THETA[ns,]<-theta
MTS[ns,]<-c(mu,tau,sigma)
                       }
#####



#####
# pdf("mcmc.pdf",family="Times",height=5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(3,1))
subscan<-(1:1000)*5
plot(subscan,MTS[subscan,1],xlab="scan",ylab=expression(mu),type="l")
plot(subscan,MTS[subscan,2],xlab="scan",ylab=expression(tau),type="l")
plot(subscan,MTS[subscan,3],xlab="scan",ylab=expression(sigma),type="l")
# dev.off()
#####



#####
# pdf("mcmc_2.pdf",family="Times",height=5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(3,1))
subscan<-(4900:5000)
plot(subscan,MTS[subscan,1],xlab="scan",ylab=expression(mu),type="l")
abline(h=mean( MTS[subscan,1]),lty=2)
plot(subscan,MTS[subscan,2],xlab="scan",ylab=expression(tau),type="l")
abline(h=mean( MTS[subscan,2]),lty=2)
plot(subscan,MTS[subscan,3],xlab="scan",ylab=expression(sigma),type="l")
abline(h=mean( MTS[subscan,3]),lty=2)
# dev.off()
#####
