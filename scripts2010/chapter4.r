#####
pdf("fig4_1.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,.25,1),mgp=c(1.75,.75,0))
par(mfrow=c(2,3))
set.seed(1)
a<-68 ; b<-45
set.seed(1)
theta.support<-seq(0,3,length=100)
theta.sim10<-rgamma(10,a,b)
theta.sim100<-rgamma(100,a,b)
theta.sim1000<-rgamma(1000,a,b)

xlim<-c(.75,2.25)
ylim=c(0,2.5)
lty=1

hist( theta.sim10, prob=T,xlim=xlim,ylim=ylim,xlab="",main="",ylab="")
lines(theta.support,dgamma(theta.support,a,b),col="gray",lwd=2,lty=lty)
text(2.1,2.25,expression(paste(italic(S),"=10",sep="")))

hist( theta.sim100, prob=T,xlim=xlim,ylim=ylim,xlab="",main="" ,ylab="")
lines(theta.support,dgamma(theta.support,a,b),col="gray",lwd=2,lty=lty)
text(2.1,2.25,expression(paste(italic(S),"=100",sep="")))



hist( theta.sim1000, prob=T,xlim=xlim,ylim=ylim,xlab="",main="" ,ylab="")
lines(theta.support,dgamma(theta.support,a,b),col="gray",lwd=2,lty=lty)
text(2.1,2.25,expression(paste(italic(S),"=1000",sep="")))


plot(density(theta.sim10),xlim=xlim,ylim=ylim,xlab=expression(theta),main="",ylab="")
lines(theta.support,dgamma(theta.support,a,b),col="gray",lwd=2,lty=lty)

plot(density(theta.sim100),xlim=xlim,ylim=ylim,xlab=expression(theta),main="",ylab="")
lines(theta.support,dgamma(theta.support,a,b),col="gray",lwd=2,lty=lty)

plot(density(theta.sim1000),xlim=xlim,ylim=ylim,xlab=expression(theta),main="",ylab="")
lines(theta.support,dgamma(theta.support,a,b),col="gray",lwd=2,lty=lty)
dev.off()
#####

set.seed(1)
a<-2  ; b<-1
sy<-66; n<-44

theta.sim10<-rgamma(10,a+sy,b+n)
theta.sim100<-rgamma(100,a+sy,b+n)
theta.sim1000<-rgamma(1000,a+sy,b+n)

(a+sy)/(b+n) 

mean(theta.sim10)
mean(theta.sim100)
mean(theta.sim1000)

pgamma(1,75,a+sy,b+n)

mean( theta.sim10<1.75)
mean( theta.sim100<1.75)
mean( theta.sim1000<1.75)

qgamma(c(.025,.975),a+sy,b+n)
quantile( theta.sim10, c(.025,.975))
quantile( theta.sim100, c(.025,.975))
quantile( theta.sim1000, c(.025,.975))



######
pdf("fig4_2.pdf",family="Times",height=1.75,width=5)
par(mfrow=c(1,3),mar=c(2.75,2.75,.5,.5),mgp=c(1.70,.70,0))

set.seed(1)
a<-2   ; b<-1
sy<-66 ; n<-44

nsim<-1000
theta.sim<-rgamma(nsim,a+sy,b+n)

#cumulative mean

cmean<-cumsum(theta.sim)/(1:nsim)
cvar<- cumsum(theta.sim^2)/(1:nsim) - cmean^2
ccdf<- cumsum(theta.sim<1.75)/ (1:nsim)
cq<-NULL
for(j in 1:nsim){ cq<-c(cq,quantile(theta.sim[1:j],probs=0.975)) }

sseq<- c(1,(1:100)*(nsim/100))
cmean<-cmean[sseq] 
cq<-cq[sseq] 
ccdf<-ccdf[sseq] 

plot(sseq,cmean,type="l",xlab="# of Monte Carlo samples",ylab="cumulative mean",
     col="black")
abline(h= (a+sy)/(b+n),col="gray",lwd=2)

plot(sseq,ccdf,type="l",xlab="# of Monte Carlo samples",ylab="cumulative cdf at 1.75",col="black")
abline(h= pgamma(1.75,a+sy,b+n),col="gray",lwd=2)

plot(sseq,cq,type="l",xlab="# of Monte Carlo samples",ylab="cumulative 97.5% quantile",col="black")
abline(h= qgamma(.975,a+sy,b+n),col="gray",lwd=2)

dev.off()
######




#####
source("../Data/alldata.r")
table(Y$DEG[Y$YEAR==1998])
y1<-Y$PRAYER[Y$YEAR==1998 & Y$RELIG==1 ]
y1<-1*(y1==1)
y1<-y1[!is.na(y1) ]
sy1<-sum(y1)
n1<-length(y1)
sy1/n1

y2<-Y$PRAYER[Y$YEAR==1998 & Y$RELIG!=1 ]
y2<-1*(y2==1)
y2<-y2[!is.na(y2) ]
sy2<-sum(y2)
n2<-length(y2)
sy2/n2


table(Y$FEMALE[Y$YEAR==1998])
y<-Y$FEMALE[Y$YEAR==1998]
y<-1*(y==1)
y<-y[!is.na(y) ]
sy<-sum(y)
n<-length(y)
sy/n


sy<-sy2
n<-n2


###



set.seed(1)

a<-1 ; b<-1 
theta.prior.sim<-rbeta(10000,a,b)
gamma.prior.sim<- log( theta.prior.sim/(1-theta.prior.sim) )

n0<-860-441 ; n1<-441
theta.post.sim<-rbeta(10000,a+n1,b+n0)
gamma.post.sim<- log( theta.post.sim/(1-theta.post.sim) )

pdf("fig4_3.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(2,3))
par(cex=.8)

par(mfrow=c(1,2),mar=c(3,3,1,1), mgp=c(1.75,.75,.0))
plot(density(gamma.prior.sim,adj=2),xlim=c(-5,5),main="", xlab=expression(gamma),
    ylab=expression(italic(p(gamma))),col="gray")
plot(density(gamma.post.sim,adj=2),xlim=c(-5,5),main="",xlab=expression(gamma),
#  ylab=expression(italic(paste("p(",gamma,"|",y[1],"...",y[n],")")))) 
ylab=expression(paste(italic("p("),gamma,"|",y[1],"...",y[n],")",
   sep="")) )
lines(density(gamma.prior.sim,adj=2),col="gray")

dev.off()
#####


#####
set.seed(1)
a<-2 ; b<-1
sy1<-217 ;  n1<-111
sy2<-66  ;  n2<-44

theta1.mc<-rgamma(10000,a+sy1, b+n1)
theta2.mc<-rgamma(10000,a+sy2, b+n2)

y1.mc<-rpois(10000,theta1.mc)
y2.mc<-rpois(10000,theta2.mc)


mean(theta1.mc>theta2.mc)

mean(y1.mc>y2.mc)


pdf("fig4_4.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,1))
plot(density(theta1.mc/theta2.mc,adj=2),main="",xlim=c(.75,2.25),
xlab=expression(gamma==theta[1]/theta[2]),
#ylab=expression(italic(paste("p(",gamma,"|",bold(y[1]),",",bold(y[2]),")",
#   sep="")) ))
ylab=expression(paste(italic("p("),gamma,"|",bold(y[1]),",",bold(y[2]),")",
   sep="")) )

dev.off()

pdf("fig4_5.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,1))

diff.mc<-y1.mc-y2.mc

ds<- -11:11
plot(ds,(table(c(diff.mc,ds))-1)/length(diff), type="h",lwd=3,
    xlab=expression(italic(D==tilde(Y)[1]-tilde(Y)[2])),
  ylab=expression(paste(italic("p(D"),"|",bold(y[1]),",",bold(y[2]),")",sep="")))
dev.off()
#####


##### 


######
y1<-Y$CHILDS[Y$FEMALE==1 &  Y$YEAR>=1990  & Y$AGE==40 & Y$DEG<3 ]
y1<-y1[!is.na(y1)]

##
set.seed(1)

a<-2 ; b<-1
t.mc<-NULL
for(s in 1:10000) {
theta1<-rgamma(1,a+sum(y1), b+length(y1))
y1.mc<-rpois(length(y1),theta1)
t.mc<-c(t.mc,sum(y1.mc==2)/sum(y1.mc==1))
                    }

t.obs<-sum(y1==2)/sum(y1==1)
mean(t.mc>=t.obs)


#a<-2 ; b<-1
#t.mc<-NULL
#for(s in 1:10000) {
#theta1<-rgamma(1,a+sum(y1), b+length(y1))
#y1.mc<-rpois(length(y1),theta1)
#t.mc<-rbind(t.mc,c(mean(y1.mc),var(y1.mc) ))
#                    }





##
pdf("fig4_6.pdf",family="Times",height=3.5,width=7)

par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))

ecdf<-(table(c(y1,0:9))-1 )/sum(table(y1))
#ecdf.mc<-(table(c(y1.mc,0:9))-1 )/sum(table(y1.mc))
ecdf.mc<- dnbinom(0:9,size=a+sum(y1),mu=(a+sum(y1))/(b+length(y1)))
plot(0:9+.1,ecdf.mc,type="h",lwd=5,xlab="number of children",
     ylab=expression(paste("Pr(",italic(Y[i]==y[i]),")",sep="")),col="gray",
      ylim=c(0,.35))
points(0:9-.1, ecdf,lwd=5,col="black",type="h")

legend(1.8,.35,
     legend=c("empirical distribution","predictive distribution"),
     lwd=c(2,2),col=
       c("black","gray"),bty="n",cex=.8)


hist(t.mc,prob=T,main="",ylab="",xlab=expression(t(tilde(Y))) )

segments(t.obs,0,t.obs,.25,col="black",lwd=3)
dev.off()


