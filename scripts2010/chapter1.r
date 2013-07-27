####
a<-2 ; b<-20
a/(a+b)
(a-1)/(a-1+b-1)
pbeta(.20,a,b) - pbeta(.05,a,b)
pbeta(.10,a,b)


pdf("fig1_1.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))

 dbinom(0,20,.05)
n<-20
x<-0:n
del<-.25
plot( range(x-del), c(0,.4),xlab="number infected in the sample",
     ylab="probability",type="n")

points( x-del,dbinom(x,n,.05),type="h",col=gray(.75),lwd=3)
points( x,dbinom(x,n,.10),type="h",col=gray(.5),lwd=3)
points( x+del,dbinom(x,n,.20),type="h",col=gray(0),lwd=3)
legend(10,.35,legend=c(
    expression(paste(theta,"=0.05",sep="")), 
    expression(paste(theta,"=0.10",sep="")),
    expression(paste(theta,"=0.20",sep="")) ),
     lwd=c(3,3,3), 
    col=gray(c(.75,.5,0)) ,bty="n") 

a<-2 ; b<-20
y<-0 ; n<-20


(a+y)/(a+b+n)
(a+y-1)/(a-1+b+n-1)
pbeta(.20,a+y,b+n-y) - pbeta(.05,a+y,b+n-y)
pbeta(.10,a+y,b+n-y)




theta<-seq(0,1,length=500)
plot(theta, dbeta(theta,a+y,b+n-y),
     type="l",
     xlab="percentage infected in the population",
     ylab="", lwd=2, ylim=c(0,16)
     )
lines(theta, dbeta(theta,a,b),col="gray",lwd=2)
legend(.5,14,legend=c( expression(paste(italic("p"),"(",theta,")",sep="")), 
     expression(paste(italic("p"),"(",theta,"|",italic("y"),")",sep=""))  ), 
  bty="n", lwd=c(2,2),col=c("gray","black"))


dev.off()

(a+y)/(b+n-y)
(a+y-1)/(a+y-1+b+n-y-1)
pbeta(.20,a+y,b+n-y) - pbeta(.05,a+y,b+n-y)
pbeta(.10,a+y,b+n-y)


#### sensitivity analysis


pdf("fig1_2.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))
g<-50
th0<-seq(0.01,.5,length=g)
nu0<-seq(1,25,length=g) 

PP10<-PM<-PLQ<-PUQ<-matrix(0,g,g)
for(i in 1:g) {for(j in 1:g) {
 a<-nu0[i]*th0[j]
 b<-nu0[i]*(1-th0[j]) 
   
 PM[i,j]<- (a+y)/(a+y+b+n-y) 
 PP10[i,j]<- pbeta(.10,a+y,b+n-y)
 PLQ[i,j]<- qbeta(.05,a+y,b+n-y) 
 PUQ[i,j]<- qbeta(.95,a+y,b+n-y) 
                         }}

contour(nu0,th0,PM,xlab=expression(italic(w)), ylab=expression(theta[0]))
contour(nu0,th0,PP10,xlab=expression(italic(w)), 
   levels=c(0.1,0.3,.5,.70,.90,.975) )

dev.off()

a<-1 ; b<-1
(a+y)/(b+n-y)
(a+y-1)/(a+y-1+b+n-y-1)
pbeta(.10,a+y,b+n-y) 





#### adj wald interval 
a<-2 ; b<-2 
th<-  (y+a)/(n+a+b)
th+c(-1,1)*1.96*sqrt(th*(1-th)/n)

qbeta(c(.025,.975),a+y,b+n-y)
###########

########## diabetes example

library(lars) ; data(diabetes) ;source("regression_gprior.r")
yf<-diabetes$y
yf<-(yf-mean(yf))/sd(yf)

Xf<-diabetes[[3]]
Xf<-t( (t(Xf)-apply(Xf,2,mean))/apply(Xf,2,sd))

###
n<-length(yf)
set.seed(1)

i.te<-sample(1:n,100)
i.tr<-(1:n)[-i.te]

y<-yf[i.tr] ; y.te<-yf[i.te]
X<-Xf[i.tr,]; X.te<-Xf[i.te,]
#####

#####
p<-dim(X)[2]
S<-10000

if(2==3) {
BETA<-Z<-matrix(NA,S,p)
z<-rep(1,dim(X)[2] )
lpy.c<-lpy.X(y,X[,z==1,drop=FALSE])
for(s in 1:S)
{
      for(j in sample(1:p))
    {
      zp<-z ; zp[j]<-1-zp[j]
      lpy.p<-lpy.X(y,X[,zp==1,drop=FALSE])
      r<- (lpy.p - lpy.c)*(-1)^(zp[j]==0)
      z[j]<-rbinom(1,1,1/(1+exp(-r)))
      if(z[j]==zp[j]) {lpy.c<-lpy.p}
     }
  beta<-z;if(sum(z)>0){beta[z==1]<-lm.gprior(y,X[,z==1,drop=FALSE],S=1)$beta }
  Z[s,]<-z
  BETA[s,]<-beta
  if(s%%10==0) {
    dput(list(BETA=BETA,Z=Z), file="diabetes.bma") 
               }

} 
  }
#####

####
bfit<-dget("diabetes.bma")
ZPM<-apply(bfit$Z,2,mean,na.rm=TRUE)
beta.bma<-apply(bfit$BETA,2,mean,na.rm=TRUE)

pdf("fig1_3.pdf",family="Times",height=3.5, width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(ZPM,xlab="regressor index",ylab=expression(
 paste( "Pr(",italic(beta[j] != 0),"|",italic(y),",X)",sep="")),type="h",lwd=2)
dev.off()
####

####
y.te.bma<-X.te%*%beta.bma
pdf("fig1_4.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0),mfrow=c(1,2))

y.te.bma<-X.te%*%beta.bma
beta.ols<-lm(y~-1+X)$coef
y.te.ols<-X.te%*%beta.ols

plot(y.te,y.te.bma,xlab=expression(italic(y)[test]),
     ylim=range(c(y.te.bma,y.te.ols,y.te)),
     xlim=range(c(y.te.bma,y.te.ols,y.te)), 
     ylab=expression(hat(italic(y))[test])) ; abline(0,1)

plot(y.te,y.te.ols,xlab=expression(italic(y)[test]), 
     ylim=range(c(y.te.bma,y.te.ols,y.te)),
     xlim=range(c(y.te.bma,y.te.ols,y.te)), 
     ylab=expression(hat(italic(y))[test])) ; abline(0,1)
dev.off()
mean( (y.te-y.te.ols)^2 )
mean( (y.te-y.te.bma)^2 )



####






