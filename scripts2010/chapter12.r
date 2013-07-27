dat<-read.table("http://lib.stat.cmu.edu/aoas/107/data.txt",header=TRUE)
####


yincc<-match(dat$INC,sort(unique(dat$INC)))
ydegr<-dat$DEGREE+1
yage<-dat$AGE
ychild<-dat$CHILD
ypdeg<-1*(dat$PDEG>2)
tmp<-lm(ydegr~ychild+ypdeg+ychild:ypdeg)


#####
pdf("fig12_1.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))
plot(table(dat$DEG+1)/sum(table(dat$DEG+1)),
  lwd=2,type="h",xlab="DEG",ylab="probability")
plot(table(dat$CHILD)/sum(table(dat$CHILD)),lwd=2,type="h",xlab="CHILD",ylab="probability" )
dev.off()
#####

#####

X<-cbind(ychild,ypdeg,ychild*ypdeg)
y<-ydegr
keep<- (1:length(y))[ !is.na( apply( cbind(X,y),1,mean) ) ]
X<-X[keep,] ; y<-y[keep]
ranks<-match(y,sort(unique(y))) ; uranks<-sort(unique(ranks))
n<-dim(X)[1] ; p<-dim(X)[2]
iXX<-solve(t(X)%*%X)  ; V<-iXX*(n/(n+1)) ; cholV<-chol(V)

###starting values
set.seed(1)
beta<-rep(0,p) 
z<-qnorm(rank(y,ties.method="random")/(n+1))
g<-rep(NA,length(uranks)-1)
K<-length(uranks)
BETA<-matrix(NA,1000,p) ; Z<-matrix(NA,1000,n) ; ac<-0
mu<-rep(0,K-1) ; sigma<-rep(100,K-1)
S<-25000
for(s in 1:S) 
{

  #update g 
  for(k in 1:(K-1)) 
  {
  a<-max(z[y==k])
  b<-min(z[y==k+1])
  u<-runif(1, pnorm( (a-mu[k])/sigma[k] ),
              pnorm( (b-mu[k])/sigma[k] ) )
  g[k]<- mu[k] + sigma[k]*qnorm(u)
  }

  #update beta
  E<- V%*%( t(X)%*%z )
  beta<- cholV%*%rnorm(p) + E

  #update z
  ez<-X%*%beta
  a<-c(-Inf,g)[ match( y-1, 0:K) ]
  b<-c(g,Inf)[y]  
  u<-runif(n, pnorm(a-ez),pnorm(b-ez) )
  z<- ez + qnorm(u)


  #help mixing
  c<-rnorm(1,0,n^(-1/3))  
  zp<-z+c ; gp<-g+c
  lhr<-  sum(dnorm(zp,ez,1,log=T) - dnorm(z,ez,1,log=T) ) + 
         sum(dnorm(gp,mu,sigma,log=T) - dnorm(g,mu,sigma,log=T) )
  if(log(runif(1))<lhr) { z<-zp ; g<-gp ; ac<-ac+1 }

  if(s%%(S/1000)==0) 
  { 
    cat(s/S,ac/s,"\n")
    BETA[s/(S/1000),]<-  beta
    Z[s/(S/1000),]<- z
  }

} 
#####
pdf("fig12_2.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))
plot(X[,1]+.25*(X[,2]),Z[1000,],
 pch=15+X[,2],col=c("gray","black")[X[,2]+1],
 xlab="number of children",ylab="z", ylim=range(c(-2.5,4,Z[1000,])),
    xlim=c(0,9))

beta.pm<-apply(BETA,2,mean)
ZPM<-apply(Z,2,mean)
abline(0,beta.pm[1],lwd=2 ,col="gray")
abline(beta.pm[2],beta.pm[1]+beta.pm[3],col="black",lwd=2 )
#legend(3.75,4.25,legend=c("parents without college","parents with college"),pch=c(15,16),col=c("gray","black"))
legend(5,4,legend=c("PDEG=0","PDEG=1"),pch=c(15,16),col=c("gray","black"))


plot(density(BETA[,3],adj=2),lwd=2,xlim=c(-.5,.5),main="",
    xlab=expression(beta[3]),ylab="density")
sd<-sqrt(  solve(t(X)%*%X/n)[3,3] )
x<-seq(-.7,.7,length=100)
lines(x,dnorm(x,0,sd),lwd=2,col="gray")
legend(-.5,6.5,legend=c("prior","posterior"),lwd=c(2,2),col=c("gray","black"),bty="n")
dev.off()
#####

beta.pm<-apply(BETA,2,mean)
beta.pm[1]+beta.pm[3]
quantile(BETA[,3],prob=c(.025,.0975))
quantile(BETA[,3],prob=c(0.025,0.975))

source("treg.r")
rfit<-treg(y,X)

pdf("fig12_3.pdf",family="Times",height=1.75,width=5) 
par(mfrow=c(1,3),mar=c(2.75,2.75,.5,.5),mgp=c(1.7,.7,0))
lab<-c(expression(beta[1]),expression(beta[2]),expression(beta[3]))
ymx<-c(12,3.1,7.25)
laby<-c("density","","")
for(j in 1:3) {
plot(density(rfit$BETA[,j],adj=2),lwd=2,main="",
 xlab=lab[j],col="black",ylim=c(0,ymx[j]),ylab=laby[j])
lines(density(BETA[,j],adj=2),col="gray",lwd=2)
if(j==4) {
 legend(-.2,12,legend=c("ordered probit","rank likelihood"), 
       lwd=c(2,2),col=c("gray","black"))
          } 
               }
dev.off()
#################


