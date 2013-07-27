#####
pdf("fig5_1.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))

par(mfrow=c(1,1))
theta<-2 ; sigma<- .5
x<-seq(0,10,length=100)
plot(x,dnorm(x,theta,sigma),type="l",
xlab=expression(italic(y)),
ylab=expression(paste(italic("p(y"),"|",theta,",",sigma^2,")",sep="")),col=gray(.15),lwd=2)

theta<-5 ; sigma<-2
lines(x, dnorm(x,theta,sigma),col=gray(.5),lwd=2)

theta<-7 ; sigma<-1
lines(x, dnorm(x,theta,sigma),col=gray(.85),lwd=2)
legend( 5,.7, 
c(expression(paste(theta==2,",",sigma^2==.25,sep="")),
  expression(paste(theta==5,",",sigma^2==4,sep="")),
  expression(paste(theta==7,",",sigma^2==1,sep="")) 
 ), 
        col=gray(c(.15,.5,.85)), lwd=c(2,2,2),lty=c(1,1,1),bty="n")

dev.off()
#####

#####
pdf("fig5_2.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,.25,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,1))

library(alr3)
data(heights)
y<-heights[,2]
#y<-round(heights[,2])
hist(y,main="",ylab="",
#     xlab=expression(italic(y)),
     xlab="height in inches",
     prob=TRUE,nclass=15,
    col="gray")

y.av<-mean(y) ; y.sd<-sd(y)
ys<-seq(min(y)*.9,max(y)*1.1,length=100)
lines(ys,dnorm(ys,y.av,y.sd),lwd=2)
dev.off()
#####


#####
pdf("fig5_3.pdf",family="Times",height=3.5,width=7)

library(Flury)
data(midge)  
y<-midge[midge[,1]=="Af",3]

mu0<-1.9
tau0<-(.5*1.9)
t02<-tau0^2

par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))

ybar<- mean(y)
s2<- var(y)
n<- length(y)
sigma<-sqrt(s2)

mun<-( mu0/(t02) + n*ybar/s2)/( 1/t02+n/s2)
t2n<-1/(1/t02 +n/s2)

qnorm(c(.025,.975),mun,sqrt(t2n))

ys<-seq(0,mu0*2,length=500)
plot(ys,dnorm(ys,mun,sqrt(t2n)),type="l",col="black",xlab=expression(theta),
    ylab=expression(paste(italic("p("),theta,"|",italic(y[1]),"...",italic(y[n]),",",
          sigma^2==0.017,")",sep="")),lwd=2)  
lines(ys,dnorm(ys,mu0,sqrt(t02)),type="l",col="gray",lwd=2)
dev.off()
#####


#####
# prior
mu0<-1.9  ; k0<-1
s20<-.01 ; nu0<-1

# data
y<-c(1.64,1.70,1.72,1.74,1.82,1.82,1.82,1.90,2.08)
n<-length(y) ; ybar<-mean(y) ; s2<-var(y)

# posterior inference
kn<-k0+n ; nun<-nu0+n
mun<- (k0*mu0 + n*ybar)/kn  
s2n<- (nu0*s20 +(n-1)*s2 +k0*n*(ybar-mu0)^2/(kn))/(nun)
mun
s2n

dinvgamma<-function(x,a,b) {
ld<- a*log(b) -lgamma(a) -(a+1)*log(x)  -b/x 
exp(ld)
                            }

gs<-100
theta<-seq(1.6,2.0,length=gs)
is2<-seq(15,160 ,length=gs)
s2g<-seq(.001,.045,length=gs)

ld.th.is2<-ld.th.s2<-matrix(0,gs,gs)
for(i in 1:gs) { for(j in 1:gs) {
    ld.th.is2[i,j]<- dnorm(theta[i],mun,1/sqrt( is2[j] *10) ,log=TRUE) +
                   dgamma(is2[j],10/2,10*s2n/2, log=TRUE )
    ld.th.s2[i,j]<- dnorm(theta[i],mun,sqrt(s2g[j]/10) ,log=TRUE) +
                    log( dinvgamma(s2g[j],10/2,10*s2n/2 ))
                     }} 
pdf("fig5_4.pdf",family="Times",height=3.5,width=7)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
grays<- gray( (10:0)/10)
image(theta,is2,exp(ld.th.is2),col=grays,xlab=expression(theta),
       ylab=expression(tilde(sigma)^2) ) 
image(theta,s2g,exp(ld.th.s2), col=grays,xlab=expression(theta),
       ylab=expression(sigma^2) )
dev.off()


# monte carlo sampling
pdf("fig5_5.pdf",family="Times",height=7,width=7)
set.seed(1)
S<-10000
s2.postsample<-1/rgamma(S,  (nu0+n)/2, s2n*(nu0+n)/2 )
theta.postsample<-rnorm(S, mun, sqrt(s2.postsample/(k0+n)))
quantile(theta.postsample, c(.025,.975))
#plot(density(theta.postsample,adjust=3) )
#x<-seq(1.6,2.0,length=200) ; lines(x,dnorm(x,mun,sqrt(t2n)),col="red")
###
layout(matrix(c(1,1,2,3),2,2,byrow=T))
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
image(theta,s2g,exp(ld.th.s2), col=grays,xlab=expression(theta),
       ylab=expression(sigma^2),xlim=c(1.60,2.0),ylim=c(.001,.07) )
points(theta.postsample[1:5000], s2.postsample[1:5000],pch=".",
   xlab=expression(theta),ylab=expression(sigma^2),xlim=c(1.65,1.95),ylim=c(.005,
    .07) )
plot(density(s2.postsample,adjust=3),main="",xlab=expression(sigma^2), 
     xlim=c(0,.075),ylab=expression( paste(italic("p("),
     sigma^2,"|",italic(y[1]),"...",italic(y[n]),")",sep=""))) 
# abline(v=s2n)
plot(density(theta.postsample,adjust=3),main="",xlab=expression(theta),xlim=c(1.60,2.0),ylab=expression( paste(italic("p("),
     theta,"|",italic(y[1]),"...",italic(y[n]),")",sep=""))) 
#abline(v=mun)
quantile( theta.postsample,c(.025,.975))
abline(v=quantile( theta.postsample,c(.025,.975)),col="gray",lwd=2)
### t-test based confidence interval
n<-length(y) ; ybar<-mean(y) ; s2<-var(y)

ybar+qt( c(.025,.975), n-1) *sqrt(s2/n)
abline( v= ybar+qt( c(.025,.975), n-1) *sqrt(s2/n), col="black",lwd=2)
#legend(1.9,6,legend=c("t interval", "posterior interval"),lwd=c(2,2),
#   col=c("black","gray"),bty="y")

dev.off()
#####

#####
pdf("fig5_6.pdf",family="Times",height=3.5,width=7)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))

b<-(100-112)^2
s2<-13^2
n<-1:50 

k<-1 ; brk1<- (n/(k+n))^2 + n*(k/(k+n))^2*b/s2
k<-2 ; brk2<- (n/(k+n))^2 + n*(k/(k+n))^2*b/s2
k<-3 ; brk3<- (n/(k+n))^2 + n*(k/(k+n))^2*b/s2

plot(range(n),c(0.4,1.1),type="n",xlab="sample size", ylab="relative MSE")
abline(h=1,lty=2,lwd=2)
lines(n, brk1,col=gray(.25),lwd=2)
lines(n, brk2,col=gray(.5),lwd=2)
lines(n, brk3,col=gray(.75),lwd=2)
legend(20,.8,legend=c(expression(kappa[0]==0),expression(kappa[0]==1), expression(kappa[0]==2), 
  expression(kappa[0]==3) ),lwd=c(2,2,2),lty=c(2,1,1,1),col=c(gray(c(0,.25,.5,.75))),bty="n")

####
theta0<-112
mu0<-100
n<-10 
s2m<-s2/n
x<-seq(theta0-4*sqrt(s2m),theta0+4*sqrt(s2m), length=100)
plot(x,dnorm(x,theta0,sqrt(s2m)),type="l",lwd=2,ylim=c(0,.13),lty=2, xlab="IQ",
    ylab="")
abline(v=theta0)
for(k in 1:3) {
w<- n/(n+k) 
lines(x,dnorm(x,w*theta0+(1-w)*mu0,sqrt(w^2*s2m)),type="l",col=gray(k/4),lwd=2) 
              } 
dev.off()
####################


source("../Data/alldata.r")
pdf("fig5_7.pdf",family="Times",height=1.75,width=5)
par(mfrow=c(1,3),mar=c(2.75,2.75,.5,.5),mgp=c(1.70,.70,0))

###
#hist(Y$INCOME98[Y$YEAR==1998] )
#plot(table( Y$INCOME98[Y$YEAR==1998]  ))
#hist( Y$AGE[Y$YEAR==1998]  )
CHILDS<-Y$CHILDS[Y$FEMALE==1&Y$YEAR==1998 & Y$AGE>=40  ]
CHILDS<-CHILDS[!is.na(CHILDS) ]
###

set.seed(1)
NY<-NULL
N<-c(5,15,45)
for(n in N) {
  for(sim in 1:5000) {
  y<-sample(CHILDS,n)
  NY<-rbind(NY, c(n,mean(y),var(y)) )
                      } }

plot(table(CHILDS)/sum(table(CHILDS)),type="h",
    xlab=expression(paste(italic(y),"=number of children",sep="" )), 
     ylab=expression(italic(p(y)) ))

x<-seq(0,6,length=200)
plot( range(NY[,2]),c(0,1.7),type="n",xlab="number of children", 
      ylab=expression( italic( p(bar(y))) ) )
for( n in N) {
   yb<- NY[NY[,1]==n,2]
   lines(density( yb,adj=2) ,col=gray(1-( n/5)/9 ), lwd=2)
   cat(n,mean(yb),sd(yb),"\n")
     }
abline(v=mean(CHILDS))
legend( 2.35,1.8,legend=c("n=5","n=15","n=45"),lwd=c(2,2,2),col=
          gray(1-( N/5)/9 ), bty="n")


# plot( NY[NY[,1]==45,2:3],xlab=expression(italic(bar(y))),
#    ylab=expression(italic(s^2)) )

source("hdr_2d.r")
plot.hdr2d(NY[NY[,1]==45,2:3],xlab=expression(italic(bar(y))), 
    ylab=expression(italic(s^2)) ,ylim=c(1,6) )


dev.off()



####
mean(CHILDS)
pq<-.5
quantile(CHILDS,pq)
mean( ( CHILDS-mean(CHILDS))^3) /( mean( ( CHILDS-mean(CHILDS))^2)^(3/2)) 

###
k0<-1; mu0<-2 ; nu0<-1 ; s20<-1

n<-45
set.seed(1)
y<-sample(CHILDS,n)
ybar<-mean(y) ; s2<-var(y)

kn<-k0+n ; nun<-nu0+n
mun<- (k0*mu0 + n*ybar)/kn    
s2n<- (nu0*s20 +(n-1)*s2 +k0*n*(ybar-mu0)^2/(kn))/(nun)
###

S<-10000
s2.postsample <- 1/rgamma(S, (nu0+n)/2, s2n*(nu0+n)/2 )
theta.postsample <- rnorm(S, mun, sqrt(s2.postsample/(k0+n)))


#####



hist(theta.postsample) ; abline(v=mean(CHILDS))

hist( qnorm( pq, theta.postsample, sqrt(s2.postsample))) 
abline(v=quantile(CHILDS,pq))

hist( pnorm( 2, theta.postsample, sqrt(s2.postsample)))   
abline(v=quantile(CHILDS,pq))

1-pnorm(7, mean(CHILDS), sd(CHILDS))
1-pnorm(6.5, mean(CHILDS), sd(CHILDS))
mean( CHILDS>= 7 )




