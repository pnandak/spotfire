# priors
mu0<-1.9  ; t20<-0.95^2
s20<-.01 ; nu0<-1

#data
y<-c(1.64,1.70,1.72,1.74,1.82,1.82,1.82,1.90,2.08)
n<-length(y) ; mean.y<-mean(y) ; var.y<-var(y)


####
G<-100 ; H<-100

mean.grid<-seq(1.505,2.00,length=G) 
prec.grid<-seq(1.75,175,length=H) 

post.grid<-matrix(nrow=G,ncol=H)

for(g in 1:G) {
  for(h in 1:H) { 
    
    post.grid[g,h]<- dnorm(mean.grid[g], mu0, sqrt(t20)) *
                     dgamma(prec.grid[h], nu0/2, s20*nu0/2 ) *
                     prod( dnorm(y,mean.grid[g],1/sqrt(prec.grid[h])) )
                  }
                }

post.grid<-post.grid/sum(post.grid)

pdf("fig6_1.pdf",height=1.75,width=5,family="Times")
par(mfrow=c(1,3),mar=c(2.75,2.75,.5,.5),mgp=c(1.70,.70,0))
image( mean.grid,prec.grid,post.grid,col=gray( (10:0)/10 ),
     xlab=expression(theta), ylab=expression(tilde(sigma)^2) )

mean.post<- apply(post.grid,1,sum)
plot(mean.grid,mean.post,type="l",xlab=expression(theta),
 ylab=expression( paste(italic("p("),
     theta,"|",italic(y[1]),"...",italic(y[n]),")",sep="")))

prec.post<-apply(post.grid,2,sum)
plot(prec.grid,prec.post,type="l",xlab=expression(tilde(sigma)^2),
     ylab=expression( paste(italic("p("),
     tilde(sigma)^2,"|",italic(y[1]),"...",italic(y[n]),")",sep=""))) 

dev.off()
#####







###
set.seed(1)
S<-1000
PHI<-matrix(nrow=S,ncol=2)
PHI[1,]<-phi<-c( mean.y, 1/var.y)

### Gibbs sampling
for(s in 2:S) {

# generate a new theta value from its full conditional
mun<-  ( mu0/t20 + n*mean.y*phi[2] ) / ( 1/t20 + n*phi[2] )
t2n<- 1/( 1/t20 + n*phi[2] )
phi[1]<-rnorm(1, mun, sqrt(t2n) )

# generate a new sigma^2 value from its full conditional
nun<- nu0+n
s2n<- (nu0*s20 + (n-1)*var.y + n*(mean.y-phi[1])^2 ) /nun
phi[2]<- rgamma(1, nun/2, nun*s2n/2)

PHI[s,]<-phi         }
###



pdf("fig6_2.pdf",height=1.75,width=5,family="Times")
par(mfrow=c(1,3),mar=c(2.75,2.75,.5,.5),mgp=c(1.70,.70,0))
m1<-5
plot( PHI[1:m1,],type="l",xlim=range(PHI[1:100,1]), ylim=range(PHI[1:100,2]),
       lty=1,col="gray",xlab=expression(theta),ylab=expression(tilde(sigma)^2))
text(  PHI[1:m1,1], PHI[1:m1,2], c(1:m1) )

m1<-15
plot( PHI[1:m1,],type="l",xlim=range(PHI[1:100,1]), ylim=range(PHI[1:100,2]),
       lty=1,col="gray",xlab=expression(theta),ylab=expression(tilde(sigma)^2))
text(  PHI[1:m1,1], PHI[1:m1,2], c(1:m1) )

m1<-100
plot( PHI[1:m1,],type="l",xlim=range(PHI[1:100,1]), ylim=range(PHI[1:100,2]),
       lty=1,col="gray",xlab=expression(theta),ylab=expression(tilde(sigma)^2))
text(  PHI[1:m1,1], PHI[1:m1,2], c(1:m1) )
dev.off()
#####


#####
pdf("fig6_3.pdf",family="Times",height=1.75,width=5)
par(mfrow=c(1,3),mar=c(2.75,2.75,.5,.5),mgp=c(1.70,.70,0))
sseq<-1:1000


image( mean.grid,prec.grid,post.grid,col=gray( (10:0)/10 ),
     xlab=expression(theta), ylab=expression(tilde(sigma)^2) ,
     xlim=range(PHI[,1]),ylim=range(PHI[,2]) )
points(PHI[sseq,1],PHI[sseq,2],pch=".",cex=1.25 )

plot(density(PHI[,1],adj=2),  xlab=expression(theta),main="",
     xlim=c(1.55,2.05),
 ylab=expression( paste(italic("p("),
     theta,"|",italic(y[1]),"...",italic(y[n]),")",sep="")))
abline(v=quantile(PHI[,1],prob=c(.025,.975)),lwd=2,col="gray")
### t-test based confidence interval
n<-length(y) ; ybar<-mean(y) ; s2<-var(y)
ybar+qt( c(.025,.975), n-1) *sqrt(s2/n)
abline( v= ybar+qt( c(.025,.975), n-1) *sqrt(s2/n), col="black",lwd=1)


plot(density(PHI[,2],adj=2), xlab=expression(tilde(sigma)^2),main="",
     ylab=expression( paste(italic("p("),
     tilde(sigma)^2,"|",italic(y[1]),"...",italic(y[n]),")",sep=""))) 



dev.off()
#####

quantile(PHI[,1],c(.025,.5,.975))
quantile(PHI[,2],c(.025,.5, .975))
quantile(1/sqrt(PHI[,2]),c(.025,.5, .975))



##### 
pdf("fig6_5.pdf",family="Times",height=4,width=4)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
mu0<-10 ; tau0<-5
s20<-2  ; nu0<-1  

K<-50
L<-50
theta.grid<-seq( 12,18, length=K)
prec.grid<-seq(.05,.25, length=L)
post.grid<-matrix(nrow=K,ncol=L)

plot( 0,0,xlim=range(theta.grid), ylim=range(prec.grid),
          xlab=expression(theta), ylab=expression(1/sigma^2) )
abline( v=theta.grid, col="gray")
abline( h=prec.grid, col="gray")
dev.off()
#####


#####
pdf("fig6_6.pdf",family="Times",height=4,width=8)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))

set.seed(1)
y<-rnorm(25,15,3)
for(k in 1:K) {
for(l in 1:L) {
    post.grid[k,l]<- dnorm(theta.grid[k],mu0,tau0) *
                     dgamma(prec.grid[l], nu0/2, s20*nu0/2 ) *
                     prod( dnorm(y,theta.grid[k],1/sqrt(prec.grid[l])) )
               }}
post.grid<-post.grid/sum(post.grid)

par(mfrow=c(1,2))
image( theta.grid,prec.grid,post.grid,col=terrain.colors(15),
     xlab=expression(theta), ylab=expression(1/sigma^2))
abline(v=mean(y),lty=2)
abline(h=1/var(y), lty=2)

#abline(v=mu0,lty=2,col="blue")
#abline(h=1/tau0, lty=2,col="blue")

contour( theta.grid,prec.grid,post.grid,
         xlab=expression(theta), ylab=expression(1/sigma^2))
abline(v=mean(y),lty=2)
abline(h=1/var(y), lty=2)
dev.off()
#####




###### Intro to MCMC diagnostics
mu<-c(-3,0,3)
s2<-c(.33,.33,.33)
w<-c(.45,.1,.45)

ths<-seq(-5,5,length=100)
plot(ths, w[1]*dnorm(ths,mu[1],sqrt(s2[1])) +
          w[2]*dnorm(ths,mu[2],sqrt(s2[2])) +
          w[3]*dnorm(ths,mu[3],sqrt(s2[3])) ,type="l" )



#### MC Sampling
set.seed(1)
S<-2000
d<-sample(1:3,S, prob=w,replace=TRUE)
th<-rnorm(S,mu[d],sqrt(s2[d]))
THD.MC<-cbind(th,d)
####

#### MCMC sampling
th<-0
THD.MCMC<-NULL
S<-10000
set.seed(1)
for(s in 1:S) {
d<-sample(1:3 ,1,prob= w*dnorm(th,mu,sqrt(s2))   )
th<-rnorm(1,mu[d],sqrt(s2[d]) )
THD.MCMC<-rbind(THD.MCMC,c(th,d) )
               }

plot(THD.MCMC[,1])
lines( mu[THD.MCMC[,2]])

###
pdf("fig6_4.pdf",family="Times",height=3.5,width=7)
par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
ths<-seq(-6,6,length=1000)
plot(ths, w[1]*dnorm(ths,mu[1],sqrt(s2[1])) +
          w[2]*dnorm(ths,mu[2],sqrt(s2[2])) +
          w[3]*dnorm(ths,mu[3],sqrt(s2[3])) ,type="l" , xlab=expression(theta),ylab=
     expression( paste( italic("p("),theta,")",sep="") ),lwd=2 ,ylim=c(0,.40))
hist(THD.MC[,1],add=TRUE,prob=TRUE,nclass=20,col="gray")
lines( ths, w[1]*dnorm(ths,mu[1],sqrt(s2[1])) +
          w[2]*dnorm(ths,mu[2],sqrt(s2[2])) +
          w[3]*dnorm(ths,mu[3],sqrt(s2[3])),lwd=2 )
dev.off()
###
###
pdf("fig6_5.pdf",family="Times",height=3.5,width=7)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
Smax<-1000
ths<-seq(-6,6,length=1000)
plot(ths, w[1]*dnorm(ths,mu[1],sqrt(s2[1])) +
          w[2]*dnorm(ths,mu[2],sqrt(s2[2])) +
          w[3]*dnorm(ths,mu[3],sqrt(s2[3])) ,type="l" , xlab=expression(theta),
 ylab=expression( paste( italic("p("),theta,")",sep="") ),lwd=2 ,ylim=c(0,.40))
hist(THD.MCMC[1:Smax,1],add=TRUE,prob=TRUE,nclass=20,col="gray")
lines( ths, w[1]*dnorm(ths,mu[1],sqrt(s2[1])) +
          w[2]*dnorm(ths,mu[2],sqrt(s2[2])) +
          w[3]*dnorm(ths,mu[3],sqrt(s2[3])),lwd=2 )
plot(THD.MCMC[1:Smax,1],xlab="iteration",ylab=expression(theta))
dev.off()
####
####
pdf("fig6_6.pdf",family="Times",height=3.5,width=7)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
Smax<-10000
ths<-seq(-6,6,length=1000)
plot(ths, w[1]*dnorm(ths,mu[1],sqrt(s2[1])) +
          w[2]*dnorm(ths,mu[2],sqrt(s2[2])) +
          w[3]*dnorm(ths,mu[3],sqrt(s2[3])) ,type="l" , xlab=expression(theta),
 ylab=expression( paste( italic("p("),theta,")",sep="") ),lwd=2,ylim=c(0,.40) )
hist(THD.MCMC[1:Smax,1],add=TRUE,prob=TRUE,nclass=20,col="gray")
lines( ths, w[1]*dnorm(ths,mu[1],sqrt(s2[1])) +
          w[2]*dnorm(ths,mu[2],sqrt(s2[2])) +
          w[3]*dnorm(ths,mu[3],sqrt(s2[3])),lwd=2 )
plot(THD.MCMC[1:Smax,1],xlab="iteration",ylab=expression(theta))
dev.off()
#####


acf(THD.MCMC[,1],lag.max=50)

library(coda)
effectiveSize(THD.MCMC[,1])

##### MCMC diagnostics for semiconjugate normal analysis

stationarity.plot<-function(x,...){

S<-length(x)
scan<-1:S
ng<-min( round(S/100),10)
group<-S*ceiling( ng*scan/S) /ng

boxplot(x~group,...)               }



#####
pdf("fig6_7.pdf",family="Times",height=3.5,width=7)
par(mfrow=c(1,2))
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(PHI[,1],xlab="iteration",ylab=expression(theta))  
plot(1/PHI[,2],xlab="iteration",ylab=expression(sigma^2))  

dev.off()





acf(PHI[,1]) -> tmp1
acf(1/PHI[,2]) -> tmp2

effectiveSize( PHI[,1] )
effectiveSize(1/PHI[,2] )



