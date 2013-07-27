########
odat<-dget("../Data/nels_2002_data")
colnames(odat)<-c("sch_id","sch_enroll","sch_freelunch","sch_cnrtl",
   "sch_urban","mteach_deg","eteach_deg","mteach_years","eteach_years" , 
    "stu_sex","stu_lang","stu_pared","stu_income","stu_mathscore",
    "stu_readscore","stu_mhw","stu_ehw","stu_readhours","stu_ses")
odat<-as.data.frame(odat)

ids<-dget("ids_selectschools")
group<-odat$sch_id
indset<-apply( odat[,1,drop=F],1,is.element,ids)
dat<-odat[indset,]

mathdat<-dat[,c(1,3,19,14)]
mathdat[,3]<-(mathdat[,3]-mean(mathdat[,3]))/sd(mathdat[,3]) 
########


########

groups<-ids
m<-length(ids)
Y<-list() ; X<-list() ; N<-NULL
for(j in 1:m) 
{
  Y[[j]]<-mathdat[mathdat[,1]==ids[j], 4] 
  N[j]<- sum(dat$sch_id==ids[j])
  xj<-mathdat[mathdat[,1]==ids[j], 3] 
  xj<-(xj-mean(xj))
  X[[j]]<-cbind( rep(1,N[j]), xj  )
}
#######

S2.LS<-BETA.LS<-NULL
for(j in 1:m) {
  fit<-lm(Y[[j]]~-1+X[[j]] )
  BETA.LS<-rbind(BETA.LS,c(fit$coef)) 
  S2.LS<-c(S2.LS, summary(fit)$sigma^2) 
                } 
####

#####
pdf("fig11_1.pdf",family="Times",height=1.75,width=5)
par(mar=c(2.75,2.75,.5,.5),mgp=c(1.7,.7,0))
par(mfrow=c(1,3))

plot( range(mathdat[,3]),range(mathdat[,4]),type="n",xlab="SES", 
   ylab="math score")
for(j in 1:m) {    abline(BETA.LS[j,1],BETA.LS[j,2],col="gray")  }

BETA.MLS<-apply(BETA.LS,2,mean)
abline(BETA.MLS[1],BETA.MLS[2],lwd=2)

plot(N,BETA.LS[,1],xlab="sample size",ylab="intercept")
abline(h= BETA.MLS[1],col="black",lwd=2)
plot(N,BETA.LS[,2],xlab="sample size",ylab="slope")
abline(h= BETA.MLS[2],col="black",lwd=2)

dev.off()
#####

if(2==3) {
##### hierarchical regression model
p<-dim(X[[1]])[2]
theta<-mu0<-apply(BETA.LS,2,mean)
nu0<-1 ; s2<-s20<-mean(S2.LS)
eta0<-p+2 ; Sigma<-S0<-L0<-cov(BETA.LS) ; BETA<-BETA.LS
THETA.b<-S2.b<-NULL
iL0<-solve(L0) ; iSigma<-solve(Sigma)
source("~hoff/USBWork/rfunctions.r")
Sigma.ps<-matrix(0,p,p)
SIGMA.PS<-NULL
BETA.ps<-BETA*0
BETA.pp<-NULL
set.seed(1)
mu0[2]+c(-1.96,1.96)*sqrt(L0[2,2])
for(s in 1:10000) {
  ##update beta_j 
  for(j in 1:m) 
  {  
    Vj<-solve( iSigma + t(X[[j]])%*%X[[j]]/s2 )
    Ej<-Vj%*%( iSigma%*%theta + t(X[[j]])%*%Y[[j]]/s2 )
    BETA[j,]<-rmvnorm(1,Ej,Vj) 
  } 
  ##

  ##update theta
  Lm<-  solve( iL0 +  m*iSigma )
  mum<- Lm%*%( iL0%*%mu0 + iSigma%*%apply(BETA,2,sum))
  theta<-t(rmvnorm(1,mum,Lm))
  ##

  ##update Sigma
  mtheta<-matrix(theta,m,p,byrow=TRUE)
  iSigma<-rwish( solve( S0+t(BETA-mtheta)%*%(BETA-mtheta) )  ,  eta0+m) 
  ##

  ##update s2
  RSS<-0
  for(j in 1:m) { RSS<-RSS+sum( (Y[[j]]-X[[j]]%*%BETA[j,] )^2 ) }
  s2<-1/rgamma(1,(nu0+sum(N))/2, (nu0*s20+RSS)/2 )
  ##
  ##store results
  if(s%%10==0) 
  { 
    cat(s,s2,"\n")
    S2.b<-c(S2.b,s2);THETA.b<-rbind(THETA.b,t(theta))
    Sigma.ps<-Sigma.ps+solve(iSigma) ; BETA.ps<-BETA.ps+BETA
    SIGMA.PS<-rbind(SIGMA.PS,c(solve(iSigma)))
    BETA.pp<-rbind(BETA.pp,rmvnorm(1,theta,solve(iSigma)) )
  }
  ##
}

save.image("data.f11_3")
}

load("data.f11_3")

#####
library(coda)
effectiveSize(S2.b)
effectiveSize(THETA.b[,1])
effectiveSize(THETA.b[,2])

apply(SIGMA.PS,2,effectiveSize)

tmp<-NULL;for(j in 1:dim(SIGMA.PS)[2]) { tmp<-c(tmp,acf(SIGMA.PS[,j])$acf[2]) }


acf(S2.b)
acf(THETA.b[,1])
acf(THETA.b[,2])
#####

pdf("fig11_3.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))

plot(density(THETA.b[,2],adj=2),xlim=range(BETA.pp[,2]), 
      main="",xlab="slope parameter",ylab="posterior density",lwd=2)
lines(density(BETA.pp[,2],adj=2),col="gray",lwd=2)
legend( -3 ,1.0 ,legend=c( expression(theta[2]),expression(tilde(beta)[2])), 
        lwd=c(2,2),col=c("black","gray"),bty="n") 

quantile(THETA.b[,2],prob=c(.025,.5,.975))
mean(BETA.pp[,2]<0) 

BETA.PM<-BETA.ps/1000
plot( range(mathdat[,3]),range(mathdat[,4]),type="n",xlab="SES",
   ylab="math score")
for(j in 1:m) {    abline(BETA.PM[j,1],BETA.PM[j,2],col="gray")  }
abline( mean(THETA.b[,1]),mean(THETA.b[,2]),lwd=2 )
dev.off()

########################


xs<-seq(5,100,5)/100
pops<-c("akr","b6","f1","mlh1","mom1","msh2","rb1","rb9")
DAT<-list()

for(j in 1:length(pops))
{
  err<-i<-0
  N<-NULL
  while(err==0)
  {
    x<-scan(paste("../Data/TumorData/",pops[j],".txt",sep=""),skip=i,nlines=1,quiet=T)
    if(length(x)>0)
    {
      pos<-c(1:length(x))/length(x)
      tm<-rep(pos,x)
      cnts<-xs*NA
      for(k in 1:length(cnts)){ cnts[k]<-sum( tm<=xs[k]) }
      cnts[2:length(cnts)]<-cnts[2:length(cnts)]-cnts[1:(length(cnts)-1)]
      N<-rbind(N,cnts)
      i<-i+2
    }
    if(length(x)==0){err<-1}
  }
  DAT[[j]]<-N
}


pdeg<-3
X<-cbind(rep(1,20),poly(xs,degree=pdeg) )

par(mfrow=c(4,2))
for(j in 1:8)
  {
  plot(apply(DAT[[j]],2,mean),type="h",lwd=4)
  cat(j,dim(DAT[[j]])[1],mean(DAT[[j]]),"\n")
   }


  
k<-3
m<-dim(DAT[[k]])[1] ; n<-dim(DAT[[k]])[2] ; p<-dim(X)[2]
Y<-DAT[[k]]

pdf("fig11_4.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))
plot(c(0,1),range(Y),type="n",xlab="location",ylab="number of tumors")
for(j in 1:m) { lines(xs,Y[j,],col="gray") }
lines( xs,apply(Y,2,mean),lwd=3)

lya<-log(apply(Y,2,mean))
Xs<-cbind( rep(1,n),poly(xs,deg=4,raw=TRUE))
fit2<- lm(lya~-1+Xs[,1:3] )
fit3<- lm(lya~-1+Xs[,1:4] )
fit4<- lm(lya~-1+Xs[,1:5] )
  
yh2<-Xs[,1:3]%*%fit2$coef
yh3<-Xs[,1:4]%*%fit3$coef
yh4<-Xs[,1:5]%*%fit4$coef
  
plot(xs,lya,type="l",lwd=3,xlab="location",ylab="log average number of tumors",
     ylim=range(c(lya,yh2,yh3,yh4)) )
  
points(xs,yh2,pch="2",col="black")
lines(xs,yh2,col="gray")
points(xs,yh3,pch="3",col="black")
lines(xs,yh3,col="gray")
points(xs,yh4,pch="4",col="black")
lines(xs,yh4,col="gray")
dev.off()
    



load("tumor.k4.S50000.rawTRUE")

library(coda)
round(apply(THETA.PS,2,effectiveSize),2)
round(apply(SIGMA.PS,2,effectiveSize),2)

### compare prior and posterior variance
SIGMA.PPS<-NULL
iS0<-solve(S0)
for(s in 1:5000) {
 tmp<-solve(rwish(iS0,eta0))
 SIGMA.PPS<-rbind(SIGMA.PPS,c(tmp))
                  }

par(mfrow=c(2,3))
for(j in c(1,7,13,19,25)) {
plot(density(log(SIGMA.PS[,j]),adj=2) ,type="l",xlab=expression(paste("log ",Sigma[11])),ylab="density",main="",xlim=range(c(-7,log(SIGMA.PPS[,j]))),lwd=2 )
lines( density(log(SIGMA.PPS[,j]),adj=2),col="gray",lwd=2)
legend(-7.6,.72,legend=c("prior","posterior"),
       lwd=c(2,2),col=c("gray","black"),bty="n")
                           }
###


###
eXB.post<-NULL
for(s in 1:dim(THETA.PS)[1])
{
  beta<-rmvnorm(1,THETA.PS[s,],matrix(SIGMA.PS[s,],p,p))
  eXB.post<-rbind(eXB.post,t(exp(X%*%t(beta) )) )
}

qEB<-apply( eXB.post,2,quantile,probs=c(.025,.5,.975))

eXT.post<- exp(t(X%*%t(THETA.PS )) )
qET<-apply( eXT.post,2,quantile,probs=c(.025,.5,.975))
yXT.pp<-matrix( rpois(prod(dim(eXB.post)),eXB.post),
                dim(eXB.post)[1],dim(eXB.post)[2] )

qYP<-apply( yXT.pp,2,quantile,probs=c(.025,.5,.975))


pdf("fig11_5.pdf",family="Times",height=1.75,width=5)
par(mar=c(2.75,2.75,.5,.5),mgp=c(1.7,.7,0))
par(mfrow=c(1,3))

plot( c(0,1),range(c(0,qET,qEB,qYP)),type="n",xlab="location",
   ylab="number of tumors")
lines(xs, qET[1,],col="black",lwd=1)
lines(xs, qET[2,],col="black",lwd=2)
lines(xs, qET[3,],col="black",lwd=1)

plot( c(0,1),range(c(0,qET,qEB,qYP)),type="n",xlab="location",
   ylab="")
lines(xs, qEB[1,],col="black",lwd=1)
lines(xs, qEB[2,],col="black",lwd=2)
lines(xs, qEB[3,],col="black",lwd=1)

plot( c(0,1),range(c(0,qET,qEB,qYP)),type="n",xlab="location",
   ylab="")
lines(xs, qYP[1,],col="black",lwd=1)
lines(xs, qYP[2,],col="black",lwd=2)
lines(xs, qYP[3,],col="black",lwd=1)

dev.off()



