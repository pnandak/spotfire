#4.1.1
conso.dat=read.table("donneesconso.txt",header=T)
attach(conso.dat)

simuesti=function(n,k) {
  pop=Revenu
  N=length(Revenu)
  mu=mean(pop)
  varp=(N-1)/N*var(pop)
  varech=numeric(k)
  varemp=numeric(k)
  for (i in 1:k) {
    ech=sample(pop,n,replace=T)
    varech[i]=var(ech)
    varemp[i]=(n-1)/n*varech[i]
  }
  yrange=range(varech,varemp)
  par(mfrow=c(2,1))
  plot(1:k,varemp,type="p",col="blue",
       ylim=yrange,main="Variance empirique")
  abline(h=varp,col="red")
  abline(h=mean(varemp),col="blue")
  legend("topleft",legend=c("varp","varemp"),
         col=c("red","blue"),lwd=1)
  plot(1:k,varech,type="p",col="blue",
       ylim=yrange,main="Variance echantillon")
  abline(h=varp,col="red")
  abline(h=mean(varech),col="blue")
  legend("topleft",legend=c("varp","varech"),
         col=c("red","blue"),lwd=1)
  par(mfrow=c(1,1))
}

simuesti(3,200)

x11()
simuesti(30,200)



#4.1.2

simunif=function(teta,n,k) {
  est1=numeric(k)
  est2=numeric(k)
  for (i in 1:k) {
    echant=sample(1:teta,n,replace=T)
    est1[i]=2*mean(echant)-1
    est2[i]=max(echant)
  }
  yrange=range(est1,est2)
  plot(1:k,est1,type="p",col="blue",ylim=yrange,main="",sub="",
       xlab="",ylab="")
  points(1:k,est2,type="p",col="red",ylim=yrange,main="",sub="",
         xlab="",ylab="")
  abline(h=teta,col="black")
  abline(h=mean(est1),col="blue")
  abline(h=mean(est2),col="red")
  r1=round(sum(est1-teta)^2/k,2)
  r2=round(sum(est2-teta)^2/k,2)
  text(1,min(yrange)+10,paste("r1 = ",r1,"  r2 = ",r2),pos=4)
}

simunif(1000,20,200)


#4.2.1

simucf1=function(m,sigma,k,l0) {
  vec=rnorm(k,m,sigma)
  plot(l0:k,cumsum(vec)[l0:k]/(l0:k),
       type="l",xlab="i",ylab="moyenne")
  abline(h=m)
}
simucf1(10,2,300,10)


#4.2.2
simucf2=function(k,l0) {
  pop=Revenu
  mu=mean(pop)
  vec=sample(pop,k,replace=T)
  plot(l0:k,cumsum(vec)[l0:k]/(l0:k),
       type="l",xlab="i",ylab="moyenne")
  abline(h=mu)
}

simucf2(300,10)

#4.2.3
simucf3=function(m,sigma,k,l0) {
  vec=rnorm(k,m,sigma)
  s=numeric(k)
  for (i in 1:k)
    s[i]=sd(vec[1:i])
  plot(l0:k,s[l0:k],type="l",xlab="i",ylab="ecart-type")
  abline(h=sigma)
}

simucf3(10,2,300,10)


#4.2.4
simucf4=function(k,l0) {
  pop=Magnetoscope
  p=sum(pop)/length(pop)
  vec=sample(pop,k,replace=T)
  plot(l0:k,cumsum(vec)[l0:k]/(l0:k),
       type="l",xlab="i",ylab="frequence",ylim=c(0,1))
  abline(h=p)
}
simucf4(1000,10)


simucf4bis=function(p,k,l0) {
  vec=rbinom(k,1,p)
  plot(l0:k,cumsum(vec)[l0:k]/(l0:k),type="l",xlab="i",
       ylab="frequence",ylim=c(0,1))
  abline(h=p)
}
simucf4bis(0.4,300,10)

#4.2.5
simucf5=function(k,l0) {
  vec=rcauchy(k)
  plot(l0:k,cumsum(vec)[l0:k]/(l0:k),type="l",xlab="i",
       ylab="frequence")
  #abline(h=p)
}
simucf5(300,10)



#4.3.1
thcl1=function(n,k) {
  pop=Revenu
  mu=mean(pop)
  s=sqrt((length(pop)-1)/length(pop))*sd(pop)
  y=numeric(k)
  for(i in 1:k) {
    tirage=sample(pop,n,replace=T)
    y[i]=sqrt(n)*(mean(tirage)-mu)/s
  }
  hist(y,prob=T,xlab="")
  curve(dnorm(x),add=T,col="red")
}
thcl1(30,1000)

thcl1ecdf=function(n,k) {
  pop=Revenu
  mu=mean(pop)
  s=sqrt((length(pop)-1)/length(pop))*sd(pop)
  y=numeric(k)
  for(i in 1:k) {
    tirage=sample(pop,n,replace=T)
    y[i]=sqrt(n)*(mean(tirage)-mu)/s
  }
  plot(ecdf(y),xlim=c(-3,3),xlab="",ylab="")
  curve(pnorm(x),add=T,col="red")
}
thcl1ecdf(30,1000)

#4.3.2
thcl2=function(n,k,teta) {
  mu=1/teta
  s=1/teta
  y=numeric(k)
  for(i in 1:k) {
    tirage=rexp(n,teta)
    y[i]=sqrt(n)*(mean(tirage)-mu)/s
  }
  hist(y,prob=T,xlab="")
  curve(dnorm(x),add=T,col="red")
}
thcl2(30,1000,1)

thcl2ecdf=function(n,k,teta) {
  mu=1/teta
  s=1/teta
  y=numeric(k)
  for(i in 1:k) {
    tirage=rexp(n,teta)
    y[i]=sqrt(n)*(mean(tirage)-mu)/s
  }
  plot(ecdf(y),xlim=c(-3,3),xlab="",ylab="")
  curve(pnorm(x),add=T,col="red")
}
thcl2ecdf(30,1000,1)

#4.3.3
thcl3=function(n,k) {
  pop=Lavevaisselle
  p=sum(pop==1)/length(pop)
  y=numeric(k)
  for(i in 1:k) {
    tirage=sample(pop,n,replace=T)
    y[i]=(sum(tirage==1)-n*p)/sqrt(n*p*(1-p))
  }
  hist(y,prob=T,xlab="")
  curve(dnorm(x),add=T,col="red")
}
thcl3(30,1000)

thcl3ecdf=function(n,k) {
  pop=Lavevaisselle
  p=sum(pop==1)/length(pop)
  y=numeric(k)
  for(i in 1:k) {
    tirage=sample(pop,n,replace=T)
    y[i]=(sum(tirage==1)-n*p)/sqrt(n*p*(1-p))
  }
  plot(ecdf(y),xlim=c(-3,3),xlab="",ylab="")
  curve(pnorm(x),add=T,col="red")
}
thcl3ecdf(30,1000)



#4.4.1
esticonf1=function(m,sigma,alpha,n,k) {
  m1=numeric(k)
  m2=numeric(k)
  e=qnorm(1-(alpha/2))*sigma/sqrt(n)
  for(i in 1:k) {
    moy=mean(rnorm(n,m,sigma))
    m1[i]=moy-e
    m2[i]=moy+e
  }
  plot(m1,type="n",ylim=range(m1,m2),xlab="",ylab="")
  segments(1:k,m1,1:k,m2)
  abline(h=m)
  prop=mean((m1<=m)&(m<=m2))
  text(k/2,max(m2),
       paste("proportion de recouvrement = ",round(prop,4)," ",
             "alpha = ", alpha))
}
esticonf1(10,2,0.05,30,300)

#4.4.2
esticonf2=function(alpha,n,k) {
  pop=Revenu
  mu=mean(pop)
  m1=numeric(k)
  m2=numeric(k)
  for(i in 1:k) {
    echant=sample(pop,n,replace=T)
    moy=mean(echant)
    s=sd(echant)
    e=qnorm(1-(alpha/2))*s/sqrt(n)
    m1[i]=moy-e
    m2[i]=moy+e
  }
  plot(m1,type="n",ylim=range(m1,m2),xlab="",ylab="")
  segments(1:k,m1,1:k,m2)
  abline(h=mu)
  prop=mean((m1<=mu)&(mu<=m2))
  text(k/2,max(m2),
       paste("proportion de recouvrement = ",round(prop,4)," ",
             "alpha = ", alpha))
}
esticonf2(0.05,50,300)

#4.4.3

esticonf3=function(p,n,nb,alpha) {
  estip=numeric(nb)
  estip1=numeric(nb)
  estip2=numeric(nb)
  q=qnorm(1-(alpha/2))
  for(i in 1:nb) {
    estip[i]=rbinom(1,n,p)/n
    estip1[i]=estip[i]-q*sqrt(estip[i]*(1-estip[i])/n)
    estip2[i]=estip[i]+q*sqrt(estip[i]*(1-estip[i])/n)    
  }
  prop=mean((estip1<=p)&(p<=estip2))
  etendue=range(estip1,estip2)
  plot(estip,type="n",ylim=etendue,xlab="",ylab="")
  segments(1:nb,estip1,1:nb,estip2)
  abline(h=p)
  text(nb/2,max(estip2),
       paste("proportion de recouvrement = ",round(prop,4)," ",
             "alpha = ", alpha))
}

esticonf3(0.48,500,200,0.05)


#4.4.4
esticonf4=function(m,sigma,alpha,n,k) {
  m1=numeric(k)
  m2=numeric(k)
  q=qt(1-(alpha/2),n-1)
  for(i in 1:k) {
    tirage=rnorm(n,m,sigma)
    moy=mean(tirage)
    s=sd(tirage)
    e=q*s/sqrt(n)
    m1[i]=moy-e
    m2[i]=moy+e
  }
  plot(m1,type="n",ylim=range(m1,m2),xlab="",ylab="")
  segments(1:k,m1,1:k,m2)
  abline(h=m)
  prop=mean((m1<=m)&(m<=m2))
  text(k/2,max(m2),
       paste("proportion de recouvrement = ",round(prop,4)," ",
             "alpha = ", alpha))

}
esticonf4(10,2,0.05,3,200)



esticonf4bis=function(m,sigma,alpha,n,k) {
  m1=numeric(k)
  m2=numeric(k)
  q=qnorm(1-(alpha/2))
  for(i in 1:k) {
    tirage=rnorm(n,m,sigma)
    moy=mean(tirage)
    s=sd(tirage)
    e=q*s/sqrt(n)
    m1[i]=moy-e
    m2[i]=moy+e
  }
  plot(m1,type="n",ylim=range(m1,m2),xlab="",ylab="")
  segments(1:k,m1,1:k,m2)
  abline(h=m)
  prop=mean((m1<=m)&(m<=m2))
  text(k/2,max(m2),
       paste("proportion de recouvrement = ",round(prop,4)," ",
             "alpha = ", alpha))
}
esticonf4bis(10,2,0.05,3,200)

#4.4.5

esticonf5=function(m,sigma,alpha,n,k) {
  var1=numeric(k)
  var2=numeric(k)
  q1=qchisq(1-(alpha/2),n-1)
  q2=qchisq(alpha/2,n-1)
  for(i in 1:k) {
    tirage=rnorm(n,m,sigma)
    s2=var(tirage)
    var1[i]=s2*(n-1)/q1
    var2[i]=s2*(n-1)/q2
  }
  plot(var1,type="n",ylim=range(var1,var2),xlab="",ylab="")
  segments(1:k,var1,1:k,var2)
  abline(h=sigma^2)
  prop=mean((var1<=sigma^2)&(sigma^2<=var2))
  text(k/2,max(var2),
       paste("proportion de recouvrement = ",round(prop,4)," ",
             "alpha = ", alpha))

}
esticonf5(10,2,0.05,100,200)

#synthese

poistronc=function(n,la) {
  res=numeric(n)
  for( i in 1:n)
    while ((res[i]=rpois(1,la)) == 0) {}
  return(res)
}


ptemv1=function(n,la,nb,epsilon) {
  echant=poistronc(n,la)
  xbar=mean(echant)
  lambdan=xbar
  deltan=(lambdan-xbar*(1-exp(-lambdan)))/(1-xbar*exp(-lambdan))
  lambdanp1=lambdan-deltan
  i=1
  while((deltan>epsilon) & (i <= nb)) {
    cat(i,lambdan,"\n")
    lambdan=lambdanp1
    deltan=(lambdan-xbar*(1-exp(-lambdan)))/(1-xbar*exp(-lambdan))
    lambdanp1=lambdan-deltan
    i=i+1
  }
  return(lambdanp1)
}
    
ptemv2=function(vec,nb,epsilon) {
  xbar=mean(vec)
  lambdan=xbar
  deltan=(lambdan-xbar*(1-exp(-lambdan)))/(1-xbar*exp(-lambdan))
  lambdanp1=lambdan-deltan
  i=1
  while((deltan>epsilon) & (i <= nb)) {
    lambdan=lambdanp1
    deltan=(lambdan-xbar*(1-exp(-lambdan)))/(1-xbar*exp(-lambdan))
    lambdanp1=lambdan-deltan
    i=i+1
  }
  return(lambdanp1)
}
    
cpept=function(k,n,la,nb,epsilon) {
  emv=numeric(k)
  emm=numeric(k)
  for( i in 1:k) {
    echant=poistronc(n,la)
    xbar=mean(echant)
    emv[i]=ptemv2(echant,nb,epsilon)
    emm[i]=(mean(echant^2)-xbar)/xbar
  }
  l=list(emv,emm)
  names(l)=c('emv','emm')
  return(l)
}


moy=numeric(100)
vari=numeric(100)
memv=numeric(100)
memm=numeric(100)
for (i in 1:100) {
  l=cpept(200,100,2,10,0.0001)
  memv[i]=mean(l$emv)
  memm[i]=mean(l$emm)
  moy[i]=memv[i]-memm[i]
  vari[i]=var(l$emv)-var(l$emm)
}
par(mfrow=c(2,1))
plot(1:100,moy,type="o",ylim=range(moy,vari),col="blue",
     main="",xlab="",ylab="")
points(1:100,vari,typ="o", pch=23,lty=2,col="green")
abline(h=0)

plot(1:100,memv,type="o",ylim=range(memm,memv,2),col="blue",
     main="",xlab="",ylab="")
points(1:100,memm,typ="o", pch=23,lty=2,col="green")
abline(h=2)
par(mfrow=c(1,1))

intpt=function(k,n,la,nb,epsilon,alpha) {
  binf=numeric(k)
  bsup=numeric(k)
  q=qnorm(1-alpha/2)
  for( i in 1:k) {
    echant=poistronc(n,la)
    lachap=ptemv2(echant,nb,epsilon)
    e=q*(1-exp(-lachap))/sqrt(n)*sqrt(lachap/(1-lachap*exp(-lachap)-exp(-lachap)))
    binf[i]=lachap-e
    bsup[i]=lachap+e
  }
  plot(binf,type="n",ylim=range(binf,bsup),xlab="",ylab="")
  segments(1:k,binf,1:k,bsup)
  abline(h=la)
  prop=mean((binf<=la)&(la<=bsup))
  text(k/2,max(bsup),
       paste("proportion de recouvrement = ",round(prop,4)," ",
             "alpha = ", alpha))
}


intpt(200,100,2,10,0.0001,0.05)
