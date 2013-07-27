#3. Distributions des lois de  probabilité

#3.1. Densite
#3.1.1. Calcul de la densite :  P(X=x) ou  f_X(x)
dbinom(4,8,0.3)

dbinom(6,8,0.3)

dbinom(c(4,6),8,0.3)

dnorm(5,2,3)

resu1=  dbinom(c(4,6),8,0.3)

resu2=  dnorm(5,2,3)

#3.1.2. Representation de la distribution

plot(0:5, dbinom(0:5,5,0.2), type="h",
     xlab="x", ylab="P(X=x)")

curve(dnorm(x,5,1.5),0.5,9.5)

#Ex 1
plot(0:8, dpois(0:8,1),
     type="h", xlab="x", ylab="P(X=x)")

#Ex 2
curve(dchisq(x,3),0,10,
     type="l", xlab="x" )


#Ex 3
par(mfrow=c(2,1))
plot(0:50, dbinom(0:50,50,0.08),
     type="h", xlab="x", ylab="P(X=x)",
     ylim=c(0,0.25),main="Loi B(50,0.08)")
plot(0:50, dpois(0:50,4),
     type="h", xlab="x", ylab="P(X=x)",
     ylim=c(0,0.25), main = "Loi P(0.08)")
par(mfrow=c(1,1))

#Ex 4
par(mfrow=c(2,2))
curve(dnorm(x,4,1),1,8,ylab='',)
curve(dnorm(x,5,1),1,8,col=3,add=T,ylab='')
title("N(4,1) et N(5,1)")
curve(dnorm(x,4,1),-2,10,ylab='')
curve(dnorm(x,4,2),-2,10,col=3,add=T,ylab='')
title("N(4,1) et N(4,4)")
curve(dnorm(x,4,1),-1,11,ylab='')
curve(dnorm(x,5,2),-1,11,col=3,add=T,ylab='')
title("N(4,1) et N(5,4)")
par(mfrow=c(1,1))

#Ex 5
plot(0:50, dbinom(0:50,50,0.4),
     type="h", xlab="x", ylab='')
curve(dnorm(x,20,sqrt(12)),0,50,col=3,add=T,ylab='')
title("B(50,0.4) et N(20,12)")

#Ex 6
plot(0:20, dbinom(0:20,100,0.01),
     type="h", xlab="x", ylab='',ylim=c(0,0.4))
curve(dnorm(x,1,sqrt(0.99)),0,20,col=3,add=T,ylab='')
title("B(100,0.01) et N(1,0.99)")



#3.2. Fonction de repartition


#3.2.1. Calcul des valeurs prises par la fonction de repartition
pbinom(4,5,0.6).

valeurs=  dbinom(0:4,5,0.6)

sum(valeurs)

sum(dbinom(0:4,5,0.6))

pnorm(12,9,2)


#3.2.2. Representation graphique de la fonction de repartition
#       d'une loi

#3.2.2.1. X est discrète
library('stepfun')
plot(stepfun(0:15, c(0,pbinom(0:15,15,0.6))))

#3.2.2.2. X est continue
curve( pnorm(x,9,2),3,15)

abline(h=1)

abline(h=0)

#Ex 7
pnorm(-0.5,0,1)
1-pnorm(1.5,0,1)
pnorm(1.96,0,1)-pnorm(-1.96,0,1)
pnorm(2.58,0,1)-pnorm(-2.58,0,1)
1-pnorm(3,0,1)+pnorm(-3,0,1)

#Ex 8
pnorm(20,15,3)-pnorm(16,15,3)
1-pnorm(18,15,3)
pnorm(6,15,3)
1-pnorm(20.88,15,3)+pnorm(15-5.88,15,3)

curve(pnorm(x,15,3),6,24)

#Ex 9
library('stepfun')
plot(stepfun(0:50,c(0,pbinom(0:50,50,0.4))))
curve(pnorm(x,20,sqrt(12)),0,50,add=T,col=3)



#3.3. Fractiles  (ou quantiles)  d'une loi
qbinom(0.25,5,0.6)

pbinom(0:5,5,0.6)

qnorm(0.975,0,1)

pnorm(1.96) 

#Ex 10a
p=c(0.00135,0.025,0.95,0.999,0.995,0.99865)
u=qnorm(p)
cbind(p,u)

#Ex 10b
p=c(0.975,0.025)
x=qnorm(p,19,sqrt(3))
cbind(p,x)

x[1]-19-sqrt(3)*qnorm(0.975)



#3.4. Simulation


#3.4.1. Simulation de tirages  aleatoires dans un
#       ensemble fini. l'instruction sample

#3.4.1.1.  size
sample(1:6)

sample(1:6, size= 2)

sample(1:6, 2)


#3.4.1.2.  replace
sample(1:6)

sample(1:6, size=7)

sample(1:6,2, replace=T)

sample(1:6,2)  

sample(1:6,1)

sample(1:6,1, replace=T)


#3.4.1.1.  prob

production = c(rep("NC",2), rep("C",98))

sample(production,80, replace=T) 

echantillon=  sample(production,80, replace=T)
echantillon

qualite=c("NC","C")

sample(qualite,80, replace=T,
       prob=c(2/100,98/100))


compter=function(a,b) {
  d=numeric()
  for(i in 1:(length(a)))d[i]=sum(b==a[i])
  names(d)=as.character(a)
  d
}

couleurs=  c ("tr","ca","pi","co")

jeu= rep(couleurs, rep (8,4)) 

tirage= function(nb,k){ 
  resu= logical(nb) 
  for(i in 1:nb){
    resutirage= sample(jeu,5) 
    if(i<=k) cat(resutirage,"\n")
    compteresutirage=compter(couleurs,resutirage)
    compterrouge= sum(compteresutirage [c("ca","co")])
    resu[i]= compterrouge==2 
  }
  freq = mean(resu) 
  prob = choose(16,2)*choose(16,3)/choose(32,5) 
  res= c(prob, freq) 
  names(res)= c("prob","freq") 
  return(res)
}

tirage(10,10)

tirage(10000,10)



#3.4.2. Simulation par des lois preprogrammees.

rpois(10,2)

rbinom(20,5,0.6)

rbinom(80,1,0.02)

sum(rbinom(80,1,0.02))
rbinom(1,80,0.02)

x = norm(15,22,2)

x = round(rnorm(15,22,2),2)

#Ex 11
par(mfrow=c(1,2))
simul1=rnorm(1000,15,sqrt(3))
hist(simul1,col=3,probability=T,
     ylim=c(0,0.25),
     main="Hist de rnorm(1000,15,sqrt(3)) et\n dnorm(x,15,sqrt(3))")
curve(dnorm(x,15,sqrt(3)),col=4,add=T)
boxplot(simul1,
        main="Boxplot de\n rnorm(1000,15,sqrt(3))")

#Ex 12
simul2=rpois(1000,1)
maxi=max(simul2)
nbsimul2=compter(0:maxi,simul2)
bppois=barplot(nbsimul2,col=0)
points(bppois, dpois(0:maxi,1)*1000,type="h")
axis(4,at=seq(0,1000,by=100),labels=seq(0,1,by=0.1))

#Ex 13

peage=function(m1,m2,sigma,n) {
  attente=numeric(n)
  cabines=c("C1","C2")
  for(i in 1:n) {
    tirage=sample(cabines,1)
    if(tirage=="C1") 
      attente[i]=rnorm(1,m1,sigma)
    
    else attente[i]=rnorm(1,m2,sigma)
    
  }
  a=max(hist(attente,prob=T,xlab="attente")$density)
  moy=mean(attente)
  ecart=sd(attente)
  curve(dnorm(x,moy,ecart),add=T)
  text(moy,a,paste("moy=",round(moy,2),
                   " ecart=",round(ecart,2)))
  curve(0.5*(dnorm(x,m1,sigma)+dnorm(x,m2,sigma)),
        add=T,col="red")
}

par(mfrow=c(2,1))
peage(50,51,2,1000)
peage(50,60,2,1000)

#Ex 14

par(mfrow=c(2,2))
curve(dnorm(x),-3,3)
curve(dt(x,2),-3,3,col='red',add=T)
legend("topleft",legend=c("N(0,1)","T(2)"), col=c('black','red'),lwd=1)
curve(dnorm(x),-3,3)
curve(dt(x,3),-3,3,col='red',add=T)
legend("topleft",legend=c("N(0,1)","T(3)"), col=c('black','red'),lwd=1)
curve(dnorm(x),-3,3)
curve(dt(x,10),-3,3,col='red',add=T)
legend("topleft",legend=c("N(0,1)","T(10)"), col=c('black','red'),lwd=1)
curve(dnorm(x),-3,3)
curve(dt(x,30),-3,3,col='red',add=T)
legend("topleft",legend=c("N(0,1)","T(30)"), col=c('black','red'),lwd=1)

#Ex 15

stu=function(n,nb,mu,sigma) {
  vec=numeric(nb)
  for (i in 1:nb) {
    simul=rnorm(n,mu,sigma)
    xbar=mean(simul)
    sech=sd(simul)
    vec[i]=sqrt(n)*(xbar-mu)/sech
  }
  vecnew=vec[vec>=-3 & vec<=3]
  par(mfrow=c(2,1))
  hist(vecnew,probability=T)
  curve(dnorm(x),-3,3,add=T)
  curve(dt(x,n-1),-3,3,add=T,col='red')
  legend("topleft",
         legend=c("N(0,1)",paste("T(",n-1,")",sep="")),
         col=c('black','red'),
         lwd=1)
  plot(ecdf(vecnew))
  curve(pnorm(x),-3,3,add=T,col='yellow')
  curve(pt(x,n-1),-3,3,add=T,col='red')
  legend("topleft",
         legend=c("F_N(0,1)",paste("F_T(",n-1,")",sep="")),
         col=c('yellow','red'),
         lwd=1)
  par(mfrow=c(1,1))
}

stu(3,1000,10,2)

#Ex 16
par(mfrow=c(2,3))
curve(dchisq(x,1),0.01,10,main="Chisq(1)")
curve(dchisq(x,2),0.01,10,main="Chisq(2)")
curve(dchisq(x,3),0.01,10,main="Chisq(3)")
curve(dchisq(x,5),0.01,10,main="Chisq(5)")
curve(dchisq(x,10),0.01,20,main="Chisq(10)")
curve(dchisq(x,20),0.01,60,main="Chisq(20)")

#Ex 17

khi2=function(n,nb,mu,sigma) {
  vec=numeric(nb)
  for (i in 1:nb) {
    simul=rnorm(n,mu,sigma)
    sech2=var(simul)
    vec[i]=(n-1)*sech2/sigma^2
  }
  vecnew=vec[vec>=0.01 & vec<=10]
  hist(vecnew,probability=T)
  curve(dchisq(x,n-1),0.01,10,add=T)
  legend("topright",
         legend=paste("Chisq(",n-1,")",sep=""),
         col='black',
         lwd=1)
}

khi2(4,10000,10,2)

#Ex 18
par(mfrow=c(2,2))
curve(dexp(x,1),0,3,main="exp(1)")
curve(dexp(x,2),0,3,main="exp(2)")
curve(dexp(x,0.5),0,20,main="exp(0.5)")
curve(dexp(x,0.1),0,60,main="exp(0.1)")


