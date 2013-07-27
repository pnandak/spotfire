# 6.1
lap=data.frame(txfib=c(3,4,4,17,24,45,55,68,73),
               lip=c(0.9,1.3,1.0,2.4,2.8,4.4,5.2,6.3,6.6))
attach(lap)

meteo=read.table("meteo.txt",header=T)
attach(meteo)

formule.lap=lip~txfib

lap.lm=lm(lip~txfib)

lm(lip~ -1 + txfib)

lm(ren~preh+tempj+prej+radj)

meteo.lm=lm(ren~.,data=meteo)

meteobis.lm=lm(ren~.,data=meteo,subset=c(1:8,10:11))
meteobis.lm=lm(ren~.,data=meteo,subset=c(-9))
meteobis.lm=lm(ren~.,data=meteo,subset=(prej>50))

summary(lap.lm)

resmeteo.lm=summary(meteo.lm)

meteo.lm

resmeteo.lm


model0=lm(ren~tempj+prej)
model1=lm(ren~preh+tempj+prej+radj)

anova(model0,model1)

anova(lm(ren~preh+tempj),lm(ren~tempj+prej))

anova(lm(ren~1),lm(ren~.,data=meteo))

anova(meteo.lm)

library('car')
Anova(lm(ren~preh+tempj+prej+radj))

Anova(lm(ren~preh+tempj+radj))

Anova(lm(ren~preh+radj))

step(meteo.lm,k=2)
step(meteo.lm,k=log(11))

newmeteo=data.frame(preh=c(100,120),
  tempj=c(16,19),prej=c(80,50),radj=c(1,15))
predimoyrend=predict(meteo.lm,newdata=newmeteo)

predict(meteo.lm)
fitted(meteo.lm)

#.....

# Ex 1

conso.dat=read.table("donneesconso.txt",header=T)
attach(conso.dat)

compter=function(a,b) {
  d=numeric()
  for(i in 1:(length(a)))d[i]=sum(b==a[i])
  names(d)=as.character(a)
  d
}

nom.region=c('GR1','GR2','GR3')
nom.age=c('TA1','TA2')

conso.liste=list()

for(i in 1:3) {
  conso.liste[[i]]=list()
  for(j in 1:2) {
    conso.liste[[i]][[j]]=list()
    conso.liste[[i]][[j]][[1]]=
      Revenu[(Region==nom.region[i])&(Age==nom.age[j])]
    conso.liste[[i]][[j]][[2]]=
      Loisirs[(Region==nom.region[i])&(Age==nom.age[j])]
    conso.liste[[i]][[j]][[3]]=
      Pates[(Region==nom.region[i])&(Age==nom.age[j])]
    conso.liste[[i]][[j]][[4]]=
      table(Cafe[(Region==nom.region[i])&(Age==nom.age[j])])
    maxi=max(Enfants)
    conso.liste[[i]][[j]][[5]]=
      #compter(0:maxi,
              Enfants[(Region==nom.region[i])&
                             (Age==nom.age[j])]#)
    conso.liste[[i]][[j]][[6]]=
      mean(Magnetoscope[(Region==nom.region[i])&
                        (Age==nom.age[j])])
    conso.liste[[i]][[j]][[7]]=
      mean(Lavevaisselle[(Region==nom.region[i])&
                         (Age==nom.age[j])])
    names(conso.liste[[i]][[j]])=names(conso.dat)[3:9]
  }
  names(conso.liste[[i]])=nom.age
}
names(conso.liste)=nom.region



par(mfrow=c(3,2))

for (i in 1:3) {
  for (j in 1:2) {
    plot(conso.liste[[i]][[j]][[1]],
         conso.liste[[i]][[j]][[2]],
         xlim=range(Revenu),ylim=range(Loisirs),
         xlab="Revenu",ylab="Loisirs",
         main=paste(names(conso.liste[[i]])[j],
           names(conso.liste)[i]))
    reg=lm(Loisirs~Revenu,
      subset=(Region==nom.region[i])&(Age==nom.age[j]))
    abline(reg)
    sig=round(summary(reg)$sigma,2)
    pente=round(coef(reg)[2],4)
    text(mean(range(Revenu)),max(Loisirs)-1000,
         bquote(paste(hat(beta)[1]==.(pente),"\ \ \ \ \ ",
                    hat(sigma)==.(sig))))
  }
}



# Ex 2
thom.don=read.table("thom.txt",header=T)
attach(thom.don)
thom1=lm(ya~x)
thom2=lm(yb~x)
thom3=lm(yc~x)
thom4=lm(yc~x)
thom5=lm(ye~xe)

par(mfrow=c(2,3))

plot(x,ya,xlim=range(x,xe),ylim=range(ya,yb,yc,yd,ye))
abline(thom1)

plot(x,yb,xlim=range(x,xe),ylim=range(ya,yb,yc,yd,ye))
abline(thom2)

plot(x,yc,xlim=range(x,xe),ylim=range(ya,yb,yc,yd,ye))
abline(thom3)

plot(x,yd,xlim=range(x,xe),ylim=range(ya,yb,yc,yd,ye))
abline(thom4)

plot(xe,ye,xlim=range(x,xe),ylim=range(ya,yb,yc,yd,ye))
abline(thom5)

summary(thom1)
summary(thom2)
summary(thom3)
summary(thom4)
summary(thom5)



# Ex 3

simulreg=function(design,beta0,beta1,sigma,k) {
  betachap1moins=numeric(k)
  betachap1plus=numeric(k)
  n=length(design)
  q=qt(0.975,n-2)
  for (i in 1:k) {
    obs=rnorm(n,beta0+beta1*design,sigma)
    datareg=data.frame(x=design,y=obs)
    resudatareg=summary(lm(y~x,data=datareg))
    beta1res=resudatareg$coefficients[2,1]
    stderr=resudatareg$coefficients[2,2]
    betachap1moins[i]=beta1res-stderr*q
    betachap1plus[i]=beta1res+stderr*q
  }
  plot((betachap1moins+betachap1plus)/2,type="n",
       main="",xlab="",ylab="",
       ylim=range(betachap1moins,betachap1plus))
  segments(1:k,betachap1moins,1:k,betachap1plus)
  abline(h=beta1)
  prop=mean(betachap1moins<=beta1 & betachap1plus>=beta1)
  text(mean(1:k),max(betachap1plus),paste("prop = ", prop))
}

simulreg(rep(c(10,12,14,16,18,20),c(3,3,3,3,3,3)),2.5,1,3,200)

# Ex 4
lap=data.frame(txfib=c(3,4,4,17,24,45,55,68,73),
               lip=c(0.9,1.3,1.0,2.4,2.8,4.4,5.2,6.3,6.6))

lap.lm=lm(lip~txfib,data=lap)

newlap=data.frame(txfib=3:73)

respredesp=predict(lap.lm,newdata=newlap,interval="confidence")
resprednew=predict(lap.lm,newdata=newlap,interval="prediction")

plot(lap[,1],lap[,2],type="p",main="",xlab="txfib",ylab="",
     range(respredesp,resprednew,lap[,1]))
abline(lap.lm)
lines(newlap[,1],respredesp[,2],col="red")
lines(newlap[,1],respredesp[,3],col="red")
lines(newlap[,1],resprednew[,2],col="green")
lines(newlap[,1],resprednew[,3],col="green")

# Ex 5

meteo=read.table("meteo.txt",header=T)
attach(meteo)

meteo1=lm(ren~preh+tempj,data=meteo)

summary(meteo1)

# pvalue(F-statistique) <alpha : au moins une des 2 variables
# intervient
# Confirmation on rejette beta1=0 et beta2=0

meteo2=lm(ren~prej+radj,data=meteo)

summary(meteo2)

# pvalue(F-statistique) <alpha : au moins une des 2 variables
# intervient
# Mais on ne rejette pas  beta1=0 et beta2=0 !!!!!

library("ellipse")

plot(ellipse(meteo1,c(2,3)),type="l")
points(0,0)

x11()
plot(ellipse(meteo2,c(2,3)),type="l")
points(0,0)

# Ex 6
library('alr3')

data(forbes)
attach(forbes)
plot(Temp,Lpres)

forbes.lm=lm(Lpres~Temp)
summary(forbes.lm)

plot(Temp,Lpres)
abline(forbes.lm)

# pvalue(F-statistique) <alpha : la variable 
# intervient de façon hautement significative
# Confirmation on rejette beta1=0 (de façon
# hautement significative)

data(stackloss)
stack.df=data.frame(stack.loss,stack.x)
attach(stack.df)

stack.lm=lm(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=stack.df)
summary(stack.lm)

# pvalue(F-statistique) <alpha : au moins une des 3 variables
# intervient
# Confirmation on rejette beta1=0 et beta2=0 mais pas beta3=0

library('car')

Anova(lm(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=stack.df))

Anova(lm(stack.loss~Air.Flow+Water.Temp,data=stack.df))

step(stack.lm,k=2)
step(stack.lm,k=log(21))


data(LifeCycleSavings)

pairs(LifeCycleSavings, main = "LifeCycleSavings data")
fm1= lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)

summary(fm1)

step(fm1,k=2)
step(fm1,k=log(50))
