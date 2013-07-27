conc=c(0.02,0.02,0.06,0.06,0.11,0.11,0.22,
       0.22,0.56,0.56,1.1,1.1)
vit=c(76,47,97,107,123,139,159,152,191,201,
      207,200)

prot=data.frame(conc,vit)

plot(prot)

prot.nls=nls(vit~V*conc/(K+conc),data=prot,
  start=list(V=200,K=0.05))

prot.stat=summary(prot.nls)
prot.stat

library(ellipse)

plot(ellipse(prot.nls,which=c(1,2)))

beta=coef(prot.nls)
plot(prot)
curve(beta[1]*x/(beta[2]+x),add=T)

plot(fitted(prot.nls),resid(prot.nls))
qqnorm(resid(prot.nls))



#Ex 1
concp=1/conc
vitp=1/vit

protp.lm=lm( vitp~concp)
summary(protp.lm)
betap=coef(protp.lm)
plot(prot)
curve(beta[1]*x/(beta[2]+x),add=T,col="red")
curve(1/betap[1]*x/(betap[2]/betap[1]+x),
      add=T,col="green")
legend("bottomright",c("nls","lm"),
       col=c("red","green"),lty=1)


#Ex 2
#lim (infini) f(x)=theta2
#lim (-infini) f(x)=theta1
# f'(x)=-\frac{(\theta_1-\theta_2)\theta_4e^{\theta_3+\theta_4*x}}
#             {(1+e^{\theta_3+\theta_4*x})^2}
# f"(x)=0 ssi x=-\frac{\theta_3}{\theta_4}
# x_0=-\frac{\theta_3}{\theta_4}
# f(x_0)=\frac{\theta_1+\theta2}{2}

ldil=seq(1,5,by=0.5)
dens=c(1.81,1.80,1.75,1.6,1.2,0.6,0.3,0.1,0.08)

serum=data.frame(ldil,dens)

plot(serum,type="b")

locator(2)

#\theta_2 = 0.082
#\theta_1 = 1.82
# point d'inflexion entre 3 et 3.5 => pente celle
# de (3,f(3)) à (3.5,f(3.5))
 (0.6-1.2)/(3.5-3)
#\theta_4=-4*(-1.2)/(1.82-0.082)=2.761795
#\theta_3=-3.25*2.761795=-8.975834

serum.nls=nls(dens~O2+(O1-O2)/(1+exp(O3+O4*ldil)),
  data=serum,
  start=list(O1=1.82,O2=0.082,O3=-8.975834,
             O4=2.761795))

serum.stat=summary(serum.nls)
serum.stat

serum.coef=coef(serum.nls)

plot(serum)
curve(serum.coef[2]+
       (serum.coef[1]-serum.coef[2])/
         (1+exp(serum.coef[3]+serum.coef[4]*x)),
      add=T)


#Ex 3

d=1:10
n=c(10,9,12,10,8,14,20,15,16,10)
k=c(1,1,2,2,2,4,9,10,14,9)
f=k/n

dl50=data.frame(d,f)

plot(d,f,type="b")
locator(2)

#\theta_2 = 1
#\theta_1 = O.1
# point d'inflexion entre 7 et 8 => pente celle
# de (7,f(7)) à (8,f(8))
 (0.6666667-0.45)/(8-7)
#\theta_4=-4*0.2166667/(0.1-1)=0.9629631
#\theta_3=-7.5*0.9629631=-7.222223
dl50.nls=nls(f~O2+(O1-O2)/(1+exp(O3+O4*d)),
  data=dl50,
  start=list(O1=0.1,O2=1,O3=-7.222223,O4=0.9629631))


dl50.stat=summary(dl50.nls)
dl50.stat

dl50.coef=coef(dl50.nls)

plot(dl50)
curve(dl50.coef[2]+
       (dl50.coef[1]-dl50.coef[2])/
         (1+exp(dl50.coef[3]+dl50.coef[4]*x)),
      add=T)
abline(h=0.5,col="red")
locator(1)

#DL50= 7,126
