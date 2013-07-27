# Question 1
# H0: La moyenne est de 70kg
# H1: La moyenne n'est pas de 70kg
poids=c(71,62,63,64,64,63,65,63)
t.test(poids,mu=70) 
# p-value=7.9.10^-4<0.1% => On rejette H0.
# La différence par rapport à la moyenne
# annoncée est hautement significative

# Question 2
# H0: La teneur en lipide est égale pour un mâle ou une femelle
male=c(187,148,162,147,132,180,165,159,155,142,168,156,162)
femelle=c(197,179,167,174,185,161,199,165,167,161,191,175,182)
# Les variances sont-elles égales?
var.test(male,femelle)
# p-value > 0.05 on ne rejette pas l'hypothèse
# d'égalité des variances

t.test(male,femelle,var.equal=T)
# p-value=0.0025<1% => On rejette H0.
# La différence de teneur en lipide est très significative

# Question 3
# H0: La teneur moyenne en lactoferrine d'une vache à lait
# est 1mg/mL inférieure à celle d'une vache à viande
lait=c(1.65,1.60,1.76,1.68,1.61,1.42,1.74,1.68)
viande=c(0.57,0.41,0.52,0.54,0.37,0.51,0.54,0.47)
t.test(lait,viande,mu=1.,alternative="greater", var.equal=T)
# p-value<1% => On rejette H0.
# La différence de teneur moyenne en
# lactoferrine d'une vache à lait et d'une vache à viande est 
# supérieure à 1mg/l  (5% chance de se tromper)

var.test(lait,viande)
# p-value > 5%, au vue des données on ne peut pas rejetter
# l'hypothèse dégalité des variances

# Question 4
#H0: La teneur moyenne en protéines est supérieure à 6.2g
galette=c(5.8,5.8,6.4,6.2,6.0,5.9,5.8,5.9,6.2,5.8,5.9,6.0,6.1)
t.test(galette,mu=6.2,alternative="less")
# p-value<1% => On rejette H0. La teneur moyenne en protéines
# est très significativement inférieure à 6.2g

# Question 5
# H0: Il n'y a pas de lien entre l'achat de bière
# et l'achat de chips
donnees=matrix(c(92,32,10,12),ncol=2,byrow=T)
chisq.test(donnees)
# p-value=1.44%>1% => On ne rejette pas H0.
# Les données ne permettent pas d'affirmer qu'il y a un  lien
# entre l'achat de bière et l'achat de chips

# Question 6
# H0: La teneur en lactage entre le
# début de l'affinage et le 2è jour de l'affinage
# augmente d'au moins 8g/kg
debut=c(32,34.6,29.1,36.2,40.1)
fin=c(41.5,42.6,38.5,45.9,49.1)
t.test(fin,debut,mu=8.,alternative="greater",paired=T)
# p-value<5% => On rejette H0. Au seuil de niveau 5%, il y a
# augmentation d'au moind 8g/kg de la teneur en lactate
# entre le début de l'affinage et le 2è jour de l'affinage 

# Question 7
#haricot=matrix(c(182,230,240-182,310-230),ncol=2,byrow=T)
prop.test(c(182,230),c(240,310))
# p-value>5% => On ne rejette pas H0. Les données ne permettent
# pas d'affirmer qu'il y a une différence d'appréciation 

# Question 8
G1=c(8,12,15,14,10,10,5,9,18,13)
G2=c(8.5,11,18,16,9,12,6,9,20,16)

t.test(G1,G2,paired=T)
# p-value < 5%; rejet de H0. Les jugements
# ne sont pas identiques

# Question 9 
prop.test(20,300,p=0.038)
binom.test(20,300,p=0.038)
# p-value <1%. Rejet de H0

# Question 10
collostrom=c(1.50,2.39,1.82,2.45,2.45,2.26,1.95,
  1.66,2.33,2.08,2.00,1.78)
lait=c(1.21,2.18,1.43,2.24,2.34,2.17,1.82,1.36,
  2.15,1.80,1.54,1.73)
t.test(lait,collostrom,paired=T)
# p-value <5% rejet de H0


# Question 11
prop.test(c(122,98),c(150,130),alternative="greater")
# p-value > 5% Non-rejet de H0. Il n'investit pas

# Question 12
ancien=c(112,114,117,114,120,118)
nouveau=c(140,151,146,146,160,138)
t.test(nouveau,ancien,alternative="greater")
# p-value < 5% Rejet de H0. Il investit


# Question 13
prop.test(8,10,0.5,alternative="greater")
# p-value > 5% Non-rejet de H0.
binom.test(8,10,0.5,alternative="greater")
# p-value > 5% Non-rejet de H0.
prop.test(8,10,0.5,alternative="greater",correct=F)
p-value < 5% Rejet de H0.


# Question 14
xinf=c(0,4,8,12,16,20,24)
xsup=c(4,8,12,16,20,24,28)
centre=(xinf+xsup)/2
n=c(2,5,12,14,11,5,1)
donnees=rep(centre,n)
hist(donnees,breaks=c(0,4,8,12,16,20,24,28),prob=T)
m=sum(centre*n)/sum(n)
s=sqrt(sum(centre^2*n)/sum(n)-m^2)
curve(dnorm(x,m,s),add=T)

effth=sum(n)*(pnorm(xsup,m,s)-pnorm(xinf,m,s))
effth=c(sum(n)-sum(effth),effth)
effobs=c(0,n)

chisq.test(effth,effobs)

chisq.test(effobs,p=effth/sum(n))

chiobs=sum((effobs-effth)^2/effth)



# Question 15
haricot=c(773,231,238,59)
chisq.test(haricot,p=c(9/16,3/16,3/16,1/16))
#p-value=0.025 < 5% => Rejet du respect de
# la loi de la génétique

# Question 16
adolescent=c(59,41,58,54,55,72,53,52,49,57,42,
  70,58,42,53,57,68,40,65,54,49,32,56,50,59,
  43,48,65,55,51,51,44,51,59,62,45,53,55,55,
  49,34,52,69,45,54,559,36,36,29,52)
chisq.test(adolescent)
#p-value=2.10^-16 < 5% => Rejet de la normalité




# Question 17 
compter=function(b)
{
d=numeric(8)
for(i in 0:6)
d[i+1]=sum(b==i)
d[8]=sum(b>=7)
d
}
conso=read.table("donneesconsommation.txt",header=T)
attach(conso)
RA11=compter(Enfants[(Region=='GR1')&(Age=='TA1')])
lambda11=mean(Enfants[(Region=='GR1')&(Age=='TA1')])
RA21=compter(Enfants[(Region=='GR2')&(Age=='TA1')])
lambda21=mean(Enfants[(Region=='GR2')&(Age=='TA1')])
RA31=compter(Enfants[(Region=='GR3')&(Age=='TA1')])
lambda31=mean(Enfants[(Region=='GR3')&(Age=='TA1')])
RA12=compter(Enfants[(Region=='GR1')&(Age=='TA2')])
lambda12=mean(Enfants[(Region=='GR1')&(Age=='TA2')])
RA22=compter(Enfants[(Region=='GR2')&(Age=='TA2')])
lambda22=mean(Enfants[(Region=='GR2')&(Age=='TA2')])
RA32=compter(Enfants[(Region=='GR3')&(Age=='TA2')])
lambda32=mean(Enfants[(Region=='GR3')&(Age=='TA2')])
chisq.test(RA11,p=c(dpois(0:6,lambda11),1-ppois(6,lambda11)))
chisq.test(RA21,p=c(dpois(0:6,lambda21),1-ppois(6,lambda21)))
chisq.test(RA31,p=c(dpois(0:6,lambda31),1-ppois(6,lambda31)))
chisq.test(RA12,p=c(dpois(0:6,lambda12),1-ppois(6,lambda12)))
chisq.test(RA22,p=c(dpois(0:6,lambda22),1-ppois(6,lambda22)))
chisq.test(RA32,p=c(dpois(0:6,lambda32),1-ppois(6,lambda32)))
#Le nombre d'enfants d'un couple ('GR1','TA1') ne suit
# pas de loi de poisson au risque 5% (Khi-obs>12.6)
#On ne peut pas rejeter l'hypothèse pour les autres couples

par(mfrow=c(3,2))
RA11=Revenu[(Region=='GR1')&(Age=='TA1')]
plot(density(RA11))
RA21=Revenu[(Region=='GR2')&(Age=='TA1')]
plot(density(RA21))
RA31=Revenu[(Region=='GR3')&(Age=='TA1')]
plot(density(RA31))
RA12=Revenu[(Region=='GR1')&(Age=='TA2')]
plot(density(RA12))
RA22=Revenu[(Region=='GR2')&(Age=='TA2')]
plot(density(RA22))
RA32=Revenu[(Region=='GR3')&(Age=='TA2')]
plot(density(RA32))
