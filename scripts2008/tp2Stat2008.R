#2
enquete=read.table("enquete.txt",header=T)
attach(enquete)

#2.1
#2.1.1
plot(poids)

plot(1:10,poids)

plot(c(4,1,2,6,5,9,7,10,3,8),poids)

#2.1.1.1 type = "c"
plot(poids,type ="p")
plot(poids,type ="n")
plot(poids,type ="l")
plot(poids,type ="h")
plot(poids,type ="o")
plot(poids,type ="b")


# 2.1.1.2. xlab="string" et ylab="string"
plot(poids,type = "b",xlab="numero")


#2.1.1.3 pch = n
plot(poids, pch=4, xlab="numero")
plot(poids, type = "b", pch="a", xlab="numero")

#2.1.1.4. xlim = c(lo,hi) et/ou  ylab = c(lo,hi)
plot(poids, type = "b", xlab="numero", xlim=c(-10,20),ylim=c(30,90))



#2.1.1.5. xaxt="n" et/ou  yaxt="n"
plot(poids, pch=4, xlab="numero", xaxt="n")

#2.1.1.6. col ="string"
plot(poids, pch=4, xlab="numero", col="red")

#2.1.1.7. cex =l (resp. cex.axis=l)
plot(poids, pch="a", xlab="numero", cex=0.5)

#2.1.1.8 lty = m où m=1,2,3,...change la nature des lignes
#        qui relient les points.
plot(poids,type="l",lty=2)


#2.1.1.9.  axes = F efface l'entourage de la fenêtre.
plot(poids, type="l", lty=2,  axes = F)


#2.1.1.10.  main= "string", où string est une chaîne de
#           caractères met un titre au graphe.
plot(poids, type="l",  main= "poids des personnes")


#2.1.2. L'instruction curve
curve( sin(x),-3,10)

myfonction = function(x) sin(cos(x)*exp(-x/2))
curve(myfunction, -8,7)

curve(myfunction, -8,7, n=2001)

curve(sin(cos(x)*exp( -x/2)), -8,7,n=2001)

#2.1.3. L'instruction barplot
barplot(poids)

efcouleur  =  table(couleur)
barplot(efcouleur)

barplot(table(couleur))

choixpossible= c("Blanc","Noir","Bleu","Rouge")

efcouleur  =  compter(choixpossible,couleur)
barplot(efcouleur)

barplot(table(nb))

barplot(compter(0 : 4, nb))

#2.1.3.1.  space=l où l est un nombre réel, change
# l'écartement entre les barres qui seront
# espacées de l fois la
# largeur moyenne des barres.
barplot(poids, space=2)

barplot(poids, xlim=c(-3,33), width=0.8)



#2.1.3.2.  names.arg= string
prenoms= c("Karen", "Elodie", "Paul", "Paul", "Elsa",
  "Karen", "Aurelie", "Elsa", "Karen","Sophie")
barplot(poids, space=2, names.arg=prenoms)


barplot(poids, space=2, names.arg=prenoms, cex.names=0.4)

barplot(table(couleur)).



#2.1.3.3.  horiz= L
barplot(poids,  horiz=TRUE)

#2.1.4. L'instruction pie
pie(efcouleur)



#2.1.5. L'instruction hist
hist(poids)
hist(poids, prob=T)

#2.1.5.1.  breaks=vecteur
hist(poids, breaks=c(45,60,75))

hist(poids, breaks=c(45,60,75), prob=T)

hist(poids, breaks=c(45,50,70,75))

#2.1.5.2.  right= L
hist(poids, right=F)


#2.1.6. L'instruction boxplot
boxplot(poids) 


#2.1.7. Creer plusieurs graphiques
#2.1.7.1. Plusieurs graphiques sur la meme fenetre
par(mfrow=c(2,1))
hist(poids)
plot(poids)
par(mfrow=c(1,1))

#2.1.7.2. Plusieurs graphiques successifs
par(ask=T)
hist(poids)
plot(poids)
par(ask=F)


#2.2. Additionner un graphique a un graphique existant
hpoids=hist(poids)

bpoids=barplot(table(couleur))




#2.2.1. L'instruction points
plot(poids)
points(c(2,2,6.3),c(55,60,70), cex=2,pch=5, col="red")

#2.2.2. L'instruction lines
lines(1:8, rep(60,8)+c(-10,10), lty=3)

#2.2.3. L'instruction text
text(7,72, paste("moyenne= ", mean(poids)))

text (poids+c( rep(-1,4),1,rep(-1,5)),prenoms, cex=0.5)

#2.2.4. L'instruction abline
abline(h=65)

#2.2.5. L'instruction curve(.....,add=T)
curve(60+ sin(7*x),add=T)

#2.2.6. L'instruction segments
segments(c(4,8),c(55,60),c(2,10),c(70,65))

#2.2.7. L'instruction title
title( paste("Graphique du poids des  ",
             length(poids),"  personnes"))
 
#2.2.8. L'instruction axis
plot(poids, xaxt="n")
axis(1,1:10,1:10)

#2.2.8. L'instruction legend
plot(poids,type="l")

lines(c(2,3,5),c(50,48,62), col="red")


legend(8,65, legend=c("poids","ajout"),
       fill=c("black","red"))

plot(poids,type="l")
lines(c(2,3,5),c(50,48,62), lty=2)

legend(8,65, legend=c("poids","ajout"), lty=1:2)

#2.2.9. Les problemes d'echelles


#Exercices de synthese


#1.
enquete=read.table("enquete.txt",header=T)
attach(enquete)

bppoids=barplot(sort(poids,decreasing=T),
  names.arg=order(poids,decreasing=T),
  ylim=c(0,80),
  main="Poids",
  col=gray.colors(10))
text(bppoids,sort(poids,decreasing=T)+1.5,
     sort(poids,decreasing=T))

tcoul=table(couleur)

bpcoul=barplot(tcoul,names.arg=names(tcoul),
  ylim=c(0,10),
  main="Couleur choisie",
  col=gray.colors(4))
text(bpcoul,tcoul+0.5,tcoul)
axis(4,at=(0:10),labels=seq(0,1,by=0.1))

bpcoulbis=barplot(sort(tcoul,decreasing=T),
  names.arg=names(tcoul)[order(tcoul,decreasing=T)],
  ylim=c(0,10),
  main="Couleur choisie",
  col=gray.colors(4))
text(bpcoulbis,sort(tcoul,decreasing=T)+0.5,
     sort(tcoul,decreasing=T))
axis(1,bpcoulbis,labels=F)
axis(4,at=(0:10),labels=seq(0,1,by=0.1))


freqcoul=tcoul/sum(tcoul)
bpcoulter=barplot(as.matrix(sort(freqcoul)),
  ylim=c(-2,3),
  width=1,
  main="Couleur choisie",
  beside=FALSE,
  axes=FALSE,
  horiz=TRUE,
  col=0,names.arg="")
text(c(0.05,0.2,0.45,0.8),bpcoulter,names(sort(freqcoul)))
text(c(0.05,0.2,0.45,0.8),bpcoulter-0.3,
     paste(round(sort(freqcoul)*100,0),"%"))


#2. 

nomvar=names(conso.dat)[-c(1,2)]
limi=list()
limi[[1]]=10000*c(0,1,2,3,4,8,12)
limi[[2]]=1000*c(0,1,2,3,4,7)
limi[[3]]=10*c(1,2,3,4,7)

limites=numeric()

for(k in 1:3) {
  limip=numeric()
  for(i in 1:3) {
    for(j in 1:2) {
      limip=c(limip,hist(conso.liste[[i]][[j]][[k]],
        breaks=limi[[k]],
        #prob=T,
        plot=F)$density)
    }
  }
  limites[k]=max(limip)
}

limitenf=numeric()
for(i in 1:3) {
  for(j in 1:2) {
    limitenf=c(limitenf,conso.liste[[i]][[j]][[5]])
  }
}
limenf=max(limitenf)

limitcaf=numeric()
for(i in 1:3) {
  for(j in 1:2) {
    limitcaf=c(limitcaf,conso.liste[[i]][[j]][[4]])
  }
}
limecaf=max(limitcaf)


limb=list()
for(k in 1:3) limb[[k]]=range(conso.dat[,k+2])


nuage=function(k) {
  par(mfcol=c(2,3))
  for(i in 1:3) {
    for(j in 1:2) {
      plot(conso.liste[[i]][[j]][[1]],
           conso.liste[[i]][[j]][[k]],
           xlim=range(conso.dat[3]),
           ylim=range(conso.dat[2+k]),
           xlab=nomvar[1],
           ylab=nomvar[k],
           main=paste(nom.region[i]," et ",nom.age[j])) 
    }
  }
}
nuage(2)


graphe=function(k) {
  par(mfcol=c(2,3))
  for(i in 1:3) {
    for(j in 1:2) {
      moy=mean(conso.liste[[i]][[j]][[k]])
      ect=sd(conso.liste[[i]][[j]][[k]])
      hist(conso.liste[[i]][[j]][[k]],breaks=limi[[k]],
           prob=T,ylim=c(0,limites[k]),xlab=nomvar[k],
           main=paste(nom.region[i]," et ",nom.age[j]),
           sub=paste("moyenne: ",round(moy,2),
             "  ecart-type: ",round(ect,2),sep="")
           )
    }
  }
}

graphe(1)

boite=function(k) {
  par(mfcol=c(2,3))
  for(i in 1:3) {
    for(j in 1:2) {
      boxplot(conso.liste[[i]][[j]][[k]],ylim=limb[[k]],
              xlab=nomvar[k],
              main=paste(nom.region[i]," et ",nom.age[j])) 
    }
  }
}
     
boite(1)

maxi=max(Enfants)

enf=function() {
  par(mfcol=c(2,3))
  for(i in 1:3) {
    for(j in 1:2) {
      barplot(conso.liste[[i]][[j]][[5]],xlim=c(0,maxi),
              ylim=c(0,limenf),xlab="Nb enfants",ylab="Effectifs",
              main=paste(nom.region[i]," et ",nom.age[j])) 
    }
  }
}
enf()


caf=function() {
  par(mfcol=c(2,3))
  for(i in 1:3) {
    for(j in 1:2) {
      barplot(conso.liste[[i]][[j]][[4]],ylim=c(0,limecaf),
              ylab="Effectifs",
              main=paste(nom.region[i]," et ",nom.age[j])) 
    }
  }
}
caf()


compare=function() {
  par(mfcol=c(2,3))
  for(i in 1:3) {
    for(j in 1:2) {
      u=c(conso.liste[[i]][[j]][[6]],conso.liste[[i]][[j]][[7]])
      mat=matrix(c(u,1-u),ncol=2,byrow=T)
      dimnames(mat)=list(c("oui","non"),c("Magn","Lav"))
      barplot(mat,ylab="Fréquence",
              main=paste(nom.region[i]," et ",nom.age[j])) 
    }
  }
}
compare()






