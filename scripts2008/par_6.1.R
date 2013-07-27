# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo al paragrafo 6.1 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------
source("base-www.R")
#
dati <- read.table("C1.dat", head=TRUE)[,1:2]
attach(dati)
#
#-------------------------------
# k-means
#
set.seed(123)
angolo<-runif(1,0,2*pi)
#
x1.m<- mean(x1)
x2.m<- mean(x2)
r <- sqrt(var(x1)+var(x2))/2
n.fig<-0
for(K in 3:4){
for(prova in 1:3){
  angolo<- angolo+2*pi/9
  centroide<- matrix(NA,K,2)
  n.plot <- (K-3)*3+prova
  if(n.plot %in% c(1,3,4)){
   n.fig <- n.fig+1
   plot(dati, type="n")
   for(k in 1:K){
    centroide[k,1]<- x1.m+r*cos(angolo+2*pi*k/K)
    centroide[k,2]<- x2.m+r*sin(angolo+2*pi*k/K)
    points(centroide[k,1], centroide[k,2], col=1, pch=0)
  }
  k1 <-  kmeans(dati,centroide)
  points(dati, col=Col[k1$cluster], cex=cex0, pch=Pch[k1$cluster])
  for(k in 1:K) lines(t(cbind(centroide[k,] , k1$centers[k,])))
  pause(paste("fig 6.1",letters[n.fig],sep=""))
}
}
}

#----------
detach(dati)
#
dati2 <- read.table("C2.dat", head=TRUE)[,1:2]
attach(dati2)
set.seed(123)
angolo<-runif(1,0,2*pi)
#
x1.m<- mean(x1)
x2.m<- mean(x2)
r <- sqrt(var(x1)+var(x2))/2

K <- 3
  angolo<- angolo+2*pi/9
  centroide<- matrix(NA,K,2)
  n.plot <- (K-3)*3+prova
  plot(dati2, type="n")
  for(k in 1:K){
    centroide[k,1]<- x1.m+r*cos(angolo+2*pi*k/K)
    centroide[k,2]<- x2.m+r*sin(angolo+2*pi*k/K)
    points(centroide[k,1], centroide[k,2], col=1, pch=0)
  }
  k1 <-  kmeans(dati2,centroide)
  points(dati2, col=Col[k1$cluster], cex=cex0, pch=Pch[k1$cluster])
  for(k in 1:K) lines(t(cbind(centroide[k,] , k1$centers[k,])))
  pause("fig 6.1d")
detach(dati2)
#----------

d  <- dist(dati)
K<-3

c3 <- hclust(d, method="single")
plot(c3, labels=FALSE,  xlab="",ylab="", main="", sub="")
pause("fig 6.3a")
#
a <- rect.hclust(c3,k=K)
pause("")
plot(dati,type="n")
for(k in 1:K) points(dati[a[[k]],], pch=Pch[k], col=Col[k], cex=cex0)
pause("fig 6.3b")
#---
c1 <- hclust(d, method="complete")
plot(c1, labels=FALSE,  xlab="",ylab="", main="", sub="")
pause("fig 6.3c")
a <- rect.hclust(c1,k=K)
pause("")
plot(dati,type="n")
for(k in 1:K) points(dati[a[[k]],], pch=Pch[k], col=Col[k], cex=cex0)
pause("fig 6.3d")

#---
c2 <- hclust(d, method="average")
plot(c2, labels=FALSE,  xlab="",ylab="", main="", sub="")
pause("fig 6.3e")

a <- rect.hclust(c2,k=K)
pause("")
plot(dati,type="n")
for(k in 1:K) points(dati[a[[k]],], pch=Pch[k], col=Col[k], cex=cex0)
pause("fig 6.3f")
#
#----------

d  <- dist(dati2)
K<-3

c3 <- hclust(d, method="single")
plot(c3, labels=FALSE,  xlab="",ylab="", main="", sub="")
pause("fig 6.4a")
#
a <- rect.hclust(c3,k=K)
pause("")
plot(dati2,type="n")
for(k in 1:K) points(dati2[a[[k]],], pch=Pch[k], col=Col[k], cex=cex0)
pause("fig 6.4b")
#---
c1 <- hclust(d, method="complete")
plot(c1, labels=FALSE,  xlab="",ylab="", main="", sub="")
pause("fig 6.4c")
a <- rect.hclust(c1,k=K)
pause("")
plot(dati2,type="n")
for(k in 1:K) points(dati2[a[[k]],], pch=Pch[k], col=Col[k], cex=cex0)
pause("fig 6.4d")

#---
c2 <- hclust(d, method="average")
plot(c2, labels=FALSE,  xlab="",ylab="", main="", sub="")
pause("fig 6.4e")

a <- rect.hclust(c2,k=K)
pause("")
plot(dati2,type="n")
for(k in 1:K) points(dati2[a[[k]],], pch=Pch[k], col=Col[k], cex=cex0)
pause("fig 6.4f")
#
detach.all()
