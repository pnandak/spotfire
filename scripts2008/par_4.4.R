# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo al paragrafo 4.4 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------

source("base-www.R")
library(sm)
library(mgcv)
#
auto <- read.table("auto.dat", header=TRUE)
attach(auto)
y  <- percorr.urbana
x1 <- cilindrata
x2 <- peso

xlab<-"cilindrata (litro)"
ylab<-"peso (kg)"
zlab<- "percorrenza urbana (km/l)"

#-----

a2<- gam(percorr.urbana ~ s(cilindrata, bs="cr", k = 5) +
         s(peso, bs="cr", k = 6),  data=auto)
plot(a2)
pause("fig 4.11")


#-----------------------------------------
x1 <- seq(min(cilindrata), max(cilindrata), length=50)
x2 <- seq(min(peso), max(peso), length=50)
x12<- data.frame(
            cilindrata = as.vector(outer(x1,rep(1,length(x1)))),
            peso=as.vector(outer(rep(1,length(x2)),x2)))
a2p <- predict(a2, x12)
sm.options(ngrid=50)
h<- c(0.5, 150)
#
# si forzano le funzioni gam e sm.regression a stimare la funzione
# di regressione anche fuori dell'involucro convesso dei punti (x1,x2)
# per una zona limitata
#
a <- sm.regression(cbind(cilindrata,peso), percorr.urbana, h=h,
       options=list(ngrid=50, xlab=xlab, ylab=ylab, 
                  zlab=zlab),  display="none")

matrice.gam <- matrix(NA,50,50)

for (i in 1:50) for (j in 1:50) 
      if(i<=j) 
       matrice.gam[i,j]<-matrix(a2p,50,50)[i,j]

for (r in 0:8) for (i in 1:50) for (j in 1:(50-r)) 
      if(!is.na(a$estimate[i,j+r])) 
       matrice.gam[i,j]<-matrix(a2p,50,50)[i,j]

persp(x1, x2,matrice.gam,  theta=120, phi=20, 
      xlab="cilindrata", ylab="peso", zlab="percorrenza",
      ticktype="detailed")
pause("fig 4.12a")
#----

sm.options(hull=FALSE)                    
sm <- sm.regression(cbind(cilindrata, peso), percorr.urbana, h=h,
       options=list(xlab=xlab, ylab=ylab, 
                  zlab=zlab),  display="none")
sm.options(hull=TRUE)  
matrice.sm <- matrix(NA,50,50)
for (i in 1:50) for (j in 1:50) 
      if(i<=j) 
       matrice.sm[i,j]<-matrix(sm$estimate,50,50)[i,j]

for (r in 0:8) for (i in 1:50) for (j in 1:(50-r)) 
      if(!is.na(a$estimate[i,j+r])) 
       matrice.sm[i,j]<-matrix(sm$estimate,50,50)[i,j]

persp(x1, x2, matrice.sm,  theta=120, phi=20, 
      xlab="cilindrata", ylab="peso", zlab="percorrenza",
      ticktype="detailed")
pause("fig 4.12b")
#
detach.all()
