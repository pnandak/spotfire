# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo al paragrafo 4.7 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------

source("base-www.R")
library(nnet)
auto<- read.table("auto.dat", header=TRUE)
attach(auto)
#
set.seed(12345)
y0 <- percorr.urbana/max(percorr.urbana)
c0 <- cilindrata/max(cilindrata)
p0 <- peso/max(peso) 
best <- Inf
for(k in 1:12){
  nn <-  nnet(y=y0, x=cbind(c0, p0), size=3, decay=1e-3,
             maxit=1200, linout=TRUE)
  cat("k, valore:", k, nn$value) 
  if(nn$value < best) {best<-nn$value; nn0<-nn}
}
cat("\n------\nMiglior soluzione\n")
print(summary(nn0))
print(nn0$value)
cat("I valori dei 'pesi' w sono diversi da quelli riportati nella\n")
cat("Figura 4.19a, a causa della diversa versione della funzione nnet,\n")
cat("peraltro la funzione stimata è essenzialmente identica.\n\n")
#
np <- 31
x<- seq(min(cilindrata), max(cilindrata), length=np)
y<- seq(min(peso), max(peso), length=np)
nx <- length(x)
ny <- length(y)
xoy<- cbind(rep(x, ny), as.vector(matrix(y, nx, ny, byrow = TRUE)))
X <- matrix(xoy, nx * ny, 2, byrow = FALSE)
z <- predict(nn0, newdata=data.frame(cilindrata=X[,1]/max(cilindrata),
                    peso=X[,2]/max(peso)))
zz <- matrix(z, nx, ny,  byrow = FALSE)*max(percorr.urbana)
persp(x, y, zz, xlab="cilindrata",ylab="peso", zlab="percorrenza urbana",
       theta=120, phi=20, ticktype ="detailed")
pause("fig 4.19b")
#
detach.all()

