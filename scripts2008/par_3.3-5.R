
# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo ai paragrafi 3.3-3.5 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------
source("base-www.R")
dati <- read.table("ieri-domani.dat", header=TRUE)

attach(dati)

n<- NROW(dati)
source("f_vera.R")

polinomio <- function(p) lm(y.ieri ~ poly(x,p))
polinomi <- apply(cbind(1:29), 1, polinomio)
errore.train <- sapply(polinomi, deviance)

rss <- function(p) sum((y.domani - predict(polinomi[[p]]))^2)
errore.test <- apply(cbind(1:29), 1, rss)

rss60<- function(p) sum((y.domani - predict(polinomi[[p]]))^2)+
                sum((y.ieri - predict(polinomi[[p]]))^2)
Errore <- apply(cbind(1:29), 1, rss)


#---------------------------------
#varianza/distorsione

polo.f.vera <- function(p) lm(f.vera ~ poly(x,p))
poli.f.vera <- apply(cbind(1:29), 1, polo.f.vera)

bias2 <- function(p) mean((fitted(poli.f.vera[[p]]) - f.vera)^2)
bias2.oss <- apply(cbind(1:29), 1, bias2)
var.oss <- sqm.vero^2 * (1:29)/30

err <- bias2.oss + var.oss

#-----------------------------------
#errore totale

plot((1:(n-1)), err, type="b",  col=col1,  cex=cex0,
   xlab="complessità del modello", ylab="errore di stima")


pause("fig 3.6")
#---
plot(bias2.oss, type="b", col=col1, ylab="errore di stima",
     xlab="complessità del modello", cex=cex0*0.5, pch=pch1)
lines(var.oss, type="b", col=col2, cex=cex0*0.5, pch=pch2)
lines(err, type="b", cex=cex0*0.5, pch=pch3)
legend(20, 4.7e-04, legend=c("distorsione^2","varianza", "totale"), 
        col=c(col1,col2,1), pch=c(pch1,pch2,pch3))

pause("fig 3.7")

#---------------------------------
#errore su insieme di stima e di verifica

plot(errore.train, type="b",col=col1, 
     xlab="Complessità del modello", ylab="Devianza", cex=cex0, pch=pch1)
lines(errore.test, type="b",col=col2, cex=cex0, pch=pch2)
text(x=c(19,19),y=c(0.0015,0.0065), c("dati di ieri", "dati di domani"))

pause("fig 3.8")
 
#-------------------------------------
# convalida incrociata

y <- c(y.ieri,y.domani)
xx <- c(x,x)
n <- NROW(xx)

RSS <- numeric(n-1) 
for (p in 0:29) {
  m <- lm(y~poly(xx,p+1), x=TRUE)
  X <- m$x
  P <- X %*% solve(t(X)%*% X) %*% t(X) # matrice di proiezione
  p.ii <- diag(P)
  RSS.pp<- sum(residuals(m)^2/(1-p.ii)^2)
  RSS.p <-0
  RSS[p+1] <- RSS.pp
  # cat(p," ",RSS.pp, RSS.p, "\n")
}
# cat("\n")
# print(RSS)
# il minimo di RSS e` al settimo termine, cioe` p=6
plot(0:29, RSS[1:30],type="b", xlab="complessità del modello", 
      ylab="errore di stima", cex=cex0)

pause("fig 3.9")

#--------------------------------------
# AIC-BIC, etc
#
p <- 1:(NROW(dati)-1)
n <- 2*NROW(dati)

AIC  <- n*log(Errore/n)+2*p
AIC.c <- n*log(Errore/n) +2*p + 2*p*(p+1)/(n-p+1)
BIC  <- n*log(Errore/n)+ log(n)*p
HQ   <- n*log(Errore/n)+ 3*log(log(n))*p


plot(c(1,max(p)), c(min(AIC,BIC),max(AIC,BIC)), type="n", 
   xlab="Complessità del modello", ylab="Criterio AIC e BIC")
lines(AIC,col=col1, type="b", pch=pch1, cex=cex0)
lines(AIC.c,col=col2, type="b", pch=pch2, cex=cex0)
lines(BIC,col=col3, type="b", pch=pch3, cex=cex0)
lines(HQ,col=col4, type="b", pch=pch4, cex=cex0)

legend(20,-550, legend=c("AIC","AIC.c","BIC", "HQ (c=3)"),
       pch=c(pch1, pch2, pch3, pch4),
       col=c(col1,col2, col3, col4))

pause("fig 3.10")



#-------------------------------------
x <- dati[,1]
y <- as.vector(data.matrix(dati[,2:3]))
xx <- c(x,x)
n <- NROW(xx)
n0<- n/2
plot(xx, y, type="n", cex=cex0)
points(xx[1:n0], y[1:n0], pch=pch1, col=col1, cex=cex0)
points(xx[n0+1:n], y[n0+1:n], pch=pch2, col=col2, cex=cex0)
y.hat  <- fitted(lm(y ~ poly(xx,4)))
lines( xx[1:n0], y.hat[1:n0])

pause("fig 3.11")
#
detach.all()
