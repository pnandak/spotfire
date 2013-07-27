# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo al paragrafo 5.7 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------
source("base-www.R")
#
dati <- read.table("classi.dat", head=TRUE)
K  <- 3
n <- nrow(dati)
attach(dati)
#
library(nnet)
set.seed(123)
nn1 <- nnet(factor(gruppo)~x1+x2, size=4, maxit=300, decay=0.01)
n.grid<- 250
p    <- pred.square(nn1, x1, x2, n.grid)
pred <- array(p$pred, c(n.grid, n.grid,3))
ind  <- apply(pred, c(1,2),order)[3,,] 
plot(dati[,1:2], type="n", xlab="z1", ylab="z2")
for(k in 1:3)
   points(dati[gruppo==k,1:2], pch=Pch[k], col=Col[k], cex=cex0)   
contour(p$x, p$y, ind, add=TRUE, drawlabels=FALSE, nlevels=2, lty=lty0)
pause("fig 5.18a")
#-----
set.seed(85)
nn2 <- nnet(factor(gruppo)~x1+x2, size=12, maxit=1200, decay=0.01)
n.grid<- 250
p    <- pred.square(nn2, x1, x2, n.grid)
pred <- array(p$pred, c(n.grid, n.grid,3))
ind  <- apply(pred, c(1,2),order)[3,,] 
plot(dati[,1:2], type="n", xlab="z1", ylab="z2")
for(k in 1:3)
   points(dati[gruppo==k,1:2], pch=Pch[k], col=Col[k], cex=cex0)   
contour(p$x, p$y, ind, add=TRUE, drawlabels=FALSE, nlevels=2, lty=lty0)
pause("fig 5.18b")
detach(dati)
#--------------------------------------------------------
dati <- read.table("classi.dat", head=TRUE, nrows=200)
gr <- (2-dati[,3]) # i gruppi sono indicati da 0 e 1
K <- 2
n <- nrow(dati)
attach(dati)
library(e1071)
n.grid <- 150
plot(dati[,1:2], type="n", xlab="z1", ylab="z2")
for(k in 1:K) {
   g <- (gruppo==k)
   points(dati[g,1:2], pch=Pch[k], col=Col[k], cex=cex0)
   }

#-------
s3 <- svm(factor(gruppo)~x1+x2, data=dati, kernel="polynomial",
          degree=3, cost = 1)
# plot(s3, data=dati, grid=150)
plot(dati[,1:2], type="n", xlab="z1", ylab="z2")
for(k in 1:K) {
   g <- (dati[,3]==k)
   points(dati[g,1:2], pch=Pch[k], col=Col[k], cex=cex0)
   }
p <- pred.square(s3, x1,x2, grid=n.grid)
pred<- matrix(as.numeric(p$pred), n.grid, n.grid)
contour(p$x, p$y, pred, add=TRUE, levels=1.5, lty=lty0,
        drawlabels=FALSE)
pause("fig 5.21a")
#--------
s2 <- svm(factor(gruppo)~x1+x2, data=dati, kernel="radial", cost=1)

plot(dati[,1:2], type="n", xlab="z1", ylab="z2")
for(k in 1:K) {
   g <- (gruppo==k)
   points(dati[g,1:2], pch=Pch[k], col=Col[k], cex=cex0)
   }
p <- pred.square(s2, x1,x2, grid=n.grid)
pred<- matrix(as.numeric(p$pred), n.grid, n.grid)
contour(p$x, p$y, pred, add=TRUE, levels=1.5, lty=lty0,
        drawlabels=FALSE)
pause("fig 5.21b")
#
s0P<- tune.svm(factor(gruppo)~x1+x2, data=dati, kernel="polynomial",
              cost=exp(seq(-1,6,length=25)))
# best: cost=70.1
plot(dati[,1:2], type="n", xlab="z1", ylab="z2")
for(k in 1:K) {
   g <- (gruppo==k)
   points(dati[g,1:2], pch=Pch[k], col=Col[k], cex=cex0)
   }
p <- pred.square(s0P$best.model, x1,x2, grid=n.grid)
pred<- matrix(as.numeric(p$pred), n.grid, n.grid)
contour(p$x, p$y, pred, add=TRUE, levels=1.5, lty=lty0,
        drawlabels=FALSE)
pause("fig 5.21c")
#
s0R<- tune.svm(factor(gruppo)~x1+x2, data=dati,  kernel="radial",
              cost=exp(seq(-2,4,length=25)))
# best: cost=7.389
plot(dati[,1:2], type="n", xlab="z1", ylab="z2")
for(k in 1:K) {
   g <- (gruppo==k)
   points(dati[g,1:2], pch=Pch[k], col=Col[k], cex=cex0)
   }
p <- pred.square(s0R$best.model, x1,x2, grid=n.grid)
pred<- matrix(as.numeric(p$pred), n.grid, n.grid)
contour(p$x, p$y, pred, add=TRUE, levels=1.5, lty=lty0,
        drawlabels=FALSE)
pause("fig 5.21d")
#------------

detach.all()
