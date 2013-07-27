# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo al paragrafo 5.4 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------
source("base-www.R")
#
dati <- read.table("classi.dat", head=TRUE)
x1 <- dati[,1]
x2 <- dati[,2]
gr <- (2-dati[,3])
K  <- 3
n <- nrow(dati)
x1 <- z1<- dati[,1]
x2 <- z2<- dati[,2]
gr <- dati[,3]
gruppo <- factor(gr)
n.grid<- 250
#
library(MASS)
d1   <- lda(gruppo ~ x1+x2)
p    <- pred.square(d1,x1,x2,n.grid)
pred <- array(p$pred$posterior, c(n.grid, n.grid,3))
ind  <- apply(pred, c(1,2),order)[3,,] 
plot(dati[,1:2], type="n", xlab="z1", ylab="z2")
for(k in 1:3)
   points(dati[gr==k,1:2], pch=Pch[k], col=Col[k], cex=0.75)   
contour(p$x, p$y, ind, add=TRUE, drawlabels=FALSE, nlevels=2, lty=lty0)
#title("LDA, gruppo ~ x1+x2")
pause("fig 5.7a")
#---
d2   <- lda(gruppo ~ x1+x2+I(x1^2)+I(x1*x2)+I(x2^2))
p    <- pred.square(d2,x1,x2, n.grid)
pred <- array(p$pred$posterior, c(n.grid, n.grid,3))
ind  <- apply(pred, c(1,2),order)[3,,] 
plot(dati[,1:2], type="n", xlab="z1", ylab="z2", cex=0.75)
for(k in 1:3)
   points(dati[gr==k,1:2], pch=Pch[k], col=Col[k], cex=0.75)   
contour(p$x, p$y, ind, add=TRUE, drawlabels=FALSE, nlevels=2, lty=lty0)
# title("LDA, gruppo ~ x1+x2+I(x1^2)+I(x1*x2)+I(x2^2)")
pause("fig 5.7b")
#------------------------------

#                  QDA
d3   <- qda(gruppo ~ x1+x2)
p    <- pred.square(d3,x1,x2, n.grid)
pred <- array(p$pred$posterior, c(n.grid, n.grid,3))
ind  <- apply(pred, c(1,2),order)[3,,] 
plot(dati[,1:2], type="n", xlab="z1", ylab="z2", cex=0.75)
for(k in 1:3)
   points(dati[gr==k,1:2], pch=Pch[k], col=Col[k], cex=0.75)   
contour(p$x, p$y, ind, add=TRUE, drawlabels=FALSE, nlevels=2, lty=lty0)
# title("QDA, gruppo ~ x1+x2")
pause("fig 5.8a")
#------------------------------
#       
d4   <- qda(gruppo ~ x1+x2+I(x1^2)+I(x1*x2)+I(x2^2))
p    <- pred.square(d4,x1,x2, n.grid)
pred <- array(p$pred$posterior, c(n.grid, n.grid,3))
ind  <- apply(pred, c(1,2),order)[3,,] 
plot(dati[,1:2], type="n", xlab="z1", ylab="z2")
for(k in 1:3)
   points(dati[gr==k,1:2], pch=Pch[k], col=Col[k], cex=0.75)   
contour(p$x, p$y, ind, add=TRUE, drawlabels=FALSE, nlevels=2, lty=lty0)
#title("QDA, gruppo ~ x1+x2+I(x1^2)+I(x1*x2)+I(x2^2)")
pause("fig 5.8b")
 
#------------------------------
# 
succo <- read.table("succo.dat", head=TRUE) 
succo[,"negozio"] <- factor(succo[,"negozio"])
attach(succo)
#
set.seed(123)
n <- nrow(succo)
n1 <- round(n*0.75)
n2<- n-n1
permuta<- sample(1:n,n)
stima <- sort(permuta[1:n1])
verifica <- sort(permuta[(n1+1):n])
#
vr <- c(3:7,9:10)
f1 <-  as.formula(paste("scelta~", paste(names(succo)[vr], collapse="+"),
                        collapse=NULL))
d1 <- lda(f1, data=succo[stima,])
d2 <- qda(f1, data=succo[stima,])
p1 <- predict(d1, newdata=succo[verifica,])
p2 <- predict(d2, newdata=succo[verifica,])
matrice.confusione(p1$class, succo[verifica,"scelta"])
matrice.confusione(p2$class, succo[verifica,"scelta"])
pause("tab 5.5")
#----
g <- as.numeric(succo[verifica,1]=="MM")
lr1 <- lift.roc(p1$post[,2], g, type="bin", plot.it=FALSE)
lr2 <- lift.roc(p2$post[,2], g, type="bin", plot.it=FALSE)
#
plot(lr1[[1]], lr1[[2]], type="b", xlab="frazione di soggetti previsiti",
     ylab="fattore di miglioramento", col=col1, pch=pch1, cex=cex0)
lines(lr2[[1]], lr2[[2]],  type="b", pch=pch2, col=col2, cex=cex0)
legend(0.75, 2.5, legend=c("LDA", "QDA"), col=c(col1,col2),
        pch=c(pch1, pch2))
pause("fig 5.9a")
#---

 plot(lr1[[3]], lr1[[4]], type="b", xlim=c(0,1),pch=pch1,
         ylim=c(0,1),cex=cex0, xlab="1-specificità",
         ylab="sensibilità", col=col1)
 lines(lr2[[3]], lr2[[4]], type="b", cex=cex0, col=col2, pch=pch2)
 abline(0,1, lty=2, col=col3)
 legend(0.8, 0.6, legend=c("LDA", "QDA"), col=c(col1,col2),
        pch=c(pch1, pch2))
pause("fig 5.9b")

#---

detach.all()
