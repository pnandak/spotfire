# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo al paragrafo 5.6 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------
source("base-www.R")
#
x<- seq(0.5, 3, length=100)
y <-c(
  1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0,
  0, 0, 1, 1)
plot(x,y, type="n", ylim=c(0,1))
points(x,y,col=col2)
pause("fig 5.13")
#
library(tree)
t1<- tree(factor(y) ~ x)
plot(t1)
text(t1)
pause("fig 5.14a")
#
x0 <- seq(0.5, 3, length=500)
pr1<- predict(t1, newdata=data.frame(x=x0))
plot(x,y, type="n", ylim=c(0,1))
points(x,y,col=col2)
lines(x0,pr1[,2])
pause("fig 5.14b")
#---------------------

succo <- read.table("succo.dat", head=TRUE) 
succo[,"negozio"] <- factor(succo[,"negozio"])
attach(succo)
#
set.seed(123)
n  <- nrow(succo)
n1 <- round(n*0.75)
n2 <- n-n1
permuta<- sample(1:n,n)
stima <- sort(permuta[1:n1])
verifica <- sort(permuta[(n1+1):n])
v <- c(1,3:7,9:10)
vr<- v[-1]
#
library(tree)
set.seed(123)
parte1 <- sort(sample(stima, 600))
parte2 <- setdiff(stima, parte1)
f1 <-  as.formula(paste("scelta~", paste(names(succo)[vr], collapse="+"),
                        collapse=NULL))
t1<- tree(f1, data=succo[parte1,v],
         control=tree.control(nobs=length(parte1), minsize=2, mindev=0))
t2<- prune.tree(t1, newdata=succo[parte2,v])
plot(t2)
pause("fig 5.15a")
#
J <- t2$size[t2$dev==min(t2$dev)]
t3<-prune.tree(t1, best=J)
plot(t3)
text(t3)
pause("fig 5.15b")
#
p3<- predict(t3, newdata=succo[verifica,v], type="class")
matrice.confusione(p3, succo[verifica,"scelta"])
pause("tabella 5.9")
#
p3 <-  predict(t3, newdata=succo[verifica,v], type="vector")[,2]
a<- lift.roc(p3, as.numeric(succo[verifica,"scelta"]=="MM"))
pause("fig 5.16")
#--------------------------------------

dati <- read.table("classi.dat", head=TRUE)
x1 <- dati[,1]
x2 <- dati[,2]
gr <- as.factor(dati[,3])
K  <- 3
t3<- tree(gr~x1+x2,
       control=tree.control(nobs=nrow(dati), mindev=0, minsize=2))
plot(t3)
pause("fig 5.17a")
#
set.seed(5)
t4<- cv.tree(t3)
plot(t4)
pause("fig 5.17b")
#
t5<- prune.tree(t3, k=10)
plot(t5)
text(t5)
pause("fig 5.17c")
#
n.grid<- 250
p    <- pred.square(t5, x1, x2, n.grid)
pred <- array(p$pred, c(n.grid, n.grid,3))
ind  <- apply(pred, c(1,2),order)[3,,] 
plot(dati[,1:2], type="n")
for(k in 1:3)
   points(dati[gr==k,1:2], pch=Pch[k], col=Col[k], cex=cex0)
contour(p$x, p$y, ind, add=TRUE, drawlabels=FALSE,
        nlevels=2, lty=1)
pause("fig 5.17d")
#
detach.all()
