# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo a paragrafi 5.2 e 5.3 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------
source("base-www.R")
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
par(mfrow=c(2,3))
for(k in vr[-length(vr)])
  boxplot(succo[stima,k]~scelta[stima], col=col12,  ylab=names(succo)[k])
par(mfrow=c(1,1))
pause("fig 5.1a-f")
#
freq<- table(scelta, negozio)
freq<-  100*freq/outer(rep(1,2),apply(freq,2,sum))
barplot(freq[2,], width=1, space=1.25, col=col12, ylim=c(0,100),
    ylab="Percentuale di scelta di MM", xlab="negozio")
abline(h=100*sum(scelta[stima]=="MM")/n1, lty=2, col=col2)
pause("fig 5.1g")
#---
f1 <-  as.formula(paste("scelta~", paste(names(succo)[vr], collapse="+"),
                        collapse=NULL))
m1 <- glm(f1, data=succo[stima,], family=binomial)
print(summary(m1))
pause("tabella 5.1")
#-------
m2 <- update(m1, .~ .-settimana, data=succo[stima,])
print(summary(m2))
pause("tabella 5.2")
#-------
p2 <- predict(m2, newdata=succo[verifica,], type="response")
matrice.confusione(p2>0.5, succo[verifica,1])
pause("tabella 5.3")
#---
g <- as.numeric(succo[verifica,1]=="MM")
a <- lift.roc(p2, g, type="crude", plot.it=FALSE)
#
plot(a[[1]], a[[2]], type="l", xlab="frazione di soggetti previsiti",
     ylab="fattore di miglioramento")
p0<- 0.5
  i0<- order(abs(p2-p0))[1]
  q0<- sum(p2>p2[i0])/n2
  abline(v=q0, col=col3, lty=3)
  text(q0, 1, paste("soglia=",format(p0),sep="",collapse=NULL))
pause("fig 5.2a")
#---
plot(a[[3]], a[[4]], type="l", xlab="1-specificità", ylab="sensibilità")
abline(0,1, col=col3, lty=lty3)
  i0<- order(abs(p2-p0))[1]
  q0<- a[[3]][sum(p2>p2[i0])]
  abline(v=q0, col=col2, lty=3)
  text(q0, 0, paste("soglia=",format(p0),sep="",collapse=NULL))
pause("fig 5.3a")
#---
a<- lift.roc(p2, g, type="bin", plot.it=FALSE)
plot(a[[1]], a[[2]], type="b", xlab="frazione di soggetti previsiti",
     ylab="fattore di miglioramento")
p0<- 0.5
  i0<- order(abs(p2-p0))[1]
  q0<- sum(p2>p2[i0])/n2
  abline(v=q0, col=col3, lty=3)
  text(q0, 1, paste("soglia=",format(p0),sep="",collapse=NULL))
pause("fig 5.2b")
#
plot(a[[3]], a[[4]], type="b", xlab="1-specificità", ylab="sensibilità")
abline(0,1, col=col3, lty=lty3)
  i0<- order(abs(p2-p0))[1]
  q0<- a[[3]][sum(p2>p2[i0])]
  abline(v=q0, col=col2, lty=3)
  text(q0, 0, paste("soglia=",format(p0),sep="",collapse=NULL))
pause("fig 5.3b")

#-----------------------------------
dati <- read.table("classi.dat", head=TRUE, nrows=200)
x1 <- dati[,1]
x2 <- dati[,2]
gr <- (2-dati[,3]) # i gruppi sono indicati da 0 e 1
K <- 2
n <- nrow(dati)
plot(dati[,1:2], type="n", xlab="z1", ylab="z2")
for(k in 1:K) {
   g <- (dati[,3]==k)
   points(dati[g,1:2], pch=Pch[k], col=Col[k], cex=cex0)
   }
#
m1 <- lm(gr ~ x1+x2)
beta<-coef(m1)
# cerchiamo retta tale che:  b0 + b1*x1 + b2*x2 = 0.5
a0 <- (0.5-beta[1])/beta[3]
a1<- -beta[2]/beta[3]
abline(a0, a1, lty=lty0)
pause("fig 5.4a")
#
m2 <- lm(gr ~ x1+x2+ I(x1^2)+ I(x1*x2)+I(x2^2))
summary(m2) # coeff di I(x1^2) non significativo
m2 <- lm(gr ~ x1+x2+ I(x1*x2)+I(x2^2))
beta<- c(coef(m2)[1:3],0,coef(m2)[4:5])
#
plot(dati[,1:2], type="n", xlab="z1", ylab="z2")
for(k in 1:K) {
   g <- (dati[,3]==k)
   points(dati[g,1:2], pch=Pch[k], col=Col[k], cex=cex0)
   }
aq <- beta[6]
x.seq <- seq(min(x1), max(x1), length=500)
x0<- y1<- y2<- numeric(0)
for(x in x.seq){
  bq <- beta[5]*x +  beta[3]
  cq <- beta[1]+beta[2]*x+ beta[4]*x^2 -0.5
  delta2 <- bq^2-4*aq*cq
  if(delta2>= 0) {
    x0<- c(x0,x)
    y1<- c(y1, (-bq+sqrt(delta2))/(2*aq))
    y2<- c(y2, (-bq-sqrt(delta2))/(2*aq))
  }
}
lines(x0,y1)
lines(x0,y2)
lines(rep(x0[1],2),c(y1[1],y2[1]))
pause("fig 5.4b")
#
#-----------------------------------
f0 <-  as.formula(paste("as.numeric(scelta)-1 ~",
                        paste(names(succo)[vr], collapse="+"),
                        collapse=NULL))
lm1<- lm(f0, data=succo[stima,])
lm2<- update(lm1, "~.-settimana")
p3 <- predict(lm2, newdata=succo[verifica,])
# table(p3>0.5, succo[verifica,1])
plot(log(p2/(1-p2)), p3,
     xlab="logit(probabilità) previste dal modello logistico",
     ylab="valori previsti con il modello lineare", cex=cex0)
abline(h=0.5, col=col1, lty=2)
abline(v=0, col=col1, lty=2)
pause("fig 5.5")
#
#-----------------------------------
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
Y <- model.matrix(~gruppo-1)
m1   <- lm(Y ~ x1+x2)
p    <- pred.square(m1, x1, x2, n.grid)
pred <- array(p$pred, dim=c(n.grid, n.grid,3))
ind  <- apply(pred, c(1,2),order)[3,,] 
plot(dati[,1:2], type="n",xlab="z1", ylab="z2" )
for(k in 1:K)
   points(dati[gr==k,1:2], pch=Pch[k], col=Col[k], cex=0.75)
contour(p$x, p$y, ind, add=TRUE, drawlabels=FALSE, nlevels=2, lty=lty0)
title("Classificazione con: lm(Y ~ x1+x2)")
pause("fig 5.6a")
#
m2   <- lm(Y ~ x1+x2+ I(x1^2)+I(x1*x2)+I(x2^2))
p    <- pred.square(m2, x1, x2, n.grid)
pred <- array(p$pred, dim=c(n.grid, n.grid,3))
ind  <- apply(pred, c(1,2),order)[3,,] 
plot(dati[,1:2], type="n", xlab="z1", ylab="z2")
for(k in 1:3)
   points(dati[gr==k,1:2], pch=Pch[k], col=Col[k], cex=0.75)
contour(p$x, p$y, ind, add=TRUE, drawlabels=FALSE, nlevels=2, lty=lty0)
title("Classificazione con: lm(Y ~ x1+x2+ I(x1^2)+I(x1*x2)+I(x2^2))")
pause("fig 5.6b")
#
detach.all()
