# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo al paragrafo 4.3 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------

source("base-www.R")
library(splines)
#
dati <- read.table("ieri-domani.dat", header=TRUE)
attach(dati)
n<- nrow(dati)
y.range<- range(c(y.ieri, y.domani))
#------
xi <- seq(min(x),max(x),length=4)
plot(x, y.ieri, pch=2, col=col2)
abline(v=xi[2], lty=lty3)
abline(v=xi[3], lty=lty3)
#
x0 <-seq(min(x),max(x), length=200)
xx <-sort(c(x0,xi[2:3]))
m1 <- lm(y.ieri ~  bs(x, knots=xi[2:3], degree=1))
fit1<- predict(m1,  data.frame(x=xx))
lines(xx,fit1,  col=col3)
#
m2 <- lm(y.ieri ~  bs(x, knots=xi[2:3], degree=2))
fit2<- predict(m2,  data.frame(x=xx))
lines(xx,fit2, col=col4)
#
m3 <- lm(y.ieri ~  bs(x, knots=xi[2:3], degree=3))
fit3<- predict(m3,  data.frame(x=xx))
lines(xx,fit3,  col=col2)

text(x=c(1.05, 0.85, 0.65), y=rep(0.52,3), as.character(1:3))
pause("fig 4.4a")
#----
plot(c(0.5,3),c(0,2), type="n", xlab="x",
      ylab="max(0,x-1.333)")
abline(v=xi[2], lty=3)
lines(x0, pmax(0,x0-xi[2]), col=col1)
# lines(x0, pmax(0,x0-xi[3]), col=col3)

pause("fig 4.4b")
#------------------------------------
#
auto <- read.table("auto.dat", header=TRUE)
attach(auto)
# library(modreg)
y  <- percorr.urbana
x <- cilindrata
xlab <-"cilindrata (litro)"
ylab <- "percorrenza urbana (km/l)"

a075 <- smooth.spline(x, y, spar=0.75)
a090 <- smooth.spline(x, y, spar=0.95)
a125 <- smooth.spline(x, y, spar=1.25)
x0<- seq(min(x), max(x), length=250)

plot(x, y, xlab=xlab, ylab=ylab,  cex=0.6*cex0)
lines(predict(a075, x=x0), col=col1, lty=lty0)
lines(predict(a090, x=x0), col=col2, lty=lty1)
lines(predict(a125, x=x0), col=col3, lty=lty3)
legend(4, 20, legend=c(format(a075$lambda,digit=2),
                format(a090$lambda,digit=2),
                format(a125$lambda,digit=2)),
       col=c(col1,col2,col2), lty=c(lty0,lty1,lty3))
pause("fig 4.5")
#
#------------------------------------
library(mgcv)

xlab<-"cilindrata (litro)"
ylab<-"peso (kg)"
zlab<- "percorrenza urbana (km/l)"

m1 <- gam(percorr.urbana ~ s(cilindrata, peso, k=18), data=auto)

x1<- seq(min(cilindrata), max(cilindrata), length=50)
x2<- seq(min(peso), max(peso), length=50)
x12<- data.frame(cilindrata = as.vector(outer(x1, rep(1,length(x1)))),
                 peso =as.vector(outer(rep(1,length(x2)), x2)))
m1p <- predict(m1, newdata=x12)
persp(x1, x2,matrix(m1p,50,50),  theta=120, phi=20, 
      xlab="cilindrata", ylab="peso", zlab="percorrenza", ticktype="detailed")
pause("fig 4.6a")

contour(x1, x2,matrix(m1p,50,50), xlab="cilindrata", ylab="peso")
pause("fig 4.6b")
#------------------------------------

t <- 20
x <- (1:50)
y <- c(1:50)
z <- outer((x>t)*abs(x-t),(y>t)*abs(y-t))
persp(x,y,z,phi=20,theta=50,zlim=c(0,1200),zlab="g(x)")
pause("fig 4.7")
#---
library(polspline)

pm1<- polymars(percorr.urbana, auto[,c(11,12)])
 x1<- seq(min(cilindrata), max(cilindrata), length=35)
 x2<- seq(min(peso), max(peso), length=35)
 x12<- as.matrix(data.frame(
            peso= as.vector(outer(x2,rep(1,length(x2)))),
            cilindrata=as.vector(outer(rep(1,length(x1)),x1))
                 ))
pm1p <- predict(pm1, x12)
persp(x1,x2,matrix(pm1p,35,35,byrow=TRUE),  theta=120, phi=20, 
      xlab="cilindrata", ylab="peso", zlab="percorrenza",
      ticktype="detailed")
pause("fig 4.8")
#---

dati.permars<- data.frame(cilindrata,peso,alimentazione,aspirazione, 
                     carrozzeria, trazione, posiz.motore, 
                     larghezza, altezza,lunghezza)
pm1<- polymars(percorr.urbana, dati.permars,factors=c(3:7),gcv=3)
pm1

par(mfrow=c(2,2))
plot(pm1,1)
plot(pm1,2)
plot(pm1,3)
plot(pm1,4)
par(mfrow=c(1,1))
pause("fig 4.9")
#---
source("persp.polymars.R")
# versione leggermente modificata di quella ufficiale

plot(pm1,1,2,xyz=T, theta=120, phi=20,
     xlab="cilindrata", ylab="peso", zlab="percorrenza")
pause("fig 4.10")
#
detach.all()
