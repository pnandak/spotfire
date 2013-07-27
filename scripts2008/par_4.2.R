# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo al paragrafo 4.2 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------
source("base-www.R")
library(sm)
#
auto <- read.table("auto.dat", header=TRUE)
attach(auto)
#
add.window <- function(x, y, h, x.eval) {
  # funzione ripresa dallo "script rc_alter" annesso al libro
  #    Bowman & Azzalini (1997) "Applied smoothing techniques 
  #    for data analysis", Oxford University Press.
  polygon(rep(c(x.eval - 2 * h, x.eval + 2 * h), rep(2,2)),
        c(range(y), rev(range(y))), col = col12, border = F)
  lines(rep(x.eval, 2), range(y), lty = 2)
  points(x, y)
  xseq <- seq(x.eval - 3 * h, x.eval + 3 * h, length = 20)
  kernel <- dnorm(xseq, x.eval, h)
  kernel <- min(y) + 5 * kernel / max(kernel)
  lines(xseq, kernel, lty = 2)
  }
#---------------
par(mfrow=c(2,2))
y <- percorr.urbana
x <- cilindrata
x.eval <- 3
hvec   <- c(0.5, 0.5, 0.15, 1)
for (i in 1:4) {
  plot(x, y,
        xlab = "cilindrata (litro)", ylab = "percorrenza urbana (km/l)")
  # if (i == 2) {
  #  sm.regression(x, y, h = 100, poly.index = 0, lty = 2, add = T)}
  # else {
  if(i>1)  {
      add.window(x, y, hvec[i], x.eval)
      sm.regression(x, y,   h = hvec[i], add = TRUE, ngrid=250)
      text(4.5, 18,  paste("h =", as.character(hvec[i])))
      }
  }
par(mfrow=c(1,1))
pause("fig 4.1")
#---------------

xlab <-"cilindrata (litro)"
ylab <- "percorrenza urbana (km/l)"
sm.regression(cilindrata, y, h=0.21, display="se", col=col1,
      xlab=xlab, ylab=ylab, eval.points=seq(min(x),max(x),length=200))
pause("fig 4.2a")
#
plot(x,y, xlab=xlab, ylab=ylab,  cex=0.6*cex0)
lines(loess.smooth(x, y, span=0.75), col=col1)
a<- loess(y~x, degree=1, span=0.75, # family="symmetric",
          control=loess.control(iterations=16))
# lines(sort(x), fitted(a)[order(x)], col=col2)
x0<- seq(min(x), max(x), length=250)
y0<- predict(a, newdata=data.frame(x=x0), se=TRUE)
lines(x0, y0$fit , col=col3)
lines(x0, y0$fit + 2*y0$se.fit , col=col3, lty=2)
lines(x0, y0$fit - 2*y0$se.fit , col=col3, lty=2)
pause("fig 4.2b")
#---------------

y  <- percorr.urbana
x1 <- cilindrata
x2 <- peso
xlab<-"cilindrata (litro)"
ylab<-"peso (kg)"
zlab<- "percorrenza urbana (km/l)"

h<- c(0.5, 150)
sm.options(ngrid=50)
a<-sm.regression(cbind(x1,x2),y, h=h,display="none",
          options=list(xlab=xlab, ylab=ylab, zlab=zlab))

persp(a$eval.points[,1], a$eval.points[,2], a$estimate,
    xlab=xlab, ylab=ylab, zlab=zlab, cex=1, theta=120, phi=20)
pause("fig 4.3a")
#
contour(a$eval.points[,1], a$eval.points[,2], a$estimate,
    xlab=xlab, ylab=ylab)
points(x1, x2, col=col2,  cex=0.6*cex0)
z<- chull(x1,x2)
z<- c(z,z[1])
lines(x1[z],x2[z], lty=lty3, col=col3)
pause("fig 4.3b")
#
detach.all()
