# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo al paragrafo 4.6 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------

source("base-www.R")
source("f_vera.R")
x <- x.250
y <- f.vera250
par(mfrow=c(2,2))
plot(x, y, type="l", ylab="f(x)")
#
plot(x, y, type="l", lty=2, ylab="f(x)")
abline(v=0.8, lty=lty3, col=col3)
low <- (x<0.8)
m1 <-  mean(y[low])
m2 <-  mean(y[!low])
lines(x[low],  rep(m1,length=sum(low)))
lines(x[!low], rep(m2,length=sum(!low)))
#
plot(x, y, type="l", lty=2, ylab="f(x)")
abline(v=0.8, lty=lty3, col=col3)
abline(v=0.6, lty=lty3, col=col3)
low1 <- (x<0.6)
low2 <- (x<0.8) & (!low1)
m11 <-  mean(y[low1])
m12 <-  mean(y[low2])
lines(x[low1],  rep(m11,length=sum(low1)))
lines(x[low2],  rep(m12,length=sum(low2)))
lines(x[!low], rep(m2,length=sum(!low)))
#
plot(x, y, type="l", lty=2, ylab="f(x)")
abline(v=0.8, lty=lty3, col=col3)
abline(v=0.6, lty=lty3, col=col3)
abline(v=1.8, lty=lty3, col=col3)
hi <- !low
hi1<- hi & (x<1.8)
hi2<- hi & (x>1.8)
m21 <-  mean(y[hi1])
m22 <-  mean(y[hi2])
lines(x[low1],  rep(m11,length=sum(low1)))
lines(x[low2],  rep(m12,length=sum(low2)))
lines(x[hi1],  rep(m21,length=sum(hi1)))
lines(x[hi2],  rep(m22,length=sum(hi2)))
#
par(mfrow=c(1,1))
pause("fig 4.13")
#---------------------------------------------

load("f2_vera.save")

np<- 61
x <- y <- seq(0,3, length=np)

ind<- seq(1,np, by=2)
x0<- x[ind]
y0<- y[ind]
z0<- z[ind,ind]
persp(x0, y0, z0, theta=150, phi=20, r=2.5,
      xlab="x1", ylab="x2", zlab="y")
pause("fig 4.14a")
#--
library(tree)

nx <- length(x)
ny <- length(y)
xoy<- cbind(rep(x, ny), as.vector(matrix(y, nx, ny, byrow = TRUE)))
X0 <- matrix(xoy, nx * ny, 2, byrow = FALSE)
x1<- X0[,1]
x2 <- X0[,2]
zz <- matrix(z, nx*ny, 1,  byrow = FALSE)
albero <- tree(zz~x1+x2)
#
zp <- matrix(predict(albero), nrow=nx, ncol=ny)
persp(x, y, zp, theta=150, phi=20, r=2.5,
      xlab="x1", ylab="x2", zlab="y")
pause("fig 4.14b")
#
plot(albero, type="uniform")
text(albero, digits=2, pretty=3)
pause("fig 4.15a")
#
partition.tree(albero)
pause("fig 4.15b")
#------------------------------------------
dati <- read.table("ieri-domani.dat", header=TRUE)
x<- rep(dati$x,2)
y<- c(dati$y.ieri, dati$y.domani)

plot(x,y, type="n")
points(dati$x, dati$y.ieri, pch=pch1, col=col1, cex=0.8)
points(dati$x, dati$y.domani, pch=pch2, col=col2, cex=0.8)
legend(2, 0.45, legend=c("ieri","domani"),
        pch=c(pch1,pch2), col=c(col1, col2))
pause("fig 4.16")
#--
x0<- dati$x
y0<- dati$y.ieri
t3 <-  tree(y0~x0, control =
           tree.control(nobs=30, mindev = 0.001, minsize=2))
plot(t3)
pause("fig 4.17a")
#
p3<- prune.tree(t3, newdata=data.frame(x0=x0,y0=dati$y.domani))
plot(p3)
pause("fig 4.17b")
#
p4 <- prune.tree(t3, best=4)
plot(p4)
text(p4)
pause("fig 4.17c")
#
x0 <- seq(0.5, 3, length=5000)
plot(x, y, type="n")
points(dati$x, dati$y.ieri, pch=pch1, col=col1, cex=0.8)
points(dati$x, dati$y.domani, pch=pch2, col=col2, cex=0.8)
legend(2, 0.45, legend=c("ieri","domani"),
        pch=c(pch1,pch2), col=c(col1, col2))
lines(x0, predict(p4, newdata=data.frame(x=x0)), col=col3)
pause("fig 4.17d")
#
detach.all()
