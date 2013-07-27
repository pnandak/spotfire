# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo al paragrafo 3.2 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------

source("base-www.R")
dati <- read.table("ieri-domani.dat", header=TRUE)
attach(dati)
n<- nrow(dati)
y.range<- range(c(y.ieri, y.domani))

polinomio <- function(p) if(p==0) lm(y.ieri~1) else lm(y.ieri~poly(x,p))
polinomi <- apply(cbind(0:(n-1)), 1, polinomio)


x0<- data.frame(x=sort(unique(c(x,seq(min(x), max(x), length=200)))))

#------grafico funzione e punti train ----------------------------------
#
     
plot(x, y.ieri, ylim=y.range, col=col1, pch=pch1)
pause("fig 3.1")

plot(x, y.ieri, ylim=y.range, col=col1, pch=pch1, xlab="",
     main="dati e polinomio di grado 3")
lines(x0$x, predict(polinomi[[4]], newdata=x0))
pause("fig 3.2b")

plot(x, y.ieri, ylim=y.range, col=col1, pch=pch1,  xlab="",
     main="dati e polinomio di grado 6")
lines(x0$x, predict(polinomi[[7]], newdata=x0))
pause("fig 3.2c")

plot(x, y.ieri, ylim=y.range, col=col1, pch=pch1,  xlab="",
     main="dati e polinomio di grado 12")
lines(x0$x, predict(polinomi[[13]], newdata=x0))
pause("fig 3.2d")

plot(x, y.ieri, ylim=y.range, col=col1, pch=pch1,  xlab="",
     main="dati e polinomio di grado 18")
lines(x0$x, predict(polinomi[[19]], newdata=x0))
pause("fig 3.2i")

plot(x, y.ieri, ylim=y.range, col=col1, pch=pch1, 
     main="dati e polinomio di grado 24")
lines(x0$x, predict(polinomi[[25]], newdata=x0))
pause("fig 3.2l")

# plot(x, y.ieri, ylim=y.range, col=col1, pch=pch1, 
#     main="dati  e polinomio di grado n-1")
# lines(x0$x, predict(polinomi[[n]], newdata=x0))

#-------------------------------------------

dev <- as.numeric(lapply( polinomi, deviance))
plot(0:(n-1), dev, type="b", xlab="p", ylab="devianza", ylim=c(0,max(dev)))
pause("fig 3.3a")

R2 <- function(obj) summary(obj)$r.squared
r2 <-  as.numeric(lapply( polinomi, R2))
plot(0:(n-1), r2, type="b", xlab="p", ylab="R²")
pause("fig 3.3b")


#-------------------------------------------
#------ dati di domani ---------------------


plot(x, y.domani, ylim=y.range, col=col2, pch=pch2,  xlab="",
     main="dati e polinomio di grado 3")
lines(x0$x, predict(polinomi[[4]], newdata=x0))
pause("fig 3.4m")

plot(x, y.domani, ylim=y.range, col=col2, pch=pch2,  xlab="",
     main="dati e polinomio di grado 6")
lines(x0$x, predict(polinomi[[7]], newdata=x0))
pause("fig 3.4n")

plot(x, y.domani, ylim=y.range, col=col2, pch=pch2,  xlab="",
     main="dati e polinomio di grado 12")
lines(x0$x, predict(polinomi[[13]], newdata=x0))
pause("fig 3.4o")

plot(x, y.domani, ylim=y.range, col=col2, pch=pch2,  xlab="",
     main="dati e polinomio di grado 18")
lines(x0$x, predict(polinomi[[19]], newdata=x0))
pause("fig 3.4p")

plot(x, y.domani, ylim=y.range, col=col2, pch=pch2, 
     main="dati e polinomio di grado 24")
lines(x0$x, predict(polinomi[[25]], newdata=x0))
pause("fig 3.4q")


#------
y.hat <- lapply( polinomi, predict)
devianza <- function( y.hat) sum((y.domani-y.hat)^2)
dev2 <- as.numeric(lapply(y.hat, devianza))
plot(0:(n-1), dev2, type="b", xlab="p", ylab="devianza", ylim=c(0,max(dev)))
pause("fig 3.5a")

r2 <- 1-dev2/dev2[1]
plot(0:(n-1), r2, type="b", xlab="p", ylab="R²", ylim=c(0,1))
pause("fig 3.5b")
#
detach.all()
