# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo al paragrafo 2.1 (© 2003, 2004, A.Azzalini e B.Scarpa)
#------------------------------------------------------------------------
source("base-www.R")
auto<- read.table("auto.dat", header=TRUE)
attach(auto)
#
nome <-  "figura 2.1"
#
n <- nrow(auto)
#
d <- alimentazione=="benz"
pairs(auto[,
  c("percorr.urbana", "cilindrata","N.cilindri","peso")],     
  labels=c("percorrenza\nurbana", "cilindrata","numero\ncilindri",
         "peso"),
  col=ifelse(d,col1,col2), pch=ifelse(d,pch1,pch2),
  cex=10/sqrt(n)  )
pause(nome)
#---------------------------------------
#
nome <- "figura 2.2"
#
plot(cilindrata, percorr.urbana, type="n",
     ylab="percorrenza urbana (km/litro)",
     xlab="cilindrata (litri)", xlim=c(1,5.5))
d <- alimentazione=="benz"
points(cilindrata[d], percorr.urbana[d], col=col1, pch=pch1)
points(cilindrata[!d], percorr.urbana[!d], col=col2, pch=pch2)
legend(4.5, 20, pch=c(pch1, pch2), col=c(col1,col2),
       legend=c("benzina  ","diesel"))
#
pause(nome)

#---------------------------------------
nome <- "tabella 2.1"
fit3 <- lm(percorr.urbana~ cilindrata + I(cilindrata^2)+
            I(cilindrata^3)+ alimentazione)
print(summary(fit3))
pause(nome)
#
nome <- "figura 2.3" 
plot(cilindrata, percorr.urbana, type="n",
     ylab="percorrenza urbana",
     xlab="cilindrata",  xlim=c(1,5.5))
d <- alimentazione=="benz"
points(cilindrata[d], (percorr.urbana[d]), col=col1, pch=pch1)
points(cilindrata[!d], (percorr.urbana[!d]), col=col2, pch=pch2)
#
x <- (seq(min(cilindrata), max(cilindrata), length=200))
x <- seq(1,5.5,  length=200)
beta<- coef(fit3)
lines(x, beta[1]+ beta[2]*x+beta[3]*x^2+beta[4]*x^3, col=col1, lty=lty1)
lines(x,  beta[1]+ beta[2]*x+beta[3]*x^2+beta[4]*x^3+beta[5],
       col=col2, lty=lty2)
legend(4.5, 20, pch=c(pch1, pch2), col=c(col1,col2),
       legend=c("benzina","diesel"))
pause(nome)

#
nome <- "figura 2.4(a)" 
plot(fit3, which=1, sub.caption="")
pause(nome)
#
nome <- "figura 2.4(b)" 
plot(fit3, which=2, sub.caption="")
pause(nome)
#
#---------------------------------------
nome <- "figura 2.5" 
plot(cilindrata, 1/(percorr.urbana), type="n",
     ylab="consumo",
     xlab="cilindrata")
d <- alimentazione=="benz"
points(cilindrata[d], 1/(percorr.urbana[d]), col=col1, pch=pch1)
points(cilindrata[!d], 1/(percorr.urbana[!d]), col=col2, pch=pch2)
#
fit2 <- lm(1/(percorr.urbana)~ cilindrata+ alimentazione)
beta<- coef(fit2)
abline(beta[1:2], col=col1, lty=lty1)
abline(beta[1]+beta[3], sum(beta[2]) , col=col2, lty=lty2)
pause(nome)
#
nome <- "tabella 2.2"
print(summary(fit2))
pause(nome)
#
nome <- "figura 2.6"
plot(cilindrata, percorr.urbana, type="n",
     ylab="percorrenza urbana",
     xlab="cilindrata",  xlim=c(1,5.5))
d <- alimentazione=="benz"
points(cilindrata[d], (percorr.urbana[d]), col=col1, pch=pch1)
points(cilindrata[!d], (percorr.urbana[!d]), col=col2, pch=pch2)
#
x <- (seq(min(cilindrata), max(cilindrata), length=200))
x <- seq(1,5.5,  length=200)
lines(x, 1/(beta[1]+ beta[2]*x), col=col1, lty=lty1)
lines(x, 1/(beta[1]+beta[3]+ beta[2]*x),  col=col2, lty=lty2)
legend(4.5, 20, pch=c(pch1, pch2), col=c(col1,col2),
       legend=c("benzina","diesel"))
pause(nome)
#
r2.2 <- 1-sum((percorr.urbana-1/(fitted(fit2)))^2)/((n-1)*var(percorr.urbana))

fit2a <- update(fit2, . ~. + factor(N.cilindri==2), data=auto)
fit2b <- update(fit2a, . ~. + peso, data=auto)
#
nome <- "figura 2.7(a)" 
plot(fit2, which=1, sub.caption="")
pause(nome)
#
nome <- "figura 2.7(b)" 
plot(fit2, which=2, sub.caption="")
pause(nome)
#
#---------------------------------------
#
nome <- "tabella 2.3"
fit1<- lm(log(percorr.urbana)~ log(cilindrata)+ alimentazione)
print(summary(fit1))
pause(nome)
#
nome <- "figura 2.8(a)"
#
plot(log(cilindrata), log(percorr.urbana), type="n",
     ylab="log(percorrenza urbana)",
     xlab="log(cilindrata)")
d <- alimentazione=="benz"
points(log(cilindrata[d]), log(percorr.urbana[d]), col=col1, pch=pch1)
points(log(cilindrata[!d]), log(percorr.urbana[!d]), col=col2, pch=pch2)
beta<- coef(fit1)
abline(beta[1:2], col=col1, lty=lty1)
abline(beta[1]+beta[3], sum(beta[2]) , col=col2, lty=lty2)
pause(nome)
#----
nome <- "figura 2.8(b)"
plot(cilindrata, percorr.urbana, type="n",
     ylab="percorrenza urbana (km/l)",
     xlab="cilindrata (litri)",  xlim=c(1,5.5))
d <- alimentazione=="benz"
points(cilindrata[d], percorr.urbana[d], col=col1, pch=pch1)
points(cilindrata[!d], percorr.urbana[!d], col=col2, pch=pch2)
x <- log(seq(min(cilindrata), max(cilindrata), length=200))
x <- log(seq(1,5.5,  length=200))
beta<- coef(fit1)
lines(exp(x), exp(beta[1]+ beta[2]*x), col=col1, lty=lty1)
lines(exp(x), exp(beta[1]+beta[3]+ beta[2]*x),  col=col2, lty=lty2)
legend(4.5, 20, pch=c(pch1, pch2), col=c(col1,col2),
       legend=c("benzina","diesel"))
pause(nome)
#
nome <- "figura 2.9(a)"
plot(fit1, which=1, sub.caption="")
pause(nome)
#
nome <- "figura 2.9(b)"
plot(fit1, which=2, sub.caption="")
pause(nome)

#
nome<- "tabella 2.4"
r2.1 <- 1-sum((percorr.urbana-exp(fitted(fit1)))^2)/
         ((n-1)*var( percorr.urbana))
I.D   <- factor(N.cilindri==2, labels=c(">2","=2"))
fit1a <- update(fit1, . ~. + I.D, data=auto)
fit1b <- update(fit1a, . ~. + log(peso), data=auto)
fit1c <- update(fit1, . ~. + log(peso), data=auto)

print(summary(fit1b))
r2.1b <-  1-sum((percorr.urbana-exp(fitted(fit1b)))^2)/
             (202*var(percorr.urbana))
pause(nome)
#
nome<-"figura 2.10(a)"
plot(fit1b,1, sub.caption="")
pause(nome)
#
nome<-"figura 2.10(b)"
plot(fit1b,2, sub.caption="")
pause(nome)
#
nome<-"figura 2.10(c)"
plot(fit1b,3, sub.caption="")
pause(nome)
#
nome<-"figura 2.10(d)"
plot(fit1b,4, sub.caption="")
pause(nome)
#

detach.all()
