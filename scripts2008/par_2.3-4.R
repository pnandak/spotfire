# Programmi per ricostruire grafici e tabelle del libro:
#    "Analisi dei dati e 'data mining'" di A.Azzalini e B.Scarpa,
#    © Springer-Verlag Italia, 2004 (ISBN 88-470-0272-9).
#
# Codice relativo ai paragrafi 2.3-2.4 (© 2003, 2004, A.Azzalini e B.Scarpa)
# ------------------------------------------------------------------------
source("base-www.R")
#
brazil <- read.csv("brazil.csv",  row.names=1, header=TRUE)
brazil[,"id"] <- factor(brazil[,"id"])
brazil[,"satisfaction"] <- factor(brazil[,"satisfaction"], ordered=TRUE)
brazil[,"education"] <- factor(brazil[,"education"], ordered=TRUE)
for(k in c(6:9,11:40)) 
          brazil[,k] <- factor(brazil[,k])
brazil[,"ok"] <- factor(brazil[,"satisfaction"]=="3" |  
                   brazil[,"satisfaction"]=="4", labels=c("insod","sodd"))
attach(brazil)

soddisf <- (satisfaction > 2)
anziani <- (age > 45)
freq<- table(soddisf, anziani)
nome<- "tabella di p.41"
print(freq)
pause(nome)
rel <- t(t(freq)/apply(freq,2,sum))
table(anziani) # 309   191

# ------------------------------------
# calcolo W
#

# p2<- 157/191
# p<- 382/500
# p1 <- 225/309
# 225*log(p1)+84*log(1-p1)+157*log(p2)+34*log(1-p2)
# [1] -270.2
# 382*log(p)+118*log(1-p)
# [1] -273.2
# 225*log(p1)+84*log(1-p1)+157*log(p2)+34*log(1-p2)- (382*log(p)+118*log(1-p))
# [1] 2.964
# 1-pchisq(2*2.964,1)
#[1] 0.01490
#-----ovvero:
# m1<- glm(soddisf~anziani, family=binomial)
# summary(m1)
# m1$null.deviance- deviance(m1)
# [1] 5.928
cat("il file sorgente contiene dell'altro...\n")
# ------------------------------------

#
# Regressione logistica dell soddisfazione rispetto al reddito o età
#
nome <- ""
y <- as.numeric(table(soddisf, pincome)[2,])
n <- as.numeric(table(pincome))
reddito <- sort(unique(pincome))
plot(reddito, y/n, ylim=c(0,1))
plot(reddito, log((y+0.5)/(n-y+0.5)))
cat("il file sorgente contiene dell'altro ancora...\n")
#
y <- as.numeric(table(soddisf, age)[2,])
n <- as.numeric(table(age))
eta <- sort(unique(age))
par(mfrow=c(1,2))
plot(eta, y/n, ylim=c(0,1))
plot(eta, log((y+0.5)/(n-y+0.5)))
par(mfrow=c(1,1))
pause("(questo serve per scopi esplorativi)")
#
m1 <- glm(cbind(y,n-y)~ eta, family=binomial)
m2 <- glm(cbind(y,n-y)~ eta+I(eta^2), family=binomial)
#
nome <- "figura 2.12(a)"
plot(eta, y/n, ylim=c(0,1), xlim=c(15,70),
     ylab="Pr{soddisfatti|età}")
x <- seq(15,70, length=200)
lines(x, predict(m2, newdata=data.frame(eta=x), type="response"),
      col=col2, lty=lty2)
lines(x, predict(m1, newdata=data.frame(eta=x), type="response"),
      col=col1, lty=lty0)
pt <- list(x=45, y=0.4)
legend(pt, legend=c("modello lineare", "modello quadratico"),
       lty=c(lty0, lty2))
pause(nome)
#----
nome <- "figura 2.12(b)"
plot(eta, y/n, ylim=c(0,1), xlim=c(15,70),
     ylab="Pr{soddisfatti|età}", cex=sqrt(n)/6)

x <- seq(15,70, length=200)
lines(x, predict(m2, newdata=data.frame(eta=x), type="response"),
      col=col2, lty=lty2)
lines(x, predict(m1, newdata=data.frame(eta=x), type="response"),
      col=col1, lty=lty0)
# cat("scegliere un punto\n")
legend(pt, legend=c("modello lineare", "modello quadratico"),
       lty=c(lty0, lty2))
pause(nome)
#------
nome<- "tabella 2.5"
print(summary(m2))
print(summary(m1))
pause(nome)
#
detach.all()
