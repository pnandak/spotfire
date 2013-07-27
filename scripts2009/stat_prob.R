# 7. SIMULARE LA CASUALITA'

##########################################################
# Distribuzioni di probabilità nell'ambiente di R: *funzioni*
# Parola-chiave:
# norm (normale), binom (binomiale), pois (Poisson), unif (uniforme continua), ....
# Prefisso:
# d (funzione densità o funzione probabilità), 
# p (funzione di ripartizione)
# q (funzione quantilica)
# r (estrazione campioni pseudo-casuali)
# Parametri, a seconda della distribuzione:
# normale: media (mean), deviazione standard (sd)
# binomiale: numero estrazioni (size), probabilità di successo (prob)
# uniforme: minimo/massimo dato osservabile (min/max)
# Poisson: media = varianza (lambda)
#   
# Distribuzione Normale #######################################
#
# La densità
dnorm(x=1.2, mean=0, sd=1)      #valore densità normale standard in x=1.2
dnorm(-1.2, 0, 1)               #forma breve
plot(dnorm, from=-5, to=5, lwd=2, col="red", ylim=c(0,0.8),
xlab="x", ylab="Funzione densità", main="Densità gaussiane")
plot(function(x) dnorm(x, mean=1, sd=1), from=-5, to=5,lwd=2, add=TRUE)  
plot(function(x) dnorm(x, mean=0, sd=0.5), from=-5, to=5,lwd=2, add=TRUE)
plot(function(x) dnorm(x, mean=-1, sd=1.5), from=-5, to=5,lwd=2, add=TRUE)  
#
# Aree e f. di ripartizione
pnorm(-2:2, mean=0, sd=1)          #aree (-Inf,-2], (-Inf, -1], ecc.
pnorm(1, 0, 1) - pnorm(-1, 0, 1)   #area (-1, 1]
1- pnorm(1.65, 0, 1)               #area (1.65, Inf)
plot(pnorm, from=-5, to=5, lwd=2, col="red",
xlab="x", ylab="F. di ripartizione", main="F. di Ripartizione Gaussiane")
plot(function(x) pnorm(x, mean=1, sd=1), from=-5, to=5, lwd=2, add=TRUE)
#
# Quantili
qnorm(seq(0,1,by=0.2), mean=0, sd=1)   #quintili normale standard
qnorm(0.5, mean=10, sd=2)              #ovvio!   
#
# Simulazione campioni casuali
camp <- rnorm(10, mean=0, sd=1)        #campione di 10 elementi da N(0,1)
camp
stem(camp)
#
# Confronto dati empirici - modelli distributivi teorici
# Statura iscritti di leva nati nel 1977: disponibili medie regionali
# Supponiamo i dati ottenuti con un campionamento casuale (ipotesi non realistica)
# Modello distributivo gaussiano?
#
reg <- read.table(file="c:/corsoR/dati/regioni.txt", header=TRUE)
str(reg)
# Osserviamo la tipologia distributiva con stem() o boxplot()
stem(reg$stat77)
boxplot(reg$stat77, horizontal=TRUE, col="lavender",
xlab="Statura iscritti di leva nati nel 1977",
main="Statura iscritti di leva (medie regionali)")               
# Più dettagliato il qqplot
qqnorm(reg$stat77, pch=as.integer(reg$area),
ylab="Quantili osservati", xlab="Quantili teorici (unità standard)",
main="Q-Q Plot Normale")
qqline(reg$stat77, col=2)
# Test di significatività (Kolmogorov-Smirnov)
ks.test(x=reg$stat77, y="pnorm", alternative="two.sided") #modello gaussiano da rigettare?
#   
# Distribuzione Binomiale #######################################
#
# Funzione di probabilità
#
dbinom(x=4:6, size=10, prob=0.5)
dbinom(1, 10, 0.5)                         #forma breve
plot(0:10, dbinom(0:10, size=10, prob=0.5), type="h", col="red",
xlab="Numero di successi", ylab="Probabilità",
main="Distribuzione Binomiale(n= 10, p=0.5)")
#
# Funzione di ripartizione
#
pbinom(5, size=10, prob=0.5)    #probabilità di osservare non più di 5 successi
1-pbinom(5, size=10, prob=0.5)  #probabilità di osservare più di 5 successi
bin10 <- stepfun(0:10,pbinom(0:11, size=10, prob=0.5),f=0) #funzione a gradini, continua a destra
plot.stepfun(bin10, xlim=c(-1,11), ylim=c(0,1),verticals=FALSE,
col.hor="red", col.points="red",
xlab="Numero di successi", ylab="F. di ripartizione",
main="Distribuzione Binomiale(n= 10, p=0.5)")
#
# Quantili
#
qbinom(0.05, size=20, prob=0.6)
qbinom(seq(0,1,by=0.1), size=20, prob=0.4) #decili
#
# Simulazione campioni casuali (fondamentale Bi(1, p), cosiddetta distr. di Bernoulli)
#
camp <- rbinom(n=100, size=1, prob=0.5) #simulazione di 100 lanci di una moneta regolare 
camp
sum(camp)/100 #frequenza relativa dei successi (teste), v. atteso 0.5 ...
# Oppure
rbinom(n=1, size=100, prob=0.5) 
rbinom(n=10, size=100, prob=0.5) #ripetiamo 10 volte l'esperimento, come varia il numero di teste?
#################################################################################################
#
# Campionamento da popolazioni finite: funzione sample()#######################################
#
sample(1:90, size=5, replace=FALSE) #simula i numeri del lotto (estr. senza reinserimento) 
#
testacroce <- sample(c(0,1), size=20, replace=TRUE) #simula 20 lanci di una moneta regolare (un altro metodo ancora!)
sum(testacroce)
sample(c(0,1), size=20, replace=TRUE, prob=c(0.7, 0.3)) #moneta non regolare!
#
# Campione senza reinserimento di 10 capoluoghi di provincia (in tutto sono 103)
camp <- sample(1:103, size=10, replace=FALSE)
pop <- read.table(file="c:/corsoR/dati/qual_vita04.txt", header=TRUE)
pop$Prov[camp]
#
########################################################################