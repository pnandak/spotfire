# 8. METODI ELEMENTARI D'INFERENZA
#
###########################################################
#
# Funzioni in evidenza#####################################
# sample(): estrazione di campioni (pseudo) casuali
# stem(), boxplot(), qqnorm: diagrammi distribuzionali
# table(), summary(): sintesi descrittiva dei dati campionari
# binom.test(), t.test(), wilcox.test, ks.test: intervalli di confidenza e test di significatività
###########################################################
#Archivio dati "laureati":
# Campione di 467 laureati triennali in Economia nel 2003  
# V1: corso di laurea
# V2: matricola (dato perturbato)
# V3: sesso
# V4: sigla provincia di residenza
# V5: anno prima immatricolazione a Venezia
# V6: tipo immatricolazione
# V7: diploma maturità
# V8: voto maturità, V9: base voto maturità (60 o 100)
# V10: voto laurea, V11: lode (L, sì; NL, no)
#
lau <- read.table(file="c:/corsoR/dati/laureati.txt",header=FALSE)
str(lau)
dim(lau)
#
# Consideriamo l'insieme dei 467 laureati la POPOLAZIONE di riferimento 
# Preleviamo un campione di 50 laureati (estr. con reinserimento)
#
camp <- sample(1:dim(lau)[1], size=50, replace=TRUE)
sort(camp)                   #numero d'ordine dei laureati scelti a caso    
lau1 <- lau[camp,]           #campione dei dati 
dim(lau1)
# 
# Distribuzione dei laureati secondo il sesso
# 
dsex <- table(lau1[,3]) #frequenze assolute
dsex
#
# La frequenza dei laureati maschi è uguale a quella delle femmine?
#
pf <- dsex[1]/50                 #stima campionaria della frequenza delle laureate
se <- sqrt(pf*(1-pf)/50)         #stima errore standard
test <- binom.test(dsex, p=0.5, alternative="two.sided", conf.level=0.95)
test
#
# Analisi del voto di laurea
#
summary(lau1$V10)                #lode non considerata
stem(lau1$V10)
boxplot(lau1$V10, horizontal=TRUE, col="lavender",
xlab="Voto di laurea", main="Campione di n = 50 laureati in Economia")
#
# La distribuzione del voto di laurea è normale?
#
qqnorm(lau1$V10, col="red", pch=20,
xlab="Quantili teorici (unità standard)", ylab="Quantili osservati",
main="Voto di laurea: Q-Q plot di Normalità")
qqline(lau1$V10, lwd=2)
test <- ks.test(x=lau1$V10, y="pnorm", alternative="two.sided")
test 
##########################################################################
# Confrontiamo il voto di laurea dei maschi e delle femmine
# sulla base di due campioni estratti indipendentemente 
# dalla sottopopolazione dei maschi e da quella delle femmine
#
lau_f <- lau[lau$V3=="F",]              #sottopopolazione delle laureate 
lau_m <- lau[lau$V3=="M",]              #sottopopolazione dei laureati maschi
camp_f <- sample(1:dim(lau_f)[1], size=30, replace=TRUE)
camp_m <- sample(1:dim(lau_m)[1], size=30, replace=TRUE)
sort(camp_f)                            #indici delle unità campionarie (F)
sort(camp_m)                            #indici delle unità campionarie (M)
#
# Confrontiamo il voto di laurea di femmine e maschi mediante boxplot appaiati
#
boxplot(lau_f[camp_f,10],lau_m[camp_m,10], notch=TRUE, col=c("pink","lavender"),
horizontal=TRUE, xlab="Voto di laurea", names=c("Femmine", "Maschi"),
main="Laureati in Economia (F: 30 unità, M: 30 unità)")
#
# Indici di sintesi
#
summary(lau_f[camp_f,10])
summary(lau_m[camp_m,10])
#
# Confronto degli indici di posizione
#
wtest <- wilcox.test(lau_f[camp_f,10],lau_m[camp_m,10], mu=0,
alternative="two.sided", conf.int=TRUE, conf.level=0.95)
wtest
ttest <- t.test(lau_f[camp_f,10],lau_m[camp_m,10], mu=0,
alternative="two.sided", conf.int=TRUE, conf.level=0.95)
#
# Confronto delle due distribuzioni
#
kstest <- ks.test(lau_f[camp_f,10],lau_m[camp_m,10], mu=0,
alternative="two.sided")
kstest
########################################################################### 


              
 
  
