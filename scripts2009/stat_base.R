# 6. STATISTICA DI BASE
#
###########################################################
##################### Funzioni in evidenza#################
# table: tabelle di frequenza
# ecdf: funzione di ripartizione empirica
# summary: indici di sintesi
# mean, sd: media, deviazione standard
# median, mad: mediana, deviazione mediana assoluta 
# quantile: quantili
# barplot, hist, stem, boxplot: presentazioni grafiche
# is.na, na.omit: funzioni per individuare e rimuovere dati mancanti
# tapply: applicazione di una funzione separatamente ai gruppi definiti da un fattore
###########################################################
#
# Archivio dati "laureati":
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
# Tabelle di frequenza univariate: funzione table()
# Distribuzione dei laureati secondo la provincia di residenza
dprov <- table(lau[,4], dnn="Provincia") #frequenze assolute
dprov
str(dprov)                      #oggetto di classe "table"
round(100*dprov/sum(dprov), 1)  #frequenze relative
#
# Diagramma "rettangoli distanziati": funzione barplot()
barplot(100*dprov/dim(lau)[1],
xlab="Provincia", ylab="Frequenza %", col ="lavender", cex.names=0.6, 
main="Laureati triennali in Economia per provincia", sub="Ca' Foscari - 2003")
#
# Analisi del voto di laurea
#
# Quante sono le lodi?
round(table(lau[,11])/dim(lau)[1], 3)
# Qual è la % dei voti inferiori a 100?
100*length(lau$V12[lau$V12 < 100])/dim(lau)[1]
# Attribuiamo 2 punti alla lode
lau$V12 <- lau$V10; lau$V12[lau$V11 == "L"] <- 112
#
# Indici di sintesi (min, max, quartili, media): summary()
summary(lau$V12)
# Indici di sintesi (media, deviazione standard): c(mean(), sd())
c(mean(lau$V12), sd(lau$V12))
# Indici di sintesi (mediana, deviazione mediana assoluta): c(median(), mad())
c(median(lau$V12), mad(lau$V12))
# Indici di sintesi, quantili: quantile()
quantile(lau$V12,probs=seq(0,1,by=0.1)) #decili+min/max
#
# Tipologia distributiva, istogramma: hist()
# Classi di ampiezza 2 (?) 
hist(lau$V12,breaks=seq(79.5,113.5,by=2),freq=FALSE,
xlab="Voto di laurea", ylab="Densità di frequenza", col="lavender",
main="Laureati triennali in Economia (2003, n =437)")
# Statistiche-ombra dell'istogramma(frequenze, densità, ecc.):
# hist(..., plot=FALSE) #oggetto di classe "histogram"
hist(lau$V12,breaks=seq(79.5,113.5,by=2),freq=FALSE,plot=FALSE) 
#
# C'è un effetto "genere"?
#
table(lau$V3)
tapply(lau$V12, lau$V3, summary)
tapply(lau$V12, lau$V3, function(x) quantile(x, probs=seq(0,1,by=0.1)))
layout(matrix(1:2,ncol=1))
hist(lau$V12[lau$V3=="F"],breaks=seq(79.5,113.5,by=2),freq=FALSE,
xlab="Voto di laurea", ylab="Densità di frequenza", col="pink",
main="Laureati triennali in Economia (femmine, n =281)")
hist(lau$V12[lau$V3=="M"],breaks=seq(79.5,113.5,by=2),freq=FALSE,
xlab="Voto di laurea", ylab="Densità di frequenza", col="lavender",
main="Laureati triennali in Economia (maschi, n =186)")
#
# Più efficace il confronto delle funzioni di ripartizione: funzione ecdf()
#
?ecdf
plot(ecdf(lau$V12[lau$V3=="F"]),
xlab="Voto di laurea", ylab="Funzione di ripartizione",
main="Laureati triennali in Economia (2003, n =437)")
plot(ecdf(lau$V12[lau$V3=="M"]), pch=20, add=TRUE) 
points(c(80,80),c(0.9,0.8), pch=c(21,20)) #legenda
text(c(82,82),c(0.9,0.8), labels=c("F", "M"), pos=4)
#
########################################################################
# Archivio dati Qualità della vita 2004 (Il Sole-24 Ore 20/12/2004)
#
# Dati economici e sociodemografici delle 103 province italiane
# Prov: nome della provincia
# Sigla: sigla automobilistica
# Area: area territoriale (NO: nord-ovest, NE: nord-est, C: centro, S: sud, I: isole)
# Dis: persone in cerca di lavoro in rapporto alla forza lavoro (%, media 2003)
# Esp: valore aggiunto derivante dalle esportazioni (%, 2003)
# Iscr: nuove iscrizioni di imprese alle CC in rapporto alle cessazioni(ottobre 2003-settembre 2004)
# Risp: depositi bancari pro capite (euro, 31/12/2003)
# Cons: spesa pro capite per auto, moto, elettrodomestici e mobili (euro, 2003)
# Vagg: valore aggiunto pro capite a prezzi correnti (euro, 2003)
# Int: stima tasso d'interesse a breve (31/12/03)
# 
qual <- read.table(file="c:/corsoR/dati/qual_vita04.txt", header=TRUE)
str(qual)
dim(qual)
table(qual$Area)
#
# Analisi del tasso di disoccupazione
#
# Tipologia distributiva con presentazione a ramo e foglie: stem()
#
stem(qual$Dis)      #distribuzione multimodale!
summary(qual$Dis)
#
# Qual è il ruolo dell'area territoriale?
#
tapply(qual$Dis, qual$Area, summary)
#
# Confronto grafico con diagrammi scatola-baffi: boxplot()
#
boxplot(qual$Dis~qual$Area, horizontal=TRUE, col="lavender",
names=c("Centro", "Isole", "N-est", "N-ovest", "Sud"),
xlab="Tasso di disoccupazione 2003", ylab="Area territoriale",
main="La disoccupazione nelle province")
#
# Statistiche-ombra del boxplot: boxplot(..., plot=FALSE)
box <- boxplot(qual$Dis~qual$Area, plot=FALSE)  #ricerca dati anomali
qual[qual$Dis == 7.22,]
qual[qual$Dis == 13.46,]
#  
############################################################################  
#
################################ Dati mancanti##############################
#
# Dati sullo sviluppo costiero delle regioni italiane
# Le regioni senza sbocco sul mare non hanno dati su questa variabile
# Codifica nel file ":"
#
reg <- read.table(file="c:/corsoR/dati/regioni.txt", header=TRUE,
na.strings=":", row.names="nome")
str(reg)
#
# Localizzazione dei dati mancanti: is.na()
is.na(reg)
#
# Esclusione unità (regioni) con dati mancanti): na.omit
coste1 <- na.omit(reg$coste)
coste1
#
# Opzione na.rm
c(mean(reg$coste), sd(reg$coste)) #NA
c(median(reg$coste), mad(reg$coste)) #NA
c(mean(reg$coste, na.rm=TRUE), sd(reg$coste, na.rm=TRUE)) #calcola la media dei dati diversi da NA
c(median(reg$coste, na.rm=TRUE), mad(reg$coste, na.rm=TRUE))
#
# Le funzioni stem() e boxplot() escludono automaticamente le unità
# con dati mancanti
stem(reg$coste, scale=2)
boxplot(reg$coste, horizontal=TRUE, col="gray",
xlab="Sviluppo costiero (km)", main="Le coste delle regioni italiane")
#
###########################################################################


              
 
  
