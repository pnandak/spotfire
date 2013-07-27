# 9. DIPENDENZA E CORRELAZIONE
#
#########################################################################
#
####################### Funzioni in evidenza#############################
# table, prop.table: tabelle di contingenza
# cov, cor: covarianza e correlazione
# lm: retta minimi quadrati, modelli lineari
# summary: indici di sintesi
# scale: trasformazioni
#########################################################################
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
# Tabelle di frequenza bivariate: funzione table()
# Distribuzione dei laureati secondo il sesso e il diploma di scuola superiore
#
# Ricodifica dei diplomi
dipl <- rep("ALTRO",length(lau$V7))
dipl[lau$V7=="*LICENZALINGUISTICA" | lau$V7=="LICENZALINGUISTICA"] <- "LL"
dipl[lau$V7=="*MATURITA'CLASSICA"] <- "MCL" 
dipl[lau$V7=="*MATURITA'PROFESSIONALEPERILCOMMERCIO"] <- "MPC" 
dipl[lau$V7=="*MATURITA'SCIENTIFICA"] <- "MSC" 
dipl[lau$V7=="*MATURITA'TECNICACOMMERCIALE"] <- "MTC" 
dipl[lau$V7=="*MATURITA'TECNICAPERILTURISMO"] <- "MTT" 
dipl[lau$V7=="*PERITOAZIENDALE"] <- "PAZ" 
dipl[lau$V7=="*PERITOINDUSTRIALE"] <- "PIN" 
dipl[lau$V7=="MATURITA'LINGUISTICA"] <- "MLI" 
dipl[lau$V7=="MATURITA'MAGISTRALESPERIMENTALE"] <- "MMS" 
dipl[lau$V7=="MATURITA'PROFESSIONALEALBERGHIERA"] <- "MPA"
#
# Tabella delle frequenze congiunte
#
tab <- table(lau[,3],dipl, dnn=c("Sesso", "Diploma")) #frequenze assolute congiunte
tab
str(tab)                                              #oggetto di classe "table"
reltab <- round(100*tab/sum(tab), 1)                  #frequenze relative congiunte
#
# Tabella delle frequenze subordinate rispetto al sesso
#
sub_sex <- round(100*prop.table(tab,1),1)
sub_sex
barplot(t(sub_sex), beside=FALSE, xlim=c(0,1), width=0.3,
legend.text=TRUE, xlab="Sesso", ylab="Freq. subordinate dei diplomi (%)",
main="Laureati in Economia")
#
# Tabella delle frequenze subordinate rispetto al diploma
#
sub_dip <- round(100*prop.table(tab,2),1)
sub_dip
barplot(sub_dip, beside=FALSE, xlim=c(0,1), width=0.07,
legend.text=TRUE, xlab="Tipo di diploma", ylab="Freq. subordinate dei sessi (%)",
cex.names=0.7, col=c("pink", "lightblue"), main="Laureati in Economia")
#
# Statistica e test chi quadrato d'indipendenza
#
summary(tab)
###########################################################################
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
qual <- read.table(file="c:/corsoR/dati/qual_vita04.txt", header=TRUE,
row.names=1)
str(qual)
dim(qual)
#
# Indici di sintesi
summary(qual)
#
# Diagrammi scatola-baffi delle distribuzioni marginali (unità standard)
#
qual_st <- scale(qual[,3:8], center=TRUE, scale=TRUE) #standardizzazione
boxplot(data.frame(qual_st), ylab="Unità standard", col="lavender",
main="Le province italiane", sub="Indagine qualità della vita 2004")
#
# Ruolo dell'area territoriale
#
boxplot(qual_st[,3]~qual[,1], ylab="Unità standard", col="lavender",
main="Tasso di disoccupazione", sub="Indagine qualità della vita 2004")
boxplot(qual_st[,4]~qual[,1], ylab="Unità standard", col="lavender",
main="Valore aggiunto esportazioni (%)", sub="Indagine qualità della vita 2004") 
boxplot(qual_st[,5]~qual[,1], ylab="Unità standard", col="lavender",
main="Nuove iscrizioni di imprese/ cessazioni", sub="Indagine qualità della vita 2004")
#
# Matrice di correlazione
round(cor(qual[,3:8]), 2)
#
# Diagramma di dispersione Vagg, Risp
#
area1 <- rep(1,dim(qual)[1])
area1[qual$Area=="NE"] <- 2
area1[qual$Area=="C"] <- 3
area1[qual$Area=="S"] <- 4
area1[qual$Area=="I"] <- 5
plot(qual[,c(8,6)]/1000, pch=20, col=gray(area1/6),
xlab="Valore aggiunto pro capite (migliaia di euro)",
ylab="Risparmi pro capite (migliaia di euro)",
main="Valore aggiunto e risparmio nelle province italiane")
#
# Modello lineare: Risp = a0 + a1*Vagg + errore
#
lmod <- lm(qual$Risp~qual$Vagg)
summary(lmod)
#
# Analisi dei residui standardizzati
#
res_st <- lmod$residuals/sd(lmod$residuals)
stem(res_st)
boxplot(res_st, col= "lavender", 
horizontal=TRUE, xlab="Residui standardizzati",
main="Valore aggiunto e risparmio nelle province italiane")
qqnorm(res_st, pch=20, 
xlab="Quantili teorici (unità standard)",
ylab="Quantili residui standardizzati",
main="Q-Q plot normale")
qqline(res_st, col="red")
kstest <- ks.test(res_st, "pnorm", alternative="two.sided")
##########################################################################
