# DATA MINING E SISTEMI INFORMATIVI
# ESERCITAZIONE 27/2/2009
# PROGETTO DEMOGRAFIA D'IMPRESA
# FONTE DEI DATI : MOVIMPRESE (INFOCAMERE, www.infocamere.it)
###############################################################################
#INPUT DATI
movi <- read.csv2("http://venus.unive.it/romanaz/datamin/dati/movimprese08.csv",header=TRUE,na.strings=":")
str(movi)
ucanc <- read.csv2("http://venus.unive.it/romanaz/datamin/dati/canc_ufficio08.csv",header=TRUE,na.strings=":")
str(ucanc)
movi1 <- read.csv2("http://venus.unive.it/romanaz/datamin/dati/movimprese08_s1.csv",header=TRUE,na.strings=":")    #I semestre 2008
str(movi1)
movi2 <- read.csv2("http://venus.unive.it/romanaz/datamin/dati/movimprese08_s2.csv",header=TRUE,na.strings=":")    #II semestre 2008
str(movi2)
###############################################################################
#COSTRUZIONE DATA FRAME
prov <- movi[movi$CODAT == "TOT " & movi$PROV != "0",] #estrai righe corrisp. a province con cod. att. "TOT"  
str(prov)
uctprov <- ucanc[ucanc$CODAT == "TOT " & ucanc$PROV != "  ",] #come sopra, per ucanc
str(uctprov)
c(sum(prov$ISCTOT),sum(prov$CESTOT)-sum(uctprov$CESSUFFTOT))                 #controllo
###############################################################################
#VARIABILI DI STRATIFICAZIONE AREE TERRITORIALI
area <- movi1[-125,2] # regioni ("R") o province ("P")
area1 <- movi1[-125,3]# macroaree
area1 <- area1[area == "P"]
colour <- rep("black",length(area1))
colour[area1 == "NE"] <- "red"                 
colour[area1 == "C"] <- "green"
colour[area1 == "S"] <- "cyan"
colour[area1 == "I"] <- "orange"
################################################################################
# ANALISI 1: TUTTE LE PROVINCE
nati <- 100*prov$ISCTOT/prov$REGTOT                                         #t. natalità
morti <- 100*(prov$CESTOT-uctprov$CESSUFFTOT)/prov$REGTOT                   #t. mortalità
var <- 100*(prov$ISCTOT-prov$CESTOT+uctprov$CESSUFFTOT)/prov$REGTOT         #t. crescita 
stem(nati)
prov[nati > 9,1:9]                                      
stem(morti,scale=0.5)
stem(var,scale=0.5)
df <- data.frame(prov$REG,prov$PROV,area1,nati,morti,var)
summary(df)
plot(nati,morti,pch=20,col=colour,
 xlab="T. Natalità",ylab="T. Mortalità",main="Movimprese 2008")
text(nati,morti,labels=area1,cex=0.5,pos=4)
pairs(df[,4:6],col=colour,pch=20)
cor(df[,4:6])
###############################################################################
# ANALISI 2: CONFRONTO AREE TERRITORIALI
tapply(nati,area1,summary)
tapply(morti,area1,summary)
tapply(var,area1,summary)
boxplot(nati~area1,horizontal=TRUE,notch=TRUE,
 xlab="T. Natalità",main="Movimprese 2008",col="lightgrey")
boxplot(morti~area1,horizontal=TRUE,notch=TRUE,
  xlab="T. Mortalità",main="Movimprese 2008",col="lightgrey")
boxplot(var~area1,horizontal=TRUE,notch=TRUE,
 xlab="T. Variazione",main="Movimprese 2008",col="lightgrey")
abline(v=0,col="red",lty="dashed",lwd=2)
################################################################################
