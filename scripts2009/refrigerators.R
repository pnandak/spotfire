# DATA MINING E SISTEMI INFORMATIVI
# ESERCITAZIONE 6/3/2009
# CARATTERISTICHE FRIGORIFERI
# FONTE DEI DATI : TOPTEN (www.topten.ch)
###############################################################################
# VARIABILI
# door numero porte (1 o 2)
# type modello (freestanding -indipendente- o built-in -da incasso-)
# vtot, coolv, freezev volume netto totale, del reparto frigo e del congelatore (litri) 
# height, width, depth altezza, larghezza, profondità (cm)
# eff indice efficienza energetica (%, quanto più basso tanto più efficiente)
# energyc consumo annuo di energia elettrica (kWh)
# cost15 costo di esercizio (ipotizzando vita utile di 15 anni, €0.15/kWh costo energia elettrica)
###############################################################################
#INPUT DATI
frigo <- read.table("http://venus.unive.it/romanaz/datamin/dati/fridge.txt",
 header=TRUE)
str(frigo)
n <- dim(frigo)[1]
###############################################################################
# VARIABILI DI STRATIFICAZIONE door, type
tab <- table(frigo$door,frigo$type)
summary(tab)
porte <- substr(as.character(frigo$door),2,2)
colore <- rep("black",n)
colore[frigo$type == "BI"] <- "red"
################################################################################
# ANALISI INIZIALE
summary(frigo)
round(sd(frigo[,3:11]),2)
stem(frigo$vtot)
stem(frigo$freezev)
boxplot(frigo$vtot,frigo$coolv,frigo$freezev)
boxplot(frigo$vtot,frigo$coolv,frigo$freezev)
boxplot(frigo$vtot~frigo$door)
boxplot(frigo$freezev~frigo$door)
plot(frigo$vtot,frigo$freezev,pch=20,col=colore)
text(frigo$vtot,frigo$freezev,labels=porte,cex=0.6,pos=3)
boxplot(frigo$height,frigo$width,frigo$depth)
plot(frigo$height,frigo$depth,pch=20,col=colore)
text(frigo$height,frigo$depth,labels=porte,cex=0.6,pos=3)
volteor <- (frigo$height*frigo$depth*frigo$width)/1000
boxplot(volteor,frigo$vtot)
cor(volteor,frigo$vtot)
plot(frigo$vtot,volteor,pch=20,col=colore)
text(frigo$vtot,volteor,labels=porte,cex=0.6,pos=3)
pairs(frigo[,3:11],pch=20)
round(cov(frigo[,3:11]),2)
round(cor(frigo[,3:11]),2)
################################################################################
# COMPONENTI PRINCIPALI
pc <- princomp(frigo[,3:11],cor=TRUE)
str(pc)
summary(pc)
plot(pc$scores[,1:2],pch=20,col=colore,
 xlab="PC1 (60%)",ylab="PC2 (15%)",main="PCA OF REFRIFERATORS DATA")
text(pc$scores[,1:2],labels=porte,cex=0.6,pos=3)
plot(pc$scores[,c(1,3)],pch=20,col=colore,
 xlab="PC1 (60%)",ylab="PC3 (11%)",main="PCA OF REFRIFERATORS DATA")
text(pc$scores[,c(1,3)],labels=porte,cex=0.6,pos=3)
round(cor(frigo[,3:11],pc$scores[,1:3]),2)
###############################################################################
# DATI ANOMALI E PUNTI SUPPLEMENTARI
stem(scale(pc$scores[,1]))
frigo[pc$scores[,1] < -4,] # unità n. 47 possibile dato anomalo
pc1 <- princomp(frigo[-47,3:11],cor=TRUE)
str(pc1)
summary(pc1)
round(cor(frigo[-47,3:11],pc1$scores[,1:3]),2)
summary(pc1$scores[,1:3])
plot(pc1$scores[,1:2],pch=20,col=colore[-47],
 xlim=c(-9,3.5),ylim=c(-3,3.5),
 xlab="PC1 (56%)",ylab="PC2 (17%)",main="PCA OF REFRIFERATORS DATA")
text(pc1$scores[,1:2],labels=porte[-47],cex=0.6,pos=3)
pc47 <- ((frigo[47,3:11]-pc1$center)/pc1$scale)
pc47 <- as.matrix(pc47)%*%pc1$loadings[,1:2]
pc47
points(pc47,pch=20,col=colore[47])
text(pc47,labels=porte[47],cex=0.6,pos=3)
text(pc47,labels="U47",cex=0.6,pos=4,col="cyan")






