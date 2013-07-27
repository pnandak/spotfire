#-----------------------------------------------------------------------------------------------------------#
#			ETUDE DE CAS : MANIP PHARMACO
#				DESCRIPTION DE L'ETUDE :
# PPARalpha et RXR appartiennent à la superfamille des récepteurs nucléaires. Ils agissent comme facteurs de transcription sous l'effet de divers ligand. Suite à l'activation de PPARalpha par son ligand, celui-ci s'hétérodimérise avec RXR. Ce dimère se fixe sur des éléments de réponse situés dans les promoteurs des gènes cibles et régule la transcription de ces gènes.
# Le récepteur RXR est capable de former des hétérodimères avec plusieurs autres récepteurs nucléaires (une quinzaine environ). La question que nous nous posons est de savoir si l'activation de RXR par un ligand spécifique conduit au recrutement d'un ou de plusieurs partenaires de dimérisation et donc à la modulation de l'expression de divers panels de gènes cibles.
# Des souris de type sauvage ou de type PPARalpha-déficientes (4 souris/lot) ont été traitées pendant 15 jours soit avec un activateur spécifique de PPARalpha (FENO) soit avec un activateur spécifique de RXR (LGD) soit avec le véhicule seul (CMC). A l'issue des traitements, le foie des animaux a été prélevé et les ARNs hépatiques extraits puis radiomarqués au 33P par transcription inverse et hybridé à des puces à ADN (ADNc sur nylon) dédiées aux voies de signalisation impliquant RXR.
#-----------------------------------------------------------------------------------------------------------#


#-----------------------------------------------------------#
#Mise en place de la session
#-----------------------------------------------------------#

save.image("CHEMIN D'ACCES/ma session R.RData")
#Sauvegarde de la session


#-----------------------------------------------------------#
#Importation et mise en forme des données
#-----------------------------------------------------------#

#commençons par regarder ce que contient le fichier etude pharmaco.xls
#Une matrice de 383 lignes x 26 colonnes

pharma<-read.table("etude pharmaco.txt",header=T,nrows=382,sep="\t")
#On importe les données. Pour une raison que j'ignore, si on ne met pas l'argument nrows=382, R rajoute une ligne de NA a la fin de l'objet pharma

dim(pharma)
edit(pharma)
dimnames(pharma)[[2]]
#vérifications

# La colonne "Type" spécifie le type de spot correspondant à chaque ligne.
# Sur ce microarray "maison", on a créé différents types de spots :
# gene : le spot correspond à un fragment d'ADNc reconnaissant un transcrit précis. C'est ce qui va nous intéresser le plus en pratique
# spike : ce spot contient un fragment d'ADN qui correspond à un gène synthétique (zone intergénique de levure) à partir duquel on a produit un ARNm qui a été ajouté en quantité connue et identique dans les différents échantillons d'ARN de souris. On va utiliser la quantification de ces gènes pour faire notre normalisation
# ratio : ce spot correspond à un spike qui a été incorporé à un ratio connu entre les différents groupes d'échantillons. On pourra utiliser ces quantifications pour voir si la normalisation nous permet de nous rapprocher des ratios théoriques.
# bckg : backgroud ou bruit de fond. Ces spots ne contiennent que du tampon de dépôt (50% DMSO)
# gamme.BACT : l'ADNc correspondant au gène b-actine a été déposé sur la puce à différentes concentrations afin de vérifier que l'on était bien au plateau de signal.
# gamme.vide : une gamme de plasmide vide (ne contenant aucun insert) pour se faire une idée de la fixation non spécifique sur ce vecteur.
# Nous n'utiliserons pas les données correspondant à ces deux gammes dans nos manips. Ces gammes étaient présentes sur cette génération de puces pour permettre de réaliser des optimisations du protocole expérimental. On considèrera ici que les optimisations du protocole ont déjà été faites (c'est le cas, il faut me croire sur parole...)

genotype<-as.factor(c(rep("PPAR",12),rep("wt",12)))
treatment<-as.factor(rep(c(rep("CMC",4),rep("LGD",4),rep("FENO",4)),2))
groups<-as.factor(paste(as.character(genotype),as.character(treatment),sep="."))
#création des facteurs genotype, traitement et groupe

pharma<-pharma[sort.list(pharma[,1]),]
#On trie les données par ordre alphabétique des noms de spot.
#Ca ne sert pas à grand chose mais ça ne nous coûte pas grand chose non plus...

n.arrays<-dim(pharma)[2]-2
#On récupère le nombre de puces dans notre manip

spotmean<-apply(pharma[,2:25],2,function(x){tapply(x,pharma[,1],mean)})
#Pour chaque puce, on fait la moyenne des intensités des spots qui sont répliqués sur la puce

Type<-as.vector(tapply(as.vector(pharma[,26]),pharma[,1],unique))
#Pour chacune de ces moyennes, on récupère le type de spot

dim(spotmean)
length(Type)
#vérifications

pharma2<-cbind.data.frame(spotmean,Type)
#On créé une nouvelle matrice avec une valeur unique pour chaque série de spots répliqués

dim(pharma2)
dimnames(pharma2)
#vérifications

pharma2.gene<-pharma2[pharma2$Type=="gene",-dim(pharma2)[2]]
#On récupère les données correspondant aux gènes

dim(pharma2.gene)
#vérification : on étudie l'expression de 119 gènes

pharma2.spike<-pharma2[pharma2$Type=="spike",-dim(pharma2)[2]]
pharma2.ratios<-pharma2[pharma2$Type=="ratio",-dim(pharma2)[2]]
#On récupère les données correspondant aux spikes et aux ratios

n.genes<-dim(pharma2.gene)[1]
#On récupère le nombre de gènes étudiés

loggene<-log(pharma2.gene,base=2)
logspike<-log(pharma2.spike,base=2)
logratiospike<-log(pharma2.ratios,base=2)
#On transforme les données en log base 2
#j'économise le nom logratio dont on pourrait bien avoir besoin par la suite...


sel.spike<-logspike[c("Cal3","Cal4","Cal5","Cal6","Cal7","Util1","Util2","Util3"),]
#je sélectionne certains spikes uniquement
#j'ai choisi ces spikes là car ils ne sont pas dans le bruit de fond (ce qui est le cas de Cal8 et Cal9) et qu'ils ne sont pas saturés (ce qui est le cas de Cal1 et Cal2). J'ai également écarté le spike rbcl que nous avions produit au laboratoire car on a pu montrer ultérieurement que le fragment d'ADNc déposé sur la puce croisait avec certains gènes de souris.


boxplot(pharma2.gene,las=2)
boxplot(loggene,las=2)
#première observation des données
#que peut-on en dire?

boxplot(as.data.frame(t(loggene)),las=2,cex.axis=0.5)
#Boxplot des gènes


#-----------------------------------------------------------#
#Normalisation des données
#-----------------------------------------------------------#

coeff.norm<-apply(sel.spike,2,mean)
#calcul du coefficient de normalisation
#Remarque :
#Faire la moyenne arithmétique des log des intensités des spikes revient à prendre le log de la moyenne géométrique des intensités des spikes

gene.norm<-loggene-coeff.norm
ratios.norm<-logratiospike-coeff.norm
#normalisation des gènes et des ratios


#-----------------------------------------------------------#
# "Contrôles qualité"
#-----------------------------------------------------------#

#------------------------------------
#boxplots
#------------------------------------

boxplot(gene.norm,las=2)
#boxplot de chaque puce

boxplot(as.data.frame(t(gene.norm)),las=2,cex.axis=0.5)
#boxplot de chaque gène

#------------------------------------
#corrélation entre les arrays
#------------------------------------

cor.arrays<-cor(gene.norm)
#corrélations des arrays

group.cor<-list()
for (i in 1:length(levels(groups)))
{
	group.cor[[i]]<-cor.arrays[groups==levels(groups)[i],groups==levels(groups)[i]]
}
#Pour chaque groupe d’échantillon, on récupère les correlations entre les puces au sein du groupe


mean.group.cor<-sapply(group.cor,function(x){mean(x[lower.tri(x)])})
#Pour chaque groupe, on calcule la corrélation moyenne au sein du groupe

mean(mean.group.cor)
#moyenne du coefficient de corrélation moyen au sein des groupes

#------------------------------------
#comparaison des arrays deux à deux
#------------------------------------

pairs(gene.norm[,genotype=="PPAR"])
pairs(gene.norm[,genotype=="wt"])
#matrice de comparaison des puces deux à deux

pairs(gene.norm[,groups=="PPAR.CMC"])
#C’est plus lisible par groupe que par génotype

plot.chips<-function(datax,a,b)
#This function produces a xy plot of column a (x) and column b (y) of the dataset datax
#datax is a matrix with genes in rows and samples in columns
#a and b are the numbers of the columns to be plotted
{
plot(as.matrix(datax[,a]),as.matrix(datax[,b]),type="n",xlab=dimnames(datax)[[2]][a],ylab= dimnames(datax)[[2]][b])
text(as.matrix(datax[,a]),as.matrix(datax[,b]),dimnames(datax)[[1]],cex=0.5)
title(paste("xy plot of",dimnames(datax)[[2]][a],"and",dimnames(datax)[[2]][b]))
abline(0,1)
}
#On écrit cette fonction qui permet de faire facilement les graphiques des arrays deux à deux et indiquent les noms des spots, ce qui permet éventuellement de détecter des valeurs "bizarres".

plot.chips(gene.norm,1,2)

#Il y a deux puces qui semblent bizarres. Essayez de les identifier à l’aide des fonctions ci-dessus.
#Aurait-on pu utiliser le boxplot pour identifier ces deux puces bizarres ?
#Quelle(s) autre(s) méthode(s) pouvons nous utiliser pour identifier ces puces bizarres ?


#------------------------------------
#Elimination des puces "bizarres"
#------------------------------------

datagenes<-gene.norm[,-c(10,20)]
genotype<-genotype[-c(10,20)]
treatment<-treatment[-c(10,20)]
groups<-groups[-c(10,20)]

dataratios<-ratios.norm[,-c(10,20)]

#------------------------------------
#Etude des spikes "ratios"
#------------------------------------
grp<-vector()
grp[treatment=="CMC"]<-"ctrol"
grp[treatment=="FENO"|treatment=="LGD"]<-"test"
grp<-as.factor(grp)

grpratios<-apply(logratiospike[,-c(10,20)],1,function(x){tapply(x,grp,mean)})
grpratios.norm<-apply(dataratios,1,function(x){tapply(x,grp,mean)})

grpratios2<-vector()
for (i in 1:8)
{
grpratios2[i]<-if ((grpratios[2,i]-grpratios[1,i])>=0)
(2^(grpratios[2,i]-grpratios[1,i]))
else (-1/2^(grpratios[2,i]-grpratios[1,i]))
}

grpratios2.norm<-vector()
for (i in 1:8)
{
grpratios2.norm[i]<-if ((grpratios.norm[2,i]-grpratios.norm[1,i])>=0)
(2^(grpratios.norm[2,i]-grpratios.norm[1,i]))
else (-1/2^(grpratios.norm[2,i]-grpratios.norm[1,i]))
}

grpratios2
grpratios2.norm
#les ratios observés
#Les ratios théoriques (en valeur absolue) valent 3 pour Ratio1 à Ratio4 et 10 pour Ratio5 à Ratio8

sum(abs(abs(grpratios2)-c(rep(3,4),rep(10,4))))
sum(abs(abs(grpratios2.norm)-c(rep(3,4),rep(10,4))))
#Une manière de regarder l'écart entre valeurs théoriques et valeurs observées
#Manifestement la normalisation a peu changé les données


#-----------------------------------------------------------#
#Calcul des ratios et p-values
#-----------------------------------------------------------#

grpmean<-apply(datagenes,1,function(x){tapply(x,groups,mean)})
#Pour chaque gène, calcul des logratios moyens par groupe

dim(grpmean)
grpmean[,1:10]
#vérifications

#-------------------------------------
#ANOVA
#-------------------------------------

geneaov<-list()
geneaov<-apply(datagenes,1,function(x){aov(x~genotype*treatment)})
#Pour chaque gène, réalisation d'une analyse de variance avec les facteurs genotype, traitement et l'interaction genotype:traitement

class(geneaov)
length(geneaov)
#vérifications

summary(geneaov[[1]])
#Table d'analyse de variance
plot(geneaov[[1]])
#graphiques de diagnostique

resid.geneaov<-sapply(geneaov,function(x){summary(x)[[1]][4,c(1,3)]})
#Pour chaque gène, récupération des degrés de liberté de la résiduelle et de la variance résiduelle

plot(apply(datagenes,1,mean),resid.geneaov[2,],type="n",xlab="niveau moyen d'expression",ylab="variance résiduelle")
text(apply(datagenes,1,mean),resid.geneaov[2,],dimnames(grpmean)[[2]],cex=0.8)
#graphique de la variance résiduelle en fonction du niveau moyen d'expression


datagenes2<-t(datagenes)
#On transpose datagenes pour avoir les gènes en colonne et être cohérent par rapport à grpmean ou encore à resid.geneaov


#-------------------------------------------------------------------------------#
#FONCTION t.stat							#
#Cette fonction calcule un t de student et la p-value associée	#
#x : Premier jeu de données						#
#y : Deuxième jeu de données					#
#vari : variance résiduelle de l'anova					#
#ddl : degrés de liberté associés à cette variance			#
#-------------------------------------------------------------------------------#
t.stat<-function(x,y,vari,ddl)
{
	n1<-length(x)
	n2<-length(y)
	t<-(mean(x)-mean(y))/sqrt((vari*(1/n1+1/n2)))
	list(t=t,p=2*(1-pt(abs(t),ddl)),num=(mean(x)-mean(y)),denum=sqrt(vari*(1/n1+1/n2)))
}
#On écrit une fonction qui va nous permettre de faire un test de Student qui utilise les résultats de l'analyse de variance (variance résiduelle et degrés de liberté associés)

pval.ttest<-matrix(nrow=5,ncol=119)
for (i in 1:119)
{
pval.ttest[1,i]<-t.stat(
datagenes2[groups=="wt.FENO",i],
datagenes2[groups=="wt.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$p
pval.ttest[2,i]<-t.stat(
datagenes2[groups=="wt.LGD",i],
datagenes2[groups=="wt.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$p
pval.ttest[3,i]<-t.stat(
datagenes2[groups=="PPAR.FENO",i],
datagenes2[groups=="PPAR.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$p
pval.ttest[4,i]<-t.stat(
datagenes2[groups=="PPAR.LGD",i],
datagenes2[groups=="PPAR.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$p
pval.ttest[5,i]<-t.stat(
datagenes2[groups=="PPAR.CMC",i],
datagenes2[groups=="wt.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$p
}
row.names(pval.ttest)<-c("wt.FENOvsCMC","wt.LGDvsCMC","PPAR.FENOvsCMC","PPAR.LGDvsCMC","CMC.PPARvswt")
dimnames(pval.ttest)[[2]]<-dimnames(datagenes)[[1]]
#réalisation des tests de Student et récupération des p-values


logratios.ttest<-matrix(nrow=5,ncol=119)
for (i in 1:119)
{
logratios.ttest[1,i]<-t.stat(
datagenes2[groups=="wt.FENO",i],
datagenes2[groups=="wt.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$num
logratios.ttest[2,i]<-t.stat(
datagenes2[groups=="wt.LGD",i],
datagenes2[groups=="wt.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$num
logratios.ttest[3,i]<-t.stat(
datagenes2[groups=="PPAR.FENO",i],
datagenes2[groups=="PPAR.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$num
logratios.ttest[4,i]<-t.stat(
datagenes2[groups=="PPAR.LGD",i],
datagenes2[groups=="PPAR.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$num
logratios.ttest[5,i]<-t.stat(
datagenes2[groups=="PPAR.CMC",i],
datagenes2[groups=="wt.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$num
}
row.names(logratios.ttest)<-c("wt.FENOvsCMC","wt.LGDvsCMC","PPAR.FENOvsCMC","PPAR.LGDvsCMC","CMC.PPARvswt")
dimnames(logratios.ttest)[[2]]<-dimnames(datagenes)[[1]]
#réalisation des tests de Student et récupération des logratios (numérateur de la statistique de Student)


T.ttest<-matrix(nrow=5,ncol=119)
for (i in 1:119)
{
T.ttest[1,i]<-t.stat(
datagenes2[groups=="wt.FENO",i],
datagenes2[groups=="wt.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$t
T.ttest[2,i]<-t.stat(
datagenes2[groups=="wt.LGD",i],
datagenes2[groups=="wt.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$t
T.ttest[3,i]<-t.stat(
datagenes2[groups=="PPAR.FENO",i],
datagenes2[groups=="PPAR.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$t
T.ttest[4,i]<-t.stat(
datagenes2[groups=="PPAR.LGD",i],
datagenes2[groups=="PPAR.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$t
T.ttest[5,i]<-t.stat(
datagenes2[groups=="PPAR.CMC",i],
datagenes2[groups=="wt.CMC",i],
resid.geneaov[2,i][[1]],
resid.geneaov[1,i][[1]])$t
}
row.names(T.ttest)<-c("wt.FENOvsCMC","wt.LGDvsCMC","PPAR.FENOvsCMC","PPAR.LGDvsCMC","CMC.PPARvswt")
dimnames(T.ttest)[[2]]<-dimnames(datagenes)[[1]]
#réalisation des tests de Student et récupération des valeurs de T



#--------------------------------------------------
# A ce stade, on dispose des logratios et des p-values brutes du test de Student (réalisé après une anova)
#On va commencer par ajuster les p-values à l'aide de différentes méthodes
#--------------------------------------------------

rawp<-as.vector(pval.ttest)
#On met les p-values sous la forme d'un vecteur

pval.ttest[,1:10]
rawp[1:50]
#OK, les 5 premières valeurs correspondent au premier gène, les 5 suivantes au deuxième gène, etc...

library(multtest)

procs <- c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY")
res<-mt.rawp2adjp(rawp,procs)
#Chois des procédures d'ajustement et réalisation des ajustements

adjp<-res$adjp[order(res$index), ]
#On réordonne les p-values pour les retrouver dans leur ordre initial

mt.reject(adjp, seq(0, 0.1, 0.01))$r
#Nombre de gènes déclarés significatif en fonction du seuil de significativité sélectionné (risque de Type I ou FDR en fonction des méthodes)

which<-mt.reject(adjp,0.05)$which[,7]
#retourne un vecteur qui indique si oui ou non, le gène passe le seuil de 5% pour la procédure BH

rep(dimnames(datagenes)[[1]],5)[order(rep(dimnames(datagenes)[[1]],5))][which]
#Nom des gènes qui présente une p-value ajustée dépassant le seuil de 5%
#Remarques :
#		1)comme on fait 5 comparaisons différentes par gène, il est normal que certains gènes apparaissent plusieurs fois.
#		2) finalement, on a bien fait d'ordonner dès les gènes par ordre alphabétique dès le départ sinon la ligne de code ci-dessus ne fonctionne pas (certes on peut en écrire une autre mais elle est plus tordue...voir ci-dessous)

unlist(lapply(dimnames(datagenes)[[1]],rep,5))[which]
#Donne la même chose que la ligne de code précédente et fonctionne aussi si les noms des gènes n'ont pas été ordonnés par ordre alphabétique dès le départ

unique(unlist(lapply(dimnames(datagenes)[[1]],rep,5))[which])
#Nom des 29 gènes qui présentent au moins l'une de leur p-value ajustée inférieure ou égale à 5%

#------------------------------------
#Quelques comparaisons graphique des méthodes d'ajustement
#------------------------------------

ltypes=c(1,1,2,2,3,3,4,4)
cols=c(1,2,3,3,4,4,5,6)
mt.plot(adjp,as.vector(T.ttest), plottype = "pvsr", proc = c("rawp",procs), leg = c(400, 0.4), lty = ltypes, col = cols, lwd = 2)
abline(h=0.05,lty=2)
#Comment commenteriez-vous ce graphique?

mt.plot(adjp,as.vector(T.ttest), plottype = "pvst", logscale = TRUE, proc = c("rawp",procs), leg = c(-3,10), pch = ltypes, col = cols)
#Que pensez-vous de ce graphique et de ce qu'il apporte par rapport au précédent?


#--------------------------------------------------
# On va fixer notre choix sur l'ajustement par BH avec FDR=5%
# On va maintenant créer une table bilan, prête pour la publication de nos résultats
#--------------------------------------------------

adjp.ttest<-matrix(adjp[,7],byrow=F,nrow=5,ncol=119)
dimnames(adjp.ttest)<-dimnames(logratios.ttest)
#On recréé une matrice à partir de nos p-values ajustées


bilan<-matrix(nrow=5,ncol=119)
for (i in 1:5)
{
	for (j in 1:119)
	{
	bilan[i,j]<-if(adjp.ttest[i,j]<=0.05) 
(if (logratios.ttest[i,j]>=0) 
(round(2^logratios.ttest[i,j],1)) 
else (round(-1/2^logratios.ttest[i,j],1)))
			else ("NS")
	}
}
dimnames(bilan)<-dimnames(logratios.ttest)

bilan2<-bilan[,!(bilan[1,]=="NS" & bilan[2,]=="NS" & bilan[3,]=="NS" & bilan[4,]=="NS" & bilan[5,]=="NS")]

dimnames(bilan2)[[2]]
#noms des 29 gènes

unique(unlist(lapply(dimnames(datagenes)[[1]],rep,5))[which])
#On vérifie que c'est bien ce que l'on avait observé précédemment

write.table(bilan2,"bilan2.txt")
#On exporte la table de résultats pour la mettre en forme sous Excel ou autre tableur.


#-----------------------------------------------------------#
#Analyses multidimensionnelles
#-----------------------------------------------------------#

#-------------------------------------
#ANALYSE EN COMPOSANTES PRINCIPALES
#-------------------------------------

library(FactoMineR)

fullACP<-PCA(t(datagenes),graph=F)
selACP<-PCA(t(datagenes[dimnames(bilan2)[[2]],]),graph=F)
#réalisation des ACP centrées-réduites soit sur le jeu de données complet, soit sur la sélection de variables obtenue

barplot(fullACP$eig$iner,xlab="Composante",ylab="variance expliquée")
barplot(selACP$eig$iner,xlab="Composante",ylab="variance expliquée")
#eboulis de la variance expliquée

plot(fullACP,choix="ind",col.ind=as.integer(groups))
#graphique des individus
plot(selACP,choix="ind",col.ind=as.integer(groups))
#même chose pour la sélection de variables

#-------------------------------------------
# ACP complète
#-------------------------------------------

plot(apply(datagenes,2,mean),fullACP$ind$coord[,1])
#Etude du premier axe de l'ACP complète
#Comment interprétez-vous ce résultat?

plot(fullACP,axes=c(2,3),choix="ind",col.ind=as.integer(groups))
#Comment interprétez-vous ce graphique?

plot(fullACP,axes=c(2,3),choix="var",lim.cos2.var=0.5)
#le graphique des variables correspondant (avec une limite sur le cos2)


#-------------------------------------------
# ACP sur une sélection de variables
#-------------------------------------------

plot(selACP$ind$coord[,1],selACP$ind$coord[,2],type="n",
	xlab=paste("Composante 1", paste("(",round(selACP$eig$iner[1],1),"%)",sep=""),sep=" "),
	ylab=paste("Composante 2", paste("(",round(selACP$eig$iner[2],1),"%)",sep=""),sep=" "))
text(selACP$ind$coord[,1],selACP$ind$coord[,2],as.character(groups),col=as.numeric(groups))
#Les deux premiers axes de l'ACP

plot(selACP$ind$coord[,2],selACP$ind$coord[,3],type="n",
	xlab=paste("Composante 2", paste("(",round(selACP$eig$iner[2],1),"%)",sep=""),sep=" "),
	ylab=paste("Composante 3", paste("(",round(selACP$eig$iner[3],1),"%)",sep=""),sep=" "))
text(selACP$ind$coord[,2],selACP$ind$coord[,3],as.character(groups),col=as.numeric(groups))
#Le plan 2-3


plot(selACP$ind$coord[,2],selACP$ind$coord[,4],type="n",
	xlab=paste("Composante 2", paste("(",round(selACP$eig$iner[2],1),"%)",sep=""),sep=" "),
	ylab=paste("Composante 4", paste("(",round(selACP$eig$iner[4],1),"%)",sep=""),sep=" "))
text(selACP$ind$coord[,2],selACP$ind$coord[,4],as.character(groups),col=as.numeric(groups))
#Le plan 2-4

plot(selACP,axes=c(1,2),choix="var",lim.cos2.var=0.1)
#Les variables dans le premier plan principal

plot(selACP,axes=c(2,3),choix="var",lim.cos2.var=0.1)
#Le plan 2-3


#-------------------------------------
#CLASSIFICATION
#-------------------------------------

library(cluster)

plclust(hclust(as.dist(1-cor(datagenes)),method="ward"),hang=-1,labels=groups)
#classification des individus

plclust(hclust(as.dist(1-cor(datagenes[dimnames(bilan2)[[2]],])),method="ward"),hang=-1,labels=groups)
#même chose mais sur notre sélection de variables

plot(hclust(as.dist(1-cor(t(datagenes))),method="ward"),hang=-1,labels=dimnames(datagenes)[[1]],cex=0.6)
#classification des variables
#même avec une centaine de variables, cet arbre est difficile à interpréter

library(geneplotter)
library(marray)

slf<-function(d) hclust(d,method="ward")
#définition de la fonction qui permet de réaliser les classifications des gènes et des individus
distcor<-function(x){as.dist(1-cor(t(x)))}
#et de la distance utilisée

heatmap(as.matrix(datagenes),
col= maPalette(low="green",high="red",mid="black"),
distfun=distcor,
hclustfun=slf,
labRow=dimnames(datagenes)[[1]],
labCol=as.character(groups))
#dendrogramme complet


heatmap(as.matrix(datagenes[dimnames(bilan2)[[2]],]),
col= maPalette(low="green",high="red",mid="black"),
distfun=distcor,
hclustfun=slf,
labRow=dimnames(datagenes[dimnames(bilan2)[[2]],])[[1]],
labCol=as.character(groups))
#dendrogramme des 29 variables sélectionnées

