#-----------------------------------------------------------------------------------------------------------#
#			ETUDE DE CAS : MANIP PHARMACO
#				DESCRIPTION DE L'ETUDE :
# PPARalpha et RXR appartiennent � la superfamille des r�cepteurs nucl�aires. Ils agissent comme facteurs de transcription sous l'effet de divers ligand. Suite � l'activation de PPARalpha par son ligand, celui-ci s'h�t�rodim�rise avec RXR. Ce dim�re se fixe sur des �l�ments de r�ponse situ�s dans les promoteurs des g�nes cibles et r�gule la transcription de ces g�nes.
# Le r�cepteur RXR est capable de former des h�t�rodim�res avec plusieurs autres r�cepteurs nucl�aires (une quinzaine environ). La question que nous nous posons est de savoir si l'activation de RXR par un ligand sp�cifique conduit au recrutement d'un ou de plusieurs partenaires de dim�risation et donc � la modulation de l'expression de divers panels de g�nes cibles.
# Des souris de type sauvage ou de type PPARalpha-d�ficientes (4 souris/lot) ont �t� trait�es pendant 15 jours soit avec un activateur sp�cifique de PPARalpha (FENO) soit avec un activateur sp�cifique de RXR (LGD) soit avec le v�hicule seul (CMC). A l'issue des traitements, le foie des animaux a �t� pr�lev� et les ARNs h�patiques extraits puis radiomarqu�s au 33P par transcription inverse et hybrid� � des puces � ADN (ADNc sur nylon) d�di�es aux voies de signalisation impliquant RXR.
#-----------------------------------------------------------------------------------------------------------#


#-----------------------------------------------------------#
#Mise en place de la session
#-----------------------------------------------------------#

save.image("CHEMIN D'ACCES/ma session R.RData")
#Sauvegarde de la session


#-----------------------------------------------------------#
#Importation et mise en forme des donn�es
#-----------------------------------------------------------#

#commen�ons par regarder ce que contient le fichier etude pharmaco.xls
#Une matrice de 383 lignes x 26 colonnes

pharma<-read.table("etude pharmaco.txt",header=T,nrows=382,sep="\t")
#On importe les donn�es. Pour une raison que j'ignore, si on ne met pas l'argument nrows=382, R rajoute une ligne de NA a la fin de l'objet pharma

dim(pharma)
edit(pharma)
dimnames(pharma)[[2]]
#v�rifications

# La colonne "Type" sp�cifie le type de spot correspondant � chaque ligne.
# Sur ce microarray "maison", on a cr�� diff�rents types de spots :
# gene : le spot correspond � un fragment d'ADNc reconnaissant un transcrit pr�cis. C'est ce qui va nous int�resser le plus en pratique
# spike : ce spot contient un fragment d'ADN qui correspond � un g�ne synth�tique (zone interg�nique de levure) � partir duquel on a produit un ARNm qui a �t� ajout� en quantit� connue et identique dans les diff�rents �chantillons d'ARN de souris. On va utiliser la quantification de ces g�nes pour faire notre normalisation
# ratio : ce spot correspond � un spike qui a �t� incorpor� � un ratio connu entre les diff�rents groupes d'�chantillons. On pourra utiliser ces quantifications pour voir si la normalisation nous permet de nous rapprocher des ratios th�oriques.
# bckg : backgroud ou bruit de fond. Ces spots ne contiennent que du tampon de d�p�t (50% DMSO)
# gamme.BACT : l'ADNc correspondant au g�ne b-actine a �t� d�pos� sur la puce � diff�rentes concentrations afin de v�rifier que l'on �tait bien au plateau de signal.
# gamme.vide : une gamme de plasmide vide (ne contenant aucun insert) pour se faire une id�e de la fixation non sp�cifique sur ce vecteur.
# Nous n'utiliserons pas les donn�es correspondant � ces deux gammes dans nos manips. Ces gammes �taient pr�sentes sur cette g�n�ration de puces pour permettre de r�aliser des optimisations du protocole exp�rimental. On consid�rera ici que les optimisations du protocole ont d�j� �t� faites (c'est le cas, il faut me croire sur parole...)

genotype<-as.factor(c(rep("PPAR",12),rep("wt",12)))
treatment<-as.factor(rep(c(rep("CMC",4),rep("LGD",4),rep("FENO",4)),2))
groups<-as.factor(paste(as.character(genotype),as.character(treatment),sep="."))
#cr�ation des facteurs genotype, traitement et groupe

pharma<-pharma[sort.list(pharma[,1]),]
#On trie les donn�es par ordre alphab�tique des noms de spot.
#Ca ne sert pas � grand chose mais �a ne nous co�te pas grand chose non plus...

n.arrays<-dim(pharma)[2]-2
#On r�cup�re le nombre de puces dans notre manip

spotmean<-apply(pharma[,2:25],2,function(x){tapply(x,pharma[,1],mean)})
#Pour chaque puce, on fait la moyenne des intensit�s des spots qui sont r�pliqu�s sur la puce

Type<-as.vector(tapply(as.vector(pharma[,26]),pharma[,1],unique))
#Pour chacune de ces moyennes, on r�cup�re le type de spot

dim(spotmean)
length(Type)
#v�rifications

pharma2<-cbind.data.frame(spotmean,Type)
#On cr�� une nouvelle matrice avec une valeur unique pour chaque s�rie de spots r�pliqu�s

dim(pharma2)
dimnames(pharma2)
#v�rifications

pharma2.gene<-pharma2[pharma2$Type=="gene",-dim(pharma2)[2]]
#On r�cup�re les donn�es correspondant aux g�nes

dim(pharma2.gene)
#v�rification : on �tudie l'expression de 119 g�nes

pharma2.spike<-pharma2[pharma2$Type=="spike",-dim(pharma2)[2]]
pharma2.ratios<-pharma2[pharma2$Type=="ratio",-dim(pharma2)[2]]
#On r�cup�re les donn�es correspondant aux spikes et aux ratios

n.genes<-dim(pharma2.gene)[1]
#On r�cup�re le nombre de g�nes �tudi�s

loggene<-log(pharma2.gene,base=2)
logspike<-log(pharma2.spike,base=2)
logratiospike<-log(pharma2.ratios,base=2)
#On transforme les donn�es en log base 2
#j'�conomise le nom logratio dont on pourrait bien avoir besoin par la suite...


sel.spike<-logspike[c("Cal3","Cal4","Cal5","Cal6","Cal7","Util1","Util2","Util3"),]
#je s�lectionne certains spikes uniquement
#j'ai choisi ces spikes l� car ils ne sont pas dans le bruit de fond (ce qui est le cas de Cal8 et Cal9) et qu'ils ne sont pas satur�s (ce qui est le cas de Cal1 et Cal2). J'ai �galement �cart� le spike rbcl que nous avions produit au laboratoire car on a pu montrer ult�rieurement que le fragment d'ADNc d�pos� sur la puce croisait avec certains g�nes de souris.


boxplot(pharma2.gene,las=2)
boxplot(loggene,las=2)
#premi�re observation des donn�es
#que peut-on en dire?

boxplot(as.data.frame(t(loggene)),las=2,cex.axis=0.5)
#Boxplot des g�nes


#-----------------------------------------------------------#
#Normalisation des donn�es
#-----------------------------------------------------------#

coeff.norm<-apply(sel.spike,2,mean)
#calcul du coefficient de normalisation
#Remarque :
#Faire la moyenne arithm�tique des log des intensit�s des spikes revient � prendre le log de la moyenne g�om�trique des intensit�s des spikes

gene.norm<-loggene-coeff.norm
ratios.norm<-logratiospike-coeff.norm
#normalisation des g�nes et des ratios


#-----------------------------------------------------------#
# "Contr�les qualit�"
#-----------------------------------------------------------#

#------------------------------------
#boxplots
#------------------------------------

boxplot(gene.norm,las=2)
#boxplot de chaque puce

boxplot(as.data.frame(t(gene.norm)),las=2,cex.axis=0.5)
#boxplot de chaque g�ne

#------------------------------------
#corr�lation entre les arrays
#------------------------------------

cor.arrays<-cor(gene.norm)
#corr�lations des arrays

group.cor<-list()
for (i in 1:length(levels(groups)))
{
	group.cor[[i]]<-cor.arrays[groups==levels(groups)[i],groups==levels(groups)[i]]
}
#Pour chaque groupe d��chantillon, on r�cup�re les correlations entre les puces au sein du groupe


mean.group.cor<-sapply(group.cor,function(x){mean(x[lower.tri(x)])})
#Pour chaque groupe, on calcule la corr�lation moyenne au sein du groupe

mean(mean.group.cor)
#moyenne du coefficient de corr�lation moyen au sein des groupes

#------------------------------------
#comparaison des arrays deux � deux
#------------------------------------

pairs(gene.norm[,genotype=="PPAR"])
pairs(gene.norm[,genotype=="wt"])
#matrice de comparaison des puces deux � deux

pairs(gene.norm[,groups=="PPAR.CMC"])
#C�est plus lisible par groupe que par g�notype

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
#On �crit cette fonction qui permet de faire facilement les graphiques des arrays deux � deux et indiquent les noms des spots, ce qui permet �ventuellement de d�tecter des valeurs "bizarres".

plot.chips(gene.norm,1,2)

#Il y a deux puces qui semblent bizarres. Essayez de les identifier � l�aide des fonctions ci-dessus.
#Aurait-on pu utiliser le boxplot pour identifier ces deux puces bizarres ?
#Quelle(s) autre(s) m�thode(s) pouvons nous utiliser pour identifier ces puces bizarres ?


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
#les ratios observ�s
#Les ratios th�oriques (en valeur absolue) valent 3 pour Ratio1 � Ratio4 et 10 pour Ratio5 � Ratio8

sum(abs(abs(grpratios2)-c(rep(3,4),rep(10,4))))
sum(abs(abs(grpratios2.norm)-c(rep(3,4),rep(10,4))))
#Une mani�re de regarder l'�cart entre valeurs th�oriques et valeurs observ�es
#Manifestement la normalisation a peu chang� les donn�es


#-----------------------------------------------------------#
#Calcul des ratios et p-values
#-----------------------------------------------------------#

grpmean<-apply(datagenes,1,function(x){tapply(x,groups,mean)})
#Pour chaque g�ne, calcul des logratios moyens par groupe

dim(grpmean)
grpmean[,1:10]
#v�rifications

#-------------------------------------
#ANOVA
#-------------------------------------

geneaov<-list()
geneaov<-apply(datagenes,1,function(x){aov(x~genotype*treatment)})
#Pour chaque g�ne, r�alisation d'une analyse de variance avec les facteurs genotype, traitement et l'interaction genotype:traitement

class(geneaov)
length(geneaov)
#v�rifications

summary(geneaov[[1]])
#Table d'analyse de variance
plot(geneaov[[1]])
#graphiques de diagnostique

resid.geneaov<-sapply(geneaov,function(x){summary(x)[[1]][4,c(1,3)]})
#Pour chaque g�ne, r�cup�ration des degr�s de libert� de la r�siduelle et de la variance r�siduelle

plot(apply(datagenes,1,mean),resid.geneaov[2,],type="n",xlab="niveau moyen d'expression",ylab="variance r�siduelle")
text(apply(datagenes,1,mean),resid.geneaov[2,],dimnames(grpmean)[[2]],cex=0.8)
#graphique de la variance r�siduelle en fonction du niveau moyen d'expression


datagenes2<-t(datagenes)
#On transpose datagenes pour avoir les g�nes en colonne et �tre coh�rent par rapport � grpmean ou encore � resid.geneaov


#-------------------------------------------------------------------------------#
#FONCTION t.stat							#
#Cette fonction calcule un t de student et la p-value associ�e	#
#x : Premier jeu de donn�es						#
#y : Deuxi�me jeu de donn�es					#
#vari : variance r�siduelle de l'anova					#
#ddl : degr�s de libert� associ�s � cette variance			#
#-------------------------------------------------------------------------------#
t.stat<-function(x,y,vari,ddl)
{
	n1<-length(x)
	n2<-length(y)
	t<-(mean(x)-mean(y))/sqrt((vari*(1/n1+1/n2)))
	list(t=t,p=2*(1-pt(abs(t),ddl)),num=(mean(x)-mean(y)),denum=sqrt(vari*(1/n1+1/n2)))
}
#On �crit une fonction qui va nous permettre de faire un test de Student qui utilise les r�sultats de l'analyse de variance (variance r�siduelle et degr�s de libert� associ�s)

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
#r�alisation des tests de Student et r�cup�ration des p-values


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
#r�alisation des tests de Student et r�cup�ration des logratios (num�rateur de la statistique de Student)


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
#r�alisation des tests de Student et r�cup�ration des valeurs de T



#--------------------------------------------------
# A ce stade, on dispose des logratios et des p-values brutes du test de Student (r�alis� apr�s une anova)
#On va commencer par ajuster les p-values � l'aide de diff�rentes m�thodes
#--------------------------------------------------

rawp<-as.vector(pval.ttest)
#On met les p-values sous la forme d'un vecteur

pval.ttest[,1:10]
rawp[1:50]
#OK, les 5 premi�res valeurs correspondent au premier g�ne, les 5 suivantes au deuxi�me g�ne, etc...

library(multtest)

procs <- c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY")
res<-mt.rawp2adjp(rawp,procs)
#Chois des proc�dures d'ajustement et r�alisation des ajustements

adjp<-res$adjp[order(res$index), ]
#On r�ordonne les p-values pour les retrouver dans leur ordre initial

mt.reject(adjp, seq(0, 0.1, 0.01))$r
#Nombre de g�nes d�clar�s significatif en fonction du seuil de significativit� s�lectionn� (risque de Type I ou FDR en fonction des m�thodes)

which<-mt.reject(adjp,0.05)$which[,7]
#retourne un vecteur qui indique si oui ou non, le g�ne passe le seuil de 5% pour la proc�dure BH

rep(dimnames(datagenes)[[1]],5)[order(rep(dimnames(datagenes)[[1]],5))][which]
#Nom des g�nes qui pr�sente une p-value ajust�e d�passant le seuil de 5%
#Remarques :
#		1)comme on fait 5 comparaisons diff�rentes par g�ne, il est normal que certains g�nes apparaissent plusieurs fois.
#		2) finalement, on a bien fait d'ordonner d�s les g�nes par ordre alphab�tique d�s le d�part sinon la ligne de code ci-dessus ne fonctionne pas (certes on peut en �crire une autre mais elle est plus tordue...voir ci-dessous)

unlist(lapply(dimnames(datagenes)[[1]],rep,5))[which]
#Donne la m�me chose que la ligne de code pr�c�dente et fonctionne aussi si les noms des g�nes n'ont pas �t� ordonn�s par ordre alphab�tique d�s le d�part

unique(unlist(lapply(dimnames(datagenes)[[1]],rep,5))[which])
#Nom des 29 g�nes qui pr�sentent au moins l'une de leur p-value ajust�e inf�rieure ou �gale � 5%

#------------------------------------
#Quelques comparaisons graphique des m�thodes d'ajustement
#------------------------------------

ltypes=c(1,1,2,2,3,3,4,4)
cols=c(1,2,3,3,4,4,5,6)
mt.plot(adjp,as.vector(T.ttest), plottype = "pvsr", proc = c("rawp",procs), leg = c(400, 0.4), lty = ltypes, col = cols, lwd = 2)
abline(h=0.05,lty=2)
#Comment commenteriez-vous ce graphique?

mt.plot(adjp,as.vector(T.ttest), plottype = "pvst", logscale = TRUE, proc = c("rawp",procs), leg = c(-3,10), pch = ltypes, col = cols)
#Que pensez-vous de ce graphique et de ce qu'il apporte par rapport au pr�c�dent?


#--------------------------------------------------
# On va fixer notre choix sur l'ajustement par BH avec FDR=5%
# On va maintenant cr�er une table bilan, pr�te pour la publication de nos r�sultats
#--------------------------------------------------

adjp.ttest<-matrix(adjp[,7],byrow=F,nrow=5,ncol=119)
dimnames(adjp.ttest)<-dimnames(logratios.ttest)
#On recr�� une matrice � partir de nos p-values ajust�es


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
#noms des 29 g�nes

unique(unlist(lapply(dimnames(datagenes)[[1]],rep,5))[which])
#On v�rifie que c'est bien ce que l'on avait observ� pr�c�demment

write.table(bilan2,"bilan2.txt")
#On exporte la table de r�sultats pour la mettre en forme sous Excel ou autre tableur.


#-----------------------------------------------------------#
#Analyses multidimensionnelles
#-----------------------------------------------------------#

#-------------------------------------
#ANALYSE EN COMPOSANTES PRINCIPALES
#-------------------------------------

library(FactoMineR)

fullACP<-PCA(t(datagenes),graph=F)
selACP<-PCA(t(datagenes[dimnames(bilan2)[[2]],]),graph=F)
#r�alisation des ACP centr�es-r�duites soit sur le jeu de donn�es complet, soit sur la s�lection de variables obtenue

barplot(fullACP$eig$iner,xlab="Composante",ylab="variance expliqu�e")
barplot(selACP$eig$iner,xlab="Composante",ylab="variance expliqu�e")
#eboulis de la variance expliqu�e

plot(fullACP,choix="ind",col.ind=as.integer(groups))
#graphique des individus
plot(selACP,choix="ind",col.ind=as.integer(groups))
#m�me chose pour la s�lection de variables

#-------------------------------------------
# ACP compl�te
#-------------------------------------------

plot(apply(datagenes,2,mean),fullACP$ind$coord[,1])
#Etude du premier axe de l'ACP compl�te
#Comment interpr�tez-vous ce r�sultat?

plot(fullACP,axes=c(2,3),choix="ind",col.ind=as.integer(groups))
#Comment interpr�tez-vous ce graphique?

plot(fullACP,axes=c(2,3),choix="var",lim.cos2.var=0.5)
#le graphique des variables correspondant (avec une limite sur le cos2)


#-------------------------------------------
# ACP sur une s�lection de variables
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
#m�me chose mais sur notre s�lection de variables

plot(hclust(as.dist(1-cor(t(datagenes))),method="ward"),hang=-1,labels=dimnames(datagenes)[[1]],cex=0.6)
#classification des variables
#m�me avec une centaine de variables, cet arbre est difficile � interpr�ter

library(geneplotter)
library(marray)

slf<-function(d) hclust(d,method="ward")
#d�finition de la fonction qui permet de r�aliser les classifications des g�nes et des individus
distcor<-function(x){as.dist(1-cor(t(x)))}
#et de la distance utilis�e

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
#dendrogramme des 29 variables s�lectionn�es

