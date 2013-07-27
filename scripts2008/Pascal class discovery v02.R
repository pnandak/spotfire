#------------------------------------------------------------------------------------------------------------------------#
#				MISE EN PLACE DE LA SESSION
#------------------------------------------------------------------------------------------------------------------------#

save.image("CHEMIN D'ACCES/ma session R.RData")


#------------------------------------------------------------------------------------------------------------------------#
#		RECUPERATION DES DONNEES - SELECTION DE GENES
#------------------------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------#
#Importation des données
#--------------------------------------------------------------------------------------#

poulet<-read.table("dataMAloessGENE.txt")
#On importe les données normalisées dans l'objet "poulet"

#-------------------------------#
#Une série de vérifications
#-------------------------------#

dim(poulet)
#[1] 20652    55

dimnames(poulet)[[2]]
dimnames(poulet)[[1]][1:20]
poulet[1:10,1:10]

class(poulet$Status)
#[1] "factor"

levels(poulet$Status)
#[1] "gene"


#-------------------------------#
#Création d'un objet ne contenant que les données
#-------------------------------#

poulet2<-poulet[,7:55]
dimnames(poulet2)[[1]]<-dimnames(poulet)[[1]]

#-------------------------------#
#Vérifications
#-------------------------------#
dim(poulet2)
dimnames(poulet2)[[2]]


#-------------------------------#
#Création d'un facteur groupe
#-------------------------------#

grp<-as.factor(c(rep("N",9),rep("J16",7),rep("J16R5",8),rep("J16R16",9),rep("J48",7),rep("J48R24",9)))

#-------------------------------#
#Un premier aperçu des données
#-------------------------------#

boxplot(as.list(poulet2),pars=list(las=2))
#boxplot de chaque puce

boxplot(t(poulet2)~grp)
#boxplots par groupe d'individus


#-------------------------------#
#Sélection de gènes présentant un effet groupe
#-------------------------------#

aov.grp<-list()
aov.grp<-apply(poulet2,1,function(x){aov(x~grp)})
#calculs un peu long...

pval.aov<-sapply(aov.grp,function(x){summary(x)[[1]][1,5]})
#On récupère les p-values de l'anova

length(pval.aov[pval.aov<=0.05])
#[1] 14921
length(pval.aov[pval.aov<=0.1])
#[1] 16328
length(pval.aov[pval.aov<=0.01])
#[1] 11849
length(pval.aov[pval.aov<=0.001])
#[1] 8307

names(pval.aov)<-dimnames(poulet)[[1]]

library(multtest)
pval.aov.BH<-mt.rawp2adjp(pval.aov,proc="BH")
adjp.aov<-pval.aov.BH$adjp[order(pval.aov.BH$index),2]
names(adjp.aov)<-names(pval.aov)
#ajustement des p-values par la méthode de Benjamini et Hochberg

length(adjp.aov[adjp.aov<=0.05])
#[1] 14193
length(adjp.aov[adjp.aov<=0.01])
#[1] 10793
length(adjp.aov[adjp.aov<=0.001])
#[1] 7048
length(adjp.aov[adjp.aov<=0.0001])
#[1] 4623
length(adjp.aov[adjp.aov<=0.00001])
#[1] 2943

plot(c(0,0.01),c(0,11000),type="n")
for (i in seq(0,0.01,by=0.0001))
{points(i, length(adjp.aov[adjp.aov<=i]))}
#Nombre de gènes retenus en fonction du seuil de FDR fixé

X11()
plot(c(0,0.01),c(0,110),type="n")
for (i in seq(0,0.01,by=0.0001))
{points(i, length(adjp.aov[adjp.aov<=i])*i)}
#graphique du nombre de faux positifs en fonction du seuil

X11()
plot(c(0,11000),c(0,110),type="n")
for (i in seq(0,0.01,by=0.0001))
{points(length(adjp.aov[adjp.aov<=i]), length(adjp.aov[adjp.aov<=i])*i)}
#graphique du nombre de faux positifs en fonction du nombre de gènes sélectionnés

#On va réaliser une sélection relativement drastique car de très nombreux gènes présentent un effet groupe significatif

selgenes1<-names(adjp.aov)[adjp.aov<=0.00001]
length(selgenes1)
#[1] 2943
#OK, on a sélectionné 2943 gènes

poulet3<-poulet2[selgenes1,]

#------------------------------------------------------------------------------------------------------------------------#
#				IMPORTATION DES DONNEES
#------------------------------------------------------------------------------------------------------------------------#

poulet3<-read.table(“poulet3.txt”,header=T,sep=”\t”)
annot.poulet3<- read.table(“annotpoulet3.txt”,header=T,sep=”\t”)
#importation du tableau de données et de la table des annotations


#-------------------------------#
#Création d'un facteur groupe
#-------------------------------#

grp<-as.factor(c(rep("N",9),rep("J16",7),rep("J16R5",8),rep("J16R16",9),rep("J48",7),rep("J48R24",9)))


#------------------------------------------------------------------------------------------------------------------------#
#			CLASSIFICATION ASCENDANTE HIERARCHIQUE
#------------------------------------------------------------------------------------------------------------------------#
library(cluster)
#-----------------------------------------------------#
#CAH DES INDIVIDUS
#-----------------------------------------------------#

plclust(hclust(as.dist(1-cor(poulet3)),method="complete"),hang=-1,labels=grp)
X11()
plclust(hclust(as.dist(1-cor(poulet3)),method="ave"),hang=-1,labels=grp)
X11()
plclust(hclust(as.dist(1-cor(poulet3)),method="ward"),hang=-1,labels=grp)
#Observer les différences entre les dendrogrammes issus de méthodes d'agglomération différentes

plclust(hclust(as.dist(1-cor(poulet3)),method="ward"),hang=-1,labels=grp)
X11()
plclust(hclust(as.dist(1-cor(poulet2)),method="ward"),hang=-1,labels=grp)
#Observer l'effet d'une sélection de gènes

plclust(hclust(as.dist(1-cor(poulet3)),method="ward"),hang=-1,labels=grp)
#On sélectionne cette classification
X11()
plot(hclust(as.dist(1-cor(poulet3)),method="ward")$height[49:10],pch=18,col="blue")
#Choix d'un nombre de classes (ici, je dirais 6 ou 8 classes)


plclust(hclust(as.dist(1-cor(poulet3)),method="ward"),hang=-1,labels=grp)
X11()
plclust(hclust(dist(t(poulet3)),method="ward"),hang=-1,labels=grp)
#Observer les différences entre les dendrogrammes obtenus avec deux distances différentes

plclust(hclust(dist(t(poulet3)),method="ward"),hang=-1,labels=dimnames(poulet3)[[2]])
#on retrouve notre puce Np5...

plclust(hclust(as.dist(1-cor(poulet2)),method="ward"),hang=-1,labels=grp)
X11()
plclust(hclust(as.dist(1-cor(poulet2)^2),method="ward"),hang=-1,labels=grp)
#Idem : comparaison des distances basées sur la corrélation ou la corrélation au carré
#La distance basée sur cor2 peut se révéler utile dans les classifications de gènes


#-----------------------------------------------------#
#DOUBLE CLASSIFICATION
#-----------------------------------------------------#

selgenes2<-names(adjp.aov)[adjp.aov<=0.000001]
length(selgenes2)
noms.selgenes2<-poulet[adjp.aov<=0.000001,"ID"]
poulet4<-poulet2[selgenes2,]
#Une sélection de gènes un peu plus drastique pour ne pas faire (trop) planter les ordinateurs...


library(geneplotter)
library(marray)

slf<-function(d) hclust(d,method="ward")
#définition de la fonction qui permet de réaliser les classifications des gènes et des individus

heatmap(as.matrix(poulet3),
col= maPalette(low="green",high="red",mid="black"),
hclustfun=slf,
labRow=annot.poulet3[,”Name”],
labCol=as.character(grp))
#ATTENTION : ne lancer la commande ci-dessus que si votre ordinateur tourne bien!!!
#Si cette fonction ne marche pas et vous revoie un message d'erreur en lien avec un problème de mémoire, essayez de ferner votre session R et de la relancer. Rechargez les packages cluster, geneplotter et marray puis relancez la fonction ci-dessus.
#Vous pouvez aussi essayer de baisser le seuil de sélection utilisé dans poulet4 pour avoir moins de gènes
#Cette classification est basée sur distance=distance euclidienne classique et critère d'agglomération=Ward

#Il serait sans doute plus judicieux de travailler sur une distance basée sur la corrélation (en particulier pour les variables)

distcor<-function(x){as.dist(1-cor(t(x)))}
#La distance basée sur la corrélation
heatmap(as.matrix(poulet3),
col= maPalette(low="green",high="red",mid="black"),
distfun=distcor,
hclustfun=slf,
labRow=annot.poulet3[,”Name”],
labCol=as.character(grp))
#La double classification basée sur distance=1-cor et critère d'agglomération=Ward

#On va travailler avec cette double classification
#Le principal problème auquel nous sommes confrontés est le manque d'interactivité avec ce graphique. On aimerait pouvoir zoomer et se balader dans cette heatmap pour aller voir de quels gènes sont constitués les groupes, etc...
#Les commandes qui vont suivre ont pour but d'obtenir à la fois des listes de gènes et également des graphiques représentant des sous-parties de cette heatmap correspondant à des groupes de gènes "homogènes"

hv<-heatmap(as.matrix(poulet3),
col= maPalette(low="green",high="red",mid="black"),
distfun=distcor,
hclustfun=slf,
labRow=annot.poulet3[,”Name”],
labCol=as.character(grp),
keep.dendro=TRUE)
#On commence par récupérer les dendrogrammes

names(hv)
#[1] "rowInd" "colInd" "Rowv"   "Colv"
#rowInd et colInd sont deux vecteurs contenant les permutations d'index pour les lignes et les colonnes respectivement (tels que retrournés par la fonction order.dendrogram, voir l'aide correspondante)
#Rowv et Colv sont les deux denrogrammes des lignes (gènes) et des colonnes (individus) respectivement. Il s'agit d'objet de classe "dendrogram"

plot(hv$Colv)
plot(hv$Rowv)
#Chaque dendrogramme séparément

dimnames(poulet3)[[2]][hv$colInd[1]]
dimnames(poulet3)[[2]][hv$colInd[2]]
#Les vecteurs rowInd et colInd peuvent être utilisé pour retrouver dans la table de données de départ (poulet3) une donnée identifiée sur le dendrogramme. rowInd (resp colInd) donne les numéros de ligne (resp. des colonnes) des gènes (resp. des individus) dans la table de départ (poulet 3) dans l'ordre d'apparition des feuilles des dendrogrammes (de gauche à droite).

plot(hv$Rowv)
X11()
plclust(hclust(as.dist(1-cor(t(poulet3))),method="ward"),hang=-1,labels=dimnames(poulet3)[[1]])
#Cet arbre est le même que plot(hv$Rowv), à quelques rotations de branches près

hc<-hclust(as.dist(1-cor(t(poulet3))),method="ward")
#On récupère ce dendrogramme

plot(hc$height[1144:1125],pch=18,col="blue")
#Ce graphique qui nous suggère que 4 groupes de gènes pourraient constituer un premier découpage intéressant. Pour obtenir un découpage plus fin (et donc des groupes plus petits en effectif), on va considérer 7 groupes (attention, ce n’est bien sûr pas forcément un choix optimal, 7, 8 et 12 groupes semblent d’après le graphique ci-dessus des choix acceptables)


plclust(hc,hang=-1,labels=dimnames(poulet3)[[1]])
X11()
plot(hc$height[1144:1125],pch=18,col="blue")
abline(h=18)
# En coupant l'arbre à une hauteur de 18, on obtient bien 7 groupes

cuthv7<-cut(hv$Rowv,h=18)

names(cuthv7)
#[1] "upper" "lower"
#upper est une version tronquée de l'arbre de départ
#lower est une liste contenant les 7 sous-dendrogrammes générés par la coupure

plot(cuthv7$lower[[1]])
#le dendrogramme correspondant au premier sous-groupe de gènes auxquels nous allons nous intéresser

labels(cuthv7$lower[[1]])
#les noms des gènes appartenant à ce premier sous-groupe

heatmap(as.matrix(poulet3[labels(cuthv7$lower[[1]]),]),
Rowv=str(cuthv7$lower[[1]]),
Colv=hv$Colv,
col= maPalette(low="green",high="red",mid="black"),
distfun=distcor,
hclustfun=slf,
labCol=as.character(grp))
#La sous partie du dendrogramme initial contenant les gènes du sous-groupe 1
#...on aurait envie éventuellement de descendre plus bas dans le découpage des sous-groupes
#A-t-on "le droit" de le faire directement sur ce morceau de l'arbre initial?


heatmap(as.matrix(poulet3[labels(cuthv7$lower[[2]]),]),
Rowv=str(cuthv7$lower[[2]]),
Colv=hv$Colv,
col= maPalette(low="green",high="red",mid="black"),
distfun=distcor,
hclustfun=slf,
labCol=as.character(grp))
#sous-groupe n°2


heatmap(as.matrix(poulet3[labels(cuthv7$lower[[3]]),]),
Rowv=str(cuthv7$lower[[3]]),
Colv=hv$Colv,
col= maPalette(low="green",high="red",mid="black"),
distfun=distcor,
hclustfun=slf,
labCol=as.character(grp))
#sous-groupe n°3


heatmap(as.matrix(poulet3[labels(cuthv7$lower[[4]]),]),
Rowv=str(cuthv7$lower[[4]]),
Colv=hv$Colv,
col= maPalette(low="green",high="red",mid="black"),
distfun=distcor,
hclustfun=slf,
labCol=as.character(grp))
#sous-groupe n°4


heatmap(as.matrix(poulet3[labels(cuthv7$lower[[5]]),]),
Rowv=str(cuthv7$lower[[5]]),
Colv=hv$Colv,
col= maPalette(low="green",high="red",mid="black"),
distfun=distcor,
hclustfun=slf,
labCol=as.character(grp))
#sous-groupe n°5


heatmap(as.matrix(poulet3[labels(cuthv7$lower[[6]]),]),
Rowv=str(cuthv7$lower[[6]]),
Colv=hv$Colv,
col= maPalette(low="green",high="red",mid="black"),
distfun=distcor,
hclustfun=slf,
labCol=as.character(grp))
#sous-groupe n°6


heatmap(as.matrix(poulet3[labels(cuthv7$lower[[7]]),]),
Rowv=str(cuthv7$lower[[7]]),
Colv=hv$Colv,
col= maPalette(low="green",high="red",mid="black"),
distfun=distcor,
hclustfun=slf,
labCol=as.character(grp))
#sous-groupe n°7

#Il ne reste plus qu'à voir de quels gènes il s'agit...


sapply(cuthv7$lower,attributes)
#Pour voir quels sont les effectifs de chaque sous-groupe de gènes
#On va s'intéresser au groupe n°4 qui présente un effectif raisonnable (61 gènes)

annot.poulet3[labels(cuthv7$lower[[4]]),"Name"]
#Les noms des 61 gènes du groupe 4


#-----------------------------------------------------#
#IMPORTATION DES ANNOTATIONS DES GENES
#-----------------------------------------------------#

annot=read.table("annotation_oligo_29012007.txt",sep="\t",header=T)

dim(annot)
#[1] 16383    10
#vérification OK
#Ce fichier n'a pas le même nombre de ligne que le fichier poulet car certains gènes sont présents en plusieurs exemplaires sur la puce. De plus, certains gènes du fichier poulet n'ont aucune annotation leur correspondant dans le fichier annot

numlignes<-match(poulet$ID,annot$ID)
#numlignes est un vecteur contenant autant d'élément que poulet contient de lignes

length(numlignes)
dim(poulet)[1]
#vérification ok

#pour chaque ligne poulet[i,], numlignes contient la valeur numligne[i]=j où j est le numéro de la ligne annot[j,] telle que poulet[i,"ID"]=annot[j,"ID]
#numlignes contient la valeur NA si l'ID contenue dans poulet n'a pas d'équivalent dans les ID de annot

poulet.annot<-cbind.data.frame(poulet,annot[numlignes,])
#On créé une nouvelle table qui contient les données + les annotations

poulet.annot[1:10,c(1:6,56:65)]
#Voyons les infos que contient cette table


identical(as.character(poulet.annot[,4][!is.na(poulet.annot[,57])]),as.character(poulet.annot[,57][!is.na(poulet.annot[,57])]))
#vérification que les 2 colonnes ID sont bien les mêmes

poulet.annot<-poulet.annot[,-57]
#On enlève la colonne ID issue de annot pour éviter les confusions

#récupération des infos sur les gènes du sous-groupe n°4

poulet.annot[labels(cuthv7$lower[[4]]),c("Name","NomGallus")]

poulet.annot[labels(cuthv7$lower[[4]]),c("OrthoHomo","OrthoMus")]

poulet.annot[labels(cuthv7$lower[[4]]),"GObiolproc1"]

poulet.annot[labels(cuthv7$lower[[4]]),"GObiolproc2"]

poulet.annot[labels(cuthv7$lower[[4]]),"GOmolfunct1"]

poulet.annot[labels(cuthv7$lower[[4]]),"GOcellcomp1"]

#Exportation des infos les plus pertinentes (selon moi...)
write.table(poulet.annot[labels(cuthv7$lower[[4]]),c("Name","NomGallus","OrthoHomo","OrthoMus","GObiolproc1","GObiolproc2","GOmolfunct1")],file="grp4.txt")



#------------------------------------------------------------------------------------------------------------------------#
#				PAM : Partitioning around Medoids
#------------------------------------------------------------------------------------------------------------------------#

#En classification ascendante hiérarchique, on a choisi 7 clusters de gènes. On va voir ce que PAM peut éventuellement nous apporter concernant ce choix.

plot(hc$height[1144:1125],pch=18,col="blue")
#d'après ce graphique, on pourrait aussi choisir 11 ou 12 groupes (ou clusters)

#-----------------------------------------------------#
#REALISATION DE PAM AVEC 7 OU 14 CLUSTERS
#-----------------------------------------------------#
library(cluster)
pamG7<-pam(as.dist(1-cor(t(poulet3))),k=7)
pamG12<-pam(as.dist(1-cor(t(poulet3))),k=12)

#-----------------------------------------------------#
#SILHOUETTE PLOTS
#-----------------------------------------------------#
plot(pamG7,which.plots=2,nmax.lab=61)
X11()
plot(pamG12,which.plots=2,nmax.lab=61)
#A priori, on préfèrerait 7 groupes ici car la silhouette moyenne est plus forte
#On trouve néanmoins quelques sous-groupes dont la silhouette est bonne en découpant en 12 clusters ( par exemple les clusters 1 et 11)

#-----------------------------------------------------#
#CHOIX DU NOMBRE DE CLUSTER AVEC PAM
#-----------------------------------------------------#
pamlist<-list()
for (i in 2:11)  {pamlist[[i]]<-pam(as.dist(1-cor(t(poulet3))),k=i)}
#On réalise PAM avec k=2,3,…,14
#ATTENTION : CALCUL LONG... à lancer avant une pause… et uniquement si votre PC n’a pas planté jusque là!!

unlist(sapply(pamlist,function(x)x$silinfo$avg.width))
#On récupère les largeurs de silhouette moyennes

plot(2:11,unlist(sapply(pamlist,function(x)x$silinfo$avg.width)),pch=18,col="blue")
#On les représente graphiquement
#Le choix de 7 clusters semble relativement raisonnable mais 8 clusters semblent plus adaptés d’après PAM (on gagne en précision sans trop perdre en silhouette moyenne, i.e. dans la qualité des clusters)


#-----------------------------------------------------#
#REPRESENTATION DES CLUSTERS DANS LES COORDONNEES DU MDS
#-----------------------------------------------------#
#Ces fonctions utilisent trop de RAM et ne fonctionnent pas avec des classifications PAM réalisées sur trop de gènes (il est d'ailleurs probable que les MDS soient peu lisibles avec plusieurs centaines de gènes comme nous le verrons au chapitre sur le MDS). On va donc regarder les fonctions suivantes en faisant plutôt des classifications des individus. Dans le chapitre sur le MDS, nous verrons comment représenter les classes de gènes dans les coordonnées du MDS en utilisant moins de mémoire.

pamP6<-pam(as.dist(1-cor(poulet3)),k=6)
#il semble assez logique ici de prendre 6 groupes...

plot(pamP6,which.plots=1,labels=4)
#Une première représentation dans les coordonnées du MDS

plot(pamP6,which.plots=1,nmax.lab=50,color=TRUE,labels=5)
#Cliquez sur les points que vous souhaitez identifier puis taper sur "Echap" pour reprendre la main dans la ligne de commande
#On peut jouer sur l'argument labels pour ces représentations (voir ci-dessous)
#labels= 0, no labels are placed in the plot;
#labels= 1, points and ellipses can be identified in the plot (see 'identify');
#labels= 2, all points and ellipses are labelled in the plot;
#labels= 3, only the points are labelled in the plot;
#labels= 4, only the ellipses are labelled in the plot.
#labels= 5, the ellipses are labelled in the plot, and points can be identified.



#------------------------------------------------------------------------------------------------------------------------#
#				MULTIDIMENSIONAL SCALING
#------------------------------------------------------------------------------------------------------------------------#


#-----------------------------------------------------#
#DIFFERENTS CALCULS DE DISTANCE
#-----------------------------------------------------#
rS=cor(t(poulet3))
dS=1-rS
dS2=sqrt(1-rS^2)
dN=dimnames(poulet3)[[1]]
dE=dist(poulet3)

#-----------------------------------------------------#
#REALISATION DES MDS
#-----------------------------------------------------#
mdsE<-cmdscale(dE,k=2,eig=TRUE)
mdsS<-cmdscale(dS,k=2,eig=TRUE)
mdsS2<-cmdscale(dS2,k=2,eig=TRUE)

#-----------------------------------------------------#
#COMPARAISON DES GRAPHES OBTENUS
#-----------------------------------------------------#
plot(mdsE$points,type="n",xlab="cp1",ylab="cp2",main="MDS pour données poulet4, distance euclidienne")
text(mdsE$points[,1],mdsE$points[,2],dN,cex=0.4)
X11()
plot(mdsS$points,type="n",xlab="cp1",ylab="cp2",main="MDS pour données poulet4, distance correlation")
text(mdsS$points[,1],mdsS$points[,2],dN,cex=0.4)
X11()
plot(mdsS2$points,type="n",xlab="cp1",ylab="cp2",main="MDS pour données poulet4, distance correlation carree")
text(mdsS2$points[,1],mdsS2$points[,2],dN,cex=0.4)
#Ces graphiques ne sont pas évidents à interpréter vu le nombre de gène...
#On va voir si on y voit plus clair en rajoutant des couleurs en lien avec une classification hiérarchique

#-----------------------------------------------------#
#CHOIX DU NOMBRE DE DIMENSIONS EN MDS
#-----------------------------------------------------#
mdsScree<-cmdscale(dS,k=8,eig=T)
plot(mdsScree$eig,pch=18,col="blue")

#-----------------------------------------------------#
#REPRESENTATION DES CLASSES D'UNE CAH OU D'UNE PAM PAR MDS
#-----------------------------------------------------#

#Dans l'objet hc, on dispose d'une CAH des gènes avec distance=1-cor et critère d'agglomération=Ward

col<-cutree(hc,k=7)
#On coupe l'arbre en 7 classes

plot(mdsS$points,type="n",xlab="cp1",ylab="cp2",main="MDS pour données poulet4, distance correlation")
text(mdsS$points[,1],mdsS$points[,2],dN,col=col,cex=0.4)
#Représentation des classes de la CAH dans les coordonnées du MDS

#Il est possible de faire la même chose avec la fonction clusplot du package "cluster".
#Cette fonction donne accès à de nombreuses options
#Elle sélectionne automatiquement les coordonnées à utiliser (MDS pour une matrice de distances et ACP pour une matrice de coordonnées)
#En revanche, elle est beaucoup plus gourmande et fera planter votre ordinateur avec des données de cette taille...

col<-pamG7$clustering
#On récupère dans l'objet col les numéros des classes auxquel chaque gène appartient

X11()
plot(mdsS$points,type="n",xlab="cp1",ylab="cp2",main="MDS pour données poulet4, distance correlation")
text(mdsS$points[,1],mdsS$points[,2],dN,col=col,cex=0.4)
#Représentation des classes de PAM (avec 7 groupes) dans les coordonnées du MDS
#Les 7 groupes définis par PAM ne sont pas les mêmes que les 7 groupes définis par CAH.

table(cutree(hc,k=7),pamG7$clustering)
#Pour voir combien de gènes ont changés de classe


#-----------------------------------------------------#
#COMBINAISON DES METHODES
#-----------------------------------------------------#


#--------------------------#
#CAH DES GENES
#--------------------------#

#L'objet hc contient une CAH des 1145 gènes sélectionnés

#--------------------------#
#CHOIX DU NOMBRE DE CLASSES
#--------------------------#

plot(hc$height[1144:1125],pch=18,col="blue")
#Sur la base de ce graphique...

mk7<-cutree(hc,k=7)
#...On coupe l'arbre en 7 classes

#--------------------------#
#CALCUL DES BARYCENTRES DES 7 GROUPES
#--------------------------#

bary<-apply(poulet3,2,function(x){tapply(x,mk7,mean)})

#--------------------------#
#REALLOCATION PAR KMEANS
#--------------------------#

kmG7<-kmeans(poulet3,centers=bary)
#On initialise les k-means avec les barycentres des 7 classes de la CAH

table(mk7,kmG7$cluster)
#On regarde ce qui a changé après avoir appliqué les k-means

#--------------------------#
#OBSERVATION DES MODIFICATIONS PAR MDS
#--------------------------#
plot(mdsS$points,type="n",xlab="cp1",ylab="cp2",main="MDS, distance correlation, avant kmeans")
text(mdsS$points[,1],mdsS$points[,2],dN,col=as.double(mk7))

X11()
plot(mdsS$points,type="n",xlab="cp1",ylab="cp2",main="MDS, distance correlation, après kmeans")
text(mdsS$points[,1],mdsS$points[,2],dN,col=as.double(kmG7$cluster))
#On voit assez nettement sur ces représentations que les gènes qui ont été affectés par les changements de classes sont principalement ceux qui se trouvaient "en bordure" d'une classe.


#------------------------------------------------------------------------------------------------------------------------#
#			ANALYSE EN COMPOSANTES PRINCIPALES
#------------------------------------------------------------------------------------------------------------------------#
#Deux fonctions (princomp et prcomp) calulent l'ACP classique dans R. Seule prcomp accepte un nombre de variables (colonnes) supérieur au nombre d'individus (lignes). Attention, les objets (résultats) créés par ces 2 fonctions n'ont pas tout à fait la même structure (voir les aides des 2 fonctions)
#la librairie amap permet également de faire de l'ACP (et d'autres méthodes multidimensionnelles). Elle correspond à la librairie multidim de S+ (http://www.lsp.ups-tlse.fr/)
#Dans le cadre de ces travaux pratiques, nous utiliserons aussi le package FactoMineR qui contient de nombreuses fonctions faciles d'utilisation pour les représentations d'ACP.


#-----------------------------------------------------#
#FONCTION PRINCOMP
#-----------------------------------------------------#

ACP1<-princomp(poulet3,cor=TRUE)
#réalisation de l'ACP avec les gènes en individus et les échantillons en variables
#les individus sont centrés
#Quelle est l'hypothèse biologique sous-entendue dans le centrage des individus?

plot(ACP1)
#eboulis des valeurs propres

biplot(ACP1)
#Que pensez-vous de la première dimension?

plot(ACP1$scores[,1],apply(poulet3,1,mean))
#Comment interprétez-vous ce graphique?

ACP1<-princomp(t(scale(t(poulet3),scale=F)))
#ACP avec double centrage en ligne et en colonne
#Que pensez-vous du centrage des gènes

plot(ACP1)
#eboulis des valeurs propres

biplot(ACP1)
#nouveau biplot


#-----------------------------------------------------#
#FONCTION PRCOMP
#-----------------------------------------------------#

ACP2<-prcomp(t(poulet3),scale=F)
#ACP avec les individus en ligne et les gènes en colonnes

plot(ACP2)
#eboulis

boxplot(data.frame(ACP2$x),las=2,cex.axis=0.7)
#Un autre moyen de sélectionner le nombre de composantes prioncipales
X11()
biplot(ACP2$x,ACP2$rotation,xlabs=as.character(grp))
#On commence à voir des choses

ACP3<-prcomp(t(poulet3),scale=T)
#avec réduction des gènes
#Que pensez-vous de la réduction des gènes, est-ce judicieux ?

plot(ACP3)
#eboulis

biplot(ACP3$x,ACP3$rotation,xlabs=as.character(grp))
#deux premiers axes factoriels

plot(ACP3$x[,1],ACP3$x[,2],type="n",xlab="PC1",ylab="PC2")
text(ACP3$x[,1],ACP3$x[,2],as.character(grp))
#Un graphique des individus plus facile à lire

plot(ACP3$x[,2],ACP3$x[,3],type="n",xlab="PC2",ylab="PC3")
text(ACP3$x[,2],ACP3$x[,3],as.character(grp))
#Comment interprétez-vous les axes de l'ACP?

#*********************************************************************************************************************
#	FONCTION CERCLE -> Permet de tracer un cercle et notamment le cercle des corrélations
#*********************************************************************************************************************
###############################################################################
# Création d'une fonction 'cercle' qui prend en paramètre la taille du rayon
# que l'on souhaite et qui permet de tracer le cercle correspondant à ce rayon.
# Cette fonction est utile pour tracer le cercle des corrélations de rayon 1.
###############################################################################

cercle <- function (rad = 1)

	###########################################################################
	# rad	-> taille du rayon que l'on souhaite
	# 
	# Cette fonction premet de tracer un cercle dont le rayon est de longueur
	# 'rad'. Par défaut, rad=1, ce qui correspond au cercle des corrélations.
	###########################################################################
{
	teta <- (1:101 * 2 * pi)/100
	x <- rad * sin(teta)
	y <- rad * cos(teta)
	lines(x, y)
}


vec<-ACP3$rotation %*% diag(ACP3$sdev)
#Les coordonées des gènes


plot(vec[,1],vec[,2],type="n", xlim=c(-1,1),ylim=c(-1,1),main="Genes")
cercle(1)
arrows(rep(0,dim(poulet3)[1]),rep(0,dim(poulet3)[1]),vec[,1],vec[,2],lwd=0.2)
text(vec[,1],vec[,2],dimnames(poulet3)[[1]],cex=0.5)
#Représentation des gènes (et du cercle des corrélations) - Plan 1-2
#C'est plus lisible lorsqu'on ne met pas les flèches mais ça reste difficile à lire quand même...


plot(vec[,2],vec[,3],type="n", xlim=c(-1,1),ylim=c(-1,1),xlab="PC2",ylab="PC3",main="Genes")
cercle(1)
arrows(rep(0,dim(poulet3)[1]),rep(0,dim(poulet3)[1]),vec[,2],vec[,3],lwd=0.2)
text(vec[,2],vec[,3],dimnames(poulet3)[[1]],cex=0.5)
#Gènes dans le plan 2-3
#Même remarque, ce n'est pas très lisible...
#-----------------------------------------------------#
#COMPARAISON MDS-ACP
#-----------------------------------------------------#

col=cutree(hc,k=7)
plot(vec[,1],vec[,2],type="n", xlim=c(-1,1),ylim=c(-1,1),main="Genes")
cercle(1)
text(vec[,1],vec[,2],dimnames(poulet3)[[1]],cex=0.5,col=col)
#Ca vous rappelle quelque chose?

X11()
plot(mdsS$points,type="n",xlab="cp1",ylab="cp2",main="MDS pour données poulet4, distance correlation")
text(mdsS$points[,1],mdsS$points[,2],dN,col=col,cex=0.4)
#On obtient des résultats très voisins avec le MDS

plot(vec[,1],-mdsS$points[,1])
#La preuve...

#En fait, le MDS avec une distance euclidienne est équivalent à l'ACP sur le tableau de données avec les gènes en ligne.


#-----------------------------------------------------#
#ACP AVEC LE PACKAGE FactoMineR
#-----------------------------------------------------#

library(FactoMineR)

resACP<-PCA(t(poulet3),graph=F)
#Réalisation de l'ACP
#avec l'argument scale.unit=F, on réalise l'ACP sans la réduction des variables

barplot(resACP$eig$iner,xlab="Composante",ylab="variance expliquée")
#eboulis de la variance expliquée

barplot(resACP$eig$eig,xlab="Composante",ylab="valeur propre")
#éboulis des valeurs propres (idem ci-dessus au changement d'échelle près)

plot(resACP,choix="ind",col.ind=as.integer(grp))
#graphique des individus (les couleurs correspondent aux différents groupes

plot(resACP$ind$coord[,1],resACP$ind$coord[,2],type="n",
	xlab=paste("Composante 1", paste("(",round(resACP$eig$iner[1],1),"%)",sep=""),sep=" "),
	ylab=paste("Composante 2", paste("(",round(resACP$eig$iner[2],1),"%)",sep=""),sep=" "))
text(resACP$ind$coord[,1],resACP$ind$coord[,2],as.character(grp),col=as.numeric(grp))
#Une autre manière de faire le graphique...plus manuel...


plot(resACP,choix="var")
#graphique des variables

plot(resACP,choix="var",lim.cos2.var=0.7,cex=0.6)
#représentation des variables avec une limite sur le cos2.

#--------------------------#
#Essais d'améliorations pour la lecture des variables
#--------------------------#

#Le graphique des variables étant difficilement lisible, on va essayer deux choses pour l'améliorer.
#1) on va utiliser des couleurs sur les variables pour représenter les groupes de la CAH

plot(resACP,choix="var",col.var=mk7)
plot(resACP,choix="var",lim.cos2.var=0.7,cex=0.6,col.var=mk7)
#Essayez de faire le lien entre ces graphiques + le graphique des individus et la double classification

#2) on va représenter en variables supplémentaires les barycentres de ces groupes de variables obtenus par CAH

sapply(cuthv7$lower,function(x){attributes(x)$members})
# [1] 182 133  56  61 116 461 136
#Les effectifs des sous-groupes définis par CAH et représentés en double classif

tapply(mk7,as.factor(mk7),length)
#1   2   3   4   5   6   7 
# 56 133 182 461  61 116 136 
#Les effectifs des sous-groupes définis par CAH et codés dans l'objet mk7

#Ces deux vecteurs nous permettent de faire le lien entre les sous-groupes représentés par double classification et ceux définis dans mk7 (qui sont plus pratiques à utiliser pour certaines fonctions)
#
N°de groupe dans mk7	Numéro de groupe dans cuthv7	
1	3	
2	2	
3	1	
4	6	
5	4	
6	5	
7	7	
#On va donc créer un nouveau vecteur de la même forme que mk7 mais qui reprend les numéros de groupe de cuthv7

mk7b=mk7
mk7b[mk7==1]=3
mk7b[mk7==3]=1
mk7b[mk7==4]=6
mk7b[mk7==5]=4
mk7b[mk7==6]=5
#création de mk7b qui est un recodage de mk7 (attention à ne pas remplacer mk7 par mk7b dans ces lignes de codes...)

sapply(cuthv7$lower,function(x){attributes(x)$members})
tapply(mk7b,as.factor(mk7b),length)
#vérification OK

baryb<-apply(poulet3,2,function(x){tapply(x,mk7b,mean)})
#calcul des barycentres des 7 sous-groupes

dimnames(baryb)[[1]]<-paste("grp",1:7,sep="")
#On donne des noms explicites à ces sous-groupes et surtout des noms qui ne sont pas trouvés dans la table poulet4

baryb[,1:5]
rbind(poulet3,baryb)[1146:1152,1:5]
#vérification des index (lignes) où vont se trouver les barycentres

resACP2<-PCA(t(rbind(poulet3,baryb)),quanti.sup=1146:1152,graph=F)
#ACP avec des variables quantitatives supplémentaires

plot(resACP2,choix="var",lim.cos2.var=0.7,cex=0.7,col.quanti.sup=1:7)
#graphique des variables avec des couleurs pour les barycentres

plot(resACP2,choix="var",lim.cos2.var=0.7,cex=0.7,col.quanti.sup=1:7,col.var=mk7b)
#graphique des variables avec des couleurs pour les barycentres et pour les variables

plot(resACP2,axes=c(2,3),choix="var",lim.cos2.var=0.6,cex=0.6,col.quanti.sup=1:7,col.var=mk7b)
#axes 2-3

plot(resACP2,axes=c(2,3),choix="ind",col.ind=as.integer(grp))
#comment interprétez-vous ce plan factoriel 2-3?


#--------------------------#
#Indices de qualité
#--------------------------#

print(resACP2)
#nous donne la liste des sous-objets contenus dans l'objet resACP2. En particulier où trouver les contributions des individus et des variables ainsi que les cos2


