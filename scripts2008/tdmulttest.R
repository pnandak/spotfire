
setwd("C:/.../")

library(multtest)

### Importation des données

poulets = read.table("pouletsdata.txt",header=TRUE,sep="\t")

annotations = read.table("annotations.txt",sep=";")
annotations = as.character(annotations[,1])

pouletsNJ = poulets[(poulets$Regime=="N")|(poulets$Regime=="J16"),] 
pouletsNJ$Regime

### Examen des variabilités intra-groupes et nettoyage

sdpargroupe <- by(pouletsNJ[,1:20652],INDICES=pouletsNJ$Regime,FUN=sd,na.rm=TRUE)

summary(sdpargroupe$N)
summary(sdpargroupe$J16)

power.t.test(n=7,sd=0.5,sig.level=0.05,power=0.95)

### Comparison des variances intra-groupes

CompareVariance = function(variable,facteur) {
test = var.test(variable~facteur)
return(test$p.value)
}

testvar = apply(pouletsNJ[,1:20652],MARGIN=2,FUN=CompareVariance,facteur=pouletsNJ$Regime)
testvar.adj = mt.rawp2adjp(testvar,"BH")
pvalue.adj = testvar.adj$adj[order(testvar.adj$index),]
serie.alpha = seq(from=0,to=0.1,by=0.01)
vardiff = mt.reject(pvalue.adj,alpha=serie.alpha)
vardiff$r

vardiff = mt.reject(pvalue.adj,alpha=0.05)

vardiff.id = vardiff$which[,2]
genes.vardiff = (1:20652)[vardiff.id]

### Comparaison de moyennes

CompareMoyenne = function(variable,facteur,egalite.var=TRUE) {
test = t.test(variable~facteur,var.equal=egalite.var)
return(test$p.value) }

testmoy = apply(pouletsNJ[,1:20652],MARGIN=2,FUN=CompareMoyenne,facteur=pouletsNJ$Regime,
egalite.var=TRUE)
testmoy[genes.vardiff] = apply(pouletsNJ[,genes.vardiff],MARGIN=2,FUN=CompareMoyenne,
facteur=pouletsNJ$Regime,egalite.var=FALSE)

testmoy.adj = mt.rawp2adjp(testmoy,"BH")
pvalue.adj = testmoy.adj$adj[order(testmoy.adj$index),]
serie.alpha = seq(from=0,to=0.05,by=0.005)
moydiff = mt.reject(pvalue.adj,alpha=serie.alpha)
moydiff$r

annotations[order(testmoy)[1:10]]

### Test d'un contraste

library(gmodels)

TestContraste = function(variable,facteur) {
modele = aov(y~x,data=data.frame(y=variable,x=facteur),na.action=na.omit)
contraste = fit.contrast(modele,varname="x",coeff=rbind("N vs J16"=c(1,0,0,0,0,-1)))
return(contraste[4])
}

pvalue.contraste = apply(poulets[,1:20652],MARGIN=2,TestContraste,facteur=poulets$Regime)

contraste.adj = mt.rawp2adjp(pvalue.contraste,"BH")
contraste.adj = contraste.adj$adj[order(contraste.adj$index),]
serie.alpha = seq(from=0,to=0.05,by=0.005)
moydiff = mt.reject(contraste.adj,alpha=serie.alpha)
moydiff$r

annotations[order(pvalue.contraste)[1:10]]

### Test de Fisher

TestFisher = function(variable,facteur) {
modele = aov(variable~facteur,na.action=na.omit)
table.anova = anova(modele)
return(table.anova[1,5])
}

pvalue.fisher = lapply(poulets[,1:20652],function(variable,facteur) TestFisher(variable,facteur),
facteur=poulets$Regime)

pvalue.fisher = unlist(pvalue.fisher)

fisher.adj = mt.rawp2adjp(pvalue.fisher,"BH")
fisher.adj = fisher.adj$adj[order(fisher.adj$index),]
serie.alpha = seq(from=0,to=1e-6,by=1e-7)
moydiff = mt.reject(fisher.adj,alpha=serie.alpha)
moydiff$r

annotations[order(pvalue.fisher)[1:10]]
