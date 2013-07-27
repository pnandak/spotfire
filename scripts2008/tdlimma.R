

###

setwd("E:/HOME/david/ens/FC Données génomiques/DataFC07/FC2007-analyses_statXomique/données GENEPIX/")
setwd("C:/Documents and Settings/causeur/Mes documents/HOME/david/ens/FC Données génomiques/DataFC07/FC2007-analyses_statXomique/données GENEPIX/")

library(limma)

targets<-readTargets("targets.txt")

### Filtrage des spots

MonFiltrage = function(X, seuilSNR=2, seuilH=0.2) { 
   okFLAG = X$Flags > -49 
   okSNRred = X[,"SNR 635"] > seuilSNR # okSNRred=TRUE si le rapport signal rouge - bruit de fond > seuilSNR
   okSNRgreen = X[,"SNR 532"] > seuilSNR # okSNRred=TRUE si le rapport signal vert - bruit de fond > seuilSNR
   okSNR = okSNRred & okSNRgreen # okSNR=TRUE si le rapport signal - bruit de fond > seuilSNR
   NumRed = abs(X[,"F635 Median"]-X[,"F635 Mean"]) # NumRed = | signal rouge moyen - signal rouge médian | 
   DenomRed = 0.5*(X[,"F635 Median"]+X[,"F635 Mean"]) # DenomRed = 0.5 (signal rouge moyen + signal rouge médian) 
   okHRed = (NumRed/DenomRed) < seuilH # okHRed=TRUE si le rapport H rouge < seuilH
   NumGreen = abs(X[,"F635 Median"]-X[,"F635 Mean"]) # NumGreen  = | signal vert moyen - signal vert médian |
   DenomGreen = 0.5*(X[,"F635 Median"]+X[,"F635 Mean"]) # DenomGreen = 0.5 (signal vert moyen + signal vert médian)
   okHGreen = (NumGreen/DenomGreen) < seuilH # okHGreen=TRUE si le rapport H vert < seuilH
   okH = okHRed & okHGreen  # okH=TRUE si le rapport H < seuilH
   ok = okFLAG & okSNR & okH # ok=TRUE si okFLAG=TRUE et okSNR=TRUE et okH=TRUE
   return(as.numeric(ok)) }

RG<-read.maimages(files=targets$FileName,source="genepix",wt.fun=MonFiltrage)

NonFiltres <- apply(RG$weights,2,mean)
round(NonFiltres,2)

names(RG$Genes)

spottypes <- readSpotTypes("SpotTypes.txt")

RG$genes$Status <- controlStatus(spottypes,RG)

### Analyse du bruit de fond

boxplot(data.frame(log2(RG$Rb[,1:20])),main="Bruit de fond rouge")

imageplot(log2(RG$Rb[,20]),layout=RG$printer)

RGb = backgroundCorrect(RG,method="none")

### Hétérogénéité du signal

MA = normalizeWithinArrays(RG,method="none")
plotMA(MA,status=RGb$genes$Status)
MA = normalizeWithinArrays(RG,method="loess")
plotMA(MA,status=RGb$genes$Status)
plotPrintTipLoess(MA)

cols <- rep("lightgray",times=49)
cols[targets$Factor=="J16"] = "darkgray"
cols[targets$Factor=="J16R5"] = "lightblue"
cols[targets$Factor=="J16R16"] = "pink"
cols[targets$Factor=="J48"] = "magenta"
cols[targets$Factor=="J48R24"] = "yellow"
boxplot(data.frame(MA$M),main="Log-ratios",col=cols,ylim=c(-5,5))
legend(0,5,fill=c("lightgray","darkgray","lightblue","pink","magenta","yellow"),
legend=c("N","J16","J16R5","J16R16","J48","J48R24"),bg="white")

### Création du jeu de données

poulets.data = t(MA$M[MA$genes$Status=="gene",])
colnames(poulets.data) = MA$genes$ID[MA$genes$Status=="gene"]
poulets.data = data.frame(poulets.data,Regime=targets$Factor)
write.table(poulets.data,"pouletsdata.txt",sep="\t")

write.table(MA$genes$Name[MA$genes$Status=="gene"],file="annotations.txt",sep=";")

write.table(MA$weights[MA$genes$Status=="gene"],file="filtrage.txt",sep=";")
