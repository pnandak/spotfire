###############################################
## Day 4: homicides
###############################################
hom<-matrix(c(24,17,53,50,21,27,21,27,21,10,19,5,29,14,9,4,104,47,8,15),nrow=4,dimnames=list(c("Mm","Mf","Fm","Ff"),c("y1","y5","y10","y16","yold")))
hom
chisq.test(hom)
chi1<-chisq.test(hom)
chi1
summary(chi1)
round(chi1$residuals)
addmargins(as.table(hom))
mosaicplot(as.table(hom))
mosaicplot(as.table(hom),shade=TRUE)
fisher.test(hom)

hom2<-hom[,-4]
hom2[,3]<-hom2[,3]+hom[,4]
hom2
dimnames(hom2)[[2]]<-c( "y1","y5","y10y16","yold")
mosaicplot(as.table(hom2),shade=TRUE)

