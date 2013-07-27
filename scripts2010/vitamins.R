###############################################
## Day 4: Common cold
###############################################
cold<-matrix(c(335,302,76,105),ncol=2,dimnames=list(c("Placebo","VitC"),c("Cold","NoCold")))

ch1<-chisq.test(cold)
ch1
names(ch1)
residuals(ch1)
## Doing something bad to get all the information ...
class(ch1)<-"list"
ch1

###############################################
## Day 4: Vitamins
###############################################
vit<-read.table("vitamins.txt",header=TRUE)
vit<-read.table("vitamins.txt",header=TRUE,sep=";")
vit<-read.table("vitamins.txt",header=TRUE,sep=",",row.names=1)
summary(vit)
table(vit)
chisq.test(table(vit))
chisq.test(vit$status,vit$treatment)
par(mfrow=c(1,1))
mosaicplot(table(vit))
mosaicplot(table(vit),shade=TRUE)

