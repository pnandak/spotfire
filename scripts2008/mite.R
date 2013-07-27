d.mite<-read.csv("data/mite.csv")

# Sort sites
library(ggplot2)
d.mite.reshape<-melt(d.mite[,-c(1,2)],id.var=c("Substrate"),
  variable_name="Species",na.rm=T)
sites.means<-cast(d.mite.reshape,Substrate~.,mean,fill=NA)
ord.sites<-order(sites.means[,2],decreasing=F)
sorted.sites<-levels(d.mite.reshape[,1])[ord.sites]
d.mite.reshape[,1]<-factor(d.mite.reshape[,1],levels=sorted.sites)
colnames(d.mite.reshape)[3]<-"Count"
pdf("mite-sites.pdf",height=4,width=4)
qplot(jitter(Count),Substrate,data=d.mite.reshape)
dev.off()

# Sort species
d.mite.reshape<-melt(d.mite[,-c(1,2)],id.var=c("Substrate"),
  variable_name="Species",na.rm=T)
species.means<-cast(d.mite.reshape,Species~.,mean)
ord.species<-order(species.means[,2],decreasing=F)
sorted.species<-levels(d.mite.reshape[,2])[ord.species]
species.means[ord.species,2]
d.mite.reshape[,2]<-factor(d.mite.reshape[,2],levels=sorted.species)
colnames(d.mite.reshape)[3]<-"Count"
pdf("mite-species.pdf",height=6,width=4)
qplot(jitter(Count),Species,data=d.mite.reshape)
dev.off()

pdf("mite-species-rescaled.pdf",height=6,width=4)
qplot(jitter(Count),Species,data=d.mite.reshape,xlim=c(-1,150))
dev.off()

pdf("mite-species-site.pdf",height=6,width=4)
qplot(jitter(Count),Species,facets=.~Substrate,data=d.mite.reshape,
      xlim=c(-1,150))
dev.off()

# Find sites with low counts
x<-apply(d.mite[,-c(1:3)],1,sum,na.rm=T)
summary(x)
cbind(d.mite[order(x),1:3],sort(x))

# Find species with low counts
x<-apply(d.mite[,-c(1:3)],2,sum,na.rm=T)
summary(x)
sort(x)
# None are really low!

# How many of different Substrate types
table(d.mite[,3])
# Decide to drop all but Interface, Sphagn1, Sphagn2
d.mite.sub<-subset(d.mite,Substrate=="Interface"|Substrate=="Sphagn1"|Substrate=="Sphagn2")
# Reset factor levels of substrate
d.mite.sub[,3]<-factor(d.mite.sub[,3],exclude=c("Barepeat","Litter","Sphagn3","Sphagn4"))

# Ordination: sites and species
library(vegan)
d.mite.rda<-rda(d.mite.sub[,-c(1:3)])
d.mite.rda
screeplot(d.mite.rda,type="l")
plot(d.mite.rda,cex=2)
# Use Substrate type as label
plot(d.mite.rda,type="n",xlim=c(-5,27))
text(d.mite.rda,labels=d.mite.sub[,3])

# Clustering
d.mite.clust<-d.mite.sub[,-c(1:3)]
d.mite.clust.dist<-vegdist(d.mite.clust) # Uses bray distance
d.mite.clust.dist[is.na(d.mite.clust.dist)]<-1 # Necessary to set missings
                                               # as largest distance
summary(d.mite.clust.dist)
d.mite.hc<-hclust(d.mite.clust.dist,method="ward")
plot(d.mite.hc)

# Ordination with Substrate too
d.mite.rda2<-rda(d.mite.sub[,-c(1:3)],d.mite.sub[,1:2])
d.mite.rda2
plot(d.mite.rda2)
# Use Substrate type as label
plot(d.mite.rda2,type="n",xlim=c(-5,27))
text(d.mite.rda,labels=d.mite.sub[,3])


