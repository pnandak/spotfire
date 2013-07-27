# Read data
d.music<-read.csv("data/music-plusnew-sub.csv",row.names=1)
d.music.sub<-subset(d.music,select=LVar:LFreq)
  
# Compute summary statistics
mean(d.music.sub)
sd(d.music.sub)
cor(d.music.sub)

# Standardize the variables
f.std.data<-function(x){
  (x-mean(x))/sd(x)
}
d.music.std<-apply(d.music.sub,2,f.std.data)

# Calculate Euclidean distances
d.music.dst<-dist(d.music.std)

# Compute clustering
d.music.hc1<-hclust(d.music.dst,method="single")
d.music.hc2<-hclust(d.music.dst,method="ward")

# Plot dendrograms
par(mfrow=c(1,2))
plot(d.music.hc1,main="Single linkage",hang=-1)
plot(d.music.hc2,main="Wards linkage",hang=-1)

# Cut the dendrogram - write out clusters
cl<-cutree(d.music.hc2,5) # 5 is the number of clusters, change it
abline(h=10) # Draw line on the dendrogram
d.music[cl==1,1:2]
d.music[cl==2,1:2]
d.music[cl==3,1:2]
d.music[cl==4,1:2]
d.music[cl==5,1:2]

# A bit more advanced - using ggobi to explore clusters - to be demo'd
library(rggobi)
cl<-NULL
for (i in 2:10)
  cl<-cbind(cl,cutree(d.music.hc2,i))
colnames(cl)<-paste("cl",2:10,sep="")
d.music.clust<-cbind(d.music,cl)
ggobi(d.music.clust)

