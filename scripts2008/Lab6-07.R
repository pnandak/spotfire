d.towns<-read.csv("data/towns2.csv",row.names=1)
head(d.towns)
d.towns.pca<-prcomp(d.towns[,3:12],scale=T,retx=T)
options(digits=2)
d.towns.pca
for (i in 1:10)
  cat(sum(d.towns.pca$sdev[1:i]^2)/sum(d.towns.pca$sdev^2)," ")
library(ggplot2)
qplot(d.towns.pca$x[,1],d.towns$Score)
sort(d.towns.pca$x[,1])
plotmatrix(as.data.frame(d.towns.pca$x[,1:5]))
qplot(c(1:10),d.towns.pca$sd^2,geom="line",xlab="Principal Component",
   ylab="Variance")
library(rggobi)
ggobi(d.towns.pca$x)

