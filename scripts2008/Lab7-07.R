# Read data
d.spearman<-read.csv("spearman.csv")
head(d.spearman)

# Plot theoretical factors
library(ggplot2)
qplot(intelligence,..density..,data=d.spearman,geom="histogram",
      breaks=seq(0,100,10))
qplot(intelligence,..density..,data=d.spearman,geom="histogram",
      breaks=seq(0,100,20))

hist(d.spearman$intelligence,col="grey80",breaks=seq(0,100,10),
     xlab="Intelligence",main="",freq=F)
hist(d.spearman$intelligence,col="grey80",breaks=seq(0,100,20),
     xlab="Intelligence",main="",freq=F)

# Compute correlation
options(digits=2)
cor(d.spearman[,-1])

# Look at data in a tour
library(rggobi)
ggobi(d.spearman)

# Compute PCA
d.spearman.pca<-prcomp(d.spearman[,-1],scale=T,retx=T)
d.spearman.pca
screeplot(d.spearman.pca,type="lines")
d.spearman.pca$rotation[,1]*d.spearman.pca$sdev[1]
1-d.spearman.pca$rotation[,1]^2*d.spearman.pca$sdev[1]^2

# Compute factor analysis
d.spearman.fa<-factanal(d.spearman[,-1],1)
d.spearman.fa

# Read second data set
d.spearman3<-read.csv("spearman3.csv")
head(d.spearman3)
ggobi(d.spearman3)

# Compute factor models
d.spearman3.fa<-factanal(d.spearman3[,-c(1:2)],1)
d.spearman3.fa
d.spearman3.fa<-factanal(d.spearman3[,-c(1:2)],2,rotation="none")
d.spearman3.fa

# Compute PCA
d.spearman3.pca<-prcomp(d.spearman3[,-c(1,2)],scale=T,retx=T)
screeplot(d.spearman3.pca,type="lines")

# Factor rotation
varimax(d.spearman3.fa$loadings)



