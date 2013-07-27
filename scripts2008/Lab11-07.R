# Read data
d.music<-read.csv("data/music-plusnew-sub.csv",row.names=1)

# Subset datd for building rule
d.music.sub<-subset(d.music,Type=="Classical"|Type=="Rock",
    select=Type:LFreq)
type_to_num<-c("Classical"=1,"Rock"=2)
d.music.sub[,1]<-factor(d.music.sub[,1],exclude="New wave")
d.music.class<-type_to_num[d.music.sub[,1]]

# Calculate group means
options(digits=3)
apply(d.music.sub[d.music.sub[,1]=="Classical",-1],2,mean,na.rm=T)
apply(d.music.sub[d.music.sub[,1]=="Rock",-1],2,mean,na.rm=T)
var(d.music.sub[d.music.sub[,1]=="Classical",-1],na.rm=T)
var(d.music.sub[d.music.sub[,1]=="Rock",-1],na.rm=T)

# Function to compute pooled variance-covariance generally
f.Spooled<-function(x,gp){
  vc1<-var(x[gp==1,],na.rm=T)
  vc2<-var(x[gp==2,],na.rm=T)
  n1<-nrow(x[gp==1,])
  n2<-nrow(x[gp==2,])
  Sp<-((n1-1)*vc1+(n2-1)*vc2)/(n1+n2-2)
  return(Sp)
}

# Compute pooled variance-covariance matrix
d.music.Sp<-f.Spooled(d.music.sub[,-1],d.music.class)
d.music.Sp

# Scatterplot matrix
par(pty="s")
pairs(d.music.sub[,-1],pch=d.music.class,col=d.music.class)
# library(ggplot2)
# plotmatrix(d.music.sub[,-1],aes(colour=d.music.sub$Type))

# LDA
# library(MASS) 
music.lda<-lda(Type~.,data=d.music.sub,prior=c(0.5,0.5))
music.lda
music.lda$scaling # these are the linear coefficients
music.lda$means   # group means

# Manually
f.inv.mat<-function(vc) {
  ev<-eigen(vc)
  vcinv<-ev$vectors%*%diag(1/ev$values)%*%t(ev$vectors)
  vcinv
}
d.music.Spinv<-f.inv.mat(d.music.Sp)
  
music.lincoef<-t(as.matrix(music.lda$means[1,]-music.lda$means[2,]))%*%
  d.music.Spinv
music.intcpt<-music.lincoef%*%as.matrix(music.lda$means[1,]+music.lda$means[2,])/2
music.pred<-as.matrix(d.music.sub[,-1])%*%t(music.lincoef)-rep(music.intcpt,54)
cbind(d.music.sub[,1],music.pred)
table(d.music.class,music.pred<0)

# Compute the constant
t(music.lda$scaling)%*%(music.lda$means[1,]+music.lda$means[2,])/2

# Compute misclassification table
table(d.music.sub[,1],predict(music.lda,d.music.sub)$class)

# Which cases are misclassified?
predict(music.lda,d.music.sub)$x

# Classify the 5 unknown cases
predict(music.lda,d.music[58:62,])$class

# Plot these cases on the scatterplot matrix
par(pty="s")
pairs(rbind(d.music.sub[,-1],d.music[58:62,-c(1,2)]),
      pch=c(d.music.class,rep(16,5)),col=c(d.music.class,rep(4,5)))

