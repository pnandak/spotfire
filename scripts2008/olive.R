# Read in the data
d.olive<-read.table("olive.asc",sep=",",header=T,row.names=1)
d.olive<-read.table("C:\\EDA\\olive.asc",sep=",",header=T,row.names=1)

# Check if the values do approx add to 100%
summary(apply(d.olive[,-c(1,2)],1,sum)) # 

# Compute summary statistics
summary(d.olive[,-c(1,2)])
summary(d.olive[d.olive[,1]==1,-c(1,2)])
summary(d.olive[d.olive[,1]==2,-c(1,2)])
summary(d.olive[d.olive[,1]==3,-c(1,2)])
apply(d.olive[d.olive[,2]==1,-c(1,2)],2,mean)/100
nrow(d.olive[d.olive[,2]==1,-c(1,2)])
apply(d.olive[d.olive[,2]==2,-c(1,2)],2,mean)/100
nrow(d.olive[d.olive[,2]==2,-c(1,2)])
apply(d.olive[d.olive[,2]==3,-c(1,2)],2,mean)/100
nrow(d.olive[d.olive[,2]==3,-c(1,2)])
apply(d.olive[d.olive[,2]==4,-c(1,2)],2,mean)/100
nrow(d.olive[d.olive[,2]==4,-c(1,2)])
apply(d.olive[d.olive[,2]==5,-c(1,2)],2,mean)/100
nrow(d.olive[d.olive[,2]==5,-c(1,2)])
apply(d.olive[d.olive[,2]==6,-c(1,2)],2,mean)/100
nrow(d.olive[d.olive[,2]==6,-c(1,2)])
apply(d.olive[d.olive[,2]==7,-c(1,2)],2,mean)/100
nrow(d.olive[d.olive[,2]==7,-c(1,2)])
apply(d.olive[d.olive[,2]==8,-c(1,2)],2,mean)/100
nrow(d.olive[d.olive[,2]==8,-c(1,2)])
apply(d.olive[d.olive[,2]==9,-c(1,2)],2,mean)/100
nrow(d.olive[d.olive[,2]==9,-c(1,2)])

# Linear Discriminant Analysis
# load the MASS library as the first step

# generate training and test samples
indx<-c(1,7,12,15,16,22,27,32,34,35,36,41,50,54,61,68,70,75,
,76,80,95,101,102,105,106,110,116,118,119,122,134,137,140,147,148,150,
,151,156,165,175,177,182,183,185,186,187,190,192,194,201,202,211,213,217,
,218,219,225,227,241,242,246,257,259,263,266,274,280,284,289,291,292,297,
,305,310,313,314,323,330,333,338,341,342,347,351,352,356,358,359,369,374,
,375,376,386,392,405,406,415,416,418,420,421,423,426,428,435,440,451,458,
,460,462,466,468,470,474,476,480,481,482,487,492,493,500,501,509,519,522,
,530,532,541,543,545,546,551,559,567,570)

d.olive.train<-d.olive[-indx,]
d.olive.test<-d.olive[indx,]

# Set up class variable to separate south from others
c1<-rep(-1,572)
c1[d.olive[,1]!=1]<-1
c1.train<-c1[-indx]
c1.test<-c1[indx]

olive.lda1<-lda(d.olive.train[,-c(1:2)],c1.train)
olive.lda1
# plot the predictions of the current sample
olive.x<-predict(olive.lda1,d.olive.train[,-(1:2)], dimen=1)$x
olive.x2<-predict(olive.lda1,d.olive.test[,-(1:2)], dimen=1)$x
par(mfrow=c(2,2),pty="s")
hist(olive.x[c1.train==-1],10,col=2,xlim=c(-6.5,6),main="Training south",
  breaks=seq(-6.5,6, by=1/2))
hist(olive.x[c1.train==1],10,col=2,xlim=c(-6.5,6), main="Training others",
  breaks=seq(-6.5,6, by=1/2))
hist(olive.x2[c1.test==-1],10,col=2,xlim=c(-6.5,6),main="Test south",
  breaks=seq(-6.5,6, by=1/2))
hist(olive.x2[c1.test==1],10,col=2,xlim=c(-6.5,6),main="Test others",
  breaks=seq(-6.5,6, by=1/2))

table(c1.train,predict(olive.lda1,d.olive.train[,-c(1,2)])$class)
table(c1.test,predict(olive.lda1,d.olive.test[,-c(1,2)])$class)

# Load nnet library
