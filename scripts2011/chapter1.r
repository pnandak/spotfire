#### This is an update version of the code from Peter:
# dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec1") 
# setwd(dd)
library(lme4)


## made up examples for two-level sampling: random pick up 50 out of 500.
# postscript("fig1_1.ps",height=3,width=9, horizontal=F)
par(mfrow=c(1,2),mar=.25*rep(1,4),mgp=c(0,0,0))
set.seed(1)
N<-500
sf<- .10
xy<-matrix( runif(2*N,-3,3), nrow=N,ncol=2)
n<-round(N*sf)
s<-sample(rep(c(0,1),times=c(N-n,n)))
plot(xy,pch=c(21),xlab="",ylab="",xaxt="n",yaxt="n",bg="white" )
#plot(xy,pch=c(21,16)[s+1] ,bg="white",xlab="n",ylab="n",xaxt="n",yaxt="n" )
plot(xy,pch=c(21,16)[s+1], xlab="",ylab="",xaxt="n",yaxt="n" ,bg="white")
points(xy[s==1,],pch=16,cex=1.25)
# dev.off()
## notice that postcript() and dev.off() will automatically save a postcript;
## but they also close the plotting platform all together. 


## pick cells first and then sample with each selected cells:
postscript("fig1_2.ps",height=3,width=9, horizontal=F)
par(mfrow=c(1,2),mar=.25*rep(1,4),mgp=c(0,0,0))
plot(xy,type="n",xlab="",ylab="",xaxt="n",yaxt="n" )
segments( -3:3,rep(-3,7),-3:3,rep(3,7),col="gray")
segments( rep(-3,7), -3:3,rep(3,7),-3:3,col="gray")
points(xy,pch=c(21),bg="white" )

plot(xy,type="n",xlab="n",ylab="n",xaxt="n",yaxt="n" )
segments( -3:3,rep(-3,7),-3:3,rep(3,7),col="gray")
segments( rep(-3,7), -3:3,rep(3,7),-3:3,col="gray")
set.seed(1)
vv<-matrix(c(3:(-2)),nrow=6,ncol=6)
hv<-t(vv)
p1<-cbind(c(hv),c(vv))
n1<-6
s1<- p1[sample(1:36,n1),]
for(i in 1:n1) {
  x<- s1[i,1] + c(0,-1,-1,0) 
  y<- s1[i,2] + c(0,0,-1,-1)
  polygon(x,y,col="gray")
               }
points(xy,pch=c(21),bg="white" )

s12<-matrix(0,nrow=N,ncol=2)
for(i in 1:n1) {

s1i<-  xy[,1]<s1[i,1] & xy[,1]>s1[i,1]-1 & xy[,2]<s1[i,2] & xy[,2]>s1[i,2]-1 
s12[,1]<-s12[,1]+1*s1i
xyi<-xy[s1i,]

Ni<-dim(xyi)[1]
s2<-rbinom(Ni,1, sf*n1) 
s12[ s1i,2] <-s12[s1i,2] + s2
                }
s12p<-apply(s12,1,prod)
points( xy[s12p==1,] ,pch=16) 
dev.off()





## NELS data for this chapter
ndat_full<-dget("http://privatewww.essex.ac.uk/~caox/teaching/Day%201/nels_2002_data")
ndat<- ndat_full[  ,c(1,2,4,5,6,14,19)]
ndat<-ndat[ ndat[,2]>=5  &   ndat[,3]==1 & ndat[,4]==1, -c(2,3,4) ]
colnames(ndat)<- c("school","mathdeg","mathscore","ses")
ndat<-as.data.frame(ndat)


##### school specific math score data
postscript("fig1_3.ps",family="Times",height=4, width=8, horizontal=F)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))

avmscore.schools<-tapply(ndat$mathscore,ndat$school,mean,na.rm=TRUE)
id.schools<-names(avmscore.schools)
m<-length(id.schools)

par(mfrow=c(1,1))
plot(c(1,m),range(ndat$mathscore), type="n",ylab="math score",
     xlab="rank of  school-specific math score  average")

for(school in id.schools) 
{
  y<-ndat$mathscore[ndat$school==school]   
  x<-rank(avmscore.schools)[ id.schools==school]
  points( rep(x,length(y)), y,pch=16,cex=.6 )
  segments( x,min(y),x,max(y))
}

abline(h=mean(avmscore.schools))


###
mean(ndat[,3])
anova(lm(ndat[,3]~as.factor(ndat[,1])))
tapply(ndat[,3],ndat[,1],mean)
s2.schools<-tapply(ndat[,3],ndat[,1],var)
# we calculated the we get at weighted average of sample variances: $\hat{\sigma}^2 = 84.53$???
lmer( ndat[,3]~1+ (1|as.factor(ndat[,1])) ) # this gives the across-school variance. 

###
dev.off()
#####


##### school averages by sample size
postscript("fig1_4.ps",family="Times",height=4,width=8, horizontal=F)
par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))

# school-specific sample sizes
ss.schools<-tapply(!is.na(ndat$mathscore),ndat$school,sum )
plot(ss.schools,avmscore.schools,xlab="sample size",ylab="sample average")
abline(h=mean(avmscore.schools))
dev.off()
#####


##### math score by SES
BETA<-NULL
for(school in id.schools)
{
  y<-ndat$mathscore[ndat$school==school]
  x<-ndat$ses[ndat$school==school]
  beta<-lm(y~x)$coef
  BETA<-rbind(BETA,beta) 
}

postscript("fig1_5.ps",family="Times",height=3,width=8, horizontal=F)
par(mfrow=c(1,3),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(range(ndat$ses),range(ndat$mathscore),type="n",
     xlab="SES",ylab="math score")
apply(BETA,1,abline,col="gray")
abline(apply(BETA,2,mean),lwd=2)

plot(ss.schools,BETA[,1],xlab="sample size",ylab="intercept")
abline(h=mean(BETA[,1]))
plot(ss.schools,BETA[,2],xlab="sample size",ylab="slope")
abline(h=mean(BETA[,2]))
abline(h=0,col="gray")
dev.off()
#####






##### red state blue state data 
library(foreign)
# there are some problems reading this data set directly from online:
# http://privatewww.essex.ac.uk/~caox/teaching/teaching.htm
# so save it into your own working directory first and read in:
# for example, my data are saved at "C:\XunCao\Teaching\Method_Stat\multilevel\Lec\lec1"
# so: 
data.ind<- read.dta("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec1/2004_labeled_processed_race.dta",convert.factors=F)
# notice that we need to switch "\" in the path into "/". 

# dump("data.ind", "2004_labeled_processed_race")
# data.ind<-dget("2004_labeled_processed_race")


names.state<-c(
 "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA",
 "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
 "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT",
 "VA","WA","WV","WI","WY")

# individual-level data
state.ind<-data.ind$state
vrepub.ind<-data.ind$y
income.ind<-data.ind$income

# state-level data
vrepub.state<-tapply(vrepub.ind,state.ind,mean,na.rm=TRUE)
income.state<-tapply(income.ind,state.ind,mean,na.rm=TRUE)


# state average income vs. state support:
postscript("fig1_6.ps",family="Times",height=4,width=8, horizontal=F)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))

# state level plots
par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(income.state,vrepub.state,
     xlab="state average income",ylab="percentage voting republican",type="n")
text(income.state,vrepub.state,names.state,cex=.7)
abline(lm(vrepub.state~income.state))

dev.off()


# state level regressions

# state level plots
postscript("fig1_7.ps",family="Times",height=4,width=8, horizontal=F)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))


plot(range(income.state,na.rm=TRUE),c(.2,.8),type="n",
  xlab="individual income",ylab="estimated probability of voting republican")

BETA<-NULL
x.support<-seq(min(income.ind,na.rm=TRUE),max(income.ind,na.rm=TRUE),length=100)
for(state in 1:50) 
{
  y<-vrepub.ind[state.ind==state] ; x<-income.ind[state.ind==state]
  BETA<-rbind(BETA,glm( y~x,family=binomial)$coef)
  lines(x.support,1/(1+exp(-BETA[state,1]-BETA[state,2]*x.support))  )
}

hist(BETA[,2],main="",xlab=expression(beta[1])) ; abline(v=0,lwd=2,col="gray")
sum(BETA[,2]<0) 

dev.off()
