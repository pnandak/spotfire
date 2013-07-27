##### R code for Lecture 3 #####
## This produces some of the plots from the lecture.
## Use at your own risk! I have not sufficiently commented
## or debugged this code. 


nes <- read.table(file="nes96r.dat",header=TRUE) #Note: the data is in my working directory.
attach(nes)
n <- length(income)

pdf(file="income.pdf",height=6,width=6,bg="white")
hist(x=income,prob=TRUE,ylim=c(-.01,.09))
dev.off()

pdf(file="incomeInf.pdf",height=6,width=6,bg="white")
hist(x=income,prob=TRUE,ylim=c(-.01,.09))
lines(density(x=income,bw=5),col="blue",lwd=2)
legend(x=1,y=.08,legend=c("Population Density"),lwd=c(2),col=c("blue"))
dev.off()

densX <- density(x=income,bw=5)$x
densY <- density(x=income,bw=5)$y
EV <- weighted.mean(x=densX,w=densY)


pdf(file="popMean.pdf",height=6,width=6,bg="white")
hist(x=income,prob=TRUE,ylim=c(-.01,.09))
lines(density(x=income,bw=5),col="blue",lwd=2)
points(x=EV,y=-.005,pch=24,cex=3,lwd=2,col="blue")
legend(x=1,y=.08,legend=c("Density Balance Point"),pch=c(24),col=c("blue"))
dev.off()



### Sampling Distributions ###
N <- 10000
nobs <- 30
set.seed(12345)
Ymat <- replicate(n=N,expr = sample(x=densX,size = nobs ,replace=TRUE,prob=densY))
muHat1 <- Ymat[1,]
muHat2 <- apply(X=Ymat[c(1,nobs),],MARGIN=2,FUN=mean)
muHat3 <- rep(7,N)
muHat4 <- apply(X=Ymat,MARGIN=2,FUN=mean)

pdf(file="samDist.pdf",height=6,width=6,bg="white")
par(mfrow=c(2,2))
hist(x=muHat1,prob=TRUE,ylim=c(-.02,.09),main="")
points(x=mean(muHat1),y=-.013,pch=24,cex=3,lwd=2)
points(x=EV,y=-.017,pch=24,cex=3,lwd=2,col="blue")
hist(x=muHat2,prob=TRUE,ylim=c(-.02,.1),main="")
points(x=mean(muHat2),y=-.013,pch=24,cex=3,lwd=2)
points(x=EV,y=-.017,pch=24,cex=3,lwd=2,col="blue")
plot(x=muHat3,y=rep(1,N),pch=19,xlim = c(min(income),max(income)),ylim=c(-.15,1.1),ylab="Mass")
segments(x0=7,y0=0,x1=7,y1=1)
points(x=mean(muHat3),y=-.09,pch=24,cex=3,lwd=2)
points(x=EV,y=-.09,pch=24,cex=3,lwd=2,col="blue")
hist(x=muHat4,prob=TRUE,ylim=c(-.09,.4),main="")
points(x=mean(muHat4),y=-.05,pch=24,cex=3,lwd=2)
points(x=EV,y=-.07,pch=24,cex=3,lwd=2,col="blue")
dev.off()

##### MSE ########


n <- 100
nSets <- 10000
S0squared <- rep(0,nSets)
S1squared <- rep(0,nSets)
for(s in 1:nSets){
	x <- rnorm(5,mean=0,sd=1)
	S0squared[s] <- ((n-1)/n)*var(x)
	S1squared[s] <- var(x)	
	}
	
mean(S0squared -1)
mean(S1squared -1)

var(S0squared)
var(S1squared)

mean((S0squared -1)^2)
mean((S1squared -1)^2)


##### Large Sample Properties ####

### Asymptotic Bias ###


mu <- 2 
x.seq <- seq(0,4,.01)
pdf("asymbias.pdf",width=12)
par(mfrow=c(1,3))
plot(x= x.seq, y = dnorm(x=x.seq,mean=mu + 1),type="l",main="n = 1", xlab=expression(hat(theta)),ylab= expression(paste("f(",hat(theta),")")  ) ,cex.main=2, cex.lab = 2)
plot(x= x.seq, y = dnorm(x=x.seq,mean=mu+ 1/sqrt(10),sd=1),type="l",main="n = 10", xlab=expression(hat(theta)),ylab= expression(paste("f(",hat(theta),")")  ),cex.main=2, cex.lab = 2 )
plot(x= x.seq, y = dnorm(x=x.seq,mean=mu+ 1/sqrt(100),sd=1),type="l",main="n = 100", xlab=expression(hat(theta)),ylab= expression(paste("f(",hat(theta),")")  ) ,cex.main=2, cex.lab = 2)
dev.off()



#############################

nes2004 <- read.csv("nes04dat.txt",header=T) 

pid <- nes2004$V043116
pid <- pid[pid < 7]
table(pid)/length(pid)
mean(pid)

n <- 10
nSim <- 10
pdf("pidinterval.pdf")
plot(x=0,y=1,type="n",xlim=c(0,6), ylim=c(1,nSim),xlab=expression(mu), ylab="sample",main="Interval Estimates")
for(i in 1:nSim){
x <- sample(x=(0:6),size=n,replace=T,prob=table(pid)/length(pid))
intervalEst <- c(mean(x)-sd(x),mean(x)+sd(x))
segments(intervalEst[1],i,intervalEst[2],i)
}
dev.off()

## Feeling Thermometer ##

jeFTS <- nes2004$V043042
jeFTS <- jeFTS[jeFTS <= 100]
hcFTS <- nes2004$V043044
hcFTS <- hcFTS[hcFTS <= 100]

pdf("FTShist.pdf")
par(mfrow=c(2,1))
hist(hcFTS)
hist(jeFTS)
dev.off()

n <- 10
nSim <- 10
pdf("ftsSD.pdf")
par(mfrow=c(2,1))
plot(x=0,y=1,type="n",xlim=c(0,100), ylim=c(1,nSim),xlab=expression(hat(mu)), ylab="sample",main="Clinton FTS Mean Interval Estimates")
for(i in 1:nSim){
x <- sample(x=hcFTS,size=n,replace=T)
intervalEst <- c(mean(x)-sd(x),mean(x)+sd(x))
segments(intervalEst[1],i,intervalEst[2],i)
}
plot(x=0,y=1,type="n",xlim=c(0,100), ylim=c(1,nSim),xlab=expression(hat(mu)), ylab="sample",main="Edwards FTS Mean Interval Estimates")
for(i in 1:nSim){
x <- sample(x=jeFTS,size=n,replace=T)
intervalEst <- c(mean(x)-sd(x),mean(x)+sd(x))
segments(intervalEst[1],i,intervalEst[2],i)
}
dev.off()

## Normal/t- intervals ##
n.hc <- length(hcFTS)
hc.int <- mean(hcFTS) + c(-1,1)*qt(p=.975,df=n.hc-1)*sd(hcFTS)/sqrt(n.hc)
n.je <- length(jeFTS)
je.int <- mean(jeFTS) + c(-1,1)*qt(p=.975,df=n.hc-1)*sd(jeFTS)/sqrt(n.je)
pdf("hcjeCI.pdf")
plot(x=0,y=1,type="n",xlim=c(40,60), ylim=c(0,3),xlab=expression(hat(mu)),ylab="",main="Clinton and Edwards 95% CIs")
segments(hc.int[1],2,hc.int[2],2,lwd=2,col="red")
segments(je.int[1],1,je.int[2],1,lwd=2,col="blue")
legend(42,2.5,legend=c("Clinton","Edwards"),lwd=2,col=c("red","blue"))
dev.off()

### Null Distribution of the t-statistic ###



x.seq <- seq(-3,3,.1)
y.seq <- dt(x=x.seq,df=n.je - 1)
pdf("null.pdf")
plot(x.seq,y.seq,type="l", lwd=2,col="blue",xlab="test statistic",ylab="f(test statistic)",main="Null Distribution")
dev.off()

### p-value plot ###
t.test(jeFTS,alternative="two.sided",mu=55)

pdf("pvalue.pdf")
par(mfrow=c(2,1))
plot(x.seq,y.seq,type="l", lwd=2,col="blue",xlab="test statistic",ylab="f(test statistic)",main="Two Sided p-value")
abline(v=.4613,col="red", lwd=2)
abline(v=-.4613,col="green", lwd=2)
legend(-3,.35,legend=c("t-obs","-t-obs"),lwd=2,col=c("red","green"))

plot(x.seq,y.seq,type="l", lwd=2,col="blue",xlab="test statistic",ylab="f(test statistic)",main="One Sided p-value")
abline(v=.4613,col="red", lwd=2)
legend(-3,.35,legend=c("t-obs"),lwd=2,col=c("red"))
dev.off()

pdf("rejreg.pdf")
par(mfrow=c(2,1))
plot(x.seq,y.seq,type="l", lwd=2,col="blue",xlab="test statistic",ylab="f(test statistic)",main=expression(paste("Two Sided Rejection Region (",alpha,"=5%)")))
abline(v=c(-1,1)*qt(p=.975,df=n.je-1),col="blue", lwd=2)
abline(v=.4613,col="red", lwd=2)
#abline(v=.4613,col="red")
#abline(v=-.4613,col="green")
legend(-3,.35,legend=c("fences","t-obs"),lwd=2,col=c("blue","red"))

plot(x.seq,y.seq,type="l", lwd=2,col="blue",xlab="test statistic",ylab="f(test statistic)",main=expression(paste("One Sided Rejection Region (",alpha,"=5%)")))
abline(v=qt(p=.95,df=n.je-1),col="blue", lwd=2)
abline(v=.4613,col="red", lwd=2)
legend(-3,.35,legend=c("fence","t-obs"),lwd=2,col=c("blue","red"))
dev.off()

pdf("rejregpvalue.pdf")
par(mfrow=c(2,1))
plot(x.seq,y.seq,type="l", lwd=2,col="blue",xlab="test statistic",ylab="f(test statistic)",main=expression(paste("Two Sided Rejection Region (",alpha,"=5%)")))
abline(v=c(-1,1)*qt(p=.975,df=n.je-1),col="blue", lwd=2)
abline(v=.4613,col="red", lwd=2)
#abline(v=.4613,col="red")
abline(v=-.4613,col="green", lwd=2)
legend(-3,.35,legend=c("fences","t-obs","-t-obs"),lwd=2,col=c("blue","red","green"))

plot(x.seq,y.seq,type="l", lwd=2,col="blue",xlab="test statistic",ylab="f(test statistic)",main=expression(paste("One Sided Rejection Region (",alpha,"=5%)")))
abline(v=qt(p=.95,df=n.je-1),col="blue", lwd=2)
abline(v=.4613,col="red", lwd=2)
legend(-3,.35,legend=c("fence","t-obs"),lwd=2,col=c("blue","red"))
dev.off()

x.seq <- seq(50,60,.1)
y.seq <- dnorm(x.seq,mean=55,sd=sd(jeFTS)/sqrt(n.je))


pdf("rejregCI.pdf")
plot(x.seq,y.seq,type="l", lwd=2,col="blue",xlab=expression(bar(X)),ylab=expression(paste("f(",bar(X),"|H 0)")),main=expression(paste("Rejection Regions and CIs (",alpha,"=5%)")))
abline(v=55 + c(-1,1)*qt(p=.975,df=n.je-1)*sd(jeFTS)/sqrt(n.je)
,col="blue", lwd=2)
abline(v=mean(jeFTS),col="red", lwd=2)
segments(je.int[1],.2,mean(jeFTS),.2,col="red", lwd=2)
segments(je.int[2],.2,mean(jeFTS),.2,col="red", lwd=2)
segments(je.int[1],.18,je.int[1],.22,col="red", lwd=2)
segments(je.int[2],.18,je.int[2],.22,col="red", lwd=2)
legend(50,.35,legend=c("fences","CI"),lwd=2,col=c("blue","red"))
dev.off()






