#### Lecture 5 R code ####
## The usual warning applies.##


EMdata <- read.table(file="EpstMershData.txt",    
                     header=TRUE)       
EMdata <- na.omit(EMdata)
attach(EMdata)
head(EMdata)

n <- length(CLlib)
mod <- lm(CLlib~SCscore)
cor(CLlib,SCscore)
cor(CLlib,SCscore)^2
mod$coef[2]
cor(CLlib,SCscore)*(sd(CLlib)/sd(SCscore))

pdf(file="justicePlot.pdf",bg="white")
plot(x=SCscore,y=CLlib,type="n")
text(x=SCscore,y=CLlib,labels=Justice)
abline(mod,col="red")
abline(a=mean(CLlib) - sd(CLlib)/sd(SCscore)*mean(SCscore),b=sd(CLlib)/sd(SCscore),col="green")
dev.off()

pdf(file="residuals.pdf",bg="white")
plot(x=SCscore,y=CLlib,type="n")
text(x=SCscore,y=CLlib,labels=Justice)
abline(mod,col="red")
segments(x0=SCscore,y0=CLlib,x1=SCscore,y1=predict(mod),col="green")
dev.off()

pdf(file="lmPlot.pdf",bg="white")
plot(x=SCscore,y=CLlib,type="n")
text(x=SCscore,y=CLlib,labels=Justice)
abline(mod,col="red")
dev.off()

### Least Squares Derivation Plots ###

## Sample Average ##

y <- CLlib
mu.tilde <- seq(0,100,.5)
S <- function(mu.tilde,y){
	sum((y-mu.tilde)^2)
	}
S.mu.tilde <- rep(0,length(mu.tilde))
for(i in 1:length(mu.tilde)){
	S.mu.tilde[i] <- S(mu.tilde[i],y)
	}

pdf("objectiveFunction1.pdf")
plot(mu.tilde,S.mu.tilde,type="l",lwd=2,col="red",xlab= expression(tilde(mu)),ylab=expression(paste("S(",tilde(mu),")")),main="Sum of Squared Residuals")
dev.off()

S1 <- function(mu.tilde,y){
	sum(-2*y + 2*mu.tilde)
	}

pdf("objectiveFunction2.pdf")
plot(mu.tilde,S.mu.tilde,type="l",lwd=2,col="red",xlab= expression(tilde(mu)),ylab=expression(paste("S(",tilde(mu),")")),main="Sum of Squared Residuals")
abline(a=S(mu.tilde=20,y=y) - 20*S1(mu.tilde=20,y=y),b=S1(mu.tilde=20,y=y),lwd=2,col="blue")
text(x=20,y=10010,labels="|",col="blue",cex=2)
dev.off()

pdf("objectiveFunction3.pdf")
plot(mu.tilde,S.mu.tilde,type="l",lwd=2,col="red",xlab= expression(tilde(mu)),ylab=expression(paste("S(",tilde(mu),")")),main="Sum of Squared Residuals")
abline(a=S(mu.tilde=35,y=y) - 35*S1(mu.tilde=35,y=y),b=S1(mu.tilde=35,y=y),lwd=2,col="blue")
text(x=35,y=10010,labels="|",col="blue",cex=2)
dev.off()

pdf("objectiveFunction4.pdf")
plot(mu.tilde,S.mu.tilde,type="l",lwd=2,col="red",xlab= expression(tilde(mu)),ylab=expression(paste("S(",tilde(mu),")")),main="Sum of Squared Residuals")
abline(a=S(mu.tilde=mean(CLlib),y=y) - mean(CLlib)*S1(mu.tilde=mean(CLlib),y=y),b=S1(mu.tilde=mean(CLlib),y=y),lwd=2,col="blue")
text(x=mean(CLlib),y=10010,labels="|",col="blue",cex=2)
dev.off()

## Simple OLS ##

residFunc <- function(a,b,y,x){
	sq.resid <- (y - (a + b*x))^2
	return(sum( sq.resid ))
	}

residFunc(a=27.56,b=43.15,y=CLlib,x=SCscore)

residFunc(a=28,b=43.15,y=CLlib,x=SCscore)
residFunc(a=27.56,b=43,y=CLlib,x=SCscore)


a.seq <- seq(25,30,.1)
b.seq <- seq(40,45,.1)
SSEmat <- matrix(0,nr=length(a.seq),nc=length(b.seq))
for(i in 1:length(a.seq)){
	for(j in 1:length(b.seq)){
		SSEmat[i,j] <- residFunc(a=a.seq[i],b=b.seq[j],y=CLlib,x=SCscore)
		}
	}

#par(mar=c(1,1,1,1))
pdf("OLSobjectiveFunction1.pdf")
image(a.seq,b.seq,SSEmat,xlab=expression(tilde(beta)[0]),ylab=expression(tilde(beta)[1]),labcex=1.5,main = "Sum of Squared Residuals")
contour(a.seq,b.seq,SSEmat,add=TRUE)
dev.off()

pdf("OLSobjectiveFunction2.pdf")
persp(a.seq,b.seq,SSEmat,theta=0,phi=40,col="lightblue",shade=.3,border=NA,zlab="SSR",xlab="beta0",ylab="beta1")
dev.off()

pdf("OLSobjectiveFunction3.pdf")
image(a.seq,b.seq,SSEmat,xlab=expression(tilde(beta)[0]),ylab=expression(tilde(beta)[1]),labcex=1.5)
contour(a.seq,b.seq,SSEmat,add=TRUE)
points(x=27.56,y=43.15,col="blue")
abline(v=27.56,col="blue")
abline(h=43.15,col="blue")
dev.off()

### Goodness of Fit ###

pdf(file="R2Plot.pdf",bg="white")
plot(x=SCscore,y=CLlib,type="n")
text(x=SCscore,y=CLlib,labels=Justice)
abline(mod,col="red",lwd=2)
abline(h=mean(CLlib),col="green",lwd=2)
dev.off()


### Inference Section ###

load("HW3.RData")
usa <- money[issp$country == "USA United States",,]

usa.ceo <- data.frame(usa[,"CEO",])
n <- dim(usa.ceo)[1]
summary(usa.ceo)

mod.ceo <- lm(usa.ceo$ideal ~ usa.ceo$perceived)
pdf("ceo.pdf")
plot(usa.ceo$perceived,usa.ceo$ideal,main="Regression of Ideal on Perceived Income",xlab="Perceived Income",ylab="Ideal Income")
abline(mod.ceo,lwd=2,col="red")
dev.off()

### Confidence Intervals ###

qt(p=.975,df=n-2)
confint(mod.ceo)
### Tests ###
summary(mod.ceo)
t.obs <- (.421 - 1)/.020
pt(q=t.obs,df=n-2)

### Diagnostics ###

## Nonlinearity ##
pdf("diagLin1.pdf")
plot(usa.ceo$perceived,usa.ceo$ideal,main="Regression of Ideal on Perceived Income",xlab="Perceived Income",ylab="Ideal Income")
abline(mod.ceo,lwd=2,col="red")
lines(lowess(usa.ceo$perceived,usa.ceo$ideal,f=1/3,iter=0),col="gold",lwd=2)
dev.off()

pdf("diagNCVar1.pdf")
plot(fitted(mod.ceo),residuals(mod.ceo))
abline(h=0)
dev.off()

pdf("diagNCVar2.pdf")
plot(mod.ceo,which=3)
dev.off()

pdf("diagNorm.pdf")
plot(mod.ceo,which=2)
dev.off()

## Normal Q-Q plots ##

pdf("normQQ.pdf")
par(mfrow=c(1,2))
set.seed(123)
x.seq = seq(-3,3,.1)
plot(x.seq,dnorm(x.seq),type="l",xlab="Z",ylab="f(z)",main="Standard Normal Quantiles")
quants <- qnorm(p=seq(from=1/10,to=9/10,by=1/10))
abline(v=quants,lwd=2)
u.sam <- sample(residuals(mod.ceo),size=9)
u.sam.norm <- (u.sam - mean(u.sam))/sd(u.sam)
plot(quants,sort(u.sam.norm),xlab="Theoretical Quantiles",ylab="Sample Quantiles", main="Q-Q norm",xlim=c(-3,3),ylim=c(-3,3))
points(x=quants,y=rep(-3,9),pch="|",cex=2)
dev.off()

##


perceived.log <- log(usa.ceo$perceived)
ideal.log <- log(usa.ceo$ideal)

mod.ceo.log <- lm(ideal.log ~ perceived.log)
plot(ideal.log ~ perceived.log)
abline(mod.ceo.log,lwd=2,col="red")

plot(mod.ceo.log,which=1)
plot(mod.ceo.log,which=3)
plot(mod.ceo.log,which=2)

confint(mod.ceo.log)
summary(mod.ceo.log)

## Nonlinearity ##
pdf("diagLin1Log.pdf")
plot(ideal.log ~ perceived.log,main="Regression of Log Ideal on Log Perceived Income",xlab="Log Perceived Income",ylab="Log Ideal Income")
abline(mod.ceo.log,lwd=2,col="red")
lines(lowess(perceived.log,ideal.log,f=1/3,iter=0),col="gold",lwd=2)
dev.off()

pdf("diagNCVar1Log.pdf")
plot(fitted(mod.ceo.log),residuals(mod.ceo.log))
abline(h=0)
dev.off()

pdf("diagNCVar2Log.pdf")
plot(mod.ceo.log,which=3)
dev.off()

pdf("diagNormLog.pdf")
plot(mod.ceo.log,which=2)
dev.off()


