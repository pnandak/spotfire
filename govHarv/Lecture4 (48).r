#### Lecture 4 R code ####
## The usual warnings apply ##

#### Muslim/Democracy Data ####
library(foreign)
fishdata <- read.spss("Fish_data.sav",
                    use.value.labels=FALSE,
                    to.data.frame=TRUE)
names(fishdata)
fishdata = na.omit(fishdata)
attach(fishdata)

pdf("easyReg1.pdf")
plot(MUSLIM,FHREVERS,xlim=c(-.5,1.5),main="Regression of FH Score on Muslim Cat.")
points(0,mean(FHREVERS[MUSLIM == 0]),col="red",pch=19,cex=3)
points(1,mean(FHREVERS[MUSLIM == 1]),col="red",pch=19,cex=3)
dev.off()

pdf("easyReg2.pdf")
plot(MUSLIM,FHREVERS,xlim=c(-.5,1.5),main="Regression of FH Score on Muslim Cat.")
points(0,mean(FHREVERS[MUSLIM == 0]),col="red",pch=19,cex=3)
points(1,mean(FHREVERS[MUSLIM == 1]),col="red",pch=19,cex=3)
abline(lm(FHREVERS~MUSLIM),col="red",lwd=2)
dev.off()


#### Romanian Rebellion Data ####

library(car)
data(Chirot)
dim(Chirot)
attach(Chirot)

pdf("plotChirot1.pdf")
plot(inequality,intensity,main="Regression of Intensity on Inequality")
dev.off()

pdf("plotChirot2.pdf")
plot(inequality,intensity,main="Regression of Intensity on Inequality")
abline(lm(intensity~inequality),col="red",lwd=2)
dev.off()


summary(intensity)

pdf("transLinChirot.pdf")
par(mfrow=c(1,2))
plot(inequality, log(intensity+2))
abline(lm(log(intensity+2)~inequality),col="red",lwd=2)
plot(log(inequality), log(intensity+2))
abline(lm(log(intensity+2)~ log(inequality)),col="red",lwd=2)
dev.off()

summary(lm(log(intensity+2)~inequality))
summary(lm(log(intensity+2)~ log(inequality)))

knots <- quantile(inequality)[2:4]
knots[1] = knots[1]-.01
knots[3] = knots[3] + .01

ineq1 <- inequality < knots[1]
ineq2 <- inequality > knots[1] & inequality <= knots[2]
ineq3 <- inequality > knots[2] & inequality <= knots[3]
ineq4 <- inequality > knots[3]

n <- length(inequality)
ineqFac <- rep(0,n)
ineqFac[ineq1] <- 1
ineqFac[ineq2] <- 2
ineqFac[ineq3] <- 3
ineqFac[ineq4] <- 4	
	
const <- tapply(intensity,ineqFac,mean)

pdf("constChirot.pdf", height=6, width=6, bg="white")
plot(inequality, intensity,cex=1.2)
abline(v=knots,col="green",lwd=2)
segments(.2,const[1],knots[1],const[1],lwd=2,col="red")
segments(knots[1],const[2],knots[2],const[2],lwd=2,col="red")
segments(knots[2],const[3],knots[3],const[3],lwd=2,col="red")
segments(knots[3],const[4],.9,const[4],lwd=2,col="red")
dev.off()

pdf("kernel1a.pdf", height=6,width=6,bg="white")
plot(inequality, intensity)
lines(seq(.2,.8,.001),1/10*dunif(seq(.2,.8,.001),.4,.5) -2,lwd=3,col="green")
text(x=.45,y=-1.8,labels=expression(x),cex=1.3,col="red")
points(.45,mean(intensity[inequality >.4 & inequality <= .5]),col="red",cex=2,pch=19) 
dev.off()

pdf("kernel1b.pdf", height=6,width=6,bg="white")
plot(inequality, intensity)
lines(seq(.2,.8,.001),1/10*dunif(seq(.2,.8,.001),.45,.55) -2,lwd=3,col="green")
points(.45,mean(intensity[inequality >.4 & inequality <= .5]),col="red",cex=2,pch=19) 
text(x=.5,y=-1.8,labels=expression(x),cex=1.3,col="red")
points(.5,mean(intensity[inequality >.45 & inequality <= .55]),col="red",cex=2,pch=19) 
dev.off()

pdf("kernel1c.pdf", height=6,width=6,bg="white")
plot(inequality, intensity)
lines(seq(.2,.8,.001),1/10*dunif(seq(.2,.8,.001),.5,.6) -2,lwd=3,col="green")
points(.45,mean(intensity[inequality >.4 & inequality <= .5]),col="red",cex=2,pch=19) 
points(.5,mean(intensity[inequality >.45 & inequality <= .55]),col="red",cex=2,pch=19) 
text(x=.55,y=-1.8,labels=expression(x),cex=1.3,col="red")
points(.55,mean(intensity[inequality >.5 & inequality <= .6]),col="red",cex=2,pch=19) 
dev.off()

pdf("kernel1d.pdf", height=6,width=6,bg="white")
plot(inequality, intensity)
lines(ksmooth(inequality,intensity, kernel="box",bandwidth=.1),col="red",lwd=2)
dev.off()

pdf("kernel1e.pdf", height=6,width=6,bg="white")
plot(inequality, intensity)
lines(seq(.2,.8,.001),1/5*dunif(seq(.2,.8,.001),.3,.6) -2,lwd=3,col="green")
text(x=.45,y=-1.8,labels=expression(x),cex=1.3,col="red")
points(.45,mean(intensity[inequality >.3 & inequality <= .6]),col="red",cex=2,pch=19) 
dev.off()

pdf("kernel1f.pdf", height=6,width=6,bg="white")
plot(inequality, intensity)
lines(ksmooth(inequality,intensity, kernel="box",bandwidth=.1),col="red",lwd=2)
lines(ksmooth(inequality,intensity, kernel="box",bandwidth=.2),col="gold",lwd=2)
dev.off()

pdf("kernel2a.pdf", height=6,width=6,bg="white")
plot(inequality, intensity)
lines(seq(.2,.8,.001),.25*dnorm(seq(.2,.8,.001),.45,.05) -2,lwd=3,col="green")
text(x=.45,y=-1.8,labels=expression(x),cex=1.3,col="red")
dev.off()

pdf("kernel2b.pdf", height=6,width=6,bg="white")
plot(inequality, intensity)
lines(ksmooth(inequality,intensity,kernel="normal",bandwidth=.1),col="red",lwd=2)
dev.off()

pdf("kernel2c.pdf", height=6,width=6,bg="white")
plot(inequality, intensity)
lines(ksmooth(inequality,intensity,kernel="normal",bandwidth=.05),col="blue",lwd=2)
lines(ksmooth(inequality,intensity,kernel="normal",bandwidth=.1),col="red",lwd=2)
lines(ksmooth(inequality,intensity,kernel="normal",bandwidth=.2),col="gold",lwd=2)
dev.off()

ineq1 <- inequality[inequality < knots[1]]
ineq2 <- inequality[inequality > knots[1] & inequality <= knots[2]]
ineq3 <- inequality[inequality > knots[2] & inequality <= knots[3]]
ineq4 <- inequality[inequality > knots[3]]

inten1 <- intensity[inequality < knots[1]]
inten2 <- intensity[inequality > knots[1] & inequality <= knots[2]]
inten3 <- intensity[inequality > knots[2] & inequality <= knots[3]]
inten4 <- intensity[inequality > knots[3]]

pdf("linBinChirot.pdf", height=6, width=6, bg="white")
plot(inequality, intensity,cex=1.2)
abline(v=knots,col="green",lwd=2)
a = lm(inten1~ineq1)$coef[1]
b = lm(inten1~ineq1)$coef[2]
segments(min(ineq1),a+ b*min(ineq1) ,knots[1],a+ b*knots[1],col="red",lwd=2)
a = lm(inten2~ineq2)$coef[1]
b = lm(inten2~ineq2)$coef[2]
segments(knots[1],a+ b*knots[1] ,knots[2],a+ b*knots[2],col="red",lwd=2)
a = lm(inten3~ineq3)$coef[1]
b = lm(inten3~ineq3)$coef[2]
segments(knots[2],a+ b*knots[2] ,knots[3],a+ b*knots[3],col="red",lwd=2)
a = lm(inten4~ineq4)$coef[1]
b = lm(inten4~ineq4)$coef[2]
segments(knots[3],a+ b*knots[3] ,max(ineq4),a+ b*max(ineq4),col="red",lwd=2)
dev.off()

pdf("lowessChirot1.pdf")
plot(inequality, intensity,cex=1.2)
lines(lowess(inequality,intensity,f=2/3,iter=0),col="red",lwd=2)
dev.off()

pdf("lowessChirot2.pdf")
plot(inequality, intensity,cex=1.2)
lines(lowess(inequality,intensity,f=2/3,iter=0),col="red",lwd=2)
lines(lowess(inequality,intensity,f=2/3,iter=20),col="gold",lwd=2)
dev.off()


plot(tradition, intensity)

pdf("lowessChirot3.pdf")
cp = 85
plot(inequality, intensity,cex=1.2, type="n")
points(inequality[tradition >= cp], intensity[tradition >= cp],col="blue")
points(inequality[tradition < cp], intensity[tradition < cp],col="red")
lines(lowess(inequality,intensity,f=2/3,iter=0),col="red",lwd=2)
lines(lowess(inequality,intensity,f=2/3,iter=20),col="gold",lwd=2)
dev.off()

pdf("avPlot.pdf")
av.plot(model = lm(intensity~inequality+commerce), variable=inequality)
dev.off()

######## Bias/Variance Section ############


x <- seq(min(inequality),max(inequality),.01)

a = lm(log(intensity+2)~inequality)$coef[1]
b = lm(log(intensity+2)~inequality)$coef[2]
mu = exp(a+b*1.1*x) -2

pdf("trueReg.pdf")
plot(inequality, intensity)
lines(x,mu,lwd=2,col="blue")
dev.off()

y <-  seq(min(intensity),max(intensity),.01)
z <- outer(y,mu,dnorm)



pdf("imagecontour.pdf",, height=6, width=6, bg="white")
image(x,y,t(z),xlab="x",ylab="y")
contour(x,y,t(z),xlab="x",ylab="y",add=T)
lines(x,mu,lwd=2,col="blue")
points(inequality, intensity)
dev.off()

pdf("perspective.pdf",, height=6, width=6, bg="white")
persp(x,y,t(z),theta=0,phi=40,expand=0.5,col="lightblue",zlab="f(y|x)", shade = .3, border = NA)
dev.off()


## Simulating Data ##

mu.simset = exp(a+b*1.1*inequality) -2
set.seed(123)
y <- rnorm(n=length(inequality),mean=mu.simset,sd=1)

pdf("simData.pdf")
plot(inequality,intensity,ylab="intensity",type="n")
points(inequality,y)
lines(x,mu,col="blue",lwd=2)
dev.off()


### In Class Demonstration ###
mu.simset = exp(a+b*1.1*inequality) -2
y <- rnorm(n=length(inequality),mean=mu.simset,sd=1)
plot(inequality,intensity,ylab="intensity",type="n")
points(inequality,y)
lines(x,mu,col="blue",lwd=2)
##################################


## Repeated Sampling ##
N <- 100
n <- length(inequality)
const <- matrix(0,nr=N,nc=4)
pdf("bias.pdf",, height=6, width=6, bg="white")
plot(inequality,intensity,ylab="intensity",type="n")
#points(inequality,y)
x <- seq(min(inequality),max(inequality),.01)
lines(x,mu,col="blue",lwd=2)
abline(v=knots,col="green",lwd=2)
set.seed(123)
for(s in 1:N){
	x <- inequality 
	mu.simset = exp(a+b*1.1*inequality) -2
	y <- rnorm(n=length(inequality),mean=mu.simset,sd=1)
 # simulate y values
	x1 <- x < knots[1]
	x2 <- x > knots[1] & x <= knots[2]
	x3 <- x > knots[2] & x <= knots[3]
	x4 <- x > knots[3]
	xFac <- rep(0,n)
	xFac[x1] <- 1
	xFac[x2] <- 2
	xFac[x3] <- 3
	xFac[x4] <- 4	
	const[s,] <- tapply(y,xFac,mean)
	segments(0,const[s,1],knots[1],const[s,1],col="red")
	segments(knots[1],const[s,2],knots[2],const[s,2],col="red")
	segments(knots[2],const[s,3],knots[3],const[s,3],col="red")
	segments(knots[3],const[s,4],2*pi,const[s,4],col="red")
}
EV <- apply(const,2,mean)
segments(0,EV[1],knots[1],EV[1],col="purple",lwd=3)
	segments(knots[1],EV[2],knots[2],EV[2],col="purple",lwd=3)
	segments(knots[2],EV[3],knots[3],EV[3],col="purple",lwd=3)
	segments(knots[3],EV[4],2*pi,EV[4],col="purple",lwd=3)
dev.off()

## Increased Variance ##

## Repeated Sampling ##
N <- 100
n <- length(inequality)
const <- matrix(0,nr=N,nc=8)
pdf("variance.pdf",, height=6, width=6, bg="white")
plot(inequality,intensity,ylab="intensity",type="n")
#points(inequality,intensity)
x <- seq(min(inequality),max(inequality),.01)
lines(x,mu,col="blue",lwd=2)
knots <- quantile(inequality,probs=seq(0,1,1/8))[2:8]
abline(v=knots,col="green",lwd=2)
set.seed(123)
for(s in 1:N){
	x <- inequality #generate x values 
	mu.simset = exp(a+b*1.1*inequality) -2
	y <- rnorm(n=length(inequality),mean=mu.simset,sd=1) # simulate y values
	x1 <- x < knots[1]
	x2 <- x > knots[1] & x <= knots[2]
	x3 <- x > knots[2] & x <= knots[3]
	x4 <- x > knots[3] & x <= knots[4]
	x5 <- x > knots[4] & x <= knots[5]
	x6 <- x > knots[5] & x <= knots[6]
	x7 <- x > knots[6] & x <= knots[7]
	x8 <- x > knots[7]
	xFac <- rep(0,n)
	xFac[x1] <- 1
	xFac[x2] <- 2
	xFac[x3] <- 3
	xFac[x4] <- 4
	xFac[x5] <- 5
	xFac[x6] <- 6	
	xFac[x7] <- 7
	xFac[x8] <- 8
	const[s,] <- tapply(y,xFac,mean)
	segments(0,const[s,1],knots[1],const[s,1],col="red")
	segments(knots[1],const[s,2],knots[2],const[s,2],col="red")
	segments(knots[2],const[s,3],knots[3],const[s,3],col="red")
	segments(knots[3],const[s,4],knots[4],const[s,4],col="red")
	segments(knots[4],const[s,5],knots[5],const[s,5],col="red")
	segments(knots[5],const[s,6],knots[6],const[s,6],col="red")
	segments(knots[6],const[s,7],knots[7],const[s,7],col="red")
	segments(knots[7],const[s,8],1,const[s,8],col="red")
}
EV <- apply(const,2,mean)
	segments(0,EV[1],knots[1],EV[1],col="purple",lwd=3)
	segments(knots[1],EV[2],knots[2],EV[2],col="purple",lwd=3)
	segments(knots[2],EV[3],knots[3],EV[3],col="purple",lwd=3)
	segments(knots[3],EV[4],knots[4],EV[4],col="purple",lwd=3)
	segments(knots[4],EV[5],knots[5],EV[5],col="purple",lwd=3)
	segments(knots[5],EV[6],knots[6],EV[6],col="purple",lwd=3)
	segments(knots[6],EV[7],knots[7],EV[7],col="purple",lwd=3)
	segments(knots[7],EV[8],1,EV[8],col="purple",lwd=3)

dev.off()


