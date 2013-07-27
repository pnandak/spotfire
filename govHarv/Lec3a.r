#### Lecture 3 - Feb. 8 ####

library(car)
attach(Florida)

buchshare <- round(BUCHANAN/Total*100, 2)

cbind(Florida[,c("BUCHANAN", "Total")], buchshare)

pdf("buchshare.pdf")
dotchart(sort(buchshare), lcolor="white", xlab="Buchanan Vote Share (%)")
dev.off()

cbind(Florida[,c("BUCHANAN", "Total")], buchshare)



mean(buchshare)
median(buchshare)

var(buchshare)
sd(buchshare)



buchdev <- data.frame(rownames(Florida), buchshare, abs(buchshare - mean(buchshare)), (buchshare - mean(buchshare))^2, abs(buchshare - median(buchshare)), (buchshare - median(buchshare))^2)
buchdev <- data.frame(rownames(Florida),round(buchdev[,-1], 2))
colnames(buchdev) <- c("County","Obs","AbsMean","SqMean","AbsMed","SqMed")


### Empirircal Rule Buchanan Data ###

UpperBound <- mean(buchshare) + 1*sd(buchshare)
UpperBound
LowerBound <- mean(buchshare) - 1*sd(buchshare)
LowerBound
mean(buchshare > LowerBound & buchshare < UpperBound)

pdf("buchshareEmpiricalRule.pdf")
dotchart(sort(buchshare), lcolor="white", xlab="Buchanan Vote Share (%)")
abline(v=c(LowerBound,UpperBound))
dev.off()

### Importing Data and Data Frames ###

library(foreign)
fishdata <- read.spss("Fish_data.sav",use.value.labels=FALSE,to.data.frame=TRUE)



fishdata
head(fishdata)
names(fishdata)
summary(fishdata)


MUSLIM # doesn't work

fishdata$MUSLIM

attach(fishdata)

MUSLIM # now it works

### Tables ###

?table
votes <- c(rep("Bush",12),rep("Kerry",11), rep("Nader",2))

table(votes)

table(MUSLIM)

MUSLIMfac <- factor(MUSLIM)
levels(MUSLIMfac) <- c("not muslim","muslim")

table(MUSLIMfac)

OPECfac <- factor(OPEC)
levels(OPECfac) <- c("not opec","opec")

table(MUSLIM,OPEC)
table(MUSLIMfac,OPECfac)

### Graphs ###

## Bar Plots ##

?barplot

barplot(table(votes),col=c("red","blue","green"))

pdf("bar.pdf")
barplot(table(MUSLIMfac))
dev.off()

pdf("barstack.pdf")
barplot(table(MUSLIMfac,OPECfac),col=c("red","blue"),legend.text=TRUE)
dev.off()

barplot(table(MUSLIMfac,OPECfac),col=c("red","blue"))

locator()

?legend
legend(x=1.6,y=120,legend= levels(MUSLIMfac),col=c("red","blue"),pch=15)

barplot(table(MUSLIMfac,OPECfac),col=c("red","blue"),legend.text=TRUE,beside = TRUE)

## Quantiles, Boxplots, and Quantile Plots ##
?quantile
quantile(FHREVERS)
quantile(FHREVERS,na.rm=TRUE)
boxplot(FHREVERS)

quantile(GDP90LGN,na.rm=TRUE)
bpstat <- boxplot(GDP90LGN)$stats


pdf("box1.pdf")
boxplot(GDP90LGN, at=.8, ylab="GDP90LGN")
dev.off()

pdf("box2.pdf")
boxplot(GDP90LGN, at=.8, ylab="GDP90LGN")
text(1.3, bpstat[3], "median", cex=1.4)
dev.off()

pdf("box3.pdf")
boxplot(GDP90LGN, at=.8, ylab="GDP90LGN")
text(1.3, bpstat[3], "median", cex=1.4)
text(1.3, bpstat[4], "upper quartile", cex=1.4)
text(1.3, bpstat[2], "lower quartile", cex=1.4)
dev.off()

pdf("box4.pdf")
boxplot(GDP90LGN, at=.8, ylab="GDP90LGN")
text(1.3, bpstat[3], "median", cex=1.4)
text(1.3, bpstat[4], "upper quartile", cex=1.4)
text(1.3, bpstat[2], "lower quartile", cex=1.4)
text(1.3, bpstat[5], "non-outlier max", cex=1.4)
text(1.3, bpstat[1]+.1, "non-outlier min", cex=1.4)
dev.off()

quantile(FHREVERS,probs=seq(from=0,to=100,length=length(na.omit(FHREVERS))),na.rm=TRUE)

x <- rt(n=30,df=4)

quantile(x)
boxplot(x,plot=FALSE)$stats
boxplot(x)

## Stem Leaf Plot ##

?stem

stem(FHREVERS)
sort(FHREVERS)
hist(FHREVERS)

stem(GDP90LGN)
sort(GDP90LGN)
hist(GDP90LGN, breaks=14)
 
## Histograms ##

?hist


pdf("histf.pdf")
hist(GDP90LGN,freq=TRUE)
dev.off()

pdf("histd.pdf")
hist(GDP90LGN,freq=FALSE)
dev.off()


pdf("histrug.pdf")
hist(GDP90LGN,freq=FALSE)
points(x = GDP90LGN,y=rep(0,length(GDP90LGN)),pch="|")
dev.off()


## Density Plots ##

?density
?plot.density

plot(density(GDP90LGN)) # What went wrong?

plot(density(GDP90LGN,na.rm=TRUE))

hist(GDP90LGN)
points(x = GDP90LGN,y=rep(0,length(GDP90LGN)),pch="|")
lines(density(GDP90LGN,na.rm=TRUE))

# What went wrong? #

hist(GDP90LGN, freq=FALSE)
points(x = GDP90LGN,y=rep(0,length(GDP90LGN)),pch="|")
lines(density(GDP90LGN,na.rm=TRUE))

mean(GDP90LGN < 2.5)

# What went wrong? #

mean(na.omit(GDP90LGN) < 2.5)

mean(na.omit(GDP90LGN) < 2.5)/.5 ## Looking at the plot, what do you expect this to be?

## Different Breaks ##

hist(GDP90LGN, breaks=c(1.5,1.9,2.5,3.5,5))

## histograms and density plots as n increases ##

n <- 1000
x <- rnorm(n)
hist(x,freq=FALSE)
lines(density(x))

## Empirical CDF ##

library(Hmisc)

?ecdf

ecdf(GDP90LGN)

sort(GDP90LGN)

## Empirical CDF as n increases ##

n <- 100
x <- rnorm(n)
ecdf(x)
sort(x)

## Histogram, Density, and Empirical CDF as n increases ##

n <- 10000
x <- rnorm(n)

par(mfrow=c(1,2))

# first plot
hist(x,freq=FALSE)
lines(density(x))

# second plot
ecdf(x)

### Convergence to Population Values ###

n <- 10
x <- rnorm(n,mean=0,sd=1) #true mean = 0, true sd = 1 
mean(x)
sd(x)

nSim <- 1000
meanVec <- rep(0,nSim)
sdVec <- rep(0,nSim)
for(n in 2:1000){
	x <- rnorm(n,mean=0,sd=1)
	meanVec[n] <- mean(x)
	sdVec[n] <- sd(x)
	}

plot(1:nSim,meanVec,type="l")
plot(1:nSim,sdVec,type="l")

#### Plots ####

# symmetric #

pdf("bP1.pdf")
exampleData <- c(2,3,3,4,4,4,5,5,6)
par(mar=c(4,4,2,2)+.1)
barplot(table(exampleData) ,ylim=c(-.25,3),col="red")
segments(x0=1.4,x1=2.4,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=1,y1=1,lwd=2)
segments(x0=3.8,x1=4.8,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=2,y1=2,lwd=2)
segments(x0=.2,x1=7.2,y0=0,y1=0,lwd=2)
points(x=3.1,y=-.15,pch=24,cex=3,lwd=2,col="blue")
dev.off()

# move one observation #
pdf("bP2.pdf")
exampleData <- c(2,3,3,4,4,4,5,6,7)
par(mar=c(4,4,2,2)+.1)
barplot(table(exampleData) ,ylim=c(-.25,3),col="red")
segments(x0=1.4,x1=2.4,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=2,y1=2,lwd=2)
segments(x0=.2,x1=7.2,y0=0,y1=0,lwd=2)
points(x=3.1+.22222222*(4.3-3.1),y=-.15,pch=24,cex=3,lwd=2,col="blue")
dev.off()

# move two #
pdf("bP3.pdf")
exampleData <- c(2,3,4,4,4,5,6,7,7)
par(mar=c(4,4,2,2)+.1)
barplot(table(exampleData) ,ylim=c(-.25,3),col="red")
segments(x0=2.6,x1=3.6,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=2,y1=2,lwd=2)
segments(x0=6.2,x1=7.2,y0=1,y1=1,lwd=2)
segments(x0=.2,x1=7.2,y0=0,y1=0,lwd=2)
points(x=3.1+.6666667*(4.3-3.1),y=-.15,pch=24,cex=3,lwd=2,col="blue")
dev.off()

### Median ###

# symmetric #

pdf("bP4.pdf")
exampleData <- c(2,3,3,4,4,4,5,5,6)
par(mar=c(4,4,2,2)+.1)
barplot(table(exampleData) ,ylim=c(-.25,3),col="red")
segments(x0=1.4,x1=2.4,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=1,y1=1,lwd=2)
segments(x0=3.8,x1=4.8,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=2,y1=2,lwd=2)
segments(x0=.2,x1=7.2,y0=0,y1=0,lwd=2)
points(x=3.1,y=-.15,pch=24,cex=3,lwd=2,col="blue")
abline(v=3.1,col="yellow")
legend(x=3.9,y=3,legend=c("mean","median"),pch=15,col=c("blue","yellow"),cex=1.75)
dev.off()

# move one observation #
pdf("bP5.pdf")
exampleData <- c(2,3,3,4,4,4,5,6,7)
par(mar=c(4,4,2,2)+.1)
barplot(table(exampleData) ,ylim=c(-.25,3),col="red")
segments(x0=1.4,x1=2.4,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=2,y1=2,lwd=2)
segments(x0=.2,x1=7.2,y0=0,y1=0,lwd=2)
points(x=3.1+.22222222*(4.3-3.1),y=-.15,pch=24,cex=3,lwd=2,col="blue")
abline(v=3.1,col="yellow")
legend(x=4.1,y=3,legend=c("mean","median"),pch=15,col=c("blue","yellow"),cex=2)
dev.off()

# move two #
pdf("bP6.pdf")
exampleData <- c(2,3,4,4,4,5,6,7,7)
par(mar=c(4,4,2,2)+.1)
barplot(table(exampleData) ,ylim=c(-.25,3),col="red")
segments(x0=2.6,x1=3.6,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=2,y1=2,lwd=2)
segments(x0=6.2,x1=7.2,y0=1,y1=1,lwd=2)
segments(x0=.2,x1=7.2,y0=0,y1=0,lwd=2)
points(x=3.1+.6666667*(4.3-3.1),y=-.15,pch=24,cex=3,lwd=2,col="blue")
abline(v=3.1,col="yellow")
legend(x=4.1,y=3,legend=c("mean","median"),pch=15,col=c("blue","yellow"),cex=2)
dev.off()

### Variance Plots ###

pdf("var1.pdf")
exampleData <- c(2,3,3,4,4,4,5,5,6)
par(mar=c(4,4,2,2)+.1)
barplot(table(exampleData) ,ylim=c(-.25,3),col="red")
segments(x0=1.4,x1=2.4,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=1,y1=1,lwd=2)
segments(x0=3.8,x1=4.8,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=2,y1=2,lwd=2)
segments(x0=.2,x1=7.2,y0=0,y1=0,lwd=2)
dev.off()

pdf("var2.pdf")
exampleData <- c(2,2,3,4,4,4,5,6,6)
par(mar=c(4,4,2,2)+.1)
barplot(table(exampleData) ,ylim=c(-.25,3),col="red")
segments(x0=.2,x1=1.2,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=1,y1=1,lwd=2)
segments(x0=5,x1=6,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=2,y1=2,lwd=2)
segments(x0=.2,x1=7.2,y0=0,y1=0,lwd=2)
dev.off()

pdf("var3.pdf")
exampleData <- c(2,2,3,3,4,5,5,6,6)
par(mar=c(4,4,2,2)+.1)
barplot(table(exampleData) ,ylim=c(-.25,3),col="red")
segments(x0=1.4,x1=2.4,y0=1,y1=1,lwd=2)
segments(x0=.2,x1=1.2,y0=1,y1=1,lwd=2)
segments(x0=2.6,x1=3.6,y0=1,y1=1,lwd=2)
segments(x0=5,x1=6,y0=1,y1=1,lwd=2)
segments(x0=3.8,x1=4.8,y0=1,y1=1,lwd=2)
segments(x0=.2,x1=7.2,y0=0,y1=0,lwd=2)
dev.off()
