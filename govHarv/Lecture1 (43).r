#########################
#### Lecture 1 R code ###
#########################

### Example 1 ###

library(foreign)
library(lattice)


fishdata <- read.spss("Fish_data.sav",
                    use.value.labels=FALSE,
                    to.data.frame=TRUE)


fishdata <- fishdata[-46,]
colnames(fishdata)[2] <- "Democracy"
colnames(fishdata)[4] <- "Income"
colnames(fishdata)[16] <- "Muslim"

fishdata <- na.omit(fishdata[,c("Democracy", "Income", "Muslim", "OPEC")])

summary(fishdata)

fishdata[fishdata$Muslim==1,3] <- "Muslim"
fishdata[fishdata$Muslim==0,3] <- "Non-Muslim"
fishdata[,3] <- as.factor(fishdata[,3])

fishdata[fishdata$OPEC==1,4] <- "OPEC Member"
fishdata[fishdata$OPEC==0,4] <- "Not OPEC Member"
fishdata[4] <- as.factor(fishdata[,4])

attach(fishdata)
tapply(Income,Muslim,summary)

pdf("FishFig1Lec1.pdf", height=6, width=6, bg="white", version="1.4")
print(xyplot(Democracy~Income, data=fishdata))
dev.off()


pdf("FishFig2Lec1.pdf", height=6, width=6, bg="white")
print(xyplot(Democracy~Income, data=fishdata,
             panel=function(x,y,...){
               panel.xyplot(x,y,...)
               if (length(y)>1){
                 panel.lmline(x,y,...)
               }
             }
             ))
dev.off()

pdf("FishFig3Lec1.pdf", height=6, width=6, bg="white")
print(xyplot(Democracy~Income|Muslim, data=fishdata,
             panel=function(x,y,...){
               panel.xyplot(x,y,...)
               if (length(y)>1){
                 panel.lmline(x,y,...)
               }
             }
             ))
dev.off()



pdf("FishFig4Lec1.pdf", height=6, width=6, bg="white")
print(xyplot(Democracy~Income|Muslim*OPEC, data=fishdata,
             panel=function(x,y,...){
               panel.xyplot(x,y,...)
               if (length(y)>1){
                 panel.lmline(x,y,...)
               }
             }
             ))
dev.off()

### Example 2 ###

### "Ohio" Simulation (Suppose we know the truth) ###

N <- 2859768 + 2741167
votes <- c(rep("Bush",2859768),rep("Kerry",2741167))
mean(votes == "Bush")

n <- 1963
poll <- c(rep("Bush",ceiling(.479*n)),rep("Kerry",floor(.521*n)))
mean(poll == "Bush")

numberOfPolls <- 1000
pollResults <- rep(0,numberOfPolls)
set.seed(123)
for(i in 1:numberOfPolls){
	pollSim <- sample(votes,size=n,replace=FALSE)
	pollResults[i] <- mean(pollSim == "Bush")
	}

pdf(file="nullDist.pdf")
hist(pollResults,col="blue",freq=FALSE,main="Histogram of Simulated Polls",xlab="Bush Proportion")
dev.off()


pdf(file="nullDist2.pdf")
hist(pollResults,col="blue",freq=FALSE,main="Histogram of Simulated Polls",xlab="Bush Proportion")
abline(v = mean(votes == "Bush"),col="red",lwd=2)
abline(v=mean(poll == "Bush"),col="purple",lwd=2)
legend(x=.52,y=35,legend=c("Vote Count","Exit Poll"),col=c("red","purple"),lwd=2)
dev.off()

### Estimation ###

numberOfPolls <- 1000
pollBootstrap <- rep(0,numberOfPolls)
set.seed(123)
for(i in 1:numberOfPolls){
	pollSim <- sample(poll,size=n,replace=TRUE)
	pollBootstrap[i] <- mean(pollSim == "Bush")
	}

pdf("compareDist.pdf")
plot(density(pollResults),col="blue",xlim=c(.44,.56),lwd=2,main="Null and Estimated Sampling Distributions",xlab="Bush Proportion")
lines(density(pollBootstrap),col="red",lwd=2)
legend(x=.515,y=34,legend=c("Null Dist.","Sampling Dist."),col=c("blue","red"),lwd=2)
abline(v = mean(votes == "Bush"),col="blue",lwd=2)
abline(v=mean(poll == "Bush"),col="red",lwd=2)
dev.off()

### Example 3 ###

load("/Users/aglynn/Documents/Teaching/Courses/07Gov2000/LectureNotes/Lecture1/data_27334.RData")

ls()
names(x)
summary(x)

set.seed(123)
xsimS =runif(n=10,1,6)
ysimS =runif(n=10,0,3)
xsimF =runif(n=4,1,6)
ysimF =runif(n=4,0,3)

pdf("predict.pdf")
plot(x$InfanMort,x$PopulDens,type="n",xlab="Infant Mortality (lagged 2 yrs)", ylab="Population Density (lagged 2 yrs)",xlim=c(1,6),ylim=c(0,3))
text(x=xsimS,y=ysimS,labels="S")
text(x=xsimF,y=ysimF,labels="F")
dev.off()


pdf("predictColor1.pdf")
plot(x$InfanMort,x$PopulDens,type="n",xlab="Infant Mortality (lagged 2 yrs)", ylab="Population Density (lagged 2 yrs)",xlim=c(1,6),ylim=c(0,3))
text(x=xsimS[1:5],y=ysimS[1:5],labels="S",col=c(rep("red",5)))

text(x=xsimF[1:2],y=ysimF[1:2],labels="F",col=c(rep("red",2)))
dev.off()

pdf("predictColor2.pdf")
plot(x$InfanMort,x$PopulDens,type="n",xlab="Infant Mortality (lagged 2 yrs)", ylab="Population Density (lagged 2 yrs)",xlim=c(1,6),ylim=c(0,3))
text(x=xsimS[1:5],y=ysimS[1:5],labels="S",col=c(rep("red",5)))
text(x=xsimS[6:10],y=ysimS[6:10],labels="S",col=c(rep("blue",5)))
text(x=xsimF[1:2],y=ysimF[1:2],labels="F",col=c(rep("red",2)))
text(x=xsimF[3:4],y=ysimF[3:4],labels="F",col=c(rep("blue",2)))
dev.off()

pdf("explain.pdf")
plot(x$InfanMort,x$PopulDens,type="n",xlab="Infant Mortality (lagged 2 yrs)", ylab="Population Density (lagged 2 yrs)",main="",xlim=c(1,6),ylim=c(0,3))
set.seed(123)
text(x=runif(n=10,1,6),y=runif(n=10,0,3),labels="S")
text(x=runif(n=4,1,6),y=runif(n=4,0,3),labels="F")
dev.off()

pdf("explainHeight.pdf")
plot(x$InfanMort,x$PopulDens,type="n",xlab="Infant Mortality (lagged 2 yrs)", ylab="Population Density (lagged 2 yrs)",main="",xlim=c(1,6),ylim=c(0,3))
set.seed(123)
text(x=runif(n=10,1,6),y=runif(n=10,0,3),labels="0")
text(x=runif(n=4,1,6),y=runif(n=4,0,3),labels="1")
dev.off()





