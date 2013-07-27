### R code for Lecture 11 ###
## The usual warnings apply ##

treisman <- read.table("Treisman98TI.csv", na.strings="NA", header=TRUE, sep=",") 

row.names(treisman) <- treisman$country # We can make our lives easier by renaming the rows with the country names.
#attach(treisman)

library(car)
library(mgcv)
library(MASS) 

## Problem 1A ##

lm1.out <- lm(TI98~commonlaw+britcolony+noncolony+pctprot+elf+FMMexports93, data=treisman)
summary(lm1.out)

pdf("qq.pdf")
plot(lm1.out, which = 2)
dev.off()

# alternative plot #
pdf("carqq.pdf")
qq.plot(lm1.out)
dev.off()

## Unusual Observations ###

## Leverage Demonstration ##

lm2.out <- lm(TI98~pctprot+elf, data=treisman)
summary(lm2.out)

hatvals.lm2.out <- hatvalues(lm2.out)
sort(hatvals.lm2.out,decreasing=TRUE)
mean(hatvals.lm2.out)

data.ellipse(model.matrix(lm2.out)[,"pctprot"],model.matrix(lm2.out)[,"elf"],levels=c(.5,.95,.99),xlim=c(0,110),ylim=c(0,110),xlab="pctprot",ylab="elf",main=".5, .95, .99 Normal Probability Levels")
identify(model.matrix(lm2.out)[,"pctprot"],model.matrix(lm2.out)[,"elf"],labels=names(model.matrix(lm2.out)[,"pctprot"]))
dev.print(file="treismanHatEllipse.pdf", device=pdf, height=5, width=5,pointsize=8, bg="white")

n <- dim(model.matrix(lm2.out))[1]
k <- dim(model.matrix(lm2.out))[2] - 1
stu.res.lm2.out <- rstudent(lm2.out)
plot(stu.res.lm2.out,ylab="studentized residuals",ylim = c(-3.5,3.5))
abline(h=c(-1,1)*qt(p=.975,df=n-(k+1)-1),col="red")
#abline(h=qt(p=.995,df=n-(k+1)-1),col="red")
identify(stu.res.lm2.out, labels=names(model.matrix(lm2.out)[,"pctprot"]))
dev.print(file="treismanOutlier.pdf", device=pdf, height=5, width=5,pointsize=8, bg="white")

n <- dim(model.matrix(lm2.out))[1]
k <- dim(model.matrix(lm2.out))[2] - 1
stu.res.lm2.out <- rstudent(lm2.out)
plot(stu.res.lm2.out,ylab="studentized residuals",ylim = c(-3.5,3.5))
abline(h=c(-1,1)*qt(p=.975,df=n-(k+1)-1),col="red")
abline(h=c(-1,1)*qt(p=1-.05/70,df=n-(k+1)-1),col="red")
identify(stu.res.lm2.out, labels=names(model.matrix(lm2.out)[,"pctprot"]))
dev.print(file="BonftreismanOutlier.pdf", device=pdf, height=5, width=5,pointsize=8, bg="white")

outlier.test(lm2.out)

dfbetas.lm2.out <- dfbetas(lm2.out)
head(dfbetas.lm2.out)

plot(dfbetas.lm2.out[,2],dfbetas.lm2.out[,3],xlab=colnames(dfbetas.lm2.out)[2],ylab=colnames(dfbetas.lm2.out)[3],xlim=c(-.5,.5),ylim=c(-.5,.5),main="Influence Plot")
abline(h=c(-2/sqrt(n),2/sqrt(n)))
abline(v=c(-2/sqrt(n),2/sqrt(n)))
identify(dfbetas.lm2.out[,2],dfbetas.lm2.out[,3],rownames(dfbetas.lm2.out))
dev.print(file="treismanInfluence.pdf", device=pdf, height=5, width=5,pointsize=8, bg="white")


plot(hatvals.lm2.out, stu.res.lm2.out, xlab="hat values",
     ylab="studentized residuals", ylim=c(-5.5, 5.5))
abline(h=c(-2,2))
abline(v=c(2*mean(hatvals.lm2.out), 3*mean(hatvals.lm2.out)))
identify(hatvals.lm2.out, stu.res.lm2.out, labels=names(hatvals.lm2.out))
dev.print(file="levout.pdf", device=pdf, height=5, width=5,pointsize=8, bg="white")

pdf("lmLevout.pdf")
plot(lm2.out, which = 5)
dev.off()


av.plot(lm2.out,variable="pctprot")
dev.print(file="avPctprot.pdf", device=pdf, height=5, width=5,pointsize=8, bg="white")

av.plot(lm2.out,variable="elf")
dev.print(file="avElf.pdf", device=pdf, height=5, width=5,pointsize=8, bg="white")



## Using the identify function ##

# run this commands to create the plot
#plot(lm1.out, which = 5,id.n=0) # plotting without any labels
# run this command and click on the points you want to identify
#identify(x=hatvalues(lm1.out),y=rstandard(lm1.out),labels=names(rstandard(lm1.out)))
# use this command to create a pdf file to include in your document
#dev.print(file="levout2.pdf", device=pdf, height=5, width=5,pointsize=8, bg="white")


## Examining unusual points ##

unusualSuspects <- c("Norway","Jamaica","Canada","Singapor")
treisman[unusualSuspects,c("TI98","pctprot","elf","FMMexports93")]


### Robust Regression ###
lm2.out <- lm(TI98~pctprot+elf, data=treisman)
summary(lm2.out)

lmHuber.out <- rlm(TI98~pctprot+elf, data=treisman)
summary(lmHuber.out)


