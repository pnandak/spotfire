### Lecture 6 R code ###
## The usual warnings apply. ##

treisman <- read.table("Treisman98TI.csv", na.strings="NA", header=TRUE, sep=",")

treisman.na <- treisman[,c("TI98","commonlaw","britcolony","noncolony","pctprot","elf","FMMexports93")]

treisman <-  na.omit(treisman.na)
attach(treisman)


pdf("Summary.pdf")
plot(pctprot,TI98,main="Percent Protestant and Corruption",cex.lab=1.2)
abline(lm(TI98~pctprot),lwd=2,col="red")
dev.off()

pdf("SummaryInference.pdf")
plot(pctprot,TI98,main="Percent Protestant and Corruption",cex.lab=1.2)
abline(lm(TI98~pctprot),lwd=2,col="red")
abline(a=5,b=-.04,lwd=2,col="blue")
legend(x=60,y=8,legend=c("Sample","Population"),lwd=2,col=c("red","blue"))
dev.off()

pdf("1bin.pdf")
plot(noncolony,TI98,main="Colonization and Corruption",cex.lab=1.2)
mod1 <- lm(TI98~noncolony)
abline(mod1,lwd=2,col="red")
dev.off()

pdf("2bin.pdf")
set.seed(123)
plot(jitter(noncolony),TI98,main="Colonization, Commonlaw, and Corruption",type="n",cex.lab=1.2)
points(jitter(noncolony[commonlaw==0]),TI98[commonlaw==0],col="red",pch=16)
points(jitter(noncolony[commonlaw==1]),TI98[commonlaw==1],col="black",pch=16)
mod2 <- lm(TI98[commonlaw==0]~noncolony[commonlaw==0])
abline(mod2,lwd=2,col="red")
mod3 <- lm(TI98[commonlaw==1]~noncolony[commonlaw==1])
abline(mod3,lwd=2,col="black")
legend(x=.25,y=8,legend=c("Civil Law","Common Law"),lwd=2,col=c("red","black"))
dev.off()

pdf("2binAdd.pdf")
set.seed(123)
plot(jitter(noncolony),TI98,main="Colonization, Commonlaw, and Corruption",type="n",cex.lab=1.2)
points(jitter(noncolony[commonlaw==0]),TI98[commonlaw==0],col="red",pch=16)
points(jitter(noncolony[commonlaw==1]),TI98[commonlaw==1],col="black",pch=16)
mod4 <- lm(TI98~noncolony+commonlaw)
abline(a=mod4$coef[1],b=mod4$coef[2],lwd=2,col="red")
abline(a=mod4$coef[1]+mod4$coef[3],b=mod4$coef[2],lwd=2,col="black")
legend(x=.25,y=8,legend=c("Civil Law","Common Law"),lwd=2,col=c("red","black"))
dev.off()

library(scatterplot3d)

pdf("2binAdd3d.pdf")
s3d <- scatterplot3d(noncolony,commonlaw,TI98,angle=55,highlight.3d=TRUE,col.axis="blue",col.grid="blue",main="Colonization, Commonlaw, and Corruption",pch=16,cex.axis=1.3)
    # Now adding a regression plane to the "scatterplot3d"
  s3d$plane3d(mod4,col="red",lwd=2)
dev.off()

pdf("1bin1cont.pdf")
plot(pctprot,TI98,main="Pctprot, Commonlaw, and Corruption",type="n",cex.lab=1.2)
points(pctprot[commonlaw==0],TI98[commonlaw==0],col="red",pch=16)
points(pctprot[commonlaw==1],TI98[commonlaw==1],col="black",pch=16)
mod5 <- lm(TI98[commonlaw==0]~pctprot[commonlaw==0])
abline(mod5,lwd=2,col="red")
mod6 <- lm(TI98[commonlaw==1]~pctprot[commonlaw==1])
abline(mod6,lwd=2,col="black")
legend(x=60,y=8,legend=c("Civil Law","Common Law"),lwd=2,col=c("red","black"))
dev.off()


pdf("1bin1contAdd.pdf")
plot(pctprot,TI98,main="Pctprot, Commonlaw, and Corruption",type="n",cex.lab=1.2)
points(pctprot[commonlaw==0],TI98[commonlaw==0],col="red",pch=16)
points(pctprot[commonlaw==1],TI98[commonlaw==1],col="black",pch=16)
mod7 <- lm(TI98~pctprot+commonlaw)
abline(a=mod7$coef[1],b=mod7$coef[2],lwd=2,col="red")
abline(a=mod7$coef[1]+mod7$coef[3],b=mod7$coef[2],lwd=2,col="black")
legend(x=60,y=8,legend=c("Civil Law","Common Law"),lwd=2,col=c("red","black"))
dev.off()

pdf("1bin1contAdd3d.pdf")
s3d <- scatterplot3d(pctprot,commonlaw,TI98,angle=55,highlight.3d=TRUE,col.axis="blue",col.grid="blue",main="Pctprot, Commonlaw, and Corruption",pch=16,cex.axis=1.3)
    # Now adding a regression plane to the "scatterplot3d"
  s3d$plane3d(mod7,col="red",lwd=2)
dev.off()

mod8 <- lm(TI98~pctprot+elf)
pdf("2contAdd3d.pdf")
s3d <- scatterplot3d(pctprot,elf,TI98,angle=55,highlight.3d=TRUE,col.axis="blue",col.grid="blue",main="Pctprot, ELF, and Corruption",pch=16,cex.axis=1.3)
    # Now adding a regression plane to the "scatterplot3d"
  s3d$plane3d(mod8,col="red",lwd=2)
dev.off()

pdf("2contAdd3dRes.pdf")
s3d <- scatterplot3d(pctprot,elf,TI98,angle=55,highlight.3d=TRUE,col.axis="blue",col.grid="blue",main="Pctprot, ELF, and Corruption",pch=16,cex.axis=1.3)
    # Now adding a regression plane to the "scatterplot3d"
  s3d$plane3d(mod8,col="red",lwd=2)
  s3d$points3d(pctprot,elf,TI98,type="h",pch=16)
dev.off()

library(car)

pdf("AV.pdf")
par(mfrow=c(1,2))
av.plot(mod8,variable=pctprot)
av.plot(mod8,variable=elf)
dev.off()

(cor(TI98,pctprot) - cor(TI98,elf)*cor(pctprot,elf))/sqrt((1-cor(TI98,elf))*(1-cor(pctprot,elf)))

pdf("2contAdd3dInference.pdf")
s3d <- scatterplot3d(pctprot,elf,TI98,angle=55,highlight.3d=TRUE,col.axis="blue",col.grid="blue",main="Pctprot, ELF, and Corruption",pch=16,cex.axis=1.3)
    # Now adding a regression plane to the "scatterplot3d"
  s3d$plane3d(mod8,col="red",lwd=2)
  s3d$plane3d(4,-.02,0.01,col="blue",lwd=2)
dev.off()

pdf("1binInference.pdf")
plot(noncolony,TI98,main="Colonization and Corruption",cex.lab=1.2)
mod1 <- lm(TI98~noncolony)
abline(mod1,lwd=2,col="red")
abline(a=6, b=-1 ,lwd=2,col="blue")
legend(x=.3,y=8,legend=c("Sample","Population"),lwd=2,col=c("red","blue"))
dev.off()

pdf("sameInt.pdf")
plot(pctprot,TI98,main="Pctprot, Commonlaw, and Corruption",type="n",cex.lab=1.2)
points(pctprot[commonlaw==0],TI98[commonlaw==0],col="red",pch=16)
points(pctprot[commonlaw==1],TI98[commonlaw==1],col="black",pch=16)
mod9 <- lm(TI98~pctprot+commonlaw:pctprot)
abline(a=mod9$coef[1],b=mod9$coef[2],lwd=2,col="red")
abline(a=mod9$coef[1],b=mod9$coef[2]+mod9$coef[3],lwd=2,col="black")
legend(x=60,y=8,legend=c("Civil Law","Common Law"),lwd=2,col=c("red","black"))
dev.off()

## Continuous Interaction Example ##

x1 <- x2 <- seq(-20,20,1)
y <- 0.5 + 1.0*x1 + 2.0*x2 + 0.25*x1*x2
hf <-  function(x1,x2){
	0.5 + 1.0*x1 + 2.0*x2 + 0.25*x1*x2
	}
	y <- outer(x1,x2,hf)

pdf("interact1.pdf")
persp(x1,x2,y,theta=-30,phi=30,col="lightblue",ticktype="detailed", shade=.5)
dev.off()
pdf("interact2.pdf")
persp(x1,x2,y,theta=30,phi=30,col="lightblue",ticktype="detailed", shade=.5)
dev.off()

persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Sinc( r )"
)

## Pctprot Elf Interaction ##
library(mgcv)
mod10 <- gam(TI98 ~  pctprot + elf)
summary(mod10)
pdf("pctprotELFAdd.pdf")
vis.gam(mod10,theta=-30,phi=20)
dev.off()

## Pctprot Elf Interaction #

mod11 <- gam(TI98 ~  pctprot * elf)
summary(mod11)
pdf("pctprotELF.pdf")
vis.gam(mod11,theta=-30,phi=20)
dev.off()

pdf(file="CIs.pdf")
plot(x=mod8$coef[2:3],y=1:2,xlim=c(-.08,.04),ylim=c(.9,2.1),xlab="",ylab=expression(beta[k]) ,pch=16,col="red",axes=FALSE,main=expression(paste("95% CIs for ",beta[1]," and ",beta[2])))
axis(1,at=seq(-.08,.04,.02))
axis(2,at=c(1,2))
segments(x0=confint(mod8)[2:3,1],y0=1:2,x1=confint(mod8)[2:3,2],y1=1:2,lwd=2,col="red")
abline(v=0,lwd=2)
dev.off()


