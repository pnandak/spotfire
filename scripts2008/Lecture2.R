#### Lecture 2 Code ####

#############################
### PMF PDF and CDF plots ###
#############################

## The following code produces the 

pdf("mass.pdf", height=6, width=6, bg="white")
plot(0:2,c(.25,.5,.25),pch=19,xlim=c(-.5,2.5),ylim = c(0,1),ylab="f(x)",xlab="x",cex.lab = 1.3)
segments(0,0,0,.25,cex=2)
segments(1,0,1,.5,cex=2)
segments(2,0,2,.25,cex=2)
dev.off()

pdf("step.pdf", height=6, width=6, bg="white")

plot(x=c(0,1,2),y=c(.25,.75,1),pch=19,xlim=c(-.4,2.4),ylim=c(0,1),xlab="x",ylab="F(x)")
segments(x0=-.5,y0=0,x1=-.01,y1=0)
#segments(x0=0,0,x1=0,y1=.25,lty=2)
segments(x0=0,y0=.25,x1=.99,y1=.25)
#segments(x0=1,y0=.25,x1=1,y1=.75,lty=2)
segments(x0=1,y0=.75,x1=1.99,y1=.75)
#segments(x0=2,y0=.75,x1=2,y1=1,lty=2)
segments(x0=2,y0=1,x1=2.5,y1=1)
points(x=c(0,1,2),y=c(0,.25,.75))

dev.off()

pdf("density.pdf", height=6, width=6, bg="white")
plot(0:4,0:4,type="n",ylim = c(0,1),xlim=c(-.5,4.5),ylab="f(x)",xlab="x",lwd=2,cex.lab = 1.3)
segments(-.5,0,0,0,lwd=2)
segments(4,0,4.5,0,lwd=2)
segments(0,.25,4,.25,lwd=2)
segments(0,0,0,.25,lty=2,lwd=2)
segments(4,0,4,.25,lty=2,lwd=2)
dev.off()


pdf("cdf.pdf", height=6, width=6, bg="white")
plot(0:4,0:4,type="n",ylim = c(0,1),xlim=c(-.5,4.5),ylab="F(x)",xlab="x",lwd=2,cex.lab = 1.3)
segments(-.5,0,0,0,lwd=2)
segments(4,1,4.5,1,lwd=2)
segments(0,0,4,1,lwd=2)
dev.off()


##### Joint/Conditional Distributions ####


library(mvtnorm)
mu <- c(.5,.5)
Sigma <- matrix(c(1,.5,.5,1),nr=2,nc=2)
condSig <-
x <- seq(0,1,.01)
y <- seq(0,1,.01)

xyInput <- cbind(rep(x,length(y)),rep(y,rep(length(x),length(y))))


HeightMat <- matrix(dmvnorm(x=xyInput,mean=mu,sigma=Sigma),nr=length(y),nc=length(x))

pdf("imagecontour.pdf")
image(x,y,HeightMat); contour(x,y,HeightMat, add = T)
dev.off()

pdf("perspective.pdf")
persp(x,y,HeightMat,zlab="f(x,y)", phi = 45, theta = 30, shade = .3,col="lightblue", border = NA) 
dev.off()

pdf("cm.pdf",width=12)
par(mfrow=c(1,2))
persp(x,y,HeightMat,zlab="f(x,y)", phi = 0, theta = 0, shade = .3,col="lightblue", border = NA,cex = 2)
plot(x,dnorm(x,mean=.5,sd=1),col="blue",ylab="f(x)",type="l",lwd=2,cex.lab = 1.6)
dev.off()

x <- seq(0,1,.01)
mu <- .5 + .5*(x-.5)
sigEps <- sqrt(1-.5^2)
y <-  seq(0,1,.01)
z <- outer(y,mu,dnorm,sigEps)

y <-  seq(-3,4,.01)

pdf("disCondDens.pdf", bg="white")
par(mfrow=c(3,1))
plot(y,dnorm(y,mean=.5),type="l",main="Marginal Density",ylab="f(y)")
plot(y,dnorm(y,mean=.5 + .5*(-1 - .5),sd = sigEps),type="l",main="Conditional Density X=-1",ylab="f(y|x)")
plot(y,dnorm(y,mean=.5 + .5*(2 - .5),sd = sigEps),type="l",main="Conditional Density X=2",ylab="f(y|x)")
dev.off()

y <-  seq(0,1,.01)
pdf("condcontour.pdf", width=10, bg="white")
par(mfrow=c(1,2))
image(x,y,HeightMat,main="Joint Density"); contour(x,y,HeightMat, add = T)
image(x,y,t(z),xlab="x",ylab="y",main="Conditional Density"); contour(x,y,t(z), add=T)
dev.off()

pdf("condperspective.pdf",width=10, bg="white")
par(mfrow=c(1,2))
persp(x,y,HeightMat,zlab="f(x,y)",main="Joint Density",theta=0,phi=40, shade = .3,col="lightblue", border = NA,expand=.5) 
persp(x,y,t(z),main="Conditional Density",shade=.3,theta=0,phi=40,col="lightblue",zlab="f(y|x)", border = NA,expand=.5)
dev.off()



#### Expected Value/ Variance Plots ####

pdf("massBP.pdf", height=6, width=6, bg="white")
plot(0:2,c(.2,.45,.35),pch=19,xlim=c(-.5,2.5),ylim = c(-.1,1),ylab="f(x)",xlab="x",cex.lab = 1.3)
segments(0,0,0,.2,cex=2)
segments(1,0,1,.45,cex=2)
segments(2,0,2,.35,cex=2)
segments(0,0,2,0,cex=2)
points(x=1.15,y=-.05, pch=24, col="blue",cex=2)
dev.off()

pdf("densityBP.pdf", height=6, width=6, bg="white")
x.seq <- seq(0,12,.01)
plot(x = x.seq,dchisq(x=x.seq,df=4),type="l",ylim=c(-.01,max(dchisq(x=x.seq,df=4))), xlab="x",ylab= expression(paste("f(","x",")")  ), cex.lab = 1.3)
segments(0, 0,12,0)
points(x=4,y=-.006, pch=24, col="blue",cex=2)
dev.off()

pdf("densityVar.pdf", height=6, width=6, bg="white")
par(mfrow=c(1,2))
x.seq <- seq(-6,6,.01)
plot(x = x.seq,dnorm(x=x.seq,sd=1),type="l",ylim=c(-.02,max(dnorm(x=x.seq,sd=1))), xlab="x",ylab= expression(paste("f(","x",")")  ), cex.lab = 1.3)
segments(-6, 0,6,0)
points(x=0,y=-.013, pch=24, col="blue",cex=2)

plot(x = x.seq,dnorm(x=x.seq,sd=2),type="l",ylim=c(-.01,max(dnorm(x=x.seq,sd=2))), xlab="x",ylab= expression(paste("f(","x",")")  ), cex.lab = 1.3)
segments(-6, 0,6,0)
points(x=0,y=-.006, pch=24, col="blue",cex=2)
dev.off()

#### Conditional Expected Value/ Variance Plots ####

y <-  seq(-3,4,.01)
pdf("disCondExp.pdf", bg="white")
par(mfrow=c(3,1))
plot(y,dnorm(y,mean=.5),type="l",main="Marginal Density",ylab="f(y)",ylim=c(-.07,max(dnorm(y,mean=.5))))
points(x=.5,y=-.05, pch=24, col="blue",cex=2)
segments(-3, 0,4,0)
plot(y,dnorm(y,mean=.5 + .5*(-1 - .5),sd = sigEps),type="l",main="Conditional Density X=-1",ylab="f(y|x)",ylim=c(-.07,max(dnorm(y,mean=.5 + .5*(-1 - .5),sd = sigEps))))
points(x=.5 + .5*(-1 - .5),y=-.05, pch=24, col="blue",cex=2)
segments(-3, 0,4,0)
plot(y,dnorm(y,mean=.5 + .5*(2 - .5),sd = sigEps),type="l",main="Conditional Density X=2",ylab="f(y|x)",ylim=c(-.07,max(dnorm(y,mean=.5 + .5*(2 - .5),sd = sigEps))))
points(x=.5 + .5*(2 - .5),y=-.05, pch=24, col="blue",cex=2)
segments(-3, 0,4,0)
dev.off()

y <-  seq(0,1,.01)


pdf("condExp.pdf", width=10, bg="white")
par(mfrow=c(1,2))
image(x,y,HeightMat,main="E[X],E[Y]"); contour(x,y,HeightMat, add = T)
points(.5,.5,pch=19,col="blue")
image(x,y,t(z),xlab="x",ylab="y",main="E[Y|X]"); contour(x,y,t(z), add=T)
lines(x,mu,lwd=2,col="blue")
dev.off()

#### Asymptotic Plots #####

n <- 1:100
cn <- 1+1/n

pdf("numberSeq.pdf")
plot(n,cn, ylim=c(.9,2),ylab=expression(c[n]),col="red")
abline(h=1,col="blue")
dev.off()

### LLN Plot ###

mu <- 2 
x.seq <- seq(0,4,.01)
pdf("LLN.pdf",width=12)
par(mfrow=c(1,3))
plot(x= x.seq, y = dnorm(x=x.seq,mean=mu),type="l",main="n = 1", xlab=expression(bar(X)[n]),ylab= expression(paste("f(",bar(X)[n],")")  ) ,cex.main=2, cex.lab = 2)
plot(x= x.seq, y = dnorm(x=x.seq,mean=mu,sd=1/sqrt(10)),type="l",main="n = 10", xlab=expression(bar(X)[n]),ylab= expression(paste("f(",bar(X)[n],")")  ),cex.main=2, cex.lab = 2 )
plot(x= x.seq, y = dnorm(x=x.seq,mean=mu,sd=1/sqrt(100)),type="l",main="n = 100", xlab=expression(bar(X)[n]),ylab= expression(paste("f(",bar(X)[n],")")  ) ,cex.main=2, cex.lab = 2)
dev.off()

### Question Plot ###

mu <- 2 
x.seq <- seq(0,4,.01)
pdf("consistency.pdf",width=12)
par(mfrow=c(1,3))
plot(x= x.seq, y = dnorm(x=x.seq,mean=mu + 1),type="l",main="n = 1", xlab=expression(X[n]),ylab= expression(paste("f(",bar(X)[n],")")  ) ,cex.main=2, cex.lab = 2)
plot(x= x.seq, y = dnorm(x=x.seq,mean=mu+ 1/sqrt(10),sd=1/sqrt(10)),type="l",main="n = 10", xlab=expression(X[n]),ylab= expression(paste("f(",bar(X)[n],")")  ),cex.main=2, cex.lab = 2 )
plot(x= x.seq, y = dnorm(x=x.seq,mean=mu+ 1/sqrt(100),sd=1/sqrt(100)),type="l",main="n = 100", xlab=expression(X[n]),ylab= expression(paste("f(",bar(X)[n],")")  ) ,cex.main=2, cex.lab = 2)
dev.off()



### CLT Plot ###

mu <- 2 
x.seq <- seq(0,8,.01)

pdf("CLT.pdf",width=12)
par(mfrow=c(1,3))

plot(x = x.seq,dexp(x=x.seq,rate=1/mu),type="l",main="n = 1", xlab=expression(bar(X)[n]),ylab= expression(paste("f(",bar(X)[n],")")  ) ,cex.main=2, cex.lab = 2)

plot(x = x.seq,dgamma(x=x.seq,shape=10,rate=10/mu),type="l",main="n = 10", xlab=expression(bar(X)[n]),ylab= expression(paste("f(",bar(X)[n],")")  ) ,cex.main=2, cex.lab = 2)

plot(x = x.seq,dgamma(x=x.seq,shape=100,rate=100/mu),type="l",main="n = 100", xlab=expression(bar(X)[n]),ylab= expression(paste("f(",bar(X)[n],")")  ) ,cex.main=2, cex.lab = 2)
dev.off()
