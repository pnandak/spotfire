require(SuppDists)
require(fBasics)

stable.proc <- function(t.grid,alpha,beta,gamma=1,delta=0){
    dt.grid <- diff(t.grid)
    if(all(diff(dt.grid) <= 1e-14)) { #equispaced increments
       dX.grid <- rstable(length(dt.grid),alpha=alpha,beta=beta,gamma=gamma*dt.grid[1],delta=delta)
    } else {
        dX.grid <- rep(0,length(dt.grid))
        for(i in 1:length(dt.grid))
            dX.grid[i] <- rstable(1,alpha=alpha,beta=beta,gamma=gamma*dt.grid[i],delta=delta)
    }
    return(list(t=t.grid,X=c(0,cumsum(dX.grid))))
}


VG.proc <- function(t.grid, sigma, theta, kappa) {
    ## sigma.. variance of BM
    ## theta.. drift
    ## kappa.. intensity of subordinator

    dt.grid <- diff(t.grid)
    dS.grid <- rgamma(length(dt.grid),shape=dt.grid,rate=kappa)
    dX.grid <- rnorm(length(dt.grid),mean=theta*dS.grid,sd=sigma*sqrt(dS.grid))
    return(list(t=t.grid,X=c(0,cumsum(dX.grid))))
}


NIG.proc <- function(t.grid, sigma, theta, kappa) {
    ## sigma.. variance of BM
    ## theta.. drift
    ## kappa.. intensity of subordinator

    dt.grid <- diff(t.grid)
    dS.grid <- rinvGauss(length(dt.grid),lambda=dt.grid^2/kappa,nu=dt.grid)
    dX.grid <- rnorm(length(dt.grid),mean=theta*dS.grid,sd=sigma*sqrt(dS.grid))
    return(list(t=t.grid,X=c(0,cumsum(dX.grid))))
}


plot.simple <- function(func = stable.proc, type = "p", pch = ".", cex = 2, n = 1000,...) {
## Plot a single trajectory of a Levy process
t.grid <- seq(0,10,length=n)
p1 <- func(t.grid,...)
plot(p1$t,p1$X,col=1,type=type,pch=pch,cex=cex,axes=FALSE,frame.plot=TRUE,xlab="",ylab="")
}

plot.multi <- function(func = stable.proc, type="p", pch=".", cex=2, n=1000,...) {
## Plot 3 trajectories of a Levy process
t.grid <- seq(0,10,length=n)

p1 <- func(t.grid,...)
p2 <- func(t.grid,...)
p3 <- func(t.grid,...)

plot(p1$t,p1$X,col=1,type=type,pch=pch,ylim=range(c(p1$X,p2$X,p3$X)),xlim=range(c(p1$t,p2$t,p3$t)),cex=cex,xlab="t",ylab="")
points(p2$t,p2$X,type=type,pch=pch,col=2,cex=cex)
points(p3$t,p3$X,type=type,pch=pch,col=3,cex=cex)

}

plot.split <- function(func = stable.proc, type="p", pch=".", cex=2, n=1000,...) {
## Plot 3 trajectories of a Levy process and a QQ-Plot comparing the distribution of increments to the normal distribution

old.par <- par(mar=c(3,3,3,3))
layout(matrix(c(1,2),nrow=1,ncol=2),widths=c(7,3),heights=1)

t.grid <- seq(0,10,length=n)

p1 <- func(t.grid,...)
p2 <- func(t.grid,...)
p3 <- func(t.grid,...)

plot(p1$t,p1$X,col=1,type=type,pch=pch,ylim=range(c(p1$X,p2$X,p3$X)),xlim=range(c(p1$t,p2$t,p3$t)),cex=cex)
points(p2$t,p2$X,type=type,pch=pch,col=2,cex=cex)
points(p3$t,p3$X,type=type,pch=pch,col=3,cex=cex)

par(mar=c(3,2,3,2))
qqnorm(c(diff(p1$X),diff(p2$X),diff(p3$X)),cex.axis=.7,main="",ann=FALSE)
qqline(c(diff(p1$X),diff(p2$X),diff(p3$X)),col=2)
par(old.par)


}


 
