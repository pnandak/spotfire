

mousse <- function(nbpoints=30,survie=0.025,portee=15,Lmax=100,lmax=100,nbcerceaux=50,rayon=15) {
    agg <- trunc(runif(1,0,3))
    oldpty <- par()$pty
    par(pty="s")
    x <- vector(length=nbpoints)
    y <- vector(length=nbpoints)
    fixes <- trunc(nbpoints/10)
    x[1:fixes] <- runif(fixes,0,lmax)
    y[1:fixes] <- runif(fixes,0,Lmax)
    if (agg==0) {
        x[-(1:fixes)] <- runif(nbpoints-fixes,0,lmax)
        y[-(1:fixes)] <- runif(nbpoints-fixes,0,Lmax)
    }
    if (agg==1) {
        nb <- fixes+1
        x[nb] <- runif(1,0,lmax)
        y[nb] <- runif(1,0,Lmax)
        while (nb<=nbpoints) {
            nb <- nb+1
            x[nb] <- runif(1,0,lmax)
            y[nb] <- runif(1,0,Lmax)
            distpoints <- sqrt(apply((matrix(rep(c(x[nb],y[nb]),nb-1),nrow=nb-1,byrow=T)-cbind(x[(1:(nb-1))],y[(1:(nb-1))]))^2,1,sum))
            if (any(distpoints<portee)) { bool <- as.numeric((runif(1,0,1)<survie)) ; nb <- nb-(1-bool) }
        }
    }
    if (agg==2) {
        nb <- fixes+1
        x[nb] <- runif(1,0,lmax)
        y[nb] <- runif(1,0,Lmax)
        while (nb<=nbpoints) {
            nb <- nb+1
            x[nb] <- runif(1,0,lmax)
            y[nb] <- runif(1,0,Lmax)
            distpoints <- sqrt(apply((matrix(rep(c(x[nb],y[nb]),nb-1),nrow=nb-1,byrow=T)-cbind(x[(1:(nb-1))],y[(1:(nb-1))]))^2,1,sum))
            if (all(distpoints>portee)) { bool <- as.numeric((runif(1,0,1)<survie)) ; nb <- nb-(1-bool) }
        }
    }
plot(x,y,type="n",xlab="",ylab="",main="Points d'impact de mousses",xlim=c(0,lmax),ylim=c(0,Lmax))
text(x,y,"\\#H0745",vfont=c("serif","plain"),col="green")
text(0.1*lmax,0.95*Lmax,"Cerceau suivant")
vnbp <- vector(length=nbcerceaux)
for (i in 1:nbcerceaux) {
        identify(0.05*lmax,0.95*Lmax,"")
        plot(x,y,type="n",xlab="",ylab="",main="Points d'impact de mousses",xlim=c(0,lmax),ylim=c(0,Lmax))
        text(x,y,"\\#H0745",vfont=c("serif","plain"),col="green")
        text(0.1*lmax,0.95*Lmax,"Cerceau suivant")
        centrex <- runif(1,rayon,lmax-rayon)
        centrey <- runif(1,rayon,Lmax-rayon)
        distpoints <- sqrt(apply((matrix(rep(c(centrex,centrey),length(x)),nrow=length(x),byrow=T)-cbind(x,y))^2,1,sum))
        nbp <- length((1:nbpoints)[distpoints<rayon])
        vnbp[i] <- nbp
        points(centrex,centrey,pch=3,col=1)
        symbols(centrex,centrey,circles=rayon,add=T,xlim=c(0,lmax),ylim=c(0,Lmax),inches=F)
        text(0.1*lmax,0.9*Lmax,paste("Comptage : ",nbp,sep=""))
    }
plot(x,y,type="n",xlab="",ylab="",main="Points d'impact de mousses",xlim=c(0,lmax),ylim=c(0,Lmax))
text(x,y,"\\#H0745",vfont=c("serif","plain"),col="green")
par(pty=oldpty)
list(x=x,y=y,nbp=vnbp,agg=agg)
}
