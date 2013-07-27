#	$Id: admit.r 123 2007-04-23 18:15:50Z jackman $	
## analysis of admission data via ordered probit
###############################################################

data <- read.table(file="admit.asc",
                   header=TRUE)
data$y <- data$score
table(data$score)
data$score <- ordered(data$score)  ## coerce depvar to ordered factor
table(data$score)

library(MASS)     ## Venables and Ripley's package

op1 <- polr(score ~ gre.quant + gre.verbal + ap + pt + female,
            Hess=TRUE,
            data=data,
            method="probit")   ## ordered probit model

## compare regression analysis
ols <- lm(y ~ gre.quant + gre.verbal + ap + pt + female,
          data=data)

## prediction with the ordered probit model
yhat <- predict(op1)     ## predict category with highest predicted prob

## goodness of fit?
tab <- table(data$score,yhat)   ## cross-tabulation of actual against predicted
pcp <- sum(diag(tab))/sum(tab) * 100
pcpNull <- max(table(data$score))/sum(tab) * 100
phat <- predict(op1,type="p")

## pseudo r^2
opNull <- polr(score ~ 1,
               data=data,
               method="probit")

## two programming tasks
## 1 hitmiss table for objects of class polr
## 2 pseudo-r2


############################################################
## some fake data to visualize marginal effect of gre-quant
xseq <- seq(min(data$gre.quant),
            max(data$gre.quant),
            by=10)
x0 <- expand.grid(gre.quant=xseq,
                  gre.verbal=median(data$gre.verbal),
                  ap=0,
                  pt=0,
                  female=0)
phat <- predict(op1,
                newdata=x0,
                type="p")

pdf(file="probs.pdf")
par(las=1,
    mar=c(4,4,1,4))
plot(x=range(xseq),
     y=c(0,1),
     type="n",
     axes=FALSE,
     xlab="Hypothetical GRE Quant Score",
     ylab="Predicted Probability")
axis(1)
axis(2)
axis(4)
cols <- rainbow(5)
for(j in 1:5){
  lines(xseq,phat[,j],col=cols[j],lwd=3)
}

n <- length(data$score)
for(i in 1:n){
  rug(jitter(data$gre.quant[i]),
      col=cols[data$score[i]])
}

legend(x="topright",
       legend=paste("Pr(y=",1:5,")",sep=""),
       col=cols,
       cex=1.15,
       lwd=3,
       bty="n")
dev.off()


## visualize the difference between AP female and someone else
xseq <- seq(min(data$gre.quant),
            max(data$gre.quant),
            by=10)
x0 <- expand.grid(gre.quant=xseq,
                  gre.verbal=median(data$gre.verbal),
                  ap=c(0,1),
                  pt=0,
                  female=c(0,1))
x0$p <- predict(op1,
                newdata=x0,
                type="p")[,5]

pdf(file="compare.pdf")
par(las=1,
    mar=c(4,4,1,4))
plot(x=range(xseq),
     y=c(0,1),
     type="n",
     axes=FALSE,
     xlab="Hypothetical GRE Quant Score",
     ylab="Predicted Probability of 5")
axis(1)
axis(2)
axis(4)

ok <- x0$ap==1 & x0$female==1
lines(xseq,x0$p[ok],col="red",lwd=3)

ok <- x0$ap==0 & x0$female==0
lines(xseq,x0$p[ok],col="blue",lwd=3)

delta <- x0$p[x0$ap==1 & x0$female==1] - x0$p[x0$ap==0 & x0$female==0]
lines(xseq,delta,lwd=3)

legend(x="topleft",
       bty="n",
       legend=c("AP and Female","Not AP and Not Female","Difference"),
       col=c("red","blue","black"),
       lwd=2)

dev.off()

############################################################
## graph of what the model is doing, normal densities at
## different locations on the latent scale, depending on
## covariate values
## this is for teaching purposes only, you wouldn't
## have such a graph in any write-up of an ordinal data model
x <- cbind(gre.quant=median(data$gre.quant),
           gre.verbal=median(data$gre.verbal),
           ap=c(0,1,1),
           pt=0,
           female=c(0,0,1))
mu <- x%*%coef(op1)

## align thresholds
M <- 151
tauStar <- op1$zeta
xseq <- seq(min(tauStar)-.35,max(tauStar)+2,length=M)
m <- length(tauStar)
tau <- rep(NA,m)
indx <- rep(NA,m)
for(j in 1:m){
  indx[j] <- which.min(abs(xseq-tauStar[j]))
  tau[j] <- xseq[indx[j]]
}

##cols <- rev(heat.colors(n=m+1))   ## hot colors
##cols <- rev(cm.colors(n=m+1))
##cols <- gray(seq(from=.6,to=.3,length=5))  # gray shades
cols <- rgb(r=255/255,g=165/255,b=0,alpha=c(seq(.35,.75,length=4),1))
titleString <- c("Not AP, Male",
                  "AP, Male",
                  "AP, Female")
pdf(file="variousDensities.pdf",
    version="1.4",
    h=11,w=8.5)
par(mfrow=c(3,1),
    las=1)
for(j in 1:3){
  y <- dnorm(xseq,mean=mu[j],sd=1)
  plot(y~xseq,
       ylim=c(0,max(y)),
       xlab="",
       ylab="",
       axes=FALSE,
       type="n",
       lwd=2)
  axis(1,pos=0,
       at=tau,
       cex.axis=1.5,
       labels=expression(tau[0],tau[1],tau[2],tau[3]))
  
  ## shade each region
  polygon(x=xseq[c(1:indx[1],indx[1]:1)],
        y=c(y[1:indx[1]],rep(0,indx[1])),
        border=FALSE,
        col=cols[1])
  for(i in 2:(m+1)){
    if(i==(m+1)){
      theIndx <- indx[m]:M
      thisBorder <- FALSE
    }
    else{
      theIndx <- indx[i-1]:indx[i]
      thisBorder <- TRUE
    }
    len <- length(xseq[theIndx])
    polygon(x=xseq[c(theIndx,rev(theIndx))],
            y=c(y[theIndx],rep(0,len)),
            border=thisBorder,
            col=cols[i])
  }

  ## overlay normal curve
  lines(xseq,y,lwd=3)
  
  ## arrow on horizontal axis
  arrows(x0=par()$usr[1],
         x1=par()$usr[2],
         y0=0,
         y1=0,
         lwd=2,
         length=.125,
         code=3)

  ## title
  title(titleString[j],cex.main=1.75)

  ## predicted probabilities, to go in legend
  phat <- predict(op1,
                  newdata=data.frame(t(x[j,])),
                  type="p")
  
  ## legend
  legend(x="topleft",
         legend=paste("Pr[y=",1:(m+1),"] = ",
           format(signif(phat,2),digits=2,scientific=FALSE),
           sep=""),
         bty="n",
         cex=1.25,
         col=cols,
         pch=15)
}

dev.off()
