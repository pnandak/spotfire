# "Simulate" data following random and fixed effect specifications
# Chris Adolph
# June 2011

rm(list=ls())
library(simcf)
library(tile)
library(RColorBrewer)
set.seed(123456)

# "Random effects" model

# concept:  grade vs hours of study on a set of assignments

# Create covariates

n <- 100
t <- 5
beta = 0.75
sigmaAlpha <- 0.7
sigma <- 0.2

rXm <- c(-0.7,.7)
sdX <- 0.3

collist <- brewer.pal(9,"Set1")
while (length(collist)<n) collist <- c(collist,collist)
nt <- n*t
x <- matrix(rep(NA,nt),n,t)
y <- matrix(rep(NA,nt),n,t)
alpha <- rnorm(n,sd=sigmaAlpha)
for (i in 1:n) {
  x[i,] <- rnorm(t, mean=(runif(1)*(rXm[2]-rXm[1])-rXm[1]), sd=sdX)
  y[i,] <- alpha[i] + beta*x[i,] + rnorm(t,sd=sigma)
}

# Make traces
BWdataTraces <- dataTraces <- linesTraces <- vector("list",n)
for (i in 1:n) {
  dataTraces[[i]] <- pointsTile(x=x[i,], y=y[i,], col=collist[i], pch=16, plot=1)
  BWdataTraces[[i]] <- pointsTile(x=x[i,], y=y[i,], col="black", pch=16, plot=1)
  yfit <- c( min(x[i,])*beta + alpha[i],
             max(x[i,])*beta + alpha[i])  
  linesTraces[[i]] <- linesTile(x=c(min(x[i,]),max(x[i,])),y=yfit,col=collist[i], plot=1)
}
 yfit <- c( min(x)*beta,
             max(x)*beta)  
FlinesTraces <- linesTile(x=c(min(x),max(x)),y=yfit,col="black", lwd=4,plot=1)


# rug

rugY <- rugTile(y=alpha, type="lines", plot=1)
rugYdens <- rugTile(y=alpha, type="density", col="black", plot=1)


limits <- c(min(x), max(x), min(c(alpha+min(x)*beta, y)), max(c(alpha+min(x)*beta, y)))


tile(BWdataTraces,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="Hours of Study"),
     yaxistitle=list(labels="Assignment Grade"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="re1", width=4)
     )

tile(BWdataTraces,
     FlinesTraces,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="Hours of Study"),
     yaxistitle=list(labels="Assignment Grade"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="re2", width=4)
     )


tile(dataTraces,
     FlinesTraces,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="Hours of Study"),
     yaxistitle=list(labels="Assignment Grade"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="re3", width=4)
     )


tile(dataTraces,
     linesTraces,
     FlinesTraces,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="Hours of Study"),
     yaxistitle=list(labels="Assignment Grade"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="re4", width=4)
     )


tile(dataTraces,
     linesTraces,
     FlinesTraces,
     rugY,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="Hours of Study"),
     yaxistitle=list(labels="Assignment Grade"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="re5", width=4)
     )


tile(dataTraces,
     linesTraces,
     FlinesTraces,
     rugYdens,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="Hours of Study"),
     yaxistitle=list(labels="Assignment Grade"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="re6", width=4)
     )


# Revise example to allow correlated random effects and covariates
set.seed(323456)

n <- 30
t <- 5
beta = 0.75
sigmaAlpha <- 0.7
sigma <- 0.2
gamma <- 0.5

rXm <- c(-0.7,.7)
sdX <- 0.3

collist <- brewer.pal(9,"Set1")
while (length(collist)<n) collist <- c(collist,collist)
nt <- n*t
x <- matrix(rep(NA,nt),n,t)
y <- matrix(rep(NA,nt),n,t)
alpha <- rnorm(n,sd=sigmaAlpha)
for (i in 1:n) {
  x[i,] <- alpha[i]*gamma + rnorm(t, mean=(runif(1)*(rXm[2]-rXm[1])-rXm[1]), sd=sdX)
  y[i,] <- alpha[i] + beta*x[i,] + rnorm(t,sd=sigma)
}

# Make true traces
BWdataTraces <- dataTraces <- linesTraces <- vector("list",n)
for (i in 1:n) {
  dataTraces[[i]] <- pointsTile(x=x[i,], y=y[i,], col=collist[i], pch=16, plot=1)
  BWdataTraces[[i]] <- pointsTile(x=x[i,], y=y[i,], col="black", pch=16, plot=1)
  yfit <- c( min(x[i,])*beta + alpha[i],
             max(x[i,])*beta + alpha[i])  
  linesTraces[[i]] <- linesTile(x=c(min(x[i,]),max(x[i,])),y=yfit,col=collist[i], plot=1)
}
 yfit <- c( min(x)*beta,
             max(x)*beta)  
FlinesTraces <- linesTile(x=c(min(x),max(x)),y=yfit,col="black", lwd=4,plot=1)

# Get random effects one by one from constrained MLE
stval <- c(0,1)
llk.constrainedlin <- function(param,y,x,beta) {
    x <- as.matrix(x)
    os <- rep(1,nrow(x))
    x <- cbind(os,x)
    alpha <- param[1]
    s2 <- param[2]^2
    xb <- x%*%c(alpha,beta)
    sum(0.5*(log(s2)+(y-xb)^2/s2))  # optim is a minimizer, so min -ln L(param|y)
}


# Make RE traces
alphaHat <- lm(as.vector(y)~as.vector(x))$coef[1]
betaHat <- lm(as.vector(y)~as.vector(x))$coef[2]

relinesTraces <- vector("list",n)
for (i in 1:n) {
  alphaCurr <- optim(stval,llk.constrainedlin,method="BFGS",hessian=T,y=y[i,],x=x[i,],beta=betaHat)$par[1]
  yfitre <- c( min(x[i,])*betaHat + alphaCurr,
             max(x[i,])*betaHat + alphaCurr)  
  relinesTraces[[i]] <- linesTile(x=c(min(x[i,]),max(x[i,])),y=yfitre,col=collist[i], plot=1, lty="dashed")
}
 yfitre <- c( min(x)*betaHat + alphaHat,
             max(x)*betaHat + alphaHat)  
reFlinesTraces <- linesTile(x=c(min(x),max(x)),y=yfitre,col="black", lwd=4,plot=1, lty="dashed")

# rug

rugY <- rugTile(y=alpha, type="lines", plot=1)
rugYdens <- rugTile(y=alpha, type="density", col="black", plot=1)


limits <- c(min(x), max(x), min(c(alpha+min(x)*beta, y)), max(c(alpha+min(x)*beta, y)))

RE <- textTile(labels="RE estimate", x=0.1, y=-1.8, cex=0.7, plot=1)

Truth <- textTile(labels="Truth", x=-0.25, y=0.2, cex=0.7, plot=1)

tile(BWdataTraces,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="Hours of Study"),
     yaxistitle=list(labels="Assignment Grade"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="re1corr", width=4)
     )

tile(BWdataTraces,
     reFlinesTraces,
     RE,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="Hours of Study"),
     yaxistitle=list(labels="Assignment Grade"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="re2corr", width=4)
     )

tile(dataTraces,
     reFlinesTraces,
     RE,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="Hours of Study"),
     yaxistitle=list(labels="Assignment Grade"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="re3corr", width=4)
     )


tile(dataTraces,
     reFlinesTraces,
     relinesTraces,
     RE,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="Hours of Study"),
     yaxistitle=list(labels="Assignment Grade"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="re4corr", width=4)
     )


tile(dataTraces,
     linesTraces,
     FlinesTraces,
     Truth,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="Hours of Study"),
     yaxistitle=list(labels="Assignment Grade"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="re5corr", width=4)
     )


tile(dataTraces,
     reFlinesTraces,
     FlinesTraces,
     Truth,
     RE,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="Hours of Study"),
     yaxistitle=list(labels="Assignment Grade"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="re6corr", width=4)
     )


# Fixed effects model
set.seed(223456)


# concept:  Gelman's hierarchical model of American voting by state
#           percent Rep vs income of county 

n <- 15
t <- 15
beta <- 0.75
gamma <- -3
sigmaAlpha <- 0.02
sigma <- 0.1

rXm <- c(-0.7,.7)
sdX <- 0.2
sigmaZ <- 0.2

collist <- brewer.pal(9,"Set1")
while (length(collist)<n) collist <- c(collist,collist)
nt <- n*t
x <- matrix(rep(NA,nt),n,t)
y <- matrix(rep(NA,nt),n,t)
z <- matrix(rep(NA,nt),n,t)
alpha <- rnorm(n,sd=sigmaAlpha)
for (i in 1:n) {
  x[i,] <- rnorm(t, mean=(runif(1)*(rXm[2]-rXm[1])-rXm[1]), sd=sdX)
  z[i,] <- rep(mean(x[i,]) + rnorm(1,sd=sigmaZ), t)
}

for (i in 1:n) {
  y[i,] <- alpha[i] + beta*x[i,] + gamma*z[i,] + rnorm(t,sd=sigma)
}

# fit a fixed effects model to these data
yV <- as.vector(t(y))
xV <- as.vector(t(x))
zV <- as.vector(t(z))
unit <- as.vector(matrix(rep(1:n,t),nrow=t,ncol=n,byrow=TRUE))
FEdummies <- makeFEdummies(unit)
feRes <- lm(yV~xV+FEdummies-1)
betaHatFE <- feRes$coef[1]
feHat <- feRes$coef[2:(n+1)]
res <- lm(yV~xV)
alphaHatOVB <- res$coef[1]
betaHatOVB <- res$coef[2]
fullRes <- lm(yV~xV+zV)
alphaHatFull <- fullRes$coef[1]
betaHatFull <- fullRes$coef[2]
gammaHatFull <- fullRes$coef[3]

# Make traces
BWdataTraces <- dataTraces <- linesTraces <- vector("list",n)
for (i in 1:n) {
  dataTraces[[i]] <- pointsTile(x=x[i,], y=y[i,], col=collist[i], pch=16, plot=1)
  BWdataTraces[[i]] <- pointsTile(x=x[i,], y=y[i,], col="black", pch=16, plot=1)
  yfit <- c( min(x[i,])*betaHatFE + feHat[i],
             max(x[i,])*betaHatFE + feHat[i])  
  linesTraces[[i]] <- linesTile(x=c(min(x[i,]),max(x[i,])),y=yfit,col=collist[i], plot=1)
}
yfit <- c( min(x)*betaHatOVB + alphaHatOVB,
          max(x)*betaHatOVB + alphaHatOVB)
OVBlinesTraces <- linesTile(x=c(min(x),max(x)),y=yfit,col="black", lwd=4, lty="dashed", plot=1)
yfit <- c( min(x)*betaHatFull + mean(z)*gammaHatFull + alphaHatFull,
          max(x)*betaHatFull + mean(z)*gammaHatFull + alphaHatFull)
FlinesTraces <- linesTile(x=c(min(x),max(x)),y=yfit,col="black", lwd=4,plot=1)


# rug
rugY <- rugTile(y=feHat+min(x)*betaHatFE, type="lines", plot=1)

limits <- c(min(x), max(x), min(c(feHat+min(x)*betaHatFE, y)), max(c(feHat+min(x)*betaHatFE, y)))

tile(BWdataTraces,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="County Avg Income"),
     yaxistitle=list(labels="Republican Vote Share"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="fe1", width=4)
     )


tile(BWdataTraces,
     OVBlinesTraces,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="County Avg Income"),
     yaxistitle=list(labels="Republican Vote Share"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="fe2", width=4)
     )


tile(dataTraces,
     OVBlinesTraces,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="County Avg Income"),
     yaxistitle=list(labels="Republican Vote Share"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="fe3", width=4)
     )



tile(dataTraces,
     linesTraces,
     OVBlinesTraces,
     rugY,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="County Avg Income"),
     yaxistitle=list(labels="Republican Vote Share"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="fe4", width=4)
     )

tile(dataTraces,
     linesTraces,
     FlinesTraces,
     rugY,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="County Avg Income"),
     yaxistitle=list(labels="Republican Vote Share"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="fe5", width=4)
     )

tile(dataTraces,
     linesTraces,
     OVBlinesTraces,
     FlinesTraces,
     rugY,
     limits=limits,
     xaxis=list(add=FALSE),
     yaxis=list(add=FALSE),
     xaxistitle=list(labels="County Avg Income"),
     yaxistitle=list(labels="Republican Vote Share"),
     width=list(plot=1.5, yaxistitle=6),
     height=list(plot=1.5),
     frame=TRUE,
     output=list(file="fe6", width=4)
     )



