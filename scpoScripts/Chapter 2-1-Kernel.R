
#Load Libraries
library(foreign)
library(SemiPar)
library(mgcv)

#Set Working Directory
setwd()

#Read the data
jacob <- read.dta("jacob.dta")
attach(jacob)

#Basic Scatterplot
#Figure 2.1
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l")
points(perotvote, chal.vote, pch=21)


#OLS Fit for Comparison
ols <- lm(chal.vote ~ perotvote, data=jacob)

#Data Prep For Moving Average Smoother
cong <- as.data.frame(jacob[,2:3])
cong <- cong[order(cong$perotvote),1:2]

y <- as.matrix(cong$chal.vote)
x <- as.matrix(cong$perotvote)
n <- length(y)

#Moving Average Smoother
#Bin Parameter
k <- 51

xhat <- x[((k-1)/2+1) : (n-(k-1)/2)]   #drop observations at boundaries

yhat <- y[1:(n-k+1)]
for (i in 1:(k-1)) {
   yhat <- yhat + y[(1+i):(i+(n-k+1))]
   }
yhat <- yhat/k

#Add in MA Smoother
#Figure 2.2
  plot(x,y, type="n", cex=.65, xlab="Perot Vote (%)", ylab="Challenger's Vote Share(%)", bty="l")
  points(x, y, pch=".", cex=1.5)
  lines(xhat,yhat,lwd=1)
  abline(ols,lwd=1)

#Expanding the Bin Width
k <- 101
xhat.101 <- x[((k-1)/2+1) : (n-(k-1)/2)]   #drop observations at boundaries
yhat.101 <- y[1:(n-k+1)]
for (i in 1:(k-1)) {
   yhat.101 <- yhat.101 + y[(1+i):(i+(n-k+1))]
   }
yhat.101 <- yhat.101/k

#Contracting the Bin Width
k <- 25
xhat.25<- x[((k-1)/2+1) : (n-(k-1)/2)]   #drop observations at boundaries
yhat.25 <- y[1:(n-k+1)]
for (i in 1:(k-1)) {
   yhat.25 <- yhat.25 + y[(1+i):(i+(n-k+1))]
   }
yhat.25 <- yhat.25/k

#Figure 2.3
par(mfrow=c(3,1))
  plot(x,y, type="n", cex=.65, xlab="Perot Vote (%)", ylab="Challenger's Vote Share (%)", main="Bin Width: 25", bty="l")
  points(x, y, pch=".", cex=1.5)
  lines(xhat.25,yhat.25,lwd=1)
  
  plot(x,y, type="n", cex=.65, xlab="Perot Vote (%)", ylab="Challenger's Vote Share (%)", main="Bin Width: 51", bty="l")
  points(x, y, pch=".", cex=1.5)
  lines(xhat,yhat,lwd=1)
  
  plot(x,y, type="n", cex=.65, xlab="Perot Vote (%)", ylab="Challenger's Vote Share (%)", main="Bin Width: 101", bty="l")
  points(x, y, pch=".", cex=1.5)
  lines(xhat.101,yhat.101,lwd=1)
  
#Clean Up Workspace
rm(cong, x, y, yhat, xhat, xhat.25, yhat.25, xhat.101, yhat.101, i, k)
###############################################################################
#Kernel Density Estimation

#Illustration of Kernel Concepts

#Step1
#Defining the Window Width
attach(jacob)

ord <- order(perotvote)
perot <- perotvote[ord]
#perot <- sort(perotvote)
pre <- sort(chal.vote)
x0 <- perot[75]
diffs <- abs(perot - x0)
which.diff <- sort(diffs)[120]

#Figure 2.4
plot(perotvote, chal.vote, type="n", cex=.65, xlab="Perot Vote (%)", ylab="Challenger's Vote Share (%)", bty="l")
abline(v=c(x0 - which.diff, x0 + which.diff), lty = 2)
abline(v=x0)
points(perot[diffs > which.diff], chal.vote[diffs > which.diff], pch=16, cex=1, col=gray(.75))
points(perot[diffs <= which.diff], chal.vote[diffs <= which.diff], cex=1)
dev.off()

x.n <- perot[diffs<= which.diff]
y.n <- perot[diffs <= which.diff]

#Applying the Tricube Weight
#Tricube function

a <- seq(0,1, by=.1)

tricube <- function(z) {
    ifelse (abs(z) < 1, (1 - (abs(z))^3)^3, 0)
    }
    
    tricube(a)

#Figure 2.5
plot(range(perot), c(0,1), xlab="Perot Vote (%)", ylab="Tricube Weight", type='n', bty="l")
abline(v=c(x0-which.diff, x0+which.diff), lty=2)
abline(v=x0)
xwts <- seq(x0-which.diff, x0+which.diff, len=250)
lines(xwts, tricube((xwts-x0)/which.diff), lty=1, lwd=1)
points(x.n, tricube((x.n - x0)/which.diff), cex=1)
points(perot[diffs > which.diff], chal.vote[diffs > which.diff], pch=16, cex=1, col=gray(.75))


###########################################################################
#Kernel Smoothing
###########################################################################

Figure 2.6
par(mfrow=c(3,1))
plot(perotvote, chal.vote, type="n", cex=.65, xlab="Perot Vote (%)", ylab="Challenger's Vote Share (%)", main="Bandwidth = 4", bty="l")
points(perotvote, chal.vote, pch=".", cex=1.95)
lines(ksmooth(perotvote, chal.vote, bandwidth="4"))

plot(perotvote, chal.vote, type="n", cex=.65, xlab="Perot Vote (%)", ylab="Challenger's Vote Share (%)", main="Bandwidth = 8", bty="l")
points(perotvote, chal.vote, pch=".", cex=1.95)
lines(ksmooth(perotvote, chal.vote, bandwidth="8"), lty=1)

plot(perotvote, chal.vote, type="n", cex=.65, xlab="Perot Vote (%)", ylab="Challenger's Vote Share (%)", main="Bandwidth = 12", bty="l")
points(perotvote, chal.vote, pch=".", cex=1.95)
lines(ksmooth(perotvote, chal.vote, bandwidth="12"), lty=1)

