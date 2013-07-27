library(foreign)
library(R2WinBUGS)

#Set Working Directory
setwd()

#Note Variable Names Changed to Work in WinBugs

cong <- read.dta("jacob.dta")
cong <- na.omit(cong)
attach(cong)

cong.additive.2 <- as.data.frame(cbind(chalvote, chalspend, presvote, logchecks1, redist))
write.table(cong.additive.2, "congadd2.txt", row.names=FALSE)

program.file.name="additive3.txt"
inits.b = rep(0,20)
inits <- function(){list(beta=c(0,0,0,0,0), b=inits.b, taub=0.01, taueps=0.01)}
parameters <- list("lambda", "sigmab", "sigmaeps", "beta", "b")


data <- read.table("congadd2.txt", header=TRUE)
attach(data)
#Obtain Number of Observations
n <- length(chalspend)
X <- cbind(rep(1,n), chalspend, presvote, logchecks1, redist)
num.knots <- 20
#Define knots used for p-spline at equal spaced quantiles of the covariate
knots<-quantile(unique(chalspend), seq(0,1,length=(num.knots+2))[-c(1,(num.knots+2))])

Z.K <- (abs(outer(chalspend, knots, "-")))^3
OMEGA.all <- (abs(outer(knots, knots, "-")))^3
svd.OMEGA.all <- svd(OMEGA.all)
sqrt.OMEGA.all <- t(svd.OMEGA.all$v %*% (t(svd.OMEGA.all$u)*sqrt(svd.OMEGA.all$d)))
Z <- t(solve(sqrt.OMEGA.all, t(Z.K)))

data <- list("chalvote", "X", "Z", "n", "num.knots")

Bayes.fit <- bugs(data, inits, parameters, model.file = "additive3.txt",
                   n.chains = 1, n.iter=20000, n.burnin=5000,
                   n.thin = 2, debug = TRUE, DIC = FALSE, digits =5, codaPkg = FALSE, 
                   bugs.directory = "c:/Program Files/WinBUGS14/")
                   
attach.bugs(Bayes.fit)
#Obtain the posterior simulation from the overall mean function
X.1 <- X[,1:2]
beta.1 <- as.matrix(beta[,1:2])
m <- X.1 %*% t(beta.1)
m = X.1 %*% t(beta.1) + Z %*% t(b)

#Obtain the median and 95% credible interval for the overall mean function
Bayes.mean.inference<-rep(0,3*nrow(m))
dim(Bayes.mean.inference)<-c(nrow(m),3)
for (i in 1:nrow(m))

    {
    Bayes.mean.inference[i,]<-quantile(m[i,],probs=c(0.025,0.5,0.975))
    } 

o=order(chalspend)
chalspend.o=chalspend[o]
ordered.mean.Bayes=Bayes.mean.inference[o,]

#Plot Both For Comparison


#####################################################################################

library(mgcv)
gam <- gam(chalvote ~ s(chalspend, bs="cr") + presvote + logchecks1 + 
                       redist, data=cong) 

#Figure 7.2
par(mfrow = c(1,2))
plot(chalspend.o, chalvote, pch=".", xlab="Challenger Spending",
ylab="Challengers' Vote Share (%)", main="Bayesian Estimates", ylim=c(12,34), bty="l")
lines(chalspend.o,ordered.mean.Bayes[,2],lwd=1)
lines(chalspend.o,ordered.mean.Bayes[,1],lty=2)
lines(chalspend.o,ordered.mean.Bayes[,3],lty=2)


plot(gam, select=1, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", 
xlab="Challenger Spending", residual=FALSE, shift=18.23, main="Frequentist Estimates", bty="l")
points(chalspend, chalvote, pch=".", cex=1)
