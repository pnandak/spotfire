
#Set Working Directory
setwd()

library(foreign)
library(mgcv)
library(splines)

cong <- read.dta("jacob.dta")
attach(cong)

#Bootsrap Nonparametric Fit

#Basic Fit
gam.1 <- gam(chal.vote ~ s(perotvote, bs="cr"))
#Create Residuals
res <- (chal.vote - fitted(gam.1))

#Number of Bootstrap Resamples
nboot <- 250

#Sample Size
n <- length(case)

#Matrix To Store Bootstrap Samples
boot.est <- matrix(0, nrow=nboot, ncol=n)
perot <- seq(min(perotvote), max(perotvote), length=312)
for(i in 1:nboot){
    k.star <- sample(cong$case, n, replace=TRUE)
    y.star <- chal.vote + res[k.star]
    gam.boot <- gam(y.star ~ s(perotvote, bs="cr"), data=cong)
    pfit <- predict(gam.boot, data.frame(perotvote=perot))
    boot.est[i,] <- pfit
}

boot.con.up <- rep(0,n)
boot.con.lo <- rep(0,n)

for(i in 1:n){
  boot.con.up[i] <- quantile(boot.est[,i], prob=.975)
  boot.con.lo[i] <- quantile(boot.est[,i], prob=.025)
  }

#Figure 8.3
plot(gam.1, rug=FALSE, se=TRUE, ylab = "Challenger's Vote Share", xlab= "Perot Vote", residual=FALSE, shift=35.42, bty="l")
lines(perot, boot.con.up, lty=4)
lines(perot, boot.con.lo, lty=4)

legend(10,20, c("Bootstrapped Confidence Intervals", "Asymptotic Confidence Intervals"), lty=c(4,2), cex=.8, bty="n", y.intersp = 1.25)

#Bootstrapping Nonlinearity Tests

#Standard Model
cong.ols <- gam(chal.vote ~ exp.chal + chal.spend + inc.spend + pres.vote + logchecks1 + 
          marginal + partisan.redist + log.perot, data=cong)       
#Additive Model Fit
gam.1 <- gam(chal.vote ~ exp.chal + chal.spend + inc.spend + pres.vote + s(checks.raw) + 
          marginal + partisan.redist + log.perot, data=cong)

#Test Whether We Improve Overall Fit - Classical Method          
1 - pchisq(deviance(cong.ols) - deviance(gam.1), 1.22)

#Time For Bootstrap
#1. Calc and Save Original T-stat
#1a. Start Loop
#2. Form y* (How Many Times?)
#3. Est Standard Model
#4. Est. Gam
#5. Calc New Test Stat And Save
#5a. Drop y*star
#6. End Loop Here
#7. Calc new p-vale

#Rewrite Function to Resample Residuals
#Write Bootstrap Function To Resample Pairs
set.seed(34521253)
#Standard Tests

mod.1 <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend + pres.vote + checks.raw + 
          marginal + partisan.redist + perotvote, data=cong)       

#GCV Smoothing
mod.2 <- gam(chal.vote ~ exp.chal + s(chal.spend.raw,  bs="cr") + inc.spend + pres.vote + checks.raw +  marginal + partisan.redist + perotvote, data=cong)
 
anova(mod.1, mod.2, test='Chisq')

#Manual Smoothing Overfit
mod.3 <- lm(chal.vote ~ exp.chal + ns(chal.spend.raw, df=6) + inc.spend + pres.vote + checks.raw +  marginal + partisan.redist + perotvote, data=cong) 

mod.4 <- lm(chal.vote ~ exp.chal + chal.spend.raw + inc.spend + pres.vote + checks.raw + 
          marginal + partisan.redist + perotvote, data=cong)  

anova(mod.4, mod.3)

#Bootstrap Replications
R <- 999

#Original T-stat
t <- mod.1$deviance - mod.2$deviance
#Vector To Store Bootstrapped T-stats
t.star <- matrix(NA,R,1)
n <- length(chal.vote)
#Original Model
ols <- gam(chal.vote ~ exp.chal + chal.spend.raw + inc.spend + pres.vote + checks.raw + 
          marginal + partisan.redist + perotvote, data=cong) 
resid <- residuals(ols)
pred <- fitted(ols)

for(i in 1:R){
    k.star <- sample(cong$case, n, replace=TRUE)
    y.star <- pred + resid[k.star]
       
    mod.1 <- gam(y.star ~ exp.chal + chal.spend.raw + inc.spend + pres.vote + checks.raw + 
          marginal + partisan.redist + perotvote, data=cong)       

    mod.2 <- gam(y.star ~ exp.chal + s(chal.spend.raw, bs="cr") + inc.spend + pres.vote+ checks.raw + 
          marginal + partisan.redist + perotvote, data=cong) 
                   
t.star[i] <- mod.1$deviance - mod.2$deviance

rm(y.star, mod.1, mod.2)
}

#Calculate P-value
(sum(t.star>=t)+1)/(R+1)

#Histogram of the Distribution
library(KernSmooth)
h <- dpih(t.star)
bins <- seq(min(t.star)-0.1, max(t.star)+0.1+h, by=h)
hist(t.star, breaks=bins, col="grey")



















#Replication of Example 4.13 in Davison and Hinkley

sample.1 <- c(82,79,81,79,77,79,79,78,79,82,76,73,64)
sample.2 <- c(84,86,85,82,77,76,77,80,83,81,78,78,78)
data <- as.data.frame(sample.1,sample.2)
R <- 999

#Compare to Standard T-test
t <- mean(sample.2) - mean(sample.1)
#Vector To Store Bootstrapped T-stats
t.star <- matrix(NA,R,1)
n <- length(sample.1)
#Original Model

for(i in 1:R){
    y.star.1 <- sample(sample.1, n, replace=T)
    y.star.2 <- sample(sample.2, n, replace=T)
          
t.star[i] <- mean(sample.2) - mean(sample.1)

}

(sum(t.star>t)+1)/(R+1)

setwd("C:\\Documents and Settings\\keele.4\\My Documents\\Poli Sci 867\\Robust Estimation")
emo <- read.dta("emo.dta")

#emo <- na.omit(emo)
attach(emo)

R <- 9999

#Compare to Standard T-test
t <- t.test(anx.0 , anx.1, data=emo)$statistic
#Vector To Store Bootstrapped T-stats
t.star <- matrix(NA,R,1)
n <- length(sample.1)
#Original Model

for(i in 1:R){
    y.star.1 <- sample(anx.0, n, replace=T)
    y.star.2 <- sample(anx.1, n, replace=T)
          
t.star[i] <- t.test(anx.0, anx.1)$statistic

}

(sum(t.star>t)+1)/(R+1)
