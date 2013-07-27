###### homework 4

iraq <- read.csv("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/homework 4/iraq.csv", header=TRUE, sep=",")

attach(iraq)

summary(iraq)

# turn rownames into cluster names
rownames(iraq) <- clustnam
rownames <- rownames(iraq)

# [1] "clustnam" "clustid"  "deathpre" "permopre" "mortpre"  "deathpos"
# [7] "permopos" "mortpos"  "sunni"    "kurd"     "shia"     "turnout" 
#[13] "regvtrs"  "trrate"   "pop"      "tpctpop"

#### parts a and b
# fitting three different poisson models
fit1 <- glm(deathpos ~ mortpre + sunni + kurd + shia + pop, family="poisson")
x1 <- 5
summary(fit1)

fit2 <- glm(deathpos ~ deathpre + sunni + kurd + shia + pop, family="poisson")
x2 <- 5
summary(fit2)

fit3 <- glm(deathpos ~ mortpre + sunni + kurd + shia, family="poisson")
x3 <- 4
summary(fit3)

# Likelihood Ratio test

ll.1 <- logLik(fit1)
ll.2 <- logLik(fit2)
ll.3 <- logLik(fit3)

lr.test.13 <- 2*(ll.1 - ll.3)
lr.test.13.p <- pchisq(lr.test.13,df=1,lower.tail=F)
lr.test.13.p

# BIC

bic.test.12 <- - 2*(ll.2 - ll.1) + (x2-x1)*log(nrow(iraq))
bic.test.12

bic.test.13 <- - 2*(ll.3 - ll.1) + (x3-x1)*log(nrow(iraq))
bic.test.13

bic.test.23 <- - 2*(ll.2 - ll.3) + (x2-x3)*log(nrow(iraq))
bic.test.23

# AIC

aic.test.12 <- - 2*(ll.2 - ll.1) + (x2-x1)*2
aic.test.12

aic.test.13 <- - 2*(ll.3 - ll.1) + (x3-x1)*2
aic.test.13

aic.test.23 <- - 2*(ll.2 - ll.3) + (x2-x3)*2
aic.test.23

# Deviance
d1 <- fit1$deviance
d2 <- fit2$deviance
d3 <- fit3$deviance

#### part c
# checking for outliers
hats <- hatvalues(fit3)/mean(hatvalues(fit3))
stresid <- rstudent(fit3)

plot(hats, stresid)
abline(h=c(-2,2), v=3, lty=2)
outlier <- identify(hats, stresid, rownames(iraq))
outlier

# fitting same three models with the outliers removed
# notice subset option in the glm command
fit1.out <- glm(deathpos ~ mortpre + sunni + kurd + shia + pop, family="poisson", subset=(clustnam != "Anbar" & clustnam != "Diala2"))
x1 <- 5
summary(fit1.out)

fit2.out <- glm(deathpos ~ deathpre + sunni + kurd + shia + pop, family="poisson", subset=(clustnam != "Anbar" & clustnam != "Diala2"))
x2 <- 5
summary(fit2.out)

fit3.out <- glm(deathpos ~ mortpre + sunni + kurd + shia, family="poisson", subset=(clustnam != "Anbar" & clustnam != "Diala2"))
x3 <- 4
summary(fit3.out)

# comparing the coefficients
cbind(fit1$coef, fit1.out$coef)
cbind(fit2$coef, fit2.out$coef)
cbind(fit3$coef, fit3.out$coef)

# Likelihood Ratio test
ll.1o <- logLik(fit1.out)
ll.2o <- logLik(fit2.out)
ll.3o <- logLik(fit3.out)

lr.test.13 <- 2*(ll.1o - ll.3o)
lr.test.13.p <- pchisq(lr.test.13,df=1,lower.tail=F)
lr.test.13.p

# BIC
bic.test.12 <- - 2*(ll.2o - ll.1o) + (x2-x1)*log(31)
bic.test.12

bic.test.13 <- - 2*(ll.3o - ll.1o) + (x3-x1)*log(31)
bic.test.13

bic.test.23 <- - 2*(ll.2o - ll.3o) + (x2-x3)*log(31)
bic.test.23

# AIC
aic.test.12 <- - 2*(ll.2o - ll.1o) + (x2-x1)*2
aic.test.12

aic.test.13 <- - 2*(ll.3o - ll.1o) + (x3-x1)*2
aic.test.13

aic.test.23 <- - 2*(ll.2o - ll.3o) + (x2-x3)*2
aic.test.23

# Deviance
d1o <- fit1.out$deviance
d2o <- fit2.out$deviance
d3o <- fit3.out$deviance

vcpo <- vcov(fit3)
copo <- fit3$coeff

#### part d
# simulating across all values of pre-invasion mortality rate for governates with significant Kurd population
library(MASS)
sims <- 100
simbetas <- mvrnorm(sims, copo, vcpo)
morthat <- seq(0, 14, .05)

simmuk <- NULL
simxbk <- NULL
simyk <- NULL
simxk <- NULL

simmu <- NULL
simxb <- NULL
simy <- NULL
simx <- NULL

for (i in 1:length(morthat)) {
simmuk <- rbind(1, morthat[i], mean(sunni), 1, mean(shia))
simxbk <- simbetas%*%simmuk
simy0k <- exp(simxbk)
simyk <- rbind(simyk, simy0)
simxk <- rbind(simxk,t(t(rep(morthat[i],sims))))
}

for (i in 1:length(morthat)) {
simmuk <- rbind(1, morthat[i], mean(sunni), 1, mean(shia))
simxbk <- simbetas%*%simmuk
simy0k <- exp(simxbk)
simyk <- rbind(simyk, simy0k)
simxk <- rbind(simxk,t(t(rep(morthat[i],sims))))
}

# for other governates
for (i in 1:length(morthat)) {
simmu <- rbind(1, morthat[i], mean(sunni), 0, mean(shia))
simxb <- simbetas%*%simmu
simy0 <- exp(simxb)
simy <- rbind(simy, simy0)
simx <- rbind(simx,t(t(rep(morthat[i],sims))))
}

source("http://faculty.washington.edu/cadolph/log/plot.simulates.r")


par(mfrow=c(1,1))
plot.simulates(simxk,
               simyk,
               ci.range=c(0.95),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(0,14,0,12),
               closeplot=FALSE,
               collist="light blue"
               )
               
plot.simulates(simx,
               simy,
               ci.range=c(0.95),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(0,14,0,12),
               closeplot=TRUE,
               addtoplot=TRUE,
               collist="lavender"
               )             
               

#### part e
# fit a negative binomial to model 3 and test for overdispersion               
fit2.nb <- glm.nb(deathpos ~ mortpre + sunni + kurd + shia + pop)

alpha.nb <- 1/fit2.nb$theta 

ll.nb <- logLik(fit2.nb)

G2 <- 2*(ll.nb - ll.2)

pchisq(G2, 1, lower.tail=FALSE)               
