## Interpretation, PS 206 Class 4

logitdata <- read.table("PPIC_class1.txt", header=T, sep="\t")

logitdata <- na.omit(logitdata)

logitdata <- as.data.frame(logitdata)
attach(logitdata)

logitmodel <- glm(votemail ~  tenureres+youngkid+strongpart+age, family=binomial(link="logit"), data=logitdata)
summary(logitmodel)

## Set up interpretation

logit_coeffs <- logitmodel$coefficients
logit_covmat <- vcov(logitmodel)
ndraws <- 1000

install.packages("MASS")
library(MASS)

## random draws of coefficients

betadraw <- mvrnorm(ndraws, logit_coeffs, logit_covmat)

## variables are: constant, tenureres, youngkid, strongpart, age

hypind <- c(1,10,0,0,40)

cdfarg <- betadraw%*%hypind
hypprob <- exp(cdfarg)/(1 + exp(cdfarg))

meanprob <- mean(hypprob)
sdprob <- sd(hypprob)

print(meanprob)
print(sdprob)

## try a couple of other examples (note: normal cdf command is pnorm)


## The Zelig package makes this very convenient ##

install.packages("Zelig")
install.packages("VGAM")
library(Zelig)

logitmodel2 <- zelig(votemail ~  tenureres+youngkid+strongpart+age, model="logit", data=logitdata)
summary(logitmodel2)

hypind2 <- setx(logitmodel2, tenureres=10, youngkid=0, strongpart=0, age=40)

hypprob2 <- sim(logitmodel2, x = hypind2)

summary(hypprob2)

plot(hypprob2)



## More complicated interpretations
## This is a "difference in differences" setup

logit_coeffs <- logitmodel$coefficients
logit_covmat <- vcov(logitmodel)
ndraws <- 1000

#install.packages("MASS")
library(MASS)

## random draws of coefficients

betadraw <- mvrnorm(ndraws, logit_coeffs, logit_covmat)

## our 4 hypothetical individuals
## variables are: constant, tenureres, youngkid, strongpart, age

hypind1 <- c(1,10,0,0,20)
hypind2 <- c(1,10,1,0,20)
hypind3 <- c(1,10,0,0,80)
hypind4 <- c(1,10,1,0,80)

cdfargs1 <- betadraw%*%hypind1
cdfargs2 <- betadraw%*%hypind2
cdfargs3 <- betadraw%*%hypind3
cdfargs4 <- betadraw%*%hypind4

yk0age20 <- exp(cdfargs1)/(1 + exp(cdfargs1))
yk1age20 <- exp(cdfargs2)/(1 + exp(cdfargs2))
yk0age80 <- exp(cdfargs3)/(1 + exp(cdfargs3))
yk1age80 <- exp(cdfargs4)/(1 + exp(cdfargs4))

# differences

diffyk0 <- yk0age80 - yk0age20
diffyk1 <- yk1age80 - yk1age20

# difference in differences 

diffdiff <- diffsp1 - diffsp0

# display results

means <- cbind(mean(diffyk0),mean(diffyk1),mean(diffdiff))
sds <- cbind(sd(diffyk0),sd(diffyk1),sd(diffdiff))
zs <- means/sds
ps <- 2*(1 - pnorm(abs(zs)))
presults <- rbind(means,sds,zs,ps)
colnames(presults) <- c("diffyk0","diffyk1","diffdiff")
rownames(presults) <- c("Mean","SD","Z","P")

print(round(presults, digits=4))
