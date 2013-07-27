#######################
# R code for ordered logit   #
#######################

library(MASS)

odata <- read.table("C:\\courses\\Essex\\BES2010.txt", header=T, sep="\t")

omodel <- polr(as.factor(affect_fc) ~ rent_own+income+app_Brown, data = odata, method="logistic", Hess=TRUE)
summary(omodel)

coeffs <- omodel$coefficients
cutpoints <- omodel$zeta
ocoeffs <- c(coeffs,cutpoints)
ocovmat <- solve(omodel$Hessian)

## hypothetical individual ##

ndraws <- 1000

betadraw <- mvrnorm(ndraws, ocoeffs, ocovmat)

# vector for hypothetical individual here
# This example is a renter, middle of the income scale, neutral towards Brown

hypind <- c(1,8,5)


linkfn <- plogis
  if(omodel$method=="probit"){
    linkfn <- pnorm
  }

xb <- betadraw[,1:3]%*%hypind

probc1 <- linkfn(betadraw[,4] -xb)
probc2 <- linkfn(betadraw[,5] - xb) - probc1
probc3 <- linkfn(betadraw[,6] - xb) - probc2 - probc1
probc4 <- 1 - probc3 - probc2 - probc1


# display results

means <- cbind(mean(probc1),mean(probc2),mean(probc3),mean(probc4))
sds <- cbind(sd(probc1),sd(probc2),sd(probc3),sd(probc4))
zs <- means/sds
ps <- 2*(1 - pnorm(abs(zs)))
presults <- rbind(means,sds,zs,ps)
colnames(presults) <- c("Strongly Affected","Somewhat Affected","Hardly Affected", "Unaffected")
rownames(presults) <- c("Mean","SD","Z","P")
print(presults)

install.packages("Zelig")
install.packages("VGAM")

library(Zelig)

ologitmod<- zelig(as.factor(affect_fc) ~ rent_own+income+app_Brown, model="ologit", data=odata)
summary(ologitmod)

hypind <- setx(ologitmod, rent_own = 1, income = 8, app_Brown = 5)

hypprobs <- sim(ologitmod, x = hypind)

summary(hypprobs)

plot(hypprobs)