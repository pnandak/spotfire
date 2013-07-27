################################
### R code to estimate a MNL/CL   ###
################################


ReadData <- read.table("C:\\Essex\\BES2010.txt", header=T, sep="\t")

## Note we are not removing missing data here, since this data is already cleaned
## You might need to insert a step to remove missing values with other data

### Insert the names of the independent variables             ###
### that do not vary across alternatives ###
### inside the second set of parentheses, separated by commas ###

indspec <- subset(ReadData,select=c(persfin,natecon,affect_fc,app_Afgh,MP_resign))

### Insert the names of the independent variables             ###
### that vary across alternatives ###

altspec <- subset(ReadData,select=c(app_Brown,app_Cameron,app_Clegg))

### Enter the name for your variable that varies across alternatives ###

altlong <- "approval"

### Insert the name of the dependent variable here ###

depvar <- subset(ReadData,select=c(vote_intent))


###################################################


## pick out variable names to use ##

indnames <- colnames(indspec)
altnames <- colnames(altspec)
depname <- colnames(depvar)

## convert data from wide to long ##

MNLData <- reshape(ReadData, direction="long", idvar="id", timevar="altnum", varying=altnames, v.names=altlong)


## put together data ##

obs_id <- subset(MNLData,select=c(id))
group <- as.character(t(obs_id))

altvar <- subset(MNLData,select=c(altlong))

altnum <- subset(MNLData,select=c(altnum))

depvar1 <- subset(MNLData,select=c(depname))
y <- as.numeric(depvar1==altnum)


cons1 <- as.numeric(altnum==1)
cons2 <- as.numeric(altnum==2)

selX <- subset(MNLData, select=c(indnames))

X1 <- selX * cons1
X1 <- cbind(X1, cons1)
X2 <- selX * cons2
X2 <- cbind(X2, cons2)

## likelihood function ##

mnl.lf <- function(b, X, y, group) {

beta <- b[1:K]

exb <- exp(X%*%beta)

denom <- tapply(exb,group,sum)

exby <- exb * y
numer <- tapply(exby,group,sum)

frac <- numer/denom

logfrac <- log(frac)

logl <- -sum(logfrac)

return(logl)

}

## gradient function ##

mnl.gr <- function(b, X, y, group) {

beta <- b[1:K]

exb <- exp(X%*%beta)

denom <- tapply(exb,group,sum)

denom <- as.matrix(denom[group])

probcy <- y - (exb/denom)

grad <- t(X)%*%probcy  

return(-grad)

}  



## First a MNL

X <- data.matrix(cbind(X1, X2))

## variable names ##

X1names <- colnames(X1)
X1names[X1names=="cons1"] <- "cons"
X1names.1 <- paste(X1names,".1",sep="")
X2names <- colnames(X2)
X2names[X2names=="cons2"] <- "cons"
X2names.2 <- paste(X2names,".2",sep="")

varnames <- c(X1names.1,X2names.2)

K <- as.numeric(ncol(X))


### start values ###

svmnl <- matrix(data=0,nrow=K,ncol=1)


mnl <- optim(svmnl, mnl.lf, gr=mnl.gr, method="BFGS", control=list(trace=TRUE, REPORT=1), 
hessian=TRUE, X=X, y=y, group=group)
coeffs <- mnl$par
covmat <- solve(mnl$hessian)
stderr <- sqrt(diag(covmat))
zscore <- coeffs/stderr
pvalue <- 2*(1 - pnorm(abs(zscore)))
results <- cbind(coeffs,stderr,zscore,pvalue)
colnames(results) <- c("Coeff.", "Std. Err.", "z", "p value")
rownames(results) <- varnames
print(results)  


## Now a CL

X <- data.matrix(cbind(altvar, X1, X2))

## variable names ##

altname_f <- colnames(altvar)
X1names <- colnames(X1)
X1names[X1names=="cons1"] <- "cons"
X1names.1 <- paste(X1names,".1",sep="")
X2names <- colnames(X2)
X2names[X2names=="cons2"] <- "cons"
X2names.2 <- paste(X2names,".2",sep="")

varnames.cl <- c(altname_f,X1names.1,X2names.2)

K <- as.numeric(ncol(X))

### start values ###

svmnl <- matrix(data=0,nrow=K,ncol=1)

cl <- optim(svmnl, mnl.lf, gr=mnl.gr, method="BFGS", control=list(trace=TRUE, REPORT=1), 
hessian=TRUE, X=X, y=y, group=group)
coeffs.cl <- cl$par
covmat.cl <- solve(cl$hessian)
stderr.cl <- sqrt(diag(covmat.cl))
zscore.cl <- coeffs.cl/stderr.cl
pvalue.cl <- 2*(1 - pnorm(abs(zscore.cl)))
results.cl <- cbind(coeffs.cl,stderr.cl,zscore.cl,pvalue.cl)
colnames(results.cl) <- c("Coeff.", "Std. Err.", "z", "p value")
rownames(results.cl) <- varnames.cl
print(results.cl)  



### predicted probabilities for the CL ###

## modify the hypothetical individual if omitting thermometer scores ##

ndraws <- 1000

# comment out next line if already installed 
#install.packages("MASS")
library(MASS)

betadraw <- mvrnorm(ndraws, coeffs.cl, covmat.cl)

# create hypothetical individual
# Rates Brown a 3, others at 5.  
# Neutral on persfin and natecon (2,2), somewhat affected by financial crisis(1) , approves of war in Afghanistan (1), neutral on MP's resign (2)
# don't forget constant at the end

rate_Brown <- 3
rate_Cameron <- 5
rate_Clegg <- 5
individual_spec <- c(2, 2, 1, 1, 2, 1)

normalize <- matrix(data=0,nrow=1,ncol=(length(individual_spec)))

hypind_alt1 <- c(rate_Brown,individual_spec,normalize)
hypind_alt2 <- c(rate_Cameron,normalize,individual_spec)
hypind_alt3 <- c(rate_Clegg,normalize,normalize)

xb1 <- betadraw%*%hypind_alt1
xb2 <- betadraw%*%hypind_alt2
xb3 <- betadraw%*%hypind_alt3

exb1 <- exp(xb1)
exb2 <- exp(xb2)
exb3 <- exp(xb3)

denom <- exb1 + exb2 + exb3

prob1 <- exb1/denom
prob2 <- exb2/denom
prob3 <- exb3/denom

means <- cbind(mean(prob1),mean(prob2),mean(prob3))
sds <- cbind(sd(prob1),sd(prob2),sd(prob3))
zs <- means/sds
ps <- 1 - pnorm(abs(zs))
presults <- rbind(means,sds,zs,ps)
colnames(presults) <- c("Alternative 1","Alternative 2","Alternative 3")
rownames(presults) <- c("Mean","SD","Z","P")

print(presults)


## can also do MNLs (but not CLs) in Zelig

library(Zelig)

mnlmodel <- zelig(as.factor(vote_intent) ~ persfin+natecon+affect_fc+app_Afgh+MP_resign, model="mlogit", data=ReadData)
summary(mnlmodel)

hypind <- setx(mnlmodel, persfin=2, natecon=2, affect_fc=1, app_Afgh=1, MP_resign=1)

hypprobs <- sim(mnlmodel, x = hypind)

summary(hypprobs)

plot(hypprobs)

# you can also do a ternary plot

install.packages("vcd")
library(vcd)

ternaryplot(x = hypprobs$qi$ev, pch = "+", col = "red", main="2010 British General Election")


########
## The mlogit package is excellent for these kinds of models ##
########


install.packages("mlogit")

library(mlogit)

MNLData2 <- mlogit.data(ReadData, choice="vote_intent", shape="wide", varying=1:3, sep="_", alt.levels=c("Brown","Cameron","Clegg"))


## The MNL from earlier ##

mnl2 <- mlogit(vote_intent ~ 0 | persfin+natecon+affect_fc+app_Afgh+MP_resign, data=MNLData2, reflevel="Clegg")

summary(mnl2)


## The CL from earlier ##

clogit2 <- mlogit(vote_intent ~ app | persfin+natecon+affect_fc+app_Afgh+MP_resign, data=MNLData2, reflevel="Clegg")

summary(clogit2)


## We can also get the coefficients and covariance matrix for calculating predicted probabilities

coeffs <- clogit2$coefficients
covmat <- clogit2$hessian

## A Hausman-McFadden test for our MNL

mnl2alt <- mlogit(vote_intent ~ 0 | persfin+natecon+affect_fc+app_Afgh+MP_resign, data=MNLData2, alt.subset=c("Brown", "Cameron"))

hmftest(mnl2,mnl2alt)

