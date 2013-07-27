## Section 8
## Jens Hainmueller and Jenn Larson


### Additional Zelig Hints
library(Zelig)

# setx() flexbility


# load some data
camp <- read.table("http://www.courses.fas.harvard.edu/~gov2001/Data/camp.dat", header=T)

# fit a logit
z.out <- zelig(DWIN~JULYECQ2+PRESINC+ADAACA, model="logit", data=camp)
summary(z.out)

# get an expeced value
X.low <- setx(z.out, PRESINC = 1)
s.out <- sim(z.out, x = X.low, num=1000)
# numerical summary
summary(s.out)
#graphical summary
plot(s.out)

# get a first difference
X.low <- setx(z.out, PRESINC = 1)
X.high <- setx(z.out, PRESINC = 0)
s.out <- sim(z.out, x = X.low, x1 = X.high, num=1000)
# numerical summary
summary(s.out)
# extract FD
mean(s.out$qi$fd)
# CI
quantile(s.out$qi$fd, quantile(c(.025,.975)))
#graphical summary
plot(s.out)

# simple setx
x.out <- setx(z.out,fn = list(numeric = median, ordered = mean,others = mode))
x.out

# another way
x.out <- setx(z.out,fn = list(numeric = median, ordered = mean,others = mode))
x.out

# custom
x.out <- setx(z.out,PRESINC=10)
x.out

#set JULYECQ2 to 75th percentile
x.out[,2] <- quantile(camp$JULYECQ2, .75)
x.out[,2]

# conditional prediction (whole data at observed values)
x.out <- setx(z.out,cond=T)
x.out

# conditional prediction (whole data observed values from another dataset)
x.out <- setx(z.out,cond=T,data=ENTERDATAHERE)
x.out

#proceed with sim() as before
s.out <- sim(z.out, x.out)

# Be careful with interaction terms. You should enter interaction terms in one of the two
# ways listed below, rather than creating a new variable. This is so setx() sets the interaction
# term to the appropriate value when doing simulations.

# enter interaction terms as follows:
z.out <- zelig(DWIN~JULYECQ2*PRESINC+ADAACA, model="logit", data=camp)
summary(z.out)

#equivalent to:
z.out <- zelig(DWIN~JULYECQ2+PRESINC+ADAACA+JULYECQ2:PRESINC,model="logit", data=camp)
summary(z.out)

#this is important when you use setx()
x.out <- setx(z.out, ADAACA=3)
x.out

#which matches the correct way
mean(camp$JULYECQ2)*mean(camp$PRESINC)

#but if you had defined a new variable JULYECQ2*PRESINC, and then input
#that in z.out(), this is how setx() would set the value:
camp$JULYECQ2.PRESINC <- camp$JULYECQ2*camp$PRESINC
z.out <- zelig(DWIN~JULYECQ2+PRESINC+ADAACA+JULYECQ2.PRESINC,model="logit", data=camp)
summary(z.out)
#this is important when you use setx()
x.out <- setx(z.out, ADAACA=3)
x.out

# this is equal to:
mean(camp$JULYECQ2*camp$PRESINC)
# and not
mean(camp$JULYECQ2)*median(camp$PRESINC)

# also always make sure to adjust interactions accordingly if
# you want to simulate effects of changing lower order terms
z.out <- zelig(DWIN~JULYECQ2+PRESINC+ADAACA+JULYECQ2:PRESINC,model="logit", data=camp)
summary(z.out)

# example: first difference if presinc goes from 1 to 0
x.low  <- setx(z.out, ADAACA=3,PRESINC=1)
x.high <- setx(z.out, ADAACA=3,PRESINC=0)
x.low
x.high

##
# Producing Graphs From Lecture
# Zelig can produce the graphs Gary has been discussing in lecture, including ROC plots,
# confidence intervals, and density plots.

#ROC plots for two models:

#without interaction
z.out <- zelig(DWIN~JULYECQ2+PRESINC+ADAACA, model="logit", data=camp)
#with interaction
z.out1 <- zelig(DWIN~JULYECQ2+PRESINC+ADAACA+JULYECQ2:PRESINC,model="logit", data=camp)

# plot
rocplot(z.out$y, z.out1$y, fitted(z.out), fitted(z.out1))
legend("bottomleft", c("Without Interaction", "With Interaction"), lty=c(1:2))

#vertical CI graphs
ADAACA.range <- min(camp$ADAACA):max(camp$ADAACA)
ADAACA.range
x.low  <- setx(z.out, JULYECQ2=quantile(camp$JULYECQ2, .15), PRESINC=1,ADAACA=ADAACA.range)
x.high <- setx(z.out, JULYECQ2=quantile(camp$JULYECQ2, .85), PRESINC=1,ADAACA=ADAACA.range)
s.out <- sim(z.out, x.low, x.high)

# do the plot
plot.ci(s.out, ylab="Probability of Democrat Winning")
legend("topleft", c("Low Growth", "High Growth"), col=c("blue", "red"),lty="solid")

#and a density plot
x.low <- setx(z.out, JULYECQ2=quantile(camp$JULYECQ2, .25), PRESINC=1)
x.high <- setx(z.out, JULYECQ2=quantile(camp$JULYECQ2, .75), PRESINC=1)
s.low <- sim(z.out, x.low)
s.high <- sim(z.out, x.high)
plot(density(s.high$qi$ev, from=.5, to=1), xlab="Probability of Democrat Winning", main="Effect of Economic Growth")
lines(density(s.low$qi$ev), lty=2)
legend("topright", c("Low Growth", "High Growth"), lty=2:1)


## ALL THESE COMMANDS WORK THE SAME FOR (ALMOST) ALL MODELS


##################################
### Quick Review: Ordered Probit

# load sanction data
snct <- read.table("http://isites.harvard.edu/fs/docs/icb.topic239190.files/PS7/snct.dat",header=T)

# outcome
Y <- snct$RES # policy result: 1=failed outcome, 2=unclear but possibly positive,3=positive outcome, 4=successful outcome
# X vars: Import sanction 1/0, covert = covert actions too in addition to sanctions? 0/1
X <- cbind(1, snct[,c("IMPORT","COVERT")])

### OLS
summary(lm(RES ~ IMPORT+COVERT,data=snct))

# Ordered Probit
library(Zelig)
z.out <- zelig(as.factor(RES) ~ IMPORT+COVERT,model="oprobit",data=snct)

# Predicted Probabilities
# coefs
b <- z.out$coef
# latent Y
Y.star <-as.matrix(X[,2:3])%*%b

# Thresholds
taucut <- z.out$zeta

# PLOT DISTRIBUTION FOR MEAN Y.STAR
x <- seq(from=-2,to=6,by=.1)
plot(x,dnorm(x,mean=mean(Y.star)))
abline(v=taucut[1],col="red")
abline(v=taucut[2],col="blue")
abline(v=taucut[3],col="green")

###INITIALIZE MATRIX TO STORE PREDICTIONS
###EACH ENTRY IS PROBABILITY THAT COUNTRY I REALIZES CATEGORY J UNDER THE MODEL
pred.mat <- matrix(NA,nrow(X),length(unique(Y)))
colnames(pred.mat) <- paste("Pr that Res=",1:4)
head(pred.mat)

# compute areas under probability density function
pred.mat[,1] <- pnorm(taucut[1]-Y.star)
pred.mat[,2] <- pnorm(taucut[2]-Y.star) - pnorm(taucut[1]-Y.star)
pred.mat[,3] <- pnorm(taucut[3]-Y.star) - pnorm(taucut[2]-Y.star)
pred.mat[,4] <- 1- apply(pred.mat[,1:3],1,sum)

###PREDICTED PROBABILITIES FROM MODEL
round(pred.mat,digits=2)
round(apply(pred.mat,2,mean),digits=3)
round(table(Y)/sum(table(Y)),digits=3)

# check with zelig
X0    <- setx(z.out,cond=T)
s.out <- sim(z.out, x = X0, num=1000)
summary(s.out)


## Whatif
#
library(WhatIf)
#WhatIf implements the methods for evaluating counterfactuals introduced in King and
#(2006) and King and Zeng (2007):
#Gary King and Langche Zeng. 2006. “The Dangers of Extreme Counterfactuals,”
#Political Analysis 14 (2): 131–159.
#and
#Gary King and Langche Zeng. 2007. “When Can History Be Our Guide? The Pitfalls
#of Counterfactual Inference,” International Studies Quarterly 51 (March): 183–210.


# data from Doyle, Michael W. and Nicholas Sambanis APSR 2000 who
# investigate the effect of UN Peacekeeping Intermission
# untype4 : 1 if intervened; 0 otherwise
data(peacef)
peacef <- na.omit(peacef)
head(peacef)

# two states of the world: treated countries
treated  <- peacef[peacef$untype4==1,]
head(treated)

# two states of the world: untreated treated countries
controls <- peacef[peacef$untype4==0,]
head(controls)

# do the treated and the untreatd look a like?
apply(treated,2,mean)
apply(controls,2,mean)

apply(treated,2,median)
apply(controls,2,median)

summary(treated)
summary(controls)

# key question: how many controls do we have that
# look like at least somewhat like the treated in the absence of the treatment?

# switch treatedment inidcator to zero on the treated and run whatif
treated$untype4 <- 0

#
my.result <- whatif(data = treated, cfact = controls)
names(my.result)
summary(my.result)

# is first countreactual in convex hull
my.result$in.hull[1]

# how many controls are closeby?
plot(my.result, numcf = 1)

# Integrate Zelig and Whatif
lm.out <- lm(develop ~ untype4 + trnsfcap, data = peacef)
# In this case, we could then use WhatIf to evaluate a counterfactual as follows:
summary(whatif(data = lm.out, cfact = data.frame(untype4 = 1, trnsfcap = 0)))

