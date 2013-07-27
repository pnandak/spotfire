library(foreign)
primary <- read.dta("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/franklin data/primary.dta")
#The dataset ”PRIMARY” contains variables on U.S. 1996 primary elections for the House of Representatives.
#This subset of the data consist of all contested, NON-incumbent, primaries held before
#August 1, 1996.
#the data was borrowed from Charles Franklin's website: http://www.polisci.wisc.edu/users/franklin/
#he uses this dataset (and others, also found on his website) for homework assignments when he teaches MLE at ICPSR

# receipts = Total Receipts (1=$10,000)
# spend = Total Disbursements (1=$10,000)
# opspend = Top Opponent Spending (1=$10,000)
# oprec = Top Opponent Receipts (1=$10,000)
# winner = Did Candidate Win? 1=Y, 0=N
# effort = Prop of receipts spent

attach(primary)

winning1 <- glm(winner ~ receipts + oprec, family = binomial)
summary(winning1)
ll1 <- logLik(winning1)

winning2 <- glm(winner ~ receipts + oprec + effort, family = binomial)
summary(winning2)
ll2 <- logLik(winning2)


# Likelihood Ratio test
lr.test <- 2*(ll2 - ll1)

lr.test.p <- pchisq(lr.test, df=1, lower.tail=F)

# BIC

bic.test <- - 2*(ll2 - ll1) + 1*log(nrow(primary))

# AIC

aic.test <- - 2*(ll2 - ll1) - 1*2

# alternatively
AIC(winning2) - AIC(winning1)

bic2 <- AIC(winning2, k=log(nrow(primary)))
bic1 <- AIC(winning1, k=log(nrow(primary)))
bic2 - bic1


#### percent correctly predicted (with all of its flaws, but just in case you were wondering what it might look like)
win.fit1 <- winning1$fitted.values
win.fit1[winning1$fitted.values > .5] <- 1
win.fit1[winning1$fitted.values <= .5] <- 0

table(win.fit1, winner)

win.fit2 <- winning2$fitted.values
win.fit2[winning2$fitted.values > .5] <- 1
win.fit2[winning2$fitted.values <= .5] <- 0

table(win.fit2, winner)

#### ROC plots
library(verification)

win1.pred <- winning1$fitted.values
win2.pred <- winning2$fitted.values

par(mfrow=c(1,2))
roc.plot(winner, win1.pred)
roc.plot(winner, win2.pred)

win1.2.pred <- cbind(win1.pred, win2.pred)
roc.plot(winner, win1.2.pred)

roc.area(winner, win1.pred)
roc.area(winner, win2.pred)

# predicted probabilities
rechat <- seq(1, 129, 1)
betas <- winning1$coeff
mu1 <- NULL
y1 <- NULL
for (i in 1:length(rechat)) {
mu1[i] <- betas%*%rbind(1,rechat[i],mean(oprec))
y1[i] <- 1/(1+exp(-mu1[i]))
}

plot(rechat, y1)


#Another data set: 
#Also thanks to Charles Franklin. Found at the same place.
#In a 1987 Supreme Court case, McCleskey v. Zant argued that the death penalty was applied
#in a racially discriminatory manner. Backed by extensive statistical evidence, McCleskey’s
#counsel argued that the race of both victim and defendant played a significant role in the
#decision to impose the death penalty, even after accounting for other facts of the case. On
#this basis they argued that capital punishment was being applied in an unconstitutionally
#discriminatory manner.
#In a 5-4 decision, the Court rejected McCleskey’s claim. Justice Powell, writing for the
#majority, argued that statistical evidence of discrimination was not sufficient to overturn 
#McCleskey’s conviction. 
#the following data file is a subset of 100 cases from the original 594 used by McCleskey.

death.penalty <- read.dta("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/franklin data/deathpenalty.dta")

# ID        1--100 ID number of each case
# Death     1=Death sentence, 0=Life sentence
# BD        1=Black Defendent, 0=White defendent
# WV        1=one or more white victims, 0=no white victims
# AC        number (1-6) of aggravating circumstances
# FV        1=female victim, o-male victim
# VS        1=victim was a stranger to the defendent, 0=victim was known to defendent
# V2        1=two or more victims, 0=one victim
# MS        1=multiple stabs, 0=no multiple stabs
# YV        1=victim was 12 years of age or younger, 0=victim was over 12
# BB        1=Black defendent, black victim, 0=other combination
# BW        1=Black defendent, white victim, 0=other combination
