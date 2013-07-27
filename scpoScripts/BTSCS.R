## Binary TSCS models in R ##

library(foreign)

ORData <- read.dta("orum.dta",  convert.factors=FALSE)

## Standard logit ##

logitmod1 <- glm(dispute ~ dem + growth + allies + contig + capratio + trade, family=binomial(link="logit"), data=ORData)
summary(logitmod1)

install.packages("glmmML")
library(glmmML)

## Fixed effects logit ##

logitfemod1 <- glmmboot(dispute ~ dem + growth + allies + contig + capratio + trade, family=binomial(link="logit"), data=ORData, cluster=ordyid)
summary(logitfemod1)

## list first 10 estimated fixed effects ##

logitfemod1$frail[1:10]

## Random effects logit ##

logitremod1 <- glmmML(dispute ~ dem + growth + allies + contig + capratio + trade, family=binomial(link="logit"), data=ORData, cluster=ordyid, method="ghq", n.points=12)
summary(logitremod1)

## Logit with time dummy variables ##

ORData$py.t <- factor(ORData$py)

logitmod3 <- glm(dispute ~ dem + growth + allies + contig + capratio + trade + (py.t), family=binomial(link="logit"), data=ORData)
summary(logitmod3)

## Logit with time effects: cubic polynomial ##

ORData$py <- ORData$py + 1

ORData$py2 <- (ORData$py)^2
ORData$py3 <- (ORData$py)^3

logitmod4 <- glm(dispute ~ dem + growth + allies + contig + capratio + trade + py+ py2 + py3, family=binomial(link="logit"), data=ORData)
summary(logitmod4)
