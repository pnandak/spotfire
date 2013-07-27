## Factor Analysis in R ##

library(foreign)

FactData <- read.dta("C:\\courses\\ps207\\week3\\stereotype.dta",  convert.factors=FALSE)

factmat <- na.omit(subset(FactData, select=c(trad_values, resp_auth, gods_will, imp_polite, law_order, am_power, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14)))

cor(factmat)

factfit <- factanal(factmat, factors=3)

print(factfit)

factload <- factfit$loadings

plot(factload,type="n")
text(factload,labels=names(factmat))

install.packages("nFactors")
library(nFactors)

ev <- eigen(cor(factmat)) 
scree <- nScree(ev$values)
plotnScree(scree) 

factfit2 <- factanal(factmat, factors=3, scores="regression")

fscores <- factfit2$scores

cor(fscores)

factfit3 <- update(factfit2,rotation="promax")
fscores3 <- factfit3$scores

cor(fscores3)

matchlist <- match(rownames(FactData), rownames(fscores3), nomatch=0)

FactData2 <- cbind(subset(FactData, matchlist!=0), factfit3$scores)

example1 <- glm(home ~ age + educ + sex + belt + Factor1 + Factor2 + Factor3, family=binomial(link="probit"), data=FactData2, na.action=na.exclude)
summary(example1)

example2 <- glm(univ ~ age + educ + sex + belt + Factor1 + Factor2 + Factor3, family=binomial(link="probit"), data=FactData2, na.action=na.exclude)
summary(example2)

example3 <- glm(quot ~ age + educ + sex + belt + Factor1 + Factor2 + Factor3, family=binomial(link="probit"), data=FactData2, na.action=na.exclude)
summary(example3)

library("MASS")

example4 <- polr(as.factor(anti_disc) ~ age + educ + sex + belt + Factor1 + Factor2 + Factor3, method="probit", data=FactData2, na.action=na.exclude, Hess=TRUE)
summary(example4)

