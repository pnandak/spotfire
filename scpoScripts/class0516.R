load("bkt.Rda")


## "ordinary" logit, ignoring dependence on past states
model1 <- glm(dispute ~ dem + growth + allies + contig + capratio + trade,
              data=bkt,
              trace=TRUE,
              eps=1e-14,
              maxit=1000,
              na.action=na.omit,
              family=binomial)

## dummies on the unique values of peace years
bktLogit <- update(model1,
                   ~ . + as.factor(py))

## drop dummies with no peace years
bktLogit <- update(model1,
                   ~ . + as.factor(py),
                   subset=(!(py%in%c(24,26,27))))


## splines over peace years, fit semi-parameterically
## with gam (Hastie and Tibshirani)
require(gam)
bktLogitSpline <- gam(dispute ~ dem + growth + allies + contig + capratio + trade + s(py,4),
                      data=bkt,
                      family=binomial)

summary(bktLogitSpline)
summary.gam(bktLogitSpline)
