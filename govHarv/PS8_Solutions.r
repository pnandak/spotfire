### Gov 2001
### Problem Set 8 Solutions



### Load the data

summary(data)


### Load MatchIt and cem; and Zelig

library(MatchIt)
library(cem)
library(Zelig)


### Check Balance on original data

original.imbalance <- imbalance(group=data$D, data=data, drop=c("D", "Y"))
original.imbalance


### Implement a linear model

summary(lm(data$Y~data$D+data$X1+data$X2+data$X3+data$X4+data$X5))


### Nearest neighbor pscore matching

# Calculate propensity scores

pscores.logit <- zelig(D~X1+X2+X3+X4+X5, model="logit", data=data)$fitted

# Match on pscores

pscore.match <- matchit(data$D~pscores.logit, data=data, method="nearest", distance=pscores.logit, discard="control")

# Extract matched data

pscore.data <- match.data(pscore.match)

# Evaluate balance

pscore.imbalance <- imbalance(group=pscore.data$D, data=pscore.data, drop=c("D", "Y", "distance", "weights"))
original.imbalance
pscore.imbalance


### Run a linear model with pscore matched data

summary(zelig(Y~D+X1+X2+X3+X4+X5, data=pscore.data, model="ls"))


### Nearest neighbor Mahalanobis matching

mahalanobis.match <- matchit(D~X1+X2+X3+X4+X5, data=data, method="nearest", distance="mahalanobis", discard="control")

# Extract matched data

mahalanobis.data <- match.data(mahalanobis.match)

# Evaluate balance

mahalanobis.imbalance <- imbalance(group=mahalanobis.data$D, data=mahalanobis.data, drop=c("D", "Y", "distance", "weights"))
original.imbalance
pscore.imbalance
mahalanobis.imbalance


### Run a linear model with mahalanobis matched data

summary(zelig(Y~D+X1+X2+X3+X4+X5, data=mahalanobis.data, model="ls"))


### CEM matching

cem.match <- cem(treatment="D", data=data, drop="Y")

# Evaluate balance

original.imbalance
pscore.imbalance
mahalanobis.imbalance
cem.match$imbalance


### ATT from cem matched data

att(cem.match, Y~D+X1+X2+X3+X4+X5, data=data, model="linear")

