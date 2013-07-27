## Matching in R ##

library(foreign)
MatchData<- na.omit(read.dta("week4.dta",  convert.factors=FALSE))

## dropping data where totbexp is not  0 or 1
MatchData$bridgekeep <- 0<MatchData$totbexp & MatchData$totbexp<1
MatchData2 <- subset(MatchData, bridgekeep==FALSE)
rownames(MatchData2) <- MatchData2$county


install.packages("MatchIt")
library(MatchIt)

# exact matching (fails) #

bridge.match <- matchit(totbexp ~ spcorr_all  + whitepct2564m + amindpct2564m + avgunemp + avgurban90 + totpop2564m, data=MatchData2, method="exact")



# propensity score matching #

bridge.match <- matchit(totbexp ~ spcorr_all  + whitepct2564m + amindpct2564m + avgunemp + avgurban90 + totpop2564m, data=MatchData2, method="nearest")

summary(bridge.match)

plot(bridge.match)

plot(bridge.match, type="jitter")

plot(bridge.match, type="hist")

m.data <- match.data(bridge.match)

nrow(m.data)

# save in Stata format #

write.dta(m.data, "match1.dta")


# mahalanobis distance matching # 

bridge.match2 <- matchit(totbexp ~ spcorr_all  + whitepct2564m + amindpct2564m + avgunemp + avgurban90 + totpop2564m, data=MatchData2, method="nearest", distance="mahalanobis")

summary(bridge.match2)

plot(bridge.match2)

plot(bridge.match2, type="jitter")

plot(bridge.match2, type="hist")

m2.data <- match.data(bridge.match2)

nrow(m2.data)


# propensity score matching, 2 matches per treated observation #

bridge.match3 <- matchit(totbexp ~ spcorr_all  + whitepct2564m + amindpct2564m + avgunemp + avgurban90 + totpop2564m, data=MatchData2, method="nearest", ratio=2)

plot(bridge.match3)

plot(bridge.match3, type="jitter")

plot(bridge.match3, type="hist")

m3.data <- match.data(bridge.match3)

nrow(m3.data)


# propensity score matching,  discarding observations outside support #

bridge.match4 <- matchit(totbexp ~ spcorr_all  + whitepct2564m + amindpct2564m + avgunemp + avgurban90 + totpop2564m, data=MatchData2, method="nearest", discard="both")

plot(bridge.match4)

plot(bridge.match4, type="jitter")

plot(bridge.match4, type="hist")

m4.data <- match.data(bridge.match4)

nrow(m4.data)


install.packages("Zelig")
library(Zelig)

# Negative binomial model with unmatched data #

nbmod1 <- zelig(jumps2564_m  ~ spcorr_all  + whitepct2564m + amindpct2564m + avgunemp + avgurban90 + totbexp + offset(log(totpop2564m)), model="negbin", data=MatchData, robust=TRUE)
summary(nbmod1)

# Negative binomial model with propensity score matched data #

nbmod2 <- zelig(jumps2564_m  ~ spcorr_all  + whitepct2564m + amindpct2564m + avgunemp + avgurban90 + totbexp + offset(log(totpop2564m)), model="negbin", data=m.data, robust=TRUE)
summary(nbmod2)

# Negative binomial model with mahalanobis distance matched data #

nbmod3 <- zelig(jumps2564_m  ~ spcorr_all  + whitepct2564m + amindpct2564m + avgunemp + avgurban90 + totbexp + offset(log(totpop2564m)), model="negbin", data=m2.data, robust=TRUE)
summary(nbmod3)

# Negative binomial model with propensity score matching, 2 controls per treated #

nbmod4 <- zelig(jumps2564_m  ~ spcorr_all  + whitepct2564m + amindpct2564m + avgunemp + avgurban90 + totbexp + offset(log(totpop2564m)), model="negbin", data=m3.data, weights="weights", robust=TRUE)
summary(nbmod4)

# Negative binomial model with propensity score matching, discarding observations w/o common support #

nbmod5 <- zelig(jumps2564_m  ~ spcorr_all  + whitepct2564m + amindpct2564m + avgunemp + avgurban90 + totbexp + offset(log(totpop2564m)), model="negbin", data=m4.data, robust=TRUE)
summary(nbmod5)