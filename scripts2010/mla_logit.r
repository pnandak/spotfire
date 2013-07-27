library(R2WinBUGS)
library(arm)

# dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec6") 
dd <- c("C:/temp") 
setwd(dd)


# Set up the data for the election88 example

# Load in data for region indicators
# Use "state", an R data file (type ?state from the R command window for info)
#
# Regions:  1=northeast, 2=south, 3=north central, 4=west, 5=d.c. 
# ==> so we have three-levels by geography: individual => state => region. 
# We have to insert d.c. (it is the 9th "state" in alphabetical order)
data (state)                  # "state" is an R data file
objects()                     # look at what we have loaded so far in R. 
state.abbr <- c (state.abb[1:8], "DC", state.abb[9:50])
dc <- 9
not.dc <- c(1:8,10:51)
region <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,1,3,2,3,3,4,1,3,4,1,2,4)


# Load in data from the CBS polls in 1988
library (foreign)
polls <- read.dta ("polls.dta");unique(polls$year); unique(polls$survey)
polls[1:10, ]
attach.all (polls)

# Select just the data from the last survey (#9158)
table (survey)                # look at the survey id's
ok <- survey==9158            # define the condition
polls.subset <- polls[ok,]    # select the subset of interest
attach.all (polls.subset)     # attach the subset
# write.table (polls.subset, "polls.subset.dat")

print (polls.subset[1:5,])
dim(polls.subset)


# define other data summaries: this is going to be used when we fit the model in Bugs.
y <- bush                  # 1 if support bush, 0 if support dukakis
n <- length(y)             # of survey respondents
n.age <- max(age)          # of age categories
n.edu <- max(edu)          # of education categories
n.state <- max(state)      # of states
n.region <- max(region)    # of regions


# compute unweighted and weighted averages for the U.S.
ok <- !is.na(y)                                                      # remove the undecideds: NA
cat ("national mean of raw data:", round (mean(y[ok]==1), 3), "\n")  # notice that round(mean(y[ok]), 3) will give the same results. 
cat ("national weighted mean of raw data:",
     round (sum((weight*y)[ok])/sum(weight[ok]), 3), "\n")           # this is not so much different from the unweighted result. 


# compute weighted averages for the states: so we go down to the state level, only need to use a loop here. 
raw.weighted <- rep (NA, n.state)
names (raw.weighted) <- state.abbr
for (i in 1:n.state){
  ok <- !is.na(y) & state==i
  raw.weighted[i] <- sum ((weight*y)[ok])/sum(weight[ok])
}
raw.weighted # notice that AK has NA: but in multilevel modeling, we can still get the estimates for this state. 
polls.subset[polls.subset$state==2, ]



##
# load in 1988 census data: we will use this in step 2; not in the model fitting part of the game.
census <- read.dta ("census88.dta")
census[1:10, ]


# also include a measure of previous vote as a state-level predictor: 
v.prev<-dget("v.prev")  # this is the average Republican vote share in the previous three elections.
                        # the only group-level predictor we have with respect to state. 
                        
                        
                         
## 
## now we can go ahead and fit the models: lmer() first. 
## 1. With only two predictors and varying intercept: we consider observations as clustered within state first.
M1 <- lmer (y ~ black + female + (1 | state), family=binomial(link="logit"))
display (M1)


## 2. A fuller model with non-nested factors: education, age, and the "interaction" between the two.
age.edu <- n.edu*(age-1) + edu     
# notice this is not simply take the product but rather creating a 4 by 4 matrix. 
#          Age     
#      |_1_2__3__4 
#     1| 1 5  9 13 
# Edu 2| 2 6 10 14 
#     3| 3 7 11 15 
#     4| 4 8 12 16 
region.full <- region[state]     # this is the level above state, but we still need to extend the vector for lmer() fit. 
v.prev.full <- v.prev[state]     # group level predictor. 

M2 <- lmer (y ~ black + female + black:female + v.prev.full 
            + (1 | age) + (1 | edu) + (1 | age.edu) + (1 | state) + (1 | region.full), 
            family=binomial(link="logit"))
display (M2)



## now move to Bugs:
data <- list ("n", "n.age", "n.edu", "n.state", "n.region",
 "y", "female", "black", "age", "edu", "state", "region", "v.prev")
inits <- function () {list(
  b.0=rnorm(1), b.female=rnorm(1), b.black=rnorm(1), b.female.black=rnorm(1),
  a.age=rnorm(n.age), a.edu=rnorm(n.edu),
  a.age.edu=array (rnorm(n.age*n.edu), c(n.age,n.edu)),
  a.state=rnorm(n.state), a.region=rnorm(n.region),
  sigma.age=runif(1), sigma.edu=runif(1), sigma.age.edu=runif(1),
  sigma.state=runif(1), sigma.region=runif(1))
}
params <- c ("b.0", "b.female", "b.black", "b.female.black",
   "a.age", "a.edu", "a.age.edu", "a.state", "a.region",
   "sigma.age", "sigma.edu", "sigma.age.edu", "sigma.state", "sigma.region")

# Bugs model for election88 model
# model {
#   for (i in 1:n){
#     y[i] ~ dbin (p.bound[i], 1)                                  # dbin() is a binomial distribution, here with N=1
#     p.bound[i] <- max(0, min(1, p[i]))                           # this line of code restricts the probability between 0 and 1
#     logit(p[i]) <- Xbeta[i]                                      # now we are going to linear predictor Xbeta
#     Xbeta[i] <- b.0 + b.female*female[i] + b.black*black[i] +    # from here on, it looks very similar to linear models b/c we are dealing with Xbeta.
#       b.female.black*female[i]*black[i] +
#       a.age[age[i]] + a.edu[edu[i]] + a.age.edu[age[i],edu[i]] +
#       a.state[state[i]]
#   }
#   b.0 ~ dnorm (0, .0001)
#   b.female ~ dnorm (0, .0001)
#   b.black ~ dnorm (0, .0001)
#   b.female.black ~ dnorm (0, .0001)
# 
#   for (j in 1:n.age) {a.age[j] ~ dnorm(0, tau.age)}
#   for (j in 1:n.edu) {a.edu[j] ~ dnorm(0, tau.edu)}
#   for (j in 1:n.age) {for (k in 1:n.edu){
#     a.age.edu[j,k] ~ dnorm(0, tau.age.edu)}}
#   for (j in 1:n.state) {
#     a.state[j] ~ dnorm(a.state.hat[j], tau.state)
#     a.state.hat[j] <- a.region[region[j]] + b.v.prev*v.prev[j]}
#   b.v.prev ~ dnorm (0, .0001) 
#   for (j in 1:n.region) {a.region[j] ~ dnorm(0, tau.region)}
# 
#   tau.age <- pow(sigma.age, -2)
#   tau.edu <- pow(sigma.edu, -2)
#   tau.age.edu <- pow(sigma.age.edu, -2)
#   tau.state <- pow(sigma.state, -2)
#   tau.region <- pow(sigma.region, -2)
# 
#   sigma.age ~ dunif (0, 100)
#   sigma.edu ~ dunif (0, 100)
#   sigma.age.edu ~ dunif (0, 100)
#   sigma.state ~ dunif (0, 100)
#   sigma.region ~ dunif (0, 100)
# }


## the following line initiate the Bugs, but we won't do it this time. 
# M2.bugs <- bugs (data, inits, params, "election88.M2.bug", n.chains=3, n.iter=1000)

# postscript ("BugsM2.ps", horizontal=T)
# plot(M2.bugs)
# dev.off()

# print(M2.bugs)
# dput(M2.bugs, "M2.bugs")

# Postprocessing to get state averages: 
# create linear predictors:
M<-dget("M2.bugs")
print(M)
names(M)

attach.bugs(M)
# attach.bugs(M2.bugs)



## do some in-sample prediction here:
length(b.0)  #  there are 1500: 3x(1000-500)
linpred <- rep (NA, n)
for (i in 1:n){
  linpred[i] <- mean (b.0 + b.female*female[i] + b.black*black[i] +
    b.female.black*female[i]*black[i] + a.age[,age[i]] + a.edu[,edu[i]] +
    a.age.edu[,age[i],edu[i]]) # for each coefficient, say, b.0, there are 1500 values, then we take the mean
}                              # notice that there is no state effect.

dim(a.state)
# or maybe this: add in state effects
linpred2 <- NULL
for (i in 1:n){
  linpred2 <- rbind(linpred2, quantile(b.0 + b.female*female[i] + b.black*black[i] +
    b.female.black*female[i]*black[i] + a.age[,age[i]] + a.edu[,edu[i]] +
    a.age.edu[,age[i],edu[i]]+ a.state[, state[i]], c(.025, .5, .975)))}

linpred2[1:10, ]                            # prediction for each observation in logit probability
invlogit(linpred2[1:10, ])                  # in probability terms
cbind(invlogit(linpred2[1:10, ]), y[1:10] ) # compare to the real behavior. 
# here, actual prediction in terms of 1 or 0 is kind of tricky for logit case
# because this involves the choice of threshold "k": when p>=k, y<-1
# the choice of k would often affect prediction performance of the model. 
# there is a literature on this and there is package in R to do this if you are interested:  
# library(verification)
# This function creates Receiver Operating Characteristic (ROC)
# plots for one or more models.  A ROC curve plots the false alarm
# rate against the hit rate for a probablistic forecast for a range
# of thresholds. The area under the curve is viewed as a measure of
# a forecast's accuracy.  A measure of 1 would indicate a perfect
# model.  A measure of 0.5 would indicate a random forecast.



# plot the 8 states
par (mfrow=c(2,4))
y.jitter <- y + ifelse (y==0, runif (n, 0, .1), runif (n, -.1, 0))
state.name.all <- c(state.name[1:8], "District of Columbia", state.name[9:50])
for (j in c(2,3,4,8,6,7,5,9)) {
  plot (0, 0, xlim=range(linpred), ylim=c(0,1), yaxs="i",
        xlab="linear predictor", ylab="Pr (support Bush)",
        main=state.name.all[j], type="n")
  for (s in 1:20){
    curve (invlogit (a.state[s,j] + x), lwd=.5, add=TRUE, col="gray20")}  # notice the uncertainties are only the state effects
  curve (invlogit (median (a.state[,j]) + x), lwd=2, add=TRUE)            # 
  if (sum(state==j)>0) points (linpred[state==j], y.jitter[state==j])     # 
}




## step 2: 
# create predicted values for each of 3264 strata
L <- nrow (census)     # this is the number of strata/classification: 3264
y.pred <- array (NA, c(n.sims, L))
for (l in 1:L){
  y.pred[,l] <- invlogit(b.0 + b.female*census$female[l] +
    b.black*census$black[l] + b.female.black*census$female[l]*census$black[l] +
    a.age[,census$age[l]] + a.edu[,census$edu[l]] +
    a.age.edu[,census$age[l],census$edu[l]] + a.state[,census$state[l]])
}
dim(y.pred) # this gives an array and each column of it is one category.

# average over strata within each state
y.pred.state <- array (NA, c(n.sims, n.state))
for (s in 1:n.sims){
  for (j in 1:n.state){
    ok <- census$state==j
    y.pred.state[s,j] <- sum(census$N[ok]*y.pred[s,ok])/sum(census$N[ok])
  }
}
# the last line in the loop: census$N[ok]*y.pred[s,ok] ==> product of two vectors !!!
 
# now look at the 95% CI:
state.pred <- array (NA, c(n.state,3))
for (j in 1:n.state){
  state.pred[j,] <- quantile (y.pred.state[,j], c(.025,.5,.975))
}
rownames(state.pred)<-state.abbr
state.pred[1:10, ]    # the predicted Republican support.
