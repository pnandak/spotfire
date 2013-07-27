
# Section 9
# Matching
# Many thanks to Iain Osgood

# two packages we will use
install.packages("MatchIt")
install.packages("cem")
library(MatchIt)
library(cem)
library(Zelig)

############
### Data ###
############

## LOAD DATA: Lalonde (1986) dataset; from cem package

data(LL)
head(LL)
?LL

nrow(LL)
summary(LL)

# Data:
	# From job training program to increase earnings of program participants
	# Outcome: (post-treatment) earnings in 1978 (re78)
	# Treatment: 
		# Participation in the job training program (treated = 1 or 0)
		# Non-random
	# Pretreatment covariates
		# age (age)
		# years of education (education) 
		# marital status (married)
		# lack of a high school diploma (nodegree)
		# race (black, hispanic) 
		# unemployed in 1974 (u74) and 1975 (u75)
		# earnings in 1974 (re74) and 1975 (re75)

#Goal:
	# Effect of Program Participation on Earnings in 1978 

#Approach:
	# Use matching to control pretreatment covariates
	# Make the treatment quasi-random
	
# the unadjusted average treatment effect
mean(LL$re78[LL$treated == 1]) - mean(LL$re78[LL$treated == 0])

summary(lm(re78 ~ treated + age + education + black + married + nodegree 
   + re74 + re75 + hispanic + u74 + u75, data = LL))


########################
### Balance Checking ###
########################

## PRE MATCH: check balance (multivariate and univariate)

# imbalance() is in cem package

pre.imbalance <- imbalance(group=LL$treated, data=LL, drop=c("treated","re78"))
pre.imbalance

# confirm difference in means and quantiles for age distribution
tr <- LL$treated == 1
co <- LL$treated == 0

mean(LL$age[tr]) - mean(LL$age[co])
quantile(LL$age[], c(0,.25,.50,.75,1)) - quantile(LL$age[LL$treated ==0], c(0,.25,.50,.75,1)) 

# demonstrate why L1 for age is so low
par(mfrow=c(1,2))
plot(density(LL$age[tr]))
plot(density(LL$age[co]))

# LCS: Local Common Support (LCS) measure, which is the proportion
# of non-empty k-dimensional cells of the histogram which contain at 
# least one observation for treateds and controls.

# what looks bad here?  
	# overall multivariate balance is quite bad
	# imbalance for re34 and re75 is significant in means and quantiles
	# imbalance for age is significant in quantiles

# Perfect balance on marginal distribution of each variable 
# does not mean perfect balance on joint distribution of all variables


###############
### MatchIt ###
###############

####################
## Exact Matching ##
####################

## EXACT MATCHING: Processing

# Simplest version of matching
# Process:
	# Matches each T=1 to all possible T=0 with exactly equal Xs
# Result:
	# Perfect Balance: X values are guaranteed to be exactly the same
# Drawback:
	# Seeks to minimize bias without regard to variance
	# Can also create bias in ATT if drop many treated units

exact.match <- matchit(formula= treated ~ age + education 
    + black + married + nodegree + re74 + re75 + hispanic + 
    u74 + u75, data = LL, method = "exact")

exact.match

## EXACT MATCHING: Analysis

# How doe we find the treatment effect?
	# No remaining imbalance (e.g. exact matching) -> simple (weighted) diff in means
	# Still imbalance -> run statistical model including covariates
	# Why do we know that option one will be fine?  

# the match.data function converts the matchit output into 
# a useable data.frame, by dropping unmatched units and adding in 
# new information like weights, subclasses or distance measures.

exact.data <- match.data(exact.match)

head(exact.data)
summary(exact.data)

# note that everyone with a positive dollar income in 
# 1974 or 1975 has been dropped because they couldn't
# be matched exactly. Other covariates don't vary too.
# Is this the population we are interested in?

exact.model <- lm(re78 ~ treated, data = exact.data,
	weights=exact.data$weights)

exact.data[exact.data$subclass == 5,]

# why do we need weights?
	# There are different numbers of treated and control units 
	# matched in different groups, so the first thing we do 
	# is give all treated units weight of 1, and all control 
	# units are given weight n_ti/n_ci where n_ti is the number
	# of treated units in group i.  Thus, if there are 
	# more control units, you downweight each one

	# next, if matching is with replacment, each control unit's 
	# weight is added up across the number of groups 
      # in which it was matched (each match is only a member 
 	# of one group with exact matching, though)  

	# finally, the control group weights are scaled to sum to the 
	# number of uniquely matched control outcomes, so that the overall
	# weight of the two groups corresponds to the total amount 
	# of information they provide.  

# so what is the treatment effect?
summary(exact.model)

# what about the complete model post exact matching?  
lm(re78 ~ treated+ age + education + black + married + nodegree + re74 + re75  
   + hispanic + u74 + u75, data = exact.data, weights=exact.data$weights)

# note that we could also get the same answer by calculating 
# the appropriate weighted average

y.treat <- weighted.mean(exact.data$re78[exact.data$treated == 1], 
  exact.data$weights[exact.data$treated == 1])

y.cont <- weighted.mean(exact.data$re78[exact.data$treated == 0], 
  exact.data$weights[exact.data$treated == 0])

y.treat-y.cont

######################
## Nearest Neighbor ##
######################

## NEAREST NEIGHBOR MATCHING: Processing

# Default method for matchit()
# Procedure:
	# matches each T=1 to one "close" T=0
		# 1-1 matching = default
		# 1-k allowed with ratio argument
	# specify the metric using the distance argument
		# default is "logit" 
			# distance = the propensity score (i.e. PR(T|X))
		# other distance measures include: 
			# other binomial glms for pscore estimation
			# mahalanobis: 
				# distance in multidimensional space
				# considers covariance in Xs unlike euclidean distance
				# d(Ti, Cj) = ( t(X_Ti-X_Cj) VCV(X_Cj)^(-1) (X_Ti-X_Cj) )^1/2
				# Ti matched to Cj with the min d(Ti,Cj)
	# additional arguments include:
		# match with replacement
		# caliper: tolerance criteria limiting distance (radius) within which C is chosen

nearest.match <- matchit(formula = treated ~ age + education 
  + black + married + nodegree + re74 + re75 + hispanic + 
  u74 + u75, data = LL, method = "nearest", distance = "logit")

summary(nearest.match)
plot(nearest.match)

pre.balance <- summary(nearest.match)$sum.all
post.balance <- summary(nearest.match)$sum.matched

plot(x = (pre.balance[,1] - pre.balance[,2])/pre.balance[,3], y = 1:nrow(pre.balance),
     pch = 19, col = "burlywood2", xlab = "Standardized Imbalance", axes = FALSE, 
     xlim = c(-.25,.25), ylab = "")
points(x = (post.balance[,1] - post.balance[,2])/post.balance[,3], y = 1:nrow(post.balance),
     pch = 19, col = "cadetblue2")
abline(v = 0, lty = "dashed")
axis(1)
axis(2, at = 11:1, labels = rownames(pre.balance), las = 1, cex.axis = .8)
legend("topright", legend = c("Pre-match","Post-match"), pch = c(19,19),
   col = c("burlywood2","cadetblue2"), cex = .8)

#summary() provides: 
	# means
	# sd of control group
	# mean differences
	# comparison of quantiles
	# the matched call
	# the number of units matched, unmatched, or discarded 
	# the percent improvement in balance for each measure of balance
	# (smaller values indicate better balance)


## NEAREST NEIGHBOR MATCHING: Analysis

# extract matched data

nearest.data <- match.data(nearest.match)
head(nearest.data)

## non-parametric estimate of the ATT
mean(nearest.data$re78[nearest.data$treated == 1]) - mean(nearest.data$re78[nearest.data$treated == 0])

## A model-based estimate of the ATT 

nearest.model <- lm(re78 ~ treated + age + education + black 
  + married + nodegree + re74 + re75 + hispanic + u74 + u75,
  data = nearest.data)

summary(nearest.model) 

#########
## CEM ##
#########

### CEM MATCH

# cem is also implemented in MatchIt
# Procedure:
	# exact matching on coarsened data to determine matches
		# first, define strata s.t. each has identical values for all coarsened pre-treatment X
		# second, sort all observations into strata
		# third, discard all C observations in any stratum without at least 1 T
		# Simultaneously, cem throws out T's that consitute extreme counterfactuals
		# by restricting all data to areas of common support.
	# Step 1 requires specifying a degree of coarsening for each covariate
		# Fully automated procedure or
		# Choose coarsening ourselves (better)
			# set coarsening s.t. substantively indistinguishable values grouped together
	# Note: if you would like to reintegrate dropped T's, you can do so 
	# by imputing some counterfactuals C's using an appropriate model, 
	# but this will generate model dependence.

## cem match: automatic bin choice

auto.match <- matchit(formula = treated ~ age + education 
  + black + married + nodegree + re74 + re75 + hispanic + 
  u74 + u75, data = LL, method = "cem")

auto.match

## cem match: user chosen coarsening

re74cut <- hist(LL$re74, plot=FALSE)$breaks
re75cut <- seq(0, max(LL$re75)+1000, by=1000)
agecut <- c(20.5, 25.5, 30.5,35.5,40.5)
my.cutpoints <- list(re75=re75cut, re74=re74cut, age=agecut)

user.match <-  matchit(formula = treated ~ age + education 
  + black + married + nodegree + re74 + re75 + hispanic + 
  u74 + u75, data = LL, method = "cem", cutpoints = my.cutpoints)

user.data <- match.data(user.match)
auto.data <- match.data(auto.match)

user.imb <- imbalance(group=user.data$treated, data=user.data, drop=
  c("treated","re78","distance","weights","subclass"))
auto.imb <- imbalance(group=auto.data$treated, data=auto.data, drop=
  c("treated","re78","distance","weights","subclass"))

names(user.imb)

user.imb$L1 # user.match has better balance 
auto.imb$L1 # so less bias!

summary(user.match)$nn # user.match has less data
summary(auto.match)$nn # so more variance!

# numerical vs categorical variables
	# coarsening for numerical variables -> cutpoints arugment
	# coarsening for categorical variables -> grouping argument


### CAUSAL EFFECTS w/ CEM

## for the moment the att() function does not appear to be included 
## in the MatchIt library, so calculating an att with the full 
## set of treated units requires using cem() in the cem package

cem.match <- cem(treatment = "treated", data = LL, drop = "re78")

## Simple (weighted) difference in means; e.g. linear regression

cem.match.att <- att(obj=cem.match, formula=re78 ~ treated, data = LL, model="linear")
cem.match.att # default is linear model

## Remaining imbalance? -> model with covariates 

cem.match.att2 <- att(obj=user.match.1, formula=re78 ~ treated +
	age+education+black+married+nodegree+re74+re75+hispanic+u74+u75,
	data = LL, model="linear")

cem.match.att2


###############
###############


