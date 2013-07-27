# Section 9
# Matching


# Today:
	#Matching in R
	#Methods we'll cover:
		#exact
		#nearest neighbor
		#cem

# What is the goal of matching?
	#The set up: 
		#Interested in causal effect of T on Y
		#Assume T ignorable given X 
		#X are pretreatment variables
	#Given this, 
		#Matching is preprocessing of data
		#Goal: Groups T=1 and T=0 as similar as possible on X
			#i.e. balance on dist of X across T
		#Results: 
			#T becomes more independent of X
			#Subsequent parametric modeling assuptions become less relevant
			#Reduces model dependence


library(MatchIt)
library(cem)




############
### Data ###
############


### LOAD DATA: Lalonde (1986) dataset; from cem package

data(LL)
head(LL)

#Data:
	#From job training program to increase earnings of program participants
	#Outcome: (post-treatment) earnings in 1978 (re78)
	#Treatment: 
		#Participation in the job training program (treated = 1 or 0)
		#Non-random
	#Pretreatment covariates
		#age (age)
		#years of education (education) 
		#marital status (married)
		#lack of a high school diploma (nodegree)
		#race (black, hispanic) 
		#unemployed in 1974 (u74) and 1975 (u75)
		#earnings in 1974 (re74) and 1975 (re75)

#Goal:
	#Effect of Program Participation on Earnings in 1978 

#Approach:
	#Use matching to control pretreatment covariates
	#Make the treatment quasi-random
	





#########################
### Pre Match Balance ###
#########################


### PRE MATCH: check balance (multivariate and univariate)

#imbalance() is in cem package

pre.imbalance <- imbalance(group=LL$treated, data=LL, drop=c("treated","re78"))
pre.imbalance

#Multivariate Imbalance
	#Overall L1 statistic =
		#imbalance wrt joint distribution of covariates

#Unidimensional Imbalance
	#Each variable separately
	#statistic = 
		#difference in means for numerical (type = diff) or 
		#chi-square difference for categorical (type=Chi2) 
			#compare tallies within categories across groups
	#last columns (min, 25%, etc) =
		#difference in variables across groups at different percentiles

#Perfect balance on marginal distribution of each variable 
#does not mean perfect balance on joint distribution of all variables





###############
### MatchIt ###
###############


### EXACT MATCHING

#Simplest version of matching
#Process:
	#Matches each T=1 to all possible T=0 with exactly equal Xs
#Result:
	#Perfect Balance: X values are guaranteed to be exactly the same
#Drawback:
	#Seeks to minimize bias without regard to variance
	#Can also create bias in ATT if drop many treated units

exact.match <- matchit(
	formula=treated~age+education+black+married+nodegree+re74+re75+hispanic+u74+u75, 
	data = LL,
	method = "exact")

exact.match

summary(exact.match)

#summary() 
	#provides measures of (univariate) balance
	#balance for both full original data and matched data 
	#measures of balance are not output for exact matching



### NEAREST NEIGHBOR MATCHING (e.g. Approximate Matching)

#Default method for matchit()
#Procedure:
	#matches each T=1 to one "close" T=0
		#1-1 matching = default
		#1-k allowed with ratio arugment
	#specify the metric using the distance argument
		#default is "logit" 
			#distinace = the propensity score (i.e. PR(T|X))
				#the true propensity score is unknown
				#goal: 
					#find the truth
					#finding pscore specification that best improves balance
		#other distance measures include: 
			#other binomial glms for pscore estimation
			#mahalanobis: 
				#distance in multidimensional space
				#considers covariance in Xs
				#unlike euclidean distance
				#d(Ti, Cj) = ( t(X_Ti-X_Cj) VCV(X_Cj)^(-1) (X_Ti-X_Cj) )^1/2
				#Ti matched to Cj with the min d(Ti,Cj)
	#additional arguments include:
		#match with replacement
		#caliper: tolerance criteria limiting distance (radius) within which C is chosen

nearest.match <- matchit(
	formula=treated~age+education+black+married+nodegree+re74+re75+hispanic+u74+u75, 
	data = LL,
	method = "nearest")

summary(nearest.match)

#summary() provides: 
	#means
	#sd of control group
	#mean differences
	#comparison of quantiles
	#the matched call
	#the number of units matched, unmatched, or discarded 
	#the percent improvement in balance for each measure of balance
	#(smaller values indicate better balance)

#extract matched data

nearest.data <- match.data(nearest.match)
head(nearest.data)

#use imbalance() to see multivariate balance

nearest.imbalance <- imbalance(nearest.data$treated, data=nearest.data, drop=c("treated","re78","distance","weights"))

pre.imbalance
nearest.imbalance




### ANALYSIS

#How doe we find the treatment effect?
	#No remaining imbalance (e.g. exact matching) -> simple (weighted) diff in means
	#Still imbalance -> run statistical model including covariates

##exact matched data

exact.data <- match.data(exact.match)
head(exact.data)

exact.model <- lm(
	re78 ~ treated,
	data = exact.data,
	weights=exact.data$weights)

summary(exact.model)

#what is the treatment effect?

#why do we need weights?
	#Different numbers of treated and control units matched to each other
	#Weights used to ensure that matched treated and control groups are considered similar 

##nearest neighbor matched data

nearest.model <- lm(
	re78 ~ treated +age+education+black+married+nodegree+re74+re75+hispanic+u74+u75,
	data = nearest.data)

summary(nearest.model) 

#what type of treatment effect have we estimated in each case?
	#exact matching: maybe the SATT, but we dropped lots of T units
	#nearest neighbor matching: the SATT

#Note: discard option: 
	#specifies whether to discard units that fall outside some measure of support
	#(e.g. convex hull, C units with no T, T units with no C)
	#The default is "none" i.e. discard no units
	#Discarding units may change the quantity being estimated 
		#why? changes observations left in the analysis






###########
### CEM ###
###########


### CEM MATCH

#cem(): coarsened exact matching
#Procedure:
	#exact matching on coarsened data to determine matches
		#first, define strata s.t. each has identical values for all coarsened pre-treatment X
		#second, sort all observations into strata
		#third, discard all C observations in any stratum without at least 1 T
	#Step 1 requires specifying a degree of coarsening for each covariate
		#Fully automated procedure or
		#Choose coarsening ourselves (better)
			#set coarsening s.t. substantively indistinguishable values grouped together


## cem match: automatic bin choice

auto.match <- cem(treatment="treated", data=LL, drop="re78")
auto.match


## cem match: user choiced coarsening

re74cut <- hist(LL$re74, br=seq(0, max(LL$re74)+1000, by=1000), plot=FALSE)$breaks
re75cut <- hist(LL$re75, br=seq(0, max(LL$re75)+1000, by=1000), plot=FALSE)$breaks
agecut <- hist(LL$age, br=seq(15, 55, length=14), plot=FALSE)$breaks
my.cutpoints <- list(re75=re75cut, re74=re74cut, age=agecut)

user.match.1 <- cem(treatment="treated", data=LL, drop="re78", cutpoints=my.cutpoints)
user.match.1
auto.match

#numerical vs categorical variables
	#coarsening for numerical variables -> cutpoints arugment
	#coarsening for categorical variables -> grouping argument


## cem match: user choiced coarsening, k-to-k matching

#default cem() is n-to-j matching 
	#i.e. n T units matched to j C units
	#i.e. different number of T and C matched within each stratum
#can specify k-to-k matching if prefer
	#Procedure:
		#prune observations from a n-to-j cem solution within each stratum
			#until same number of treated and control units in each stratum 
		#uses nearest neighbor selection
			#user can specify a distance function (e.g. euclidean, maximum, etc)
			#default = random matching within cem strata

user.match.2 <- cem(treatment="treated", data=LL, drop="re78", cutpoints=my.cutpoints, k2k=TRUE)
user.match.2
user.match.1
auto.match


### CAUSAL EFFECTS w/ CEM

## Simple (weighted) difference in means; e.g. linear regression

att1.user.match.1 <- att(obj=user.match.1, formula=re78 ~ treated, data = LL, model="linear")
att1.user.match.1 #default is linear model

## Remaining imbalance? -> model with covariates 

att2.user.match.1 <- att(obj=user.match.1, formula=re78 ~ treated +
	age+education+black+married+nodegree+re74+re75+hispanic+u74+u75,
	data = LL, model="linear")
att2.user.match.1




###########################
### Post Treatment Bias ###
###########################

#what is it?

#Examples?
