library(R2WinBUGS)
library(arm)

# dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec6") 
dd <- c("C:/temp") 
setwd(dd)


## 1. prepare the data:
frisk <- read.table ("frisk_with_noise_modified.dat", header=T) 
# this has the basic data, but I guess with some noises here and there to hide the real data:
 
# stops: the number of stops by police for one specific ethnic group in one specific precinct within one year.
# pop: population size for one specific ethnic group in one specific precinct
# past.arrests: the number of arrests by police for one specific ethnic group in one specific precinct within the past 15 months.
# precinct: precinct number --- gouping criterion 1
# eth: ethnic group number  --- gouping criterion 2
# crime: crime type (gouping criterion 3???): but we do separate analysis for different crime types. 

attach.all (frisk)
n.precinct <- max (precinct)
n.eth <- max (eth)
n.crime <- max(crime)
dcjs <- log(past.arrests*15/12)   # this is the offset or log(exposure) in Poisson model. 
## dimensions of the data;
dim(frisk)
n.precinct*n.eth*n.crime          # it is a quite balanced data set. 


# need to calculate the racial composition of each precinct:
PC123<-NULL
for (i in 1:n.precinct){PC<-NULL
for (j in 1:n.eth)     {pc<-(pop[precinct==i&eth==j][1]/sum(pop[precinct==i]/4))
                        PC<-c(PC, pc)
                        }
                        PC123<-rbind(PC123, PC)
                        }
# not so sure whether 1,2,3 is in the order of blacks, hispanics, and whites --- let's assume it is. 
# also, these are just approximations, because they are other ethnicities: 4% on average for each precinct. 
pc1<-PC123[, 1] 
pc2<-PC123[, 2] 
pc3<-PC123[, 3] 

# this expand this these group-level predictors to have the same length as individual level data.
pc.1<-pc1[precinct] 
pc.2<-pc2[precinct] 
pc.3<-pc3[precinct] 
# 

y<-stops # the dependent variable: you can keep using "stops" or other names for this as well. 


# define precinct categories based on %black --- I hope this is ethnicity 1
precinct.category <- ifelse (pc.1 < .1, 1, ifelse (pc.1 < .4, 2, 3))
n.precinct.category <- max (precinct.category)

frisk <- as.data.frame (cbind (y, eth, precinct, crime, precinct.category, past.arrests, dcjs, pop, pc.1, pc.2, pc.3))
rownames(frisk)<-as.character(1:900)   
frisk[1:10, ]


##
## 2. before multilevel model, just simple pooling everything together:
library(MASS)
fit.1<-glm(formula=y~factor(eth)+factor(precinct), family=poisson, offset=log(past.arrests+1))
summary(fit.1)

fit.2<-glm(formula=y~factor(eth)+factor(precinct), family=quasipoisson, offset=log(past.arrests+1))
summary(fit.2)
## look at the overdispersion factor here: it is hugh here. 


## look at different crime types separately: this reduces the overdispersion factor.
fit.3<-glm(formula=y~ factor(eth)+factor(precinct), family=quasipoisson, offset=log(past.arrests+1), data=subset(frisk, crime==3))
summary(fit.3)


##
## 3. Multilevel analysis of NYC police stops
# 3.a. lmer() fits: 

ok<-crime==1 & precinct.category==1 ## only look at those for crime type 1:
M0.0<- lmer (y ~ 1 + (1 | eth) + (1 | precinct), offset=log(past.arrests+1),
      family=poisson(link="log"), subset=ok)
display(M0.0)
ranef(M0.0)
se.coef(M0.0)
summary(M0.0)


# add in some group-level predictor:
M0.1<- lmer (y ~ 1 + pc.1 + pc.2 + (1 | eth) + (1 | precinct), offset=log(past.arrests+1),
      family=quasipoisson(link="log"), subset=ok)
display(M0.1)


# now, we can do this for each crime type and each type of precinct defined by the % black:
# we do this in a loop:
M1 <- as.list (rep (NA, 12))
index <- 0
for (j in 1:3){    # this is type of precinct defined by the % black;
  for (k in 1:4){  # this is for crime type. 
  
    index <- index + 1
    ok <- precinct.category==j & crime==k
    M1[[index]] <- lmer (y ~ 1 + (1 | eth) + (1 | precinct),
                         offset=log(past.arrests+1),
      family=quasipoisson(link="log"), subset=ok)
      
  }
}
    
display (M1[[1]])

# alternatively, we can treat the offset as a control variable 
M2 <- as.list (rep (NA, 12))
index <- 0
for (j in 1:3){
  for (k in 1:4){
    index <- index + 1
    ok <- precinct.category==j & crime==k
   M2[[index]] <- lmer (y ~ log(past.arrests+1) + (1 | eth) + (1 | precinct),
      family=quasipoisson(link="log"), subset=ok)
  }
}

display (M2[[1]])



## Gelman's pahe has the instrunction to set up Bugs: http://www.stat.columbia.edu/~gelman/bugsR/
## 3.b.. now we are going to move to Bugs.
attach.all(frisk)                           # remeber to attach this everytime
j<-1; k<-1
ok <- precinct.category==j & crime==k
stops<-y[ok]
eth<-eth[ok]
precinct<-precinct[ok]; precinct<-as.numeric(as.factor(precinct))
offset<-log(past.arrests+1)[ok]
n<- length(stops)
n.eth<-max(eth)
n.precinct<-length(precinct)/n.eth


# stops<-rpois(69, 8)
data <- list ("stops", "n", "n.eth", "n.precinct", "eth", "precinct", "offset")
cbind(stops, n, n.eth, n.precinct, eth, precinct, offset)
inits <- function () {list(mu=rnorm(1),  b.eth=rnorm(n.eth), b.precinct=rnorm(n.precinct), 
  sigma.eth=runif(1), sigma.precinct=runif(1), sigma.epsilon=runif(1))}
params <- c ("mu.adj",  "b.eth.adj", "b.precinct.adj",
   "sigma.eth", "sigma.precinct", "sigma.epsilon")
# M1.bugs <- bugs (data, inits, params, "police.stops.bug", n.chains=3, n.iter=5000, debug=TRUE)

M1.bugs<-dget("M1.bugs")  # get the Bugs object saved. 

plot(M1.bugs)
print(M1.bugs)

# dput(M1.bugs, "M1.bugs")

attach.bugs(M1.bugs)
quantile(sigma.precinct, c(.025, .5, .975))
mean(sigma.precinct)

M1[[1]]
# see the big differences. 



## some way to do the exercise from yesterday:
library(arm)
wells <- read.table ("http://privatewww.essex.ac.uk/~caox/teaching/Day2/wells.dat", header=TRUE)
# wells <- read.table ("wells.dat", header=TRUE)

attach.all (wells)

dim(wells)
table(educ)


M1 <- lmer (switch ~ dist + arsenic + (1 |educ), family=binomial(link="logit"))
display (M1, digits=3)
ranef(M1)
se.coef(M1)

table(educ)
# educ
#  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
# 889   6  52 121 174 725 130 126 210  90 254  21 147   5  32  21  16   1 

educ.new<-educ
educ.new[educ==0]<-1
educ.new[educ>0 & educ<=6]<-2
educ.new[educ>6 & educ<=12]<-3
educ.new[educ>12]<-4

M2 <- lmer (switch ~ dist + arsenic + (1 |educ.new), family=binomial(link="logit"))
display (M2, digits=3)
ranef(M2)
se.coef(M2)
