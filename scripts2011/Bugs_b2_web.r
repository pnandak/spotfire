# Monday, 13.July.2009. 
library(R2WinBUGS)
library(arm)


# dat setup:
dd <- c("C:/temp") 
setwd(dd)

srrs2 <- read.table ("srrs2.dat", header=T, sep=",")
# this is the radon example that has been repeatedly used by the book: 
# dv: radon level in houses
# iv: house level: floor of measurement (basement=0, first floor=1)
#     county level: uranium level
# goal: estimate the distribution of radon levels in each of the approximately 3000 counties in the US.



mn <- srrs2$state=="MN"                 # now, we only consider one state: Minnesota;
radon <- srrs2$activity[mn]
y <- log (ifelse (radon==0, .1, radon)) # this replace 0 with .1 before talking the log for radon levels;
n <- length(radon)                      # number of observations within Minnesota; 
x <- srrs2$floor[mn]                    # 0 for basement, 1 for first floor; 
                                        # notice we don't use the county-level variable uranium level --- we only have a radom intercept: alpha~N(mu_alpha, sigma^2_{alpha})


# create county indicators. 
county.name<-as.vector(srrs2$county[mn])
uniq.name  <-unique(county.name)
J <-length(uniq.name)
county<-rep(NA, J)                      
for (i in 1:J){county[county.name==uniq.name[i]]<-i}

# county-level uranium:
srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
cty <- read.table ("cty.dat", header=T, sep=",")
usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows,"Uppm"]
u <- log (uranium)



# 17.1: varying intercept and varying slope model, but no correlation (also no group level predictor)
radon.data <- list ("n", "J", "x", "y", "county"); cbind(n, J, x, y, county)
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(J), mu.a=rnorm(1), mu.b=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1), sigma.b=runif(1))
}
radon.parameters <- c ("a", "b", "mu.a", "mu.b", "sigma.y", "sigma.a", "sigma.b")
radon.171 <- bugs (radon.data, radon.inits, radon.parameters, "17.1.bug", n.chains=3, n.iter=500, bugs.directory = "c:/summer/WinBUGS14/")
plot(radon.171)
print(radon.171)

attach.bugs(radon.171)

par(mfrow=c(2,2))
plot(density(mu.a), main=expression(mu[alpha])); abline(v=quantile(mu.a, c(.025, .5, .975)))
plot(density(mu.b), main=expression(mu[beta])); abline(v=quantile(mu.b, c(.025, .5, .975)))
plot(density(sigma.a), main=expression(sigma[alpha])); abline(v=quantile(sigma.a, c(.025, .5, .975)))
plot(density(sigma.b), main=expression(sigma[beta])); abline(v=quantile(sigma.b, c(.025, .5, .975)))

quantile(sigma.y, c(.025, .5, .975))

## fit the same model in lmer:
fit.1<-lmer(y ~ x+(1+x|county))
display(fit.1)





# 17.1rho: with correlation between varying intercept and varying slope.
radon.data <- list ("n", "J", "x", "y", "county")
radon.inits <- function (){
  list (B=array(rnorm(2*J), c(J,2)), mu.a=rnorm(1), mu.b=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1), sigma.b=runif(1), rho=runif(1))
}
radon.parameters <- c ("a", "b", "mu.a", "mu.b", "sigma.y", "sigma.a", "sigma.b", "rho")
radon.171rho <- bugs (radon.data, radon.inits, radon.parameters, "17.1rho.bug", n.chains=3, n.iter=500, bugs.directory = "c:/summer/WinBUGS14/")
plot(radon.171rho)
print(radon.171rho)




# 17.1 wishart1: with correlation between varying intercept and varying slope.
library(MCMCpack) # this is for "rwish()"

K<-2 # the number of parameters including the intercept.
W<-diag(2)
radon.data <- list ("n", "J", "x", "y", "county", "W")
radon.inits <- function (){
  list (B.raw=array(rnorm(2*J), c(J,2)), mu.a.raw=rnorm(1), mu.b.raw=rnorm(1), xi.a=runif(1), xi.b=runif(1),
        sigma.y=runif(1), Tau.B.raw=rwish(3, diag(2))) #Tau.B.raw=rwish(3, diag(2)) replaces "sigma.a=runif(1), sigma.b=runif(1), rho=runif(1)"
}
radon.parameters <- c ("a", "b", "mu.a", "mu.b", "sigma.y", "sigma.a", "sigma.b", "rho")
radon.171wishart <- bugs (radon.data, radon.inits, radon.parameters, "wishart1.bug", n.chains=3, n.iter=500, bugs.directory = "c:/summer/WinBUGS14/")#, debug=TRUE)
# if we put debug=TRUE in, we are going to stay in Bugs and we can see the individual MCMCs 
plot(radon.171wishart)
print(radon.171wishart)

 
# 17.1 wishartK: with correlation between varying intercept and varying slope but generalized to K varying coefficients.
library(MCMCpack) # this is for "rwish()"

K<-2 # the number of parameters including the intercept: when K=2, this is the same as in the previous example
     # however, the set up of covariates change from a vector of x to a matrix X of which the 1 column is {1, ...1}
W<-diag(K)
X<-as.matrix(cbind(1, x))
radon.data <- list ("n", "J", "X", "y", "county", "W", "K")
radon.inits <- function (){
  list (B.raw=array(rnorm(2*J), c(J,2)), mu.raw=rnorm(K), xi=runif(K),
        sigma.y=runif(1), Tau.B.raw=rwish(K+1, diag(K))) #Tau.B.raw=rwish(3, diag(2)) replaces "sigma.a=runif(1), sigma.b=runif(1), rho=runif(1)"
}
radon.parameters <- c ("B", "mu", "sigma.y", "sigma.B", "rho.B")
radon.171wishartK <- bugs (radon.data, radon.inits, radon.parameters, "wishartK.bug", n.chains=3, n.iter=500, debug=TRUE, bugs.directory = "c:/summer/WinBUGS14/")
# if we put debug=TRUE in, we are going to stay in Bugs and we can see the individual MCMCs 
plot(radon.171wishartK)
print(radon.171wishartK)




## simple non-nested model: 
## the pilot studies:
treat.airport<-read.csv("treat_airport.csv", header=T, na.strings="", colClasses=c(rep("numeric", 3)))
attach.all(treat.airport)

n.treatment <- max(treatment)
n.airport <- max(airport)
n <- length(y)

data <- list ("y", "treatment", "airport", "n", "n.treatment", "n.airport")
inits <- function (){
  list (mu=rnorm(1), gamma=rnorm(n.treatment), delta=rnorm(n.airport), 
        sigma.gamma=runif(1), sigma.delta=runif(1), sigma.y=runif(1))
}
parameters <- c("mu", "sigma.gamma", "sigma.delta", "sigma.y", "gamma", "delta")
pilots.1 <- bugs (data, inits, parameters, "pilots.1simple.bug", n.chains=3, n.iter=10000, debug=TRUE, bugs.directory = "c:/summer/WinBUGS14/")
plot(pilots.1)




##
## multilevel logistic model in Bugs: with non-nested structures. 
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
ok <- survey==9158            # define the condition
polls.subset <- polls[ok,]    # select the subset of interest
attach.all (polls.subset)     # attach the subset
# write.table (polls.subset, "polls.subset.dat")

# define other data summaries: this is going to be used when we fit the model in Bugs.
y <- bush                  # 1 if support bush, 0 if support dukakis
n <- length(y)             # of survey respondents
n.age <- max(age)          # of age categories
n.edu <- max(edu)          # of education categories
n.state <- max(state)      # of states
n.region <- max(region)    # of regions

# also include a measure of previous vote as a state-level predictor: 
v.prev<-dget("v.prev")  # this is the average Republican vote share in the previous three elections.
                        # the only group-level predictor we have with respect to state. 
                        
age.edu <- n.edu*(age-1) + edu     
# notice this is not simply take the product but rather creating a 4 by 4 matrix. 
#          Age     
#      |_1_2__3__4 
#     1| 1 5  9 13 
# Edu 2| 2 6 10 14 
#     3| 3 7 11 15 
#     4| 4 8 12 16 


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


## the following line initiate the Bugs: 
M2.bugs <- bugs (data, inits, params, "election88.M2.bug", n.chains=3, n.iter=1000, bugs.directory = "c:/summer/WinBUGS14/")

plot(M2.bugs)
print(M2.bugs)

attach.bugs(M2.bugs)

## do some in-sample prediction here:
linpred2 <- NULL
for (i in 1:n){
  linpred2 <- rbind(linpred2, quantile(b.0 + b.female*female[i] + b.black*black[i] +
    b.female.black*female[i]*black[i] + a.age[,age[i]] + a.edu[,edu[i]] +
    a.age.edu[,age[i],edu[i]]+ a.state[, state[i]], c(.025, .5, .975)))}

linpred2[1:10, ]                            # prediction for each observation in logit probability
invlogit(linpred2[1:10, ])                  # in probability terms
cbind(invlogit(linpred2[1:10, ]), y[1:10] ) # compare to the real behavior. 




##
## Multilevel Poisson in Bugs: 
## the policy stops example in NYC using data with random noises. 
frisk <- read.table ("frisk_with_noise_modified.dat", header=T) 
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

##
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


data <- list ("stops", "n", "n.eth", "n.precinct", "eth", "precinct", "offset")
cbind(stops, n, n.eth, n.precinct, eth, precinct, offset)
inits <- function () {list(mu=rnorm(1),  b.eth=rnorm(n.eth), b.precinct=rnorm(n.precinct), 
  sigma.eth=runif(1), sigma.precinct=runif(1), sigma.epsilon=runif(1))}
params <- c ("mu.adj",  "b.eth.adj", "b.precinct.adj",
   "sigma.eth", "sigma.precinct", "sigma.epsilon")
M1.bugs <- bugs (data, inits, params, "police.stops.bug", n.chains=3, n.iter=5000, debug=TRUE, bugs.directory = "c:/summer/WinBUGS14/")

plot(M1.bugs)
print(M1.bugs)

attach.bugs(M1.bugs)
quantile(sigma.precinct, c(.025, .5, .975))
mean(sigma.precinct)
