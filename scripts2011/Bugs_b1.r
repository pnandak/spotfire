# Tuesday, 23.June.2009. 
# Multilevel modeling in Bugs and R: the basics. 
library(R2WinBUGS)
library(arm)

## varying intercept model in R and Bugs. 

# 1. dat setup:
dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec8") 
setwd(dd)

srrs2 <- read.table ("srrs2.dat", header=T, sep=",")
# this is the radon example that has been repeatedly used by the book: 
# dv: radon level in houses
# iv: house level: floor of measurement (basement=0, first floor=1)
#     county level: uranium level
# goal: estimate the distribution of radon levels 
#       in each of the approximately 3000 counties in the US.



mn <- srrs2$state=="MN"                 # now, we only consider one state: Minnesota;
radon <- srrs2$activity[mn]
y <- log (ifelse (radon==0, .1, radon)) # this replace 0 with .1 before talking the log;
n <- length(radon)                      # number of observations within Minnesota; 
x <- srrs2$floor[mn]                    # 0 for basement, 1 for first floor; 
                                        # we don't use the county-level uranium level now
                                        # we only have a radom intercept: 
                                        # alpha~N(mu_alpha, sigma^2_{alpha})


# create county indicators. 
county.name<-as.vector(srrs2$county[mn])
uniq.name  <-unique(county.name)
J <-length(uniq.name)
county<-rep(NA, J)                      
for (i in 1:J){county[county.name==uniq.name[i]]<-i}


# classical complete pooling in R: 
lm.pooled<-lm(y~x)
display (lm.pooled)
# or more conventionally: 
summary(lm.pooled)


# no-pooling:
lm.unpooled.0<-lm(y~x + factor(county))
display (lm.unpooled.0)                  # notice that the first county is used as a reference group; 

lm.unpooled<-lm(y~x + factor(county) -1) # -1 erase the contant term;
display (lm.unpooled)





# 2.  Multilevel model


# fit the model using bugs:

# Bugs code for multilevel model for radon
# with floor of measurement (basement=0, first floor=1) as an individual predictor
# varying-intercept model in Bugs code: don't run it in R.
# model {
#   for (i in 1:n){
#     y[i] ~ dnorm (y.hat[i], tau.y)
#     y.hat[i] <- a[county[i]] + b*x[i]
#   }
#   b ~ dnorm (0, .0001)
#   tau.y <- pow(sigma.y, -2)
#   sigma.y ~ dunif (0, 100)
# 
#   for (j in 1:J){
#     a[j] ~ dnorm (mu.a, tau.a)
#   }
#   mu.a ~ dnorm (0, .0001)
#   tau.a <- pow(sigma.a, -2)
#   sigma.a ~ dunif (0, 100)
# }


# now back to R:
radon.data <- list ("n", "J", "x", "y", "county")      

radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "mu.a", "sigma.y", "sigma.a")

radon.1 <- bugs (radon.data, radon.inits, radon.parameters, "radon.1.bug", 
           n.chains=3, n.iter=500)  # you need to add in  bugs.directory = "c:/summer/WinBUGS14/" within ()
plot (radon.1);print(radon.1)       # b/c by default, the programme is installed under programme file. 


radon.1.noburnin <- bugs (radon.data, radon.inits, radon.parameters, "radon.1.bug", 
                    n.chains=3, n.iter=500, debug=TRUE, n.burnin=0)

attach.bugs(radon.1 )
quantile(a[, 1], c(.025, .5, .975))
quantile(b, c(.025, .5, .975))


# plot the bugs output
# postscript ("radon.bugs.ps", horizontal=FALSE, height=6, width=6.5)
plot (radon.1)
# dev.off ()


# plot showing gibbs convergence
# postscript ("gibbs.converge.ps", height=6, horizontal=T)
par (mar=c(5,5,3,2)+.1, mfrow=c(1,1))
plot (c(0,200), range(radon.1.noburnin$sims.array[6:208,,"mu.a"]), xlab="iteration",
      ylab="", xaxs="i", cex.lab=1.8, cex.axis=1.8, type="n")
mtext (expression(mu[alpha]), 2, 3, cex=2) 
for (j in 1:3){
  lines (1:203, radon.1.noburnin$sims.array[6:208,j,"mu.a"], lwd=.5)
}
# dev.off()  ## something weired here ....





# 3.  Multilevel model including uranium as a county-level predictor

# fit the model using bugs: 
# Bugs code for multilevel model for radon
# with bsmt as an individual predictor and uranium as a county-level predictor
# varying-intercept model

#model {
#  for (i in 1:n){
#    y[i] ~ dnorm (y.hat[i], tau.y)
#    y.hat[i] <- a[county[i]] + b*x[i]
#  }
#  b ~ dnorm (0, .0001)
#  tau.y <- pow(sigma.y, -2)
#  sigma.y ~ dunif (0, 100)
#
#  for (j in 1:J){
#    a[j] ~ dnorm (a.hat[j], tau.a)
#    a.hat[j] <- g.0 + g.1*u[j]
#  }
#  g.0 ~ dnorm (0, .0001)
#  g.1 ~ dnorm (0, .0001)
#  tau.a <- pow(sigma.a, -2)
#  sigma.a ~ dunif (0, 100)
#}

## get the group level predictor:
srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
cty <- read.table ("cty.dat", header=T, sep=",")
usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows,"Uppm"]
u <- log (uranium)

# 
radon.data <- list ("n", "J", "x", "y", "county", "u")
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
radon.parameters <- c ("a", "b", "g.0", "g.1", "sigma.y", "sigma.a")
radon.2 <- bugs (radon.data, radon.inits, radon.parameters, "radon.2.bug", n.chains=3, n.iter=500)
plot (radon.2)

attach.bugs(radon.2)
par(mfrow=c(2,2))
plot(density(b[-c(1:100)]), main=expression(beta))
plot(density(g.0[-c(1:100)]), main=expression(gamma[0]))
plot(density(g.1[-c(1:100)]), main=expression(gamma[1]))
plot(density(sigma.a[-c(1:100)]), main=expression(sigma[alpha]))
