library(R2WinBUGS)  # we use this package to run WinBUGS from R. 
library(arm)        # this is the package for multilevel data. 

# thanks to Beth, we have a very useful R website: http://www.personality-project.org/r/r.commands.html


# dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec5") 

dd <- c("C:/temp")  # this is the working directory if you saved the data in the temp folder on the C drive. 
setwd(dd)           # this sets the working directory as what has been specified in "dd" for R. 
                    # we can also set the working directory by hand though. 

##
## data preparation: the radon example. 
radon<-read.csv("radon.csv", header=T, na.strings="", colClasses=c(rep("numeric", 4)))
attach(radon)

u<-unique(u.full) # group-level predictor for 85 counties.

## complete pooling: 
lm.pooled <- lm (y ~ x)
display (lm.pooled)

## no pooling: also here we force the slope for every county to be the same:
lm.unpooled <- lm (y ~ x + factor(county) - 1)
display (lm.unpooled)


## start with multilevel model: 

# y_i ~ N(alpha_j[i] + beta*x, sigma.y)
# alpha_j ~ N(mu_alpha, sigma.alpha)

# or equivalently:
# y_i ~ N(mu_alpha + alpha_j[i] + beta*x, sigma.y)
# alpha_j ~ N(0, sigma.alpha)
# this is actually how the model is setup in lmer()

M1 <- lmer (y ~ x + (1 | county))
display (M1)

# lmer(formula = y ~ x + (1 | county))
#             coef.est coef.se
# (Intercept)  1.46     0.05          # this is the estimate of "mu_alpha" --- the grand mean if you want to label it this way
# x           -0.69     0.07          # this is the slope for x --- we fix it in this varying intercept and constant slope model.
# 
# Error terms:
#  Groups   Name        Std.Dev.
#  county   (Intercept) 0.33          # this is the estimate of sigma.alpha: the square root of across-group variance
#  Residual             0.76          # this is the estimate of sigma.y: the square root of within-group variance
# 
 
# now, 
fixef(M1) # this just list the estimates for "mu_alpha" --- the grand mean and the slope for x again. 
ranef(M1) # this is the estimated random intercept for each county: it is distributed around zero now.
coef(M1)  # this is the batches of coefficients for each county;
          # notice that the first column is mu_alpha + alpha_j, when alpha_j ~ N(0, sigma.alpha).

cbind(fixef(M1)[1]+ranef(M1)$county, coef(M1)$county[,1])[1:10, ]   # these are exactly the same thing here. 


##
## add in a group-level predictor for intercept:
M2 <- lmer (y ~ x + u.full + (1 | county))
display (M2)
coef(M2)

a.hat.M2 <- fixef(M2)[1] + fixef(M2)[3]*u + as.vector(ranef(M2)$county) ## this is the estimated intercept for all counties
                                                                        ## the random/varying part only comes from the third element 
b.hat.M2 <- fixef(M2)[2]                  ## coefficient for x. 




## 
##  Some examples of predictions using lmer()

# a. new house in county 26 with x=1
x.squiggle <- 1
a.hat <- fixef(M2)[1] + fixef(M2)[3]*u + ranef(M2)$county ## this is the estimated intercept for all counties
b.hat <- fixef(M2)[2]                                     ## coefficient for x. 
sigma.y.hat <- sigma.hat(M2)$sigma$data                   ## estimated sigma.y


# simulation using predictive uncertainty:
y.squiggle <- rnorm (1, a.hat[26, ] + b.hat*x.squiggle, sigma.y.hat)      
# we didn't add in inferential uncertainty
# repeat this 1000 times:
n.sims <- 1000
y.squiggle <- rnorm (n.sims, a.hat[26, ] + b.hat*x.squiggle, sigma.y.hat)
quantile (y.squiggle, c(.025,.5,.975))
exp (quantile (y.squiggle, c(.025,.5,.975)))
unlogged <- exp (y.squiggle)
mean (unlogged)


# b. new house in a new county
u.squiggle <- mean (u)              # mean value of county-level predictor: our best guess for the new county
g.0.hat <- fixef(M2)["(Intercept)"] 
g.1.hat <- fixef(M2)["u.full"]
sigma.a.hat <- sigma.hat(M2)$sigma$county
# it might help to write out the distributions here. 

# y_i ~ N(alpha_j + beta * x_i, \sigma_y^2)
# alpha_j ~ N(gamma_0 + gamma_1 * u_j, sigma_alpha^2)
# we use g for gamma here

a.squiggle <- rnorm (n.sims, g.0.hat + g.1.hat*u.squiggle, sigma.a.hat)
y.squiggle <- rnorm (n.sims, a.squiggle + b.hat*x.squiggle, sigma.y.hat)

quantile (y.squiggle, c(.025,.5,.975))
exp (quantile (y.squiggle, c(.025,.5,.975)))




##
## add in more complexities here:

# 1.  varying-intercept, varying-slope with no group-level predictors
# fit the model
M3 <- lmer (y ~ x + (1 + x | county))
display (M3)
coef (M3)[1:10, ]     # so now, both intercept and slopes change across groups. 

fixef(M3);ranef(M3)$county[1:10, ]

# the following two should give the same thing:
fixef(M3)+ranef(M3)$county[1, ]
coef(M3)$county[1, ]



# make a graph:
a.hat.M3 <- coef(M3)$county[,1]
b.hat.M3 <- coef(M3)$county[,2]

## get county names:
srrs2 <- read.table ("srrs2.dat", header=T, sep=",")
mn <- srrs2$state=="MN"        # so we pick up a state "Minnesota": the grouping will be by county.
county.name <- as.vector(srrs2$county[mn])
uniq <- unique(county.name)    # there are 85 counties. 
J <- length(uniq)
n<-length(y)

b.hat.unpooled.varying <- array (NA, c(J,2))
for (j in 1:J){
  lm.unpooled.varying <- lm (y ~ x, subset=(county==j))
  b.hat.unpooled.varying[j,] <- coef(lm.unpooled.varying)
}

x.jitter <- x + runif(n,-.05,.05)
display8 <- c (36, 1, 35, 21, 14, 71, 61, 70)
y.range <- range (y[!is.na(match(county,display8))])

lm.pooled <- lm (y ~ x); 

par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
    xlab="floor", ylab="log radon level", cex.lab=1.6, cex.axis=1.5,
    pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.3, main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.5)        # ignore this for now: just define the graph setting
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.5)   # ignore this for now: just define the graph setting
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, lty=2, col="gray10", add=TRUE)
  curve (b.hat.unpooled.varying[j,1] + b.hat.unpooled.varying[j,2]*x, lwd=.5, col="gray5", add=TRUE)
  curve (a.hat.M3[j] + b.hat.M3[j]*x, lwd=1, col="black", add=TRUE)
}



# 2.  varying-intercept, varying-slope with uranium as a group-level predictor
# fit model using lmer
M4 <- lmer (y ~ x + u.full + x:u.full + (1 + x | county))
display (M4)
fixef(M4)
ranef(M4)$county[1:10, ]
coef(M4)$county[1:10, ]

a.hat.M4 <- coef(M4)$county[, 3]*u + coef(M4)$county[,1]
b.hat.M4 <- coef(M4)$county[, 4]*u + coef(M4)$county[,2]
a.se.M4 <- se.coef(M4)$county[,1]
b.se.M4 <- se.coef(M4)$county[,2]

# plot estimated intercepts and slopes
lower <- a.hat.M4 - 2*a.se.M4
upper <- a.hat.M4 + 2*a.se.M4
par (mar=c(5,5,4,2)+.1, mfrow=c(1,1))
plot (u, a.hat.M4, cex.lab=2.4, cex.axis=2.2, ylim=range(lower,upper),
      xlab="county-level uranium measure", ylab="regression intercept", pch=20)
curve (fixef(M4)["(Intercept)"] + fixef(M4)["u.full"]*x, lwd=1, col="black", add=TRUE)
segments (u, lower, u, upper, lwd=.5, col="gray")


lower <- b.hat.M4 - 2*b.se.M4
upper <- b.hat.M4 + 2*b.se.M4
par (mar=c(5,5,4,2)+.1, mfrow=c(1,1))
plot (u, b.hat.M4, cex.lab=2.4, cex.axis=2.2, ylim=range(lower,upper),
      xlab="county-level uranium measure", ylab="regression slope", pch=20)
curve (fixef(M4)["x"] + fixef(M4)["x:u.full"]*x, lwd=1, col="black", add=TRUE)
segments (u, lower, u, upper, lwd=.5, col="gray")




## 3. last thing for today --- non-nested models:
## The pilot studies:
treat.airport<-read.csv("treat_airport.csv", header=T, na.strings="", colClasses=c(rep("numeric", 3)))
attach.all(treat.airport)

# fit the 2-way-model using lmer
pilots1 <- lmer (y ~ 1 + (1 | treatment) + (1 | airport))
display (pilots1)


# fit the same model in Bugs here:
n.treatment <- max(treatment)
n.airport <- max(airport)
n <- length(y)


data <- list ("y", "treatment", "airport", "n", "n.treatment", "n.airport")
inits <- function (){
  list (mu=rnorm(1), gamma=rnorm(n.treatment), delta=rnorm(n.airport), 
        sigma.gamma=runif(1), sigma.delta=runif(1), sigma.y=runif(1))
}
parameters <- c("mu", "sigma.gamma", "sigma.delta", "sigma.y", "gamma", "delta")



# don't run the following: we will get to this next week. 
pilots.1 <- bugs (data, inits, parameters, "pilots.1simple.bug", n.chains=3, n.iter=10000, debug=TRUE)

plot(pilots.1)
