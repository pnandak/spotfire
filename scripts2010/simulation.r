library(R2WinBUGS)
library(arm)

dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec2") 
setwd(dd)

## Simulation for probability models: 
## avg height of 10 adults: 
# 52% women, with mean of 63.7inches and SD of 2.7
# 48% men, with mean of 69.1inches and SD of 2.9


n.sims <- 1000
avg.height <- rep (NA, n.sims)

for (s in 1:n.sims){
sex <- rbinom (10, 1, 0.52)
sex
n<-length(sex)
height<-rep(NA, 10)
for (i in 1:n){
               if (sex[i]==0){height[i]<-rnorm (1, 69.5, 2.9)}
               else          {height[i]<-rnorm (1, 63.7, 2.7)} 
               }
avg.height[s]<-mean (height)
}
hist (avg.height)


n.sims <- 1000
avg.height <- rep (NA, n.sims)
for (s in 1:n.sims){
  sex <- rbinom (10, 1, 0.52)
  height <- ifelse (sex==0, rnorm (10, 69.5, 2.9), rnorm (10, 63.7, 2.7))
  avg.height[s] <- mean (height)
}
hist (avg.height)


# using functions: 
height.sim <- function (n.adults){
  sex <- rbinom (n.adults, 1, 0.52)
  height <- ifelse (sex==0, rnorm (10, 69.5, 2.9), rnorm (10, 64.5, 2.7))
  return (list (avg=mean(height), max=max(height)))
}

heights <- replicate (1000, height.sim (10))
n.adults <- 10
n.sims <- 1000
avg.height <- rep (NA, n.sims)
max.height <- rep (NA, n.sims)
for (s in 1:n.sims){
  simulation <- height.sim (n.adults)
  avg.height[s] <- simulation$avg
  max.height[s] <- simulation$max
}



##
## Simulation to summarize regressions: linear regressions --- the height and earning example. 
library ("foreign")
heights <- read.dta ("heights.dta")
attach.all (heights)

# create variables for age and ethnicity categories
age <- 90 - yearbn                     # survey was conducted in 1990
age[age<18] <- NA
age.category <- ifelse (age<35, 1, ifelse (age<50, 2, 3))
eth <- ifelse (race==2, 1, ifelse (hisp==1, 2, ifelse (race==1, 3, 4)))
male <- 2 - sex

# (for simplicity) remove cases with missing data
# and restrict to people with positive earnings and born after 1925
ok <- !is.na (earn+height+sex+age) & earn>0 & yearbn>25
heights.clean <- as.data.frame (cbind (earn, height, sex, race, hisp, ed, age, age.category, eth, male)[ok,])
n <- nrow (heights.clean)

attach.all (heights.clean)
height.jitter.add <- runif (n, -.2, .2)
log.earn <- log (earn)



earn.logmodel.3 <- lm (log.earn ~ height + male + height:male)
display(earn.logmodel.3)
x.new<-data.frame(height=68, male=1)
x.new

pred.interval <- predict(earn.logmodel.3, x.new, interval="prediction", level=.95)
exp(pred.interval)


# another way of doing this:
pred<-exp(rnorm(1000, 10, .89))
mean(pred)
quantile(pred, .5) # median
quantile(pred, c(.025, .975))



# add in inferential uncertainty:
n.sims<-1000
fit.1 <- lm(log.earn ~ height + male + height:male)
display(fit.1)

sim.1<-sim(fit.1, n.sims)
attributes(sim.1)
sim.1$coef[1:10, ]
sim.1$sigma[1:10]

mean(sim.1$coef[,2])
sd(sim.1$coef[,2])
quantile(sim.1$coef[,2], c(.025, .5, .975))
mean(sim.1$sigma)
# these numbers should fit approximately  what we see in the regression output:
summary(fit.1)

par(mfrow=c(1,2))
plot(density(sim.1$coef[,2]), main=expression(beta[height])); 
abline(v=0)
abline(v=quantile(sim.1$coef[,2], c(.025, .5, .975)),col="gray")
plot(density(sim.1$coef[,3]), main=expression(beta[male])); 
abline(v=0)
abline(v=quantile(sim.1$coef[,3], c(.025, .5, .975)),col="gray")


## now add in both types of uncertainty here:
x.pred.1<-cbind(1, c(min(height):max(height)), 1, c(min(height):max(height))*1)
x.pred.1

pred<-function(X.pred, lm.fit){
               n.pred<-dim(X.pred)[1]
               sim.fit<-sim(lm.fit, 1)
               y.pred<-rnorm(n.pred, X.pred %*% t(sim.fit$coef), sim.fit$sigma)
               return(y.pred)
               }

pred(x.pred.1, fit.1)

## do this 1000 times:
y.tilde<-replicate(1000, pred(x.pred.1, fit.1))
dim(y.tilde)
rowSums(y.tilde)/1000

par(mfrow=c(1,1))
plot(0, 0, xlim=c(min(height)-2, max(height)+2), ylim=c(min(y.tilde),max(y.tilde)), 
    xlab="height of men", 
    ylab="log income", main="predicted log income for men", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
for (i in 1:100){points(c(min(height):max(height)), y.tilde[, i],  col="lightgray")}
lines(c(min(height):max(height)), rowSums(y.tilde)/1000, lwd=1)




##
## predictive simulation for generalized linear models: logistic regression of switching on distance to nearest safe well 
wells <- read.table ("wells.dat", header=TRUE)
attach.all (wells)

fit.2 <- glm (switch ~ dist, family=binomial(link="logit"))
display (fit.2)

dev.off()
sim.1<-sim(fit.2, n.sims)
plot(sim.1$coef[,1], sim.1$coef[,2], xlab=expression(beta[0]), ylab=expression(beta[distance]))


jitter.binary <- function(a, jitt=.05){
  a + (1-2*a)*runif(length(a),0,jitt)
}

# postscript ("arsenic.logitfit.ps", height=4.5, width=5.5, horizontal=TRUE)
par(mfrow=c(1,1))
plot(c(0,max(dist, na.rm=TRUE)*1.02), c(0,1), 
    xlab="Distance (in meters) to nearest safe well", 
    ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
for (i in 1:100){curve (invlogit(sim.1$coef[i,1]+sim.1$coef[i,2]*x), lwd=1, add=TRUE, col="lightgray")}
curve (invlogit(fit.2$coef[1]+fit.2$coef[2]*x), lwd=1, add=TRUE)
points (dist, jitter.binary(switch), pch=20, cex=.1)
# dev.off ()




#########
# fake data simulation: 
a <- 2.3
b <- 0.4
sigma <- 2.2
x <- 1:5
n <- length(x)
y <- a + b*x + rnorm (n, 0, sigma)
lm.1 <- lm (y ~ x)
display (lm.1)


# check coverage of 68% and 95% intervals for b
b.hat <- coef(lm.1)[2]  # "b" is the 2nd coef in the model
b.se <- summary(lm.1)$coef[2, 2] # beta.se(lm.1)[2], this beta.se command does not work: never seen this one before. 
cover.68 <- abs (b - b.hat) < b.se   # this will be TRUE or FALSE
cover.95 <- abs (b - b.hat) < 2*b.se # this will be TRUE or FALSE
cat (paste ("cover.68: ", cover.68, "\n"))
cat (paste ("cover.95: ", cover.95, "\n"))

# do 1000 simulations
n.sims <- 1000
cover.68 <- rep (NA, n.sims)
cover.95 <- rep (NA, n.sims)
for (k in 1:n.sims){
  y <- a + b*x + rnorm (n, 0, sigma)
  lm.1 <- lm (y ~ x)
  b.hat <- coef(lm.1)[2]
  b.se <- summary(lm.1)$coef[2, 2] #beta.se(lm.1)[2]
  cover.68[k] <- abs (b - b.hat) < b.se
  cover.95[k] <- abs (b - b.hat) < 2*b.se
}
cat (paste ("cover.68: ", mean(cover.68), "\n"))
cat (paste ("cover.95: ", mean(cover.95), "\n"))




## using simulation from fitted models and compare simulated data to the actual data: prediction. 
# Time series fit and model checking for unemployment series

unemployment <- read.table ("unemployment.dat", header=TRUE)
year <- unemployment$year
y <- unemployment$unemployed.pct

# plot the unemployment rate
# postscript ("unemployment1.ps", height=3, width=4, horizontal=F)
par (mar=c(4,4,2,2))
plot (year, y, type="l", ylab="unemployment", xlab="year", yaxs="i",
  ylim=c(0, max(y)*1.05), yaxt="n", mgp=c(2,.5,0), cex.axis=1.2, cex.lab=1.2)
axis (2, c(0,5,10), paste (c(0,5,10), "%", sep=""), mgp=c(2,.5,0), cex.axis=1.2)
#dev.off()

# fit a 1st-order autogregression
n <- length (y)
print (n)
y.lag <- c (NA, y[1:(n-1)])
lm.lag <- lm (y ~ y.lag)
display (lm.lag)


# simulate replicated datasets (using beta.hat, sigma.hat): we don't add in the inferential uncertainty
b.hat <- coef (lm.lag)
s.hat <- sigma.hat (lm.lag)
n.sims <- 1000
y.rep <- array (NA, c(n.sims, n))
for (s in 1:n.sims){
  y.rep[s,1] <- y[1]
  for (t in 2:n){
    prediction <- c (1, y.rep[s,t-1]) %*% b.hat
    y.rep[s,t] <- rnorm (1, prediction, s.hat)
  }
}


# simulate replicated datasets (full uncertainty): add in inferential uncertainty for the coefficients
lm.lag.sim <- sim (lm.lag, n.sims)
for (s in 1:n.sims){
  y.rep[s,1] <- y[1]
  for (t in 2:n){
    prediction <-  c (1, y.rep[s,t-1]) %*% lm.lag.sim$coef[s,]
    y.rep[s,t] <- rnorm (1, prediction, lm.lag.sim$sigma[s])
  }
}

# plot the simulated unemployment rate series
# postscript ("unemployment2.ps", height=5, width=10, horizontal=F)
par (mfrow=c(3,5), mar=c(4,4,2,2))
for (s in 1:15){
  plot (year, y.rep[s,], type="l", ylab="unemployment", xlab="year", yaxs="i",
  ylim=c(0, max(y)*1.05), yaxt="n", mgp=c(2,.5,0), cex.axis=1.5, cex.lab=1.5, main=paste ("simulation", s), cex.main=1.5)
  axis (2, c(0,5,10), paste (c(0,5,10), "%", sep=""), mgp=c(2,.5,0), cex.axis=1.5)
}
# dev.off()


# numerical model check: the number of switches.
test <- function (y){
  n <- length (y)
  y.lag <- c (NA, y[1:(n-1)])
  y.lag2 <- c (NA, NA, y[1:(n-2)])
  sum (sign(y-y.lag) != sign(y.lag-y.lag2), na.rm=TRUE)
}
print (test (y))

test.rep <- rep (NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- test (y.rep[s,])
}
print (mean (test.rep > test(y)))
print (quantile (test.rep, c(.05,.5,.95)))
