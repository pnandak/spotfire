#Sijeong Lim

rm(list=ls())
options(scipen=10)

# Load libraries
library(MASS)
library(simcf)
library(pscl)
library(car) # for influence statistics and outlier detection 

data<-read.csv("fish.csv", header=T)
summary(data) # obs: 250 fishing group, counts: number of fish caught, livebait: 1 if used, persons: no of people in a fishing group 

# Process data for model
y <- data$count
x <- cbind(data$livebait,data$persons)
model <- count ~ livebait + persons
modeldata <- as.data.frame(cbind(y,x))
names(modeldata) <- c("count","livebait","persons")

# Fit Poisson model using optim
llk.poisson <- function(param,y,x) {
  os <- rep(1,length(x[,1]))
  x <- cbind(os,x)
  b <- param[ 1 : ncol(x) ]
  xb <- x%*%b
  -sum( xb*y - exp(xb) ) # log-likeihood for poisson
     # optim is a minimizer, so min -ln L(param|y)
}
ls.result <- lm(y~x)  # use ls estimates as starting values
stval <- ls.result$coefficients  # initial guesses
pois.result <- optim(stval,llk.poisson,method="BFGS",hessian=T,y=y,x=x)
                   # call minimizer procedure
pe.pois <- pois.result$par   # point estimates
vc.pois <- solve(pois.result$hessian)  # var-cov matrix
se.pois <- sqrt(diag(vc.pois))    # standard errors
ll.pois<- -pois.result$value
cbind(pe.pois, se.pois)

glm.pois <- glm(count~livebait+persons, modeldata, family=poisson)
summary(glm.pois)

# Set up counterfactuals (range over homevalues for average neighborhood size
person.seq <- seq(1,12,1)
xhyp <- cfMake(model,modeldata,nscen = length(person.seq))
for (i in 1:length(person.seq))
  xhyp <- cfChange(xhyp, "persons", x=person.seq[i], scen=i)

# Simulate results
sims <- 10000
simbetas <- mvrnorm(sims,pe.pois,vc.pois) # draw parameters
yhyp <- loglinsimev(xhyp, simbetas)

par(mfrow=c(1,3))
# Set up lineplot trace of result for plotting using tile
plot(person.seq, yhyp$pe, type="l", col="red", lwd="4")
lines(person.seq, yhyp$lower, col="red")
lines(person.seq, yhyp$upper, col="red")

###############################################
# Re-analyze using negative binomial regression
nb.result <- glm.nb(count~livebait+persons,modeldata)
summary(nb.result)
pe.nb <- nb.result$coefficients
vc.nb <- vcov(nb.result)
se.nb <- sqrt(diag(vc.nb)) # note that se values are larger now
ll.nb <- as.numeric(logLik(nb.result))
theta.nb <- nb.result$theta
setheta.nb <- nb.result$SE.theta
theta.nb

## what is theta is it alpha or phi?
# Type glm.nb and see how loglik is defined: "th" for theta
# make a hypothetical data with known phi and run glm.nb
covariate<-rnorm(1000, 1, 1)
dv<-rnbinom(1000, size=2, mu=exp(covariate*3)) # here, size is phi (the dispersion parameter), set beta=3
result<-glm.nb(dv~covariate) 
result$theta # equals the phi we set up 
# so alpha will be 1/theta
#going back to the fish regression
alpha.nb<-1/theta.nb
alpha.nb

# Simulate results (we re-use xhyp from above)
sims <- 10000
simbetas <- mvrnorm(sims,pe.nb,vc.nb) # draw parameters
yhyp <- loglinsimev(xhyp, simbetas)

plot(person.seq, yhyp$pe, col="blue", type='l',lwd='4')
lines(person.seq, yhyp$lower, col="blue")
lines(person.seq, yhyp$upper, col="blue")

###########################
# Re-analyze using Zero-Inflated Negative Binomial
# Estimate ZINB using zeroinfl in the pscl library
zinbmodel <- count ~ livebait + persons | livebait + persons # the first part is the count model, and the second part is the zero-inflation model. The two components can have different regressors.
zinb.result <- zeroinfl(zinbmodel, modeldata, dist="negbin")  # to do ZIP, leave out dist argument

summary(zinb.result)

# Extract results, taking care to keep track of both equations
pe.zinb.count <- zinb.result$coefficients$count
pe.zinb.zeros <- zinb.result$coefficients$zero
pe.zinb <- c(pe.zinb.count,pe.zinb.zeros)
vc.zinb <- vcov(zinb.result)
se.zinb <- sqrt(diag(vc.zinb))
se.zinb.count <- se.zinb[1:length(pe.zinb.count)]
se.zinb.zeros <- se.zinb[ (length(pe.zinb.count)+1) : length(se.zinb) ]
ll.zinb <- as.numeric(logLik(zinb.result))
theta.zinb <- zinb.result$theta
setheta.zinb <- zinb.result$SE.theta

# Simulate results  (we re-use xhyp from above)
# E(count | not a structural zero) ,so use the count model coefficients

sims <- 10000
simparam <- mvrnorm(sims,pe.zinb,vc.zinb) # draw parameters
simbetas <- simparam[,1:length(pe.zinb.count)]
#simgammas <- simparam[,(length(pe.zinb.count)+1) : length(se.zinb) ]
yhyp <- loglinsimev(xhyp, simbetas)


plot(person.seq, yhyp$pe, col="black", type='l',lwd='4')
lines(person.seq, yhyp$lower, col="black")
lines(person.seq, yhyp$upper, col="black")

# Goodness of fit tests
#NB vs Pois: BIC
NBvsPoisson.bic <- - 2*(ll.nb - ll.pois) + 1*log(nrow(x)) # 1 df -theta
# ZINB vs NB:  BIC
ZINBvsNB.bic <- - 2*(ll.zinb - ll.nb) + 3*log(nrow(x)) # 3 df - one additional set of parameters (intercept and two coefficients) for the zero inflation model
# ZINB vs NB: Vuong
print(vuong(m1=zinb.result, m2=nb.result)) #A large, positive test statistic provides evidence of the superiority of m1 over m2

# Outliers?

par(mfrow=c(1,2))
influencePlot(glm.pois, id.method="identify")
influencePlot(nb.result, id.method="identify")
data[c(89,104, 202),] #check outliers


