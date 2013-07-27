#### lab 021805

#####################
#binomial regression#
#####################

# and a mini-lesson in constructing datasets in R
# the following is taken from John Fox, An R and S-Plus Companion to Applied Regression, page 163
# if you don't have it, and plan on making R part of your statistical toolbox, the book's fairly handy to have around

# the dataset he uses as an example for binomial regression is from a fairly classic study of voting in the 1956 US Pres election by Campbell, Converse, Miller, and Stokes (1960)
# (assuming you're a political scientist AND an americanist - i fall only into the former category)
# the date is based on interviews - respondents were asked how close they perceived the election to be, the intensity of their partisan preference, and whether or not they voted
# this results in 3 basic variables:
# closeness: one-sided or close race
# preference: weak, medium, or strong preference for one or the other party
# vote: did they vote or not

# these three variables can be combined into a table, but we have to make it

#constructing a dataset in R
# closeness is a categorical, or factor variable
closeness <- factor(rep(c("one.sided", "close"), c(3,3)), levels=c("one.sided", "close"))
# preference is also categorical, but it's ordered as well (similar to factor)
preference <- ordered(rep(c("weak", "medium", "strong"), 2), levels=c("weak", "medium", "strong"))
# the numbers of people who voted
voted <- c(91, 121, 64, 214, 284, 201)
# the numbers of people who didn't vote
did.not.vote <- c(39, 49, 24, 87, 76, 25)
# the logit is the log-odds of voting
logit.turnout <- log(voted/did.not.vote)
# put 'em into a data.frame so R will recognize them as a dataset
ccms.vote <- data.frame(closeness, preference, voted, did.not.vote, logit=logit.turnout)

# see?
ccms.vote

# a little reminder about saving and loading data in R
save(ccms.vote, file="vote.R")
rm(list=ls())
load("vote.R")

attach(ccms.vote)
logit.turnout <- logit

# if you add the total of those who did and did not vote, you get the total number of respondents
sum(voted) + sum(did.not.vote)


# you could think of this as grouped count data, since the respondents are grouped into the three categories. the total number of people who voted (the variable 
# C, C, M, & S care about) is bounded. so we might as well use a binomial regression, since we don't have (yet) vote/didn't vote values for each individual.


# a snazzy plot, borrowed 100% from Fox, but it illustrates a few extras in terms of plotting that we haven't used so far
# we'll have to install the car package from cran and then load the car library for the prob.axis command
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 4.1))  # the "mar" argument in par() gives the number of lines of margin to be specified on the four sides of the plot (see help(par))
plot(rep(1:3, 2), logit.turnout, type="n", axes=F, xlab="Intensity of Preference", ylab="Logit(Voted/Did Not Vote)")
axis(1, at=1:3, labels=c("Weak", "Medium", "Strong"))   # x-axis    axis() adds an axis to a plot, the number refers to the side of the plot
axis(2) # y-axis
library(car)    # the car package must first be installed from CRAN
prob.axis(side="right", at=seq(.7, .85, by=.05), axis.title="Proportion(Voted)") # right y-axis, this function produces axes for the original scale of 
                                                                                 # a transformed variable. in this case, it's the proportion of those who actually voted.
box()   # turns the plotting space into a box
points(1:3, logit.turnout[1:3], pch=1, type="b", lty=1, lwd=3, cex=2) # one-sided   type b does both points and lines (type p does points, type l does lines)
points(1:3, logit.turnout[4:6], pch=16, type="b", lty=2, lwd=4, cex=2) # close
text(locator(2), c("Close", "One-Sided"))   # this command actually allows to point and click to where you want graph labels to go. nifty, eh?


# based upon this plot, we might create an interaction variable closeness*preference and use it to estimate voting
# this is what we'd call a saturated model, since we're estimating 6 parameters from the 6 proportions
options(contrasts=c("contr.sum", "contr.poly"))
bin.vote <- glm(cbind(voted, did.not.vote) ~ closeness*preference, family=binomial)
summary(bin.vote)


#constructing a similar dataset in R
# the following lines of code "simply" expand the table into a simulated dataset of individuals
# so the above data is the sum of this binary data
voted2 <- c(rep(1, 91), rep(0, 39), rep(1, 121), rep(0, 49), rep(1, 64), rep(0, 24), rep(1, 214), rep(0, 87), rep(1, 284), rep(0, 76), rep(1, 201), rep(0, 25))
closeness2 <- factor(rep(c("one.sided", "close"), c(388, 887)), levels=c("one.sided", "close"))
preferences2 <- ordered(rep(c("weak", "medium", "strong", "weak", "medium", "strong"), c(130, 170, 88, 301, 360, 226)), levels=c("weak", "medium", "strong"))
ccms.vote2 <- data.frame(voted2, closeness2, preferences2)

attach(ccms.vote2)
ccms.vote2

# we can use glm on this new dataset, now of 0s and 1s instead of grouped counts
bin.vote2 <- glm(voted2 ~ closeness2*preferences2, family=binomial)

# but look: same coefficients!
bin.vote
bin.vote2


# turning the simulated data into a contingency table
ccms <- table(voted2, preferences2, closeness2) 
# this can be coerced into a data frame
Ccms <- as.data.frame(ccms)
# but more on contingency tables next week . . . 

bin.vote3 <- glm(cbind(voted, did.not.vote) ~ preference, family=binomial)



#########
#poisson#
#########

po1 <- rpois(10000, 1)
po3 <- rpois(10000, 3)
po8 <- rpois(10000, 8)
po12 <- rpois(10000, 12)
po20 <- rpois(10000, 20)

# graphing a poisson distribution 
x <- sort(unique(po3))
tab1 <- table(po3)
par(mfrow=c(1,2))
plot(x, tab1, type="h") # type h is like a histogram
hist(po3) 

library(MASS)

# this is the crab dataset from the Agresti book, page 127
# the dataset is from a study of nesting horseshoe crabs 
# and the variables are:
# sat: satellites, or male crabs, residing near the female crab
# color: 1=light medium, 2=medium, 3=dark medium, 4=dark
# spine: 1=both good, 2=one worn or broken, 3=both worn or broken
# width: carapace width in centimeters
# weight: weight in kilograms

#crab <- read.csv("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/lab stuff/crab.csv", header=TRUE, sep=",")
crab <- read.csv("http://students.washington.edu/fishes/CSSS536/data/crab.csv", header=TRUE, sep=",")
attach(crab)

# let's look at the distribution of the dependent variable, satellites
satun <- sort(unique(sat))  # unique() selects out only unique values of a variable, sort() sorts them in order
tab <- table(sat)           # frequency table of satellites
plot(satun, tab, type="h", xlab="number of satellites", ylab="frequency")   # a plot of the number of satellites

# fitting a model with width as the only covariate
fit1 <- glm(sat ~ width, family="poisson")

pe1 <- fit1$coef

# calculate some expected counts
wid <- seq(21, 34, .5)
muc <- rep(0, length(wid))
for(i in 1:length(wid)) {
muc[i] <- exp(pe1[1] + pe1[2]*wid[i])
}

# and plot them
plot(wid, muc, type="p", pch=20, xlab="width in cm", ylab="expected number of satellites")
# note: we're used to seeing probabilities. now our expected values are counts.

# how many times does the count of satellites increase for a unit change in the width of a crab?
dif <- exp(pe1[2])

muc25 <- exp(pe1[1] + pe1[2]*25)
muc26 <- dif*muc25  # so if we multiply "dif" by the expected count at 25, we should get the expected count at 26
muc26.2 <- exp(pe1[1] + pe1[2]*26)

# a more interesting model
fit2 <- glm(sat ~ width + color + spine, family="poisson")

# or we could use a blast from the past:
llk.poisson <- function (param, y, x) {
     os <- rep(1, nrow(x))
     x <- cbind(os, x)
     b <- param[1 : ncol(x)]
     xb <- x%*%b
     sum( - y*xb + exp(xb) )                # see King 89 eq 5.20
     }
     
X <- cbind(width, color, spine)       # matrix of explanatory variables 
Y <- sat                                    # the dependent variable 

stval <- c(0,0,0,0)

poisson.result <- optim(stval, llk.poisson, method="BFGS", hessian=T, y=Y, x=X)

coeff <- poisson.result$par            # first coef is constant, rest will go in same order as entered above
vc <- solve(poisson.result$hessian)  
se <- sqrt(diag(vc))   
zscore <- coeff/se 
pvalue <- 2.0 * (1.0-pnorm(abs(zscore)))
tab <- cbind(coeff,se,zscore,pvalue)    # produces a table of the coefficients, standard errors, zscores, and p-values 
                                        # similar to what we're often used to seeing in journal articles
ll <- -poisson.result$value         

# can also calculate predicted probabilities for specific counts of interest
# see Long, page 226 for the equation 
muc2 <- rep(0, length(wid))
for(i in 1:length(wid)) {
muc2[i] <- exp(coeff[1] + coeff[2]*wid[i] + coeff[3]*mean(color) + coeff[4]*mean(spine))
}

prob1 <- (exp(-muc2)*(muc2^1))/factorial(1)     # the predicted probability of 1 satellite across width values
prob5 <- (exp(-muc2)*(muc2^5))/factorial(5)     # the predicted probability of 5 satellites across width values
prob10 <- (exp(-muc2)*(muc2^10))/factorial(10)  # the predicted probability of 10 satellites across width values

plot(wid, prob1, pch=20)
points(wid, prob5, pch=21)
points(wid, prob10, pch=22)
text(locator(3), c("1 sat", "5 sat", "10 sat"))

# everyone's favorite. a simulation of the predicted counts.
sims <- 1000
simbetas <- mvrnorm(sims, coeff, vc)
widhat <- seq(21, 33, 1)

simmu <- NULL
simxb <- NULL
simy <- NULL
simx <- NULL

for (i in 1:length(widhat)) {
simmu <- rbind(1, widhat[i], mean(color), mean(spine))
simxb <- simbetas%*%simmu
simy0 <- exp(simxb)
simy <- rbind(simy, simy0)
simx <- rbind(simx,t(t(rep(widhat[i],sims))))
}

source("http://faculty.washington.edu/cadolph/log/plot.simulates.r")

par(mfrow=c(1,2))
plot.simulates(simx,
               simy,
               ci.range=c(0.95),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(21,33,0,15),
               closeplot=FALSE,
               lcollist="blue"
               )

# is this possion model a good model?
# let's take a look at the residuals
hats <- hatvalues(fit2)/mean(hatvalues(fit2))
stresid <- rstudent(fit2)

plot(hats, stresid)
abline(h=c(-2,2), v=3, lty=2) 

# do the mean and variance equal one another?
mean(sat)
var(sat)

# how do the fitted values compare to the actual values?
fit <- round(fit2$fitted.values)
par(mfrow=c(1,2))
hist(fit, freq=FALSE, br=c(0,1,2,3,4,5,6,7,8,9))
hist(sat, freq=FALSE, br=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))


# more on this next week, as well, but one alternative when we have dispersed data is to fit a negative binomial model to count data
# a negative binomial assumes that data is distributed like a poisson, but does not assume that the mean equals the variance
# it instead introduces a small difference for each case
# the model is estimated and interpreted much like a poisson
# this isn't that important now, but i wanted to show you the difference in terms of standard errors and coefficients

# first, let's take a quick look at what they look like relative to a poisson
par(mfrow=c(1,2))
nb4 <- rnbinom(1000, size=1, mu=4)
nbx <- sort(unique(nb4))
nbtab <- table(nb4)
plot(nbx, nbtab, type="h")

po4 <- rpois(1000, 4)
pox <- sort(unique(po4))
potab <- table(po4)
plot(pox, potab, type="h")

# fitting a negative binomial model. it has it's own glm command: glm.nb()
fit.nb <- glm.nb(sat ~ width + color + spine, link=log)

vcnb <- vcov(fit.nb)
coeffnb <- fit.nb$coeff

mucnb <- rep(0, length(wid))
for(i in 1:length(wid)) {
mucnb[i] <- exp(coeffnb[1] + coeffnb[2]*wid[i] + coeffnb[3]*mean(color) + coeffnb[4]*mean(spine))
}

# how do the expected counts compare?
par(mfrow=c(1,1))
plot(wid, muc2, type="p", pch=20, xlab="width in cm", , ylim=c(0,10), ylab="expected number of satellites")
points(wid, mucnb, type="p", pch=21)

legend(x=26,y=6,bty="n",
       xjust=1,
       legend=c("poisson", "negative binomial"), pch=c(20, 21))
       

# how do the simulations compare?
sims <- 1000
simbetasnb <- mvrnorm(sims, coeffnb, vcnb)
widhat <- seq(21, 33, 1)

simmu <- NULL
simxbnb <- NULL
simynb <- NULL
simxnb <- NULL

for (i in 1:length(widhat)) {
simmu <- rbind(1, widhat[i], mean(color), mean(spine))
simxbnb <- simbetasnb%*%simmu
simy0 <- exp(simxbnb)
simynb <- rbind(simynb, simy0)
simxnb <- rbind(simxnb,t(t(rep(widhat[i],sims))))
}

plot.simulates(simxnb,
               simynb,
               ci.range=c(0.95),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(21,33,0,15),
               closeplot=FALSE,
               lcollist="blue"
               )
               
cbind(coeff, coeffnb)
