#### LAB february 25, 2005
# negative binomial regression, zero-inflated models, and a first look at contingency tables

library(MASS)


###################
#negative binomial#
###################

# this is the dataset on doctoral student publications that we looked at a few labs back
articles <- read.csv("http://students.washington.edu/fishes/CSSS536/data/couart2.csv", header=TRUE, sep=",")

attach(articles)

# let's run a negative binomial regression first
art.nb <- glm.nb(art ~ fem + mar + kid5 + ment + phd)
summary(art.nb)

# so, what is theta? (most importantly, what is theta in terms of alpha?)
# let's see what the actual code for glm.nb looks like - which you can do by simply typing in the name of the function
# the format should look familiar. let's start at the log-likelihood equation and work from there . . . 
# note the use of lgamma . . . 

# looks like theta is phi (see that?)
# so, how did we define alpha in terms of phi in lecture? (hint: topic 7 lecture notes around page 18)

alpha.nb <- 1/art.nb$theta  # alright, i gave it away anyways

# is this a good model for the data? is there evidence of overdispersion?

# first, let's run a poisson regression on the same covariates
art.pois <- glm(art ~ fem + mar + kid5 + ment + phd, family=poisson(link=log))

### and now a likelihood ratio test of alpha
# remember, the negative binomial collapses to a poisson as alpha goes to zero, so we want to test our estimated alpha against the null hypothesis that it equals zero
# these are nested models, essentially, since we're just estimating one more parameter for the nb model (which means we're constraining one more for the pois model)
# Gsquared = 2(lnLnb - lnLpois) (see Long page 237)

# we need the log likelihood values for each of the models
lnL.nb1 <- art.nb$twologlik/2
lnL.nb <- logLik(art.nb)
lnL.pois <- logLik(art.pois)

G2 <- 2*(lnL.nb - lnL.pois)

pchisq(G2, 1, lower.tail=FALSE) # note lower.tail option. by default, pchisq tests the probability of being <= value of interest. 
                                # we want to test the probability of being greater than alpha, or our estimated dispersion paramter
                                # one degree of freedom


### how do our two fits compare?
# let's look at the residuals
hats.p <- hatvalues(art.pois)/mean(hatvalues(art.pois))
stresid.p <- rstudent(art.pois)

hats.nb <- hatvalues(art.nb)/mean(hatvalues(art.nb))
stresid.nb <- rstudent(art.nb)

plot(hats.p, stresid.p, type="p", col="blue")
abline(h=c(-2,2), v=3, lty=2)

points(hats.nb, stresid.nb, type="p", col="green")


### and some expected counts
coeff.p <- art.pois$coeff
coeff.nb <- art.nb$coeff

pe.p <- coeff.p
pe.nb <- coeff.nb

vcp <- vcov(art.pois)
vcnb <- vcov(art.nb)

menthat1 <- seq(0,40,1)
fit.pois <- rep(0, length(menthat1))
for (i in 1:length(menthat)) {
fit.pois[i] <- exp(pe.p[1]*1 + pe.p[2]*mean(fem) + pe.p[3]*mean(mar) + pe.p[4]*mean(kid5) + pe.p[5]*(menthat[i]) + pe.p[6]*mean(phd))
}

fit.nb <- rep(0, length(menthat1))
for (i in 1:length(menthat)) {
fit.nb[i] <- exp(pe.nb[1]*1 + pe.nb[2]*mean(fem) + pe.nb[3]*mean(mar) + pe.nb[4]*mean(kid5) + pe.nb[5]*(menthat[i]) + pe.nb[6]*mean(phd))
}

plot(menthat1, fit.pois, type="p", col="blue")
points(menthat1, fit.nb, col="green")


### let's compare a poisson and a negative binomial distribution with the articles variable
# with mean and variance (for the nb) defined by the estimated mean and dispersion parameters from the models
par(mfrow=c(1,1))
nbart <- rnbinom(length(art), mu = mean(art.nb$fitted.values), size = art.nb$theta)    # take a look at the help files to see about the specification for the negbin
nbx <- sort(unique(nbart))
nbtab <- table(nbart)
plot(nbx, nbtab, type="b", col="blue", ylim=c(0,300))

part <- rpois(length(art), lambda = mean(art.pois$fitted.values))
px <- sort(unique(part))
ptab <- table(part)
points(px, ptab, type="b", col="purple")

nbx2 <- sort(unique(art))
nbtab2 <- table(art)
points(nbx2, nbtab2, type="b", col="green")

legend(6, 150, legend=c("NegBin", "Poisson", "Articles"), col=c("blue", "purple", "green"), lty=1)


######################
#zero-inflated models#
######################
 
# first, you have to install the zicounts package from CRAN
# then call the library
library(zicounts)

# let's see what's available in the zicounts package
# and then see what the zicounts() function looks like

help(zicounts)

# as you can see, zicounts() can also run poisson and negative binomial regression models
# a few things to note:
# the specification for the linear component is a bit different
# you also need to specify the name of the dataset that's being used
# the default distribution is a zero-inflated negative binomial

# so, a poisson
art.pois2 <- zicounts(resp = art ~.,x = ~ fem + mar + kid5 + ment + phd, data = articles, distrname = "Poisson")

# let's compare these coefficients with those from our glm model, above
cbind(coeff.p, art.pois2$coefficients)

# and do the same for a negative binomial
art.nb2 <- zicounts(resp = art ~.,x = ~ fem + mar + kid5 + ment + phd, data = articles, distrname = "NB")
cbind(coeff.nb, art.nb2$coefficients[1:6])  

# one of the "easy" things about R is how consistent they are with their terminology (note sarcasm, please)
# the last coefficient, and the one i left off from the art.nb2 model in the command above is log(tau)
# what the hell is that you ask? 
# tau = theta

# so to get alpha
theta <- exp(art.nb2$coefficients[7])
alpha <- 1/theta

# now for a zero-inflated poisson
# you need to explicitly specify which covariates are X (the poisson part) and which covariates are Z (the logit, or excess zeros, part)
art.zip <- zicounts(resp = art ~ ., x = ~fem + mar + kid5 + ment + phd, z = ~fem + mar + kid5 + ment + phd, data=articles, distrname="ZIP")

# zero-inflated negative binomial
# X (negative binomial part) and Z (logit part)
art.zinb <- zicounts(resp = art ~ ., x = ~fem + mar + kid5 + ment + phd, z = ~fem + mar + kid5 + ment + phd, data=articles, distrname="ZINB")

# let's compare the coefficients
coeff.zip <- art.zip$coefficients
coeff.zinb <- art.zinb$coefficients

count.coeff <- cbind(coeff.p, coeff.nb, coeff.zip, coeff.zinb)

# let's calculate the expected counts across number of mentor articles
x <- rbind(1, mean(fem), mean(mar), mean(kid5), 0:40, mean(phd))

# from the zip model
fit.lambda <- exp(coeff.zip[1:6]%*%x)

fit.pi <- 1/(1 + exp(-coeff.zip[7:12]%*%x))

fit.zip <- (1-fit.pi)*(exp(fit.lambda))

# from the zinb model
fit.lambda2 <- exp(coeff.zinb[1:6]%*%x)

fit.pi2 <- 1/(1 + exp(-coeff.zinb[7:12]%*%x))

fit.zinb <- (1-fit.pi2)*(exp(fit.lambda2))

plot(0:40, fit.zip, col="red", xlab="mentor's articles", ylab="expected count of articles")
points(0:40, fit.zinb, type="p", col="orange")

## we can test the ZIP and ZINB models using a likelihood ratio test
lnL.zinb <- -art.zinb$maxlike
lnL.zipois <- -art.zip$maxlike
G2 <- 2*(lnL.zinb - lnL.zipois)

pchisq(G2, 1, lower.tail=FALSE)

## remember, in order to test the ZIP against the Poisson, or the ZINB against the Negative Binomial, we need a non-nested test
# BIC or AIC would work


####################
#contingency tables#
####################

# take a look at last week's lab notes to see how we constructed one "from scratch"

# for this example, we'll use a dataset that's in R

data(Titanic)
Titanic

is.table(Titanic)   # the dataset is already in table format

help(ftable)                                            # creates "flat" contingency tables
ftable(Titanic, row.vars = 1:3)                         # and the different options associated with the command
ftable(Titanic, row.vars = 1:2, col.vars = "Survived")  # specify which variables you want as rows, and which as columns
ftable(Titanic, row.vars = 2:1, col.vars = "Survived")
ftable(Titanic, row.vars = 1, col.vars = "Survived")    
xx <- ftable(Survived ~ Class, data = Titanic)          # columns ~ rows, should produce same table as above
ftable(Survived ~ ., data=Titanic)                      # places survived as the column var, and all the rest as rows

Titanic2 <- as.data.frame(Titanic)                      # transform the contingency table into a data frame
c.s <- xtabs(Freq ~ Class + Survived, data=Titanic2)    # help(xtabs): this function creates a contingency table and performs a chi-square test for independence 
c.s                                                     # the table
summary(c.s)                                            # the chi-square test

c.s.g <- xtabs(Freq ~ Class + Survived + Sex, data=Titanic2)
summary(c.s.g)

# comparing a contingency table generated via xtabs() and a "flat" contingency table generated by ftable, of the same data
c.s.g           
ftable(c.s.g)

zz <- as.table(xx)                                      # using the as.table() command will also result in a chi-square test
summary(zz)


#### more next week . . . 
