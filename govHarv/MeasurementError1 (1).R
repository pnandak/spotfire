#
# Example code for measurement error pathlogies.  With coverage correction
#

#set the random number seed so we get the same answer every time
set.seed(34919)

#make printing wider
options(width=150)

#load up the dataset called "dta"
# see agl1.R for more details
dta  <- as.data.frame(dget(file="http://jsekhon.fas.harvard.edu/gov2000/R/agl1.dpt"))

#get some utility functions Sekhon has written
source("http://jsekhon.fas.harvard.edu/gov2000/R/utils1.R")

#number of Monte Carlo Simulations
simulations  <- 1000

#estimate the true model which we will assume is correct.

truth  <- lm(y~growth.lag + demand + exports + imports + lo + left + I(lo*left) + as.factor(year),
         data=dta)
#standard ols results
summary(truth)

#the X variables used in the model
X  <- get.xdata(y~growth.lag + demand + exports + imports + lo + left + I(lo*left) + as.factor(year),
                data=dta)


#number of variables
nvars  <- ncol(X)

#the true parameters as a column vector
true.parameters  <- as.matrix(truth$coeff)

#number of observations
nobs  <- nrow(X)

#matrices to gather up results
sims.coefs.clean  <- matrix(nrow=simulations,ncol=nvars)
sims.coefs.dirty  <- matrix(nrow=simulations,ncol=nvars)
sims.ses.clean  <- matrix(nrow=simulations,ncol=nvars)
sims.ses.dirty  <- matrix(nrow=simulations,ncol=nvars)


for (s in 1:simulations)
  {
    cat("s:", s,"\n")

    #generate data from the model
    epsilon  <- rnorm(nobs, mean=0, sd=1)
    Ysim  <- X %*% true.parameters + epsilon

    #contaminate the OECD demand variable which is the 3th one
    indx  <- 3
    Xdirty  <- X
    for (i in indx)
      {
        Xdirty[,i]  <- X[,i] + rnorm(nobs, mean=0, sd=sd(X[,i])*2)
      }

    lm.clean  <- lm(Ysim~-1 + X)
    lm.dirty  <- lm(Ysim~-1 + Xdirty)

    sims.coefs.clean[s,]  <- lm.clean$coef
    sims.coefs.dirty[s,]  <- lm.dirty$coef

    sims.ses.clean[s,]  <- summary(lm.clean)$coeff[,2]
    sims.ses.dirty[s,]  <- summary(lm.dirty)$coeff[,2]

  }# end of simulations loop

results  <- as.data.frame(matrix(nrow=nvars, ncol=5))
results[,1]  <- true.parameters
row.names(results)  <- row.names(true.parameters)
names(results)  <- c("truth", "coef clean", "95% coverage", "coef dirty", "95% coverage")

results[,2]  <- apply(sims.coefs.clean,2,mean)
results[,4]  <- apply(sims.coefs.dirty,2,mean)


ols.clean.95 <- matrix(0, ncol=nvars);
ols.clean.90 <- matrix(0, ncol=nvars);
for (s in 1:simulations)
  {
    for (p in 1:nvars)
      {
        lb <- sims.coefs.clean[s,p] - sims.ses.clean[s,p]*1.959964
        ub <- sims.coefs.clean[s,p] + sims.ses.clean[s,p]*1.959964
        if ( (true.parameters[p] > lb) & (true.parameters[p] < ub) )
            ols.clean.95[p] <-  ols.clean.95[p]+1

        lb <- sims.coefs.clean[s,p] - sims.ses.clean[s,p]*1.644854
        ub <- sims.coefs.clean[s,p] + sims.ses.clean[s,p]*1.644854
        if (true.parameters[p] > lb & true.parameters[p] < ub)
          ols.clean.90[p] <- ols.clean.90[p]+1
      }
  }

results[,3]  <- as.vector(ols.clean.95/simulations)

ols.dirty.95 <- matrix(0, ncol=nvars);
ols.dirty.90 <- matrix(0, ncol=nvars);
for (s in 1:simulations)
  {
    for (p in 1:nvars)
      {
        lb <- sims.coefs.dirty[s,p] - sims.ses.dirty[s,p]*1.959964
        ub <- sims.coefs.dirty[s,p] + sims.ses.dirty[s,p]*1.959964
        if ( (true.parameters[p] > lb) & (true.parameters[p] < ub) )
            ols.dirty.95[p] <-  ols.dirty.95[p]+1

        lb <- sims.coefs.dirty[s,p] - sims.ses.dirty[s,p]*1.644854
        ub <- sims.coefs.dirty[s,p] + sims.ses.dirty[s,p]*1.644854
        if (true.parameters[p] > lb & true.parameters[p] < ub)
          ols.dirty.90[p] <- ols.dirty.90[p]+1
      }
  }

results[,5]  <- as.vector(ols.dirty.95/simulations)
print(results)
