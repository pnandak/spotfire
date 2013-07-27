###### CSSS 536 problem set 1
### due January 28, 2005


##### Problem 1:   (8 pts)
#fraction of males in given population is .49 (pi)

p <- .49
n <- 30
y1 <- 22
y2 <- 16

## what is the probability of sampling 22 males in a random sample of 30?

p22.1 <- (factorial(n)/(factorial(n-y1)*factorial(y1)))*(p^y1)*((1-p)^(n-y1)) # (1) calculating by hand
 
p22.2 <- dbinom(y1, n, p)                               # (2) using density function to get exact probability

# simulating to find answer
# using rbinom
obs <- 1000000

rb <- rbinom(obs, n, p)                                 # (3) randomly draw from the bindomial distribution with 30 trials and a probability of success 
                                                        # equal to p 1mil times.
rb22 <- rb[rb==22]                                      # create vector where value of rb is equal to 22 (corresponding to 22 males)
p22.3 <- length(rb22)/length(rb)                        # probability equals number of times draw 22 males over total number of draws

# using the sample command
# generating a "population" where p(male) = p and the p(female) = 1-p
m <- 1
f <- 0
mf <- c(m, f)   
prob <- c(p, 1-p)   

pop <- rep(0, 100000)

for (i in 1:length(pop)) {
pop[i] <- sum(sample(mf, 30, replace = T, prob=prob))   # (4) sample 30 times from the "population" created above with replacement
}                                                       # see ?sample for argument help. sum the sampled 0s and 1s to get the simulated y.

males22 <- pop[pop==22]                                 # vector where pop is equal to 22 (which corresponds to 22 males)
p22.4 <- length(males22)/length(pop)

p22 <- cbind(p22.1, p22.2, p22.3, p22.4)                # just combining them for the sake of comparison - all 4 values should be fairly close



## what is the probability of sampling 16 males in a random sample of 30?

p16.1 <- (factorial(n)/(factorial(n-y2)*factorial(y2)))*(p^y2)*((1-p)^(n-y2)) # (1) by hand

p16.2 <- dbinom(y2, n, p)                               # (2) again, density function

# simulating to find answer
rb <- rbinom(obs, n, p)                                 # (3) drawing from the binomimal distribution
rb16 <- rb[rb==16]
p16.3 <- length(rb16)/length(rb)

males16 <- pop[pop==16]                                 # (4) from the sampling above
p16.4 <- length(males16)/length(pop)

p16 <- cbind(p16.1, p16.2, p16.3, p16.4)

##### Problem 2: (excel file)   (6 pts)
#P(x|y) = P(x & y)/P(y)
#P(y) = P(x & y)/P(x|y)
#P(x & y) = P(x|y)*P(y)

#x=1 0.29
#x=2 0.37
#x=3 0.34
#y=1 0.18
#y=2 0.25
#y=3 0.44
#y=4 0.13
#x=2, y=2   

# p2.9 = p(x=1|y=2) = p(x=1 & y=2)/p(y=2) = 
p2.9 <- .08/.25  # [1] 0.32

#p2.10 = p(x=3|y=4) = p(x=3 & y=4)/p(y=4) = 
p2.10 <- .06/.13 # [1] 0.4615385

#p2.11 = p(y=4|x=3) = p(y=4 & x=3)/p(x=3) =
p2.11 <- .06/.34 # [1] 0.1764706

#p2.12 = p(y=2|x=2) = p(y=2 & x=2)/p(x=2) =
p2.12 <- .1/.37 # [1] 0.2702703


##### Problem 3:    (8 pts)
# y = (1, 0, 0, 1, 0, 0, 0, 0)

n <- 8 # number of trials
y <- 2 # number of successes

# Plot likelihood and log-likelihood functions for Bernoulli parameter pi given y above
lik <- rep(0,999)       # this plotting code can be found on the website
llik <- rep(0,999)
par(mfcol=c(1,2))
for (i in 1:999) {
  p <- (i)/1000
  lik[i] <- prod(p^y * (1-p)^(n-y))  # likelihood
                                     
  llik[i] <- sum(y*log(p) + (n-y)*log(1-p))  # log-likelihood
}
p <- seq(1,999,1)/1000
plot(p,lik, main="likelihood")
plot(p,llik, main="log-likelihood")

cbind(p,lik)[lik==max(lik)]     # retrieves the values of p and the likelihood where at the maximum value for the likelihood
#[1] 0.25000000 0.01112366
cbind(p,llik)[llik==max(llik)]
#[1]  0.250000 -4.498681

#or simply
Pi1 <- p[lik==max(lik)]
Pi2 <- p[llik==max(llik)]
#[1] 0.25 expected value of pi 


##### Problem 4:    (12 pts)
# derive log-likelihood for poisson model with potentially unequal periods of observation

# generating "fake data"
obs <- 1500
const1 <- 0
beta <- 2
x <- runif(obs)
lambda <- exp(const1 + x*beta)
tim <- runif(obs)+.5
y <- rpois(obs, tim*lambda)

# writing the function 
llk.poisson <- function (param, y, x, tim) {
     os <- rep(1, length(x))
     x <- cbind(os, x)
     b <- param[1 : ncol(x)]
     xb <- x%*%b
     sum(-y*xb + tim*exp(xb) )               # see King 89 eq 5.20 for the lok-likelihood equation, 
                                             # then remember it needs to be negated since optim is actually a minimizer
     }
     
stval <- c(0,0)                              # setting starting values to be used in optim()
                                             # number of starting values should be equal to number of parameters/coefficients estimating
                                             # in this case, we just have a constant and a beta, so 2     

poisson.result <- optim(stval, llk.poisson, method="BFGS", hessian=T, y=y, x=x, tim=tim)    # call optimizer - do help(optim) if confused about the arguments
    
    #first argument gives optim() starting values, second the function to be optimized, the next is the method to use to optimize (in this case BFGS does a gradient search),
    #we want the hessian matrix when optimizing functions so that we can retrieve the variance covariance matrix, and finally we identify the data/variables that each argument
    #in the function should use

### other options
llk.poisson1 <- function (param, y, x, tim) {          
   os <- rep(1, length(x))                             
    x <- cbind(os, x)
    b <- param[1 : ncol(x)]                             
    lambda <- exp(x%*%b)
    negll <- as.vector((-lambda) + y*log(lambda*tim))
    -sum(negll)
    }

stval <- c(0,0)
poisson.result1 <- optim(stval, llk.poisson1, method="BFGS", hessian=T, y=y, x=x, tim=tim)


llk.poisson2 <- function (param, y, x, tim) {            
    os <- rep(1, length(x))
    x <- cbind(os, x)
    b <- param[1 : ncol(x)]
    xb <- x%*%b
    negll <- (y*(xb) - tim*exp(xb))
    -sum(negll)
    }    
    
 
stval <- c(0,0)    
poisson.result2 <- optim(stval, llk.poisson2, method="BFGS", hessian=T, y=y, x=x, tim=tim)

#why does this work as a generalization?

#In the binomial distribution, as n -> infinity and pi -> 0, and n*pi -> lambda, we have a poisson(lambda) distribution.  
#Remember that the Poisson assumes no two events can occur at the same exact time.  
#So really, we can think of the Poisson observation period as a series of infinitesimally brief binomial trials, each with pi -> 0, 
#but summed over all n -> infinity trials, the expected number of success during this observation period is n*pi. (Remember: the expected count for a poisson is lambda.) 
#If we double the number of periods, that's like saying we expect 2*n*pi successes.  In other words, if n*pi = lambda, then 2*n*pi = 2*lambda:  
#the extension in time just scales up lambda in a linear fashion.  
#Therefore, if we normalize the average period to be length t=1, we can allow t_i to vary, and just estimate Y assuming Y~Poisson(t*lambda).


##### Problem 5:    (6 pts)
