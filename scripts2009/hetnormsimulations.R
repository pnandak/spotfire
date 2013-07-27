# note: this is the same code that can be found in the hetero.r file in the handouts section of the course website
# there are two differences:
#   (1)i've added more comments to the code in an effort to walk through what's going on in every line
#   (2)there's an MLE function at the end to run a normal regression
#   that was simply for the purpose of running an lr.test between the heteroskedastic model and the normal regression model
#   if my comments don't help or things aren't clear - always feel free to email me! 

# Maximum likelihood estimation in R
# Heteroskedastic normal example
# Chris Adolph  www.chrisadolph.com
# 12/31/2004

library(MASS)

# Draw heteroskedastic normal data
# (Note generlized code.  Note use of vectors.)
# think in terms of modeling stochastic component of normal 

obs <- 1500
const1 <- 0
beta <- 10 # note values of beta and gamma
const2 <- 0
gamma <- 4
x <- runif(obs) # randomly drawing from uniform distribution. default min=0, max=1. explanatory variables for mu.
mu <- const1 + x*beta # modeling mean.
z <- x # explanatory variables for sigma2.
sigma2 <- exp(const2 + z*gamma) # sigma must be positive, so use exp(). modeling variance.
y <- rnorm(obs)*sqrt(sigma2) + mu 

# Plot simulated data
par(usr=c(0,1,-5,30))
plot(x,y)

# illustrating purpose of rnorm(obs) in generating y
# introduces the heteroskedasticity. otherwise, simple linear relationship.
y.1 <- sqrt(sigma2) + mu 
x11()
plot(x,y.1)

# Fit least squares model using lm
ls.result <- lm(y~x)

# Fit heteroskedastic normal model using optim
# (Note generalized code.  Note use of matrices and matrix operations.)
llk.hetnormlin <- function(param,y,x,z) { # y is vector of dv, x is matrix of iv to explain mu, z is matrix of iv to explain sigma2, 
                                          # param is the parameters which MLE will estimate. these are defined in the expressions that define the function.
  os <- rep(1,length(x)) # vector of 1s the length of x (1500)
  x <- cbind(os,x) # x becomes matrix of 1s and original x vector
  z <- cbind(os,z) # z becomes matrix of 1s and original z vector
  b <- param[ 1 : ncol(x) ] # param[1:2]. generalized so if x matrix was larger (more iv), would estimate corresponding number of parameters. 
  g <- param[ (ncol(x)+1) : (ncol(x) + ncol(z)) ] # param[3:4]. likewise. 
  xb <- x%*%b # matrix x (constant 1 and x) multiplied by parameters 1 and 2 (const1 and beta).
  s2 <- exp(z%*%g) # sigma squared. using exp() to ensure it's positive. matrix z multipled by parameters 3 and 4 (const2 and gamma).
  sum(0.5*(log(s2)+(y-xb)^2/s2))  # lnL(Beta, sigma2|y) = sum[-1/2(ln(sigma2) + ((yi-xi*beta)^2)/sigma2)] (King around 65)
}                                 # optim is a minimizer by default (despite its name), so min -ln L(param|y). hence no negative sign. 

stval <- c(0,0,0,0)  # initial guesses of const1, beta, const2, gamma
stval2 <- c(5,5,5,5) # another guess
hetnorm.result <- optim(stval,llk.hetnormlin,method="BFGS",hessian=T,y=y,x=x,z=z)
                   # call minimizer procedure
                   # default for optim function is to minimize. unless set control$fnscale to a negative number.
                   # arguments: par is starting values for parameters, fn is function to minimize, method of minimization - in this case use function values and gradients 
                   # to determine likelihood surface, return Hessian matrix?, supply data
#if cared to try the other guess:
#hetnorm.result2 <- optim(stval2,llk.hetnormlin,method="BFGS",hessian=T,y=y,x=x,z=z)
                  
                   # can also use the nlm function.
# the beginning of the help files in the R documentation for the optim and nlm commands. these are functions already in R.                   
# optim(par, fn, gr = NULL,
#           method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"),
#           lower = -Inf, upper = Inf,
#           control = list(), hessian = FALSE, ...)

# nlm(f, p, hessian = FALSE, typsize=rep(1, length(p)), fscale=1,
#         print.level = 0, ndigit=12, gradtol = 1e-6,
#         stepmax = max(1000 * sqrt(sum((p/typsize)^2)), 1000),
#         steptol = 1e-6, iterlim = 100, check.analyticals = TRUE, ...)

# The symmetric n by n matrix of second partial derivatives is called the Hessian matrix of a function

pe <- hetnorm.result$par   # coefficients
                           # should match original simulated const1, beta, const2, gamma
vc <- solve(hetnorm.result$hessian)  # var-cov matrix
                                     # inverse of the Hessian matrix (information) is the var-cov matrix.
se <- sqrt(diag(vc))    # standard errors = sqrt of the diagonal of the v-c matrix. 
ll <- -hetnorm.result$value  # likelihood at maximum 
                             


######## Everything from here up should look the same as the code in hetero2-2.r that was posted on the course website under lab section last week
#       with a few added explanation comments.


####   Simulate results
###    the numbers in parenthesis correspond to the step numbers on pages 22/32 and 23/32 of the slides from lecture
sims <- 10000               # number of simulations (m in KTW 2000)
simbetas <- mvrnorm(sims,pe,vc) # draw (simulate) parameters from the multivariate normal distribution with mean = pe and var = vc 10000 times. (3) 
simbetas[1:20,] # first 20 draws from posterior distribution of parameters
xhat <- seq(0,1,0.1) # values of x of interest (sequence from 0 to 1) (Xc in lecture notes) (1)
simy <- NULL
simx <- NULL
for (i in 1:length(xhat)) {                         # loop function: repeats below simulations for all values of x
  simmu <- simbetas[,1:2]%*%rbind(1,xhat[i])        # simulated systematic component for mu using drawn parameters (so, mu = x*beta). (4)       
  simsig2 <- exp(simbetas[,3:4]%*%rbind(1,xhat[i])) # simulated systematic component for sigma squared using drawn parameters (sigma2 = exp(x*gamma))                                                   
  simy0 <- sqrt(simsig2)*rnorm(sims)+simmu          # simulate outcome variable by randomly drawing from model's distribution (5)
  simy <- rbind(simy,simy0)                         # predicted values. simy=simy0
  simx <- rbind(simx,t(t(rep(xhat[i],sims))))       # repeats each value of xhat 10000 times 
}

plot(simx, simy)    # note distribution of predicted values of y around each x. 


### be sure to have the plot.simulates.r program in some directory. if not in your R working directory, then you'll need to specify that. 
source("F:/CSSS 536/lab/plot.simulates.r")

# Plot simulated results
x11() # opens graphics device
plot.simulates(simx,
               simy,
               ci.range=c(0.67,0.95),
               ci.mark=c("poly","dashed"),
               thin=0.0,
               usr=c(0,1,-5,30)
               )
               

#### simulating expected values
sims2 <- 1000
simyev <- rep(0, sims2)  # simulated expected values vector to be filled in the loop below
simye <- NULL
ysim <- NULL
xc <- .4  # what are the expected values when x=.4?
simbetas <- mvrnorm(sims2,pe,vc)

# simulate expected values for a single value of x
for (i in 1:length(simyev)) {                  # repeat for the entire length of simyev (sims2 or 1000 times)                
  simmu2 <- simbetas[,1:2]%*%rbind(1,xc)       # simulated mu
  simsig2 <- exp(simbetas[,3:4]%*%rbind(1,xc)) # simulated sigma2                                                  
  simy0 <- sqrt(simsig2)*(rnorm(sims2))+simmu2 # simulated predicted values     
  simye <- rbind(simye,simy0) 
  simyev[i] <- mean(simye)                     # average the simulated predicted values to get the expected value    
  }
  
# notice that the expected values are very similar to the simmu vector for the same xc. this is due to the "shortcut" on pg 24/32 of slides.        

simyev  # take a look at the simulated expected values
simmu2  # as well as the simulated mu
summary(simyev)
summary(simmu2)


## for a different value of x
sims2 <- 1000
simyev2 <- rep(0, sims2)
simye <- NULL
xsim <- NULL
ysim <- NULL
xc <- .9

# simulate expected values for a single value of x
for (i in 1:length(simyev)) {                  
  simmu2 <- simbetas[,1:2]%*%rbind(1,xc)        
  simsig2 <- exp(simbetas[,3:4]%*%rbind(1,xc))                                                    
  simy0 <- sqrt(simsig2)*rnorm(sims2)+simmu2         
  simye <- rbind(simye,simy0) 
  simyev[i] <- mean(simye)                         
}

# again, notice that this has very similar values to the simmu vector for the same xc  

simyev
simmu2
summary(simyev)
summary(simmu2)

#### more code later for simulating the expected values over a range of x values
