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
source("E:/CSSS 536/lab/plot.simulates.r")

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
hetnorm.result2 <- optim(stval2,llk.hetnormlin,method="BFGS",hessian=T,y=y,x=x,z=z)
                  
                   # can also use the nlm function.
                   
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
                             

# Simulate results
sims <- 10000
simbetas <- mvrnorm(sims,pe,vc) # draw parameters from a multivariate normal distribution with mean = point estimates and sd = v-c matrix
xhat <- seq(0,1,0.1)
simy <- NULL
simx <- NULL
for (i in 1:length(xhat)) {
  simmu <- simbetas[,1:2]%*%rbind(1,xhat[i]) # simulated const1 and beta 
  simsig2 <- exp(simbetas[,3:4]%*%rbind(1,xhat[i])) # simulated const2 and gamma
  simy0 <- sqrt(simsig2)*rnorm(sims)+simmu # should look like generated y above
  simy <- rbind(simy,simy0)
  simx <- rbind(simx,t(t(rep(xhat[i],sims))))
}

# Plot simulated results
x11() # opens graphics device
plot.simulates(simx,
               simy,
               ci.range=c(0.67,0.95),
               ci.mark=c("poly","dashed"),
               thin=0.0,
               usr=c(0,1,-5,30)
               )
               
               
### Normal Regression via ML in R

  normreg<-function(X,y, method='BFGS'){
    X<-cbind(1,X)
    neglnl<-function(theta,X,y){
        ssq<-theta[length(theta)]
        b<-theta[1:length(theta)-1]
#         print(theta)
        lnl<-as.vector(-.5*log(ssq)-(.5/ssq)*(y-X%*%b)^2)
        -sum(lnl)
        }        
        
    result<-optim(c(mean(y),rep(0,ncol(X)-1),var(y)), neglnl, hessian=T, method=method, X=X, y=y)}

ls.mle <- normreg(x,y)
llr <- -ls.mle$value
llu <- ll

# likelihood ratio test
lr.test <- 2*(llu-llr)
pchisq(lr.test, df=1, lower.tail=F)
