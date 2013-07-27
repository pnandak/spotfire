## data generating process
## n: number of replications
## X: value of treatment variable (0, 1, NULL) if NULL then the the
##    pre-intervention distribution is drawn from
## seed: seed for pseudo random number generator
##
gendata <- function(n, X=NULL, seed=NULL){

  if (!is.null(seed)){
    set.seed(seed)
  }

  u1 <- rexp(n)
  u2 <- rnorm(n)
  u3 <- runif(n, -1, 1)
  u4 <- rgamma(n, 3)
  u5 <- rpois(n, 5)
  u6 <- rnorm(n)

  z1 <<- u2*cos(u3*2 - .5)
  z2 <<- sin(u3*3) + u4*cos(u3*1.75)
  z3 <<- log(u4)*cos(u5/u4) 

  z1 <<- (z1 - mean(z1)) / sd(z1)
  z2 <<- (z2 - mean(z2)) / sd(z2)
  z3 <<- (z3 - mean(z3)) / sd(z3)
  
  
  if (is.null(X)){
    x.eta <<- u1 + z1 + z2*u1 + cos(z1*z2)
    x <<- rep(0, n)
    x[abs(x.eta) >= 2] <<- 1
  }
  else{
    if(X==0){
      x <<- rep(0, n)
    }
    else if (X==1){
      x <<- rep(1, n)
    }
    else{
      stop("X must be 0, 1, or NULL\n")
    }
    
  }

  ## y under control
  y <<- sin(z2/2) + 2*z3 + 3.5*cos(z3*z2) + u6
  ## y under treatment
  #y[x==1] <<- -3*sin(z3[x==1])*cos(z2[x==1]) + u6[x==1] 
  y[x==1] <<- 2 + 3*z3[x==1]*cos(z2[x==1]) + u6[x==1] 
  
}


## function to sample from multivariate kernel density estimate
##  (independent Gaussian kernel with user chosen SDs)
##
## n:   number of samples to generate
## X:   data matrix to base the kernel estimate on. Each row of X should
##        correspond to a variable
## SD:  array of standard deviations for the multivariate Gaussian kernels.
##        The length of SD should be equal to the number of columns in X.
##        The kth element of SD corresponds to the kth column of X.
##
## NOTE: THERE IS NO ERROR CHECKING AT THIS POINT
##
samp.kdens <- function(n, X, SD){

  k <- ncol(X)
  sampmat <- matrix(NA, n, k)

  # randomly select rows of X
  samp.inds <- sample(1:nrow(X), size=n, replace=TRUE)
  # sample from the appropriate normal distributions
  for (j in 1:k){
    sampmat[,j] <- rnorm(n, m=X[samp.inds,j], s=SD[j])
  }

  return(sampmat)
  
}






