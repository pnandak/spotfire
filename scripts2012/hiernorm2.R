#
# DESCRIPTION:
#
#   hiernorm2 fits a two level hierarchical Bayes model via Gibbs sampling.
#   The model is given by:
#
#   y_{ij} = w'_{ij}alpha + x'_{ij}beta_{j} + epsilon_{ij}
#
#   epsilon_{ij} ~ N(0, sigma2)
#
#   and the following prior distributions:
#
#   beta_{j}   ~ N(Zstar_{j}gamma, Sigma.beta)
#   alpha      ~ N(alpha.bar,  Sigma.alpha)
#   gamma      ~ N(gamma.bar,  Sigma.gamma)
#   Sigma.beta ~ InvWish(eta, R)
#   sigma2     ~ InvGamma(nu/2, delta/2)
#
#
# USAGE:
#
#  hiernorm2(y, W, X, Z, cid, alpha.bar=0, Sigma.alpha=-1, gamma.bar=0,
#            Sigma.gamma=-1, IWobs=-1, IWmean=-1, IGobs=-1, IGmean=-1,
#            burnin=1000, gibbs=5000, thin=1, storebeta=FALSE)
#
#
# ARGUMENTS:
#
#  y:           an n x 1 vector containing the observed values of the
#               response variable 
#
#  W:           an n x k matrix containing the observed values of the
#               covariates assumed to exert a constant effect on y
#
#  X:           an n x p matrix containging the observed values of the
#               covariates assumed to exert cluster-specific effects
#               on y. W and X SHOULD NOT share any columns (including
#               constant terms).
#
#  Z:           a J x q matrix of the observed values of the covariates
#               assumed to effect the prior mean of beta. Here J is the
#               number of clusters at the second level. The actual matrix,
#               Zstar, used in the program is formed as Z %x% diag(p),
#               where %x% denotes Kronecker multiplication.
#
#  cid:         an n x 1 vector giving the second level cluster IDs of
#               each observation. It is assumed that the rows of y, W, and
#               X are properly aligned, and that the rows of Z are ordered
#               according to the unique indices in cid
#               ( tapply(cid,cid,mean) should give the cluster IDs
#               corresonding to the elements of Z.)
#
#  alpha.bar    mean of the normal prior on alpha. If a scalar, this is
#               expanded to be a k x 1 vector.
#
#  Sigma.alpha  variance-covariance matrix of the normal prior on alpha.
#               If a scalar, this is expanded to be a diagonal k x k matrix.
#
#  gamma.bar    mean of the normal prior on gamma. If a scalar, this is
#               expanded to be a (p*q) x 1 vector.
#
#  Sigma.gamma  variance-covariance matrix of the normal prior on gamma.
#               If a scalar, this is expanded to be a diagonal
#               (p*q) x (p*q) matrix.
#
#  IWobs        number of pseudo-observations for the inverse Wishart
#               prior on Sigma.beta.
#
#  IWmean       E(Sigma.beta) under the inverse Wishart prior.
#
#  IGobs        number of pseudo-observation for the inverse gamma prior
#               on sigma2
#
#  IGmean       E(sigma2) under the inverse gamma prior
#
#  burnin       number of burnin iterations to be discarded
#
#  gibbs        number of draws after burnin
#
#  thin         thinning parameter for the gibbs iterations. Every
#               thin th draw is stored.
#
#  storebeta    Logical value indicating whether samples of beta
#               should be stored (TRUE), or not (FALSE)
#
# VALUE:
#
#   hiernorm2 returns an object of class hier2.output
#
# 
#
# This program is intended for instructional purposes only. Even on
# very fast machines it is not really fast enough for serious modeling
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# For a copy of the GNU General Public License
# write to the Free Software Foundation, Inc.,
# 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.
#
# Copyright (C) 2001 Kevin M. Quinn
#
# Kevin Quinn
# Assistant Professor
# Dept. of Political Science and CSSS
# Box 354322
# University of Washington
# Seattle, WA  98195-4322
# quinn@stat.washington.edu
# 
# 2/5/2001 
# Last updated 2/23/2001
#

hiernorm2 <- function(y, W, X, Z, cid, alpha.bar=0, Sigma.alpha=-1,
                      gamma.bar=0, Sigma.gamma=-1, IWobs=-1, IWmean=-1,
                      IGobs=-1, IGmean=-1, burnin=1000, gibbs=5000,
                      thin=1, storebeta=FALSE){
  

  ### get the start time ###
  start.time <- proc.time()
  
  ### store the user's function call ###
  call <- match.call()
  
  #### coerce input data into matrices ###
  y   <- as.matrix(y)
  W   <- as.matrix(W)
  X   <- as.matrix(X)
  Z   <- as.matrix(Z)
  cid <- as.matrix(cid)
  cid.unique <- unique(cid)
  
  ### check and record data dimensions ###
  n <- nrow(y) # total number of observations
  J <- nrow(Z) # number of clusters at level 2
  if (ncol(y) !=1)
    stop(message="y not column vector")
  if (nrow(W) != n)
    stop(message="W does not have same rowsize as y")
  if (nrow(X) != n)
    stop(message="X does not have same rowsize as y")
  if (nrow(W) != n)
    stop(message="cid does not have same rowsize as y")
  k <- ncol(W) # size of alpha
  p <- ncol(X) # size of beta_{j}
  q <- ncol(Z) # (p*q) is the size of gamma

  
  ### setup priors from the input ###
  # alpha.bar and Sigma.alpha
  if (max(Sigma.alpha) <= 0)
    Sigma.alpha <- 1e+10
  if (k==1)
    Sigma.alpha <- as.matrix(Sigma.alpha)
  if (length(alpha.bar)==1)
    alpha.bar <- matrix(alpha.bar, k, 1)
  if (length(alpha.bar)!=k){
    stop(message="alpha.bar not of length k") 
  }
  if (length(Sigma.alpha)< k*k)
    Sigma.alpha <- diag(Sigma.alpha, k)
  if (nrow(Sigma.alpha)!=k && ncol(Sigma.alpha) !=k){
    stop(message="Sigma.alpha not k x k")
  }

  # gamma.bar and Sigma.gamma
  if (max(Sigma.gamma) <= 0)
    Sigma.gamma <- 1e+10
  if ((p*q)==1)
    Sigma.gamma <- as.matrix(Sigma.gamma)
  if (length(gamma.bar)==1)
    gamma.bar <- matrix(gamma.bar, (p*q), 1)
  if (length(gamma.bar)!=(p*q)){
    stop(message="gamma.bar not of length (p*q)") 
  }
  if (length(Sigma.gamma)< p*q*p*q)
    Sigma.gamma <- diag(Sigma.gamma, (p*q))
  if (nrow(Sigma.gamma)!=(p*q) && ncol(Sigma.gamma) !=(p*q)){
    stop(message="Sigma.gamma not (p*q) x (p*q)")
  }
  
  # IWobs and IWmean  
  if (IWobs < p) IWobs <- p+2
  eta <- IWobs
  if (length(IWmean)< p*p)
    IWmean <- diag(IWmean, p)
  else if (p==1)
    IWmean <- as.matrix(IWmean)
  if (nrow(IWmean)!=p && ncol(IWmean) !=p){
    stop(message="IWmean not p x p")
  }
  R.inv <- IWmean * (eta - p - 1) # scale matrix
  R <- solve(R.inv)               # inverse scale matrix
  if (max(IWmean) <= 0){ # use [eta * var(y) * (X'X)^-1]^-1 as a rough guess
    R.inv <- eta * var(y)[1] * solve(t(X) %*% X)
    R <- solve(R.inv)
    IWmean <- R.inv / (eta - p - 1)
  }

  
  # nu and delta
  if (length(IGobs) != 1)
    stop(message="IGobs not a scalar")
  if (IGobs < 0)
    IGobs <- 2.2
  nu <- IGobs  
  if (IGmean < 0)
    IGmean <- nu/2 * .5 * var(y)[1]
  if (length(IGmean) != 1){
    stop(message="IGmean not a scalar")
  }
  delta <- IGmean * (nu - 2)
  
  

  ### create names of W variables and Zstar variables ###
  alpha.names <- colnames(W)
  if (!is.null(colnames(X)) && !is.null(colnames(Z))){
    gamma.names <- "junk"
    for (i in 1:q){
      for (j in 1:p){
        gamma.names <- c(gamma.names, paste(colnames(Z)[i], "x",
                                            colnames(X)[j]))
      }
    }
    gamma.names <- gamma.names[2:(p*q+1)]
  }
  else gamma.names <- NULL
  
  ### change gibbs to be evenly divisible by thin if necessary ###
  gibbs <- round(gibbs/thin)*thin
  tot.iter <- burnin + gibbs

  ### create storage matrices ###
  alpha.stored      <- matrix(NA, k, gibbs/thin)
  gamma.stored      <- matrix(NA, p*q, gibbs/thin)
  sigma2.stored     <- matrix(NA, 1, gibbs/thin)
  Sigma.beta.stored <- matrix(NA, p*p, gibbs/thin)
  ypost.stored      <- matrix(NA, n, gibbs/thin)
  ifelse(storebeta,
         beta.stored <- matrix(NA, p*q*J, gibbs/thin),
         beta.stored <- NA)
  
  ### create quantities that don't depend on parameters for re-use ###
  ###               (makes computation much faster)                ### 
  Sigma.alpha.inv                 <- solve(Sigma.alpha)
  Sigma.alpha.inv.times.alpha.bar <- crossprod(t(Sigma.alpha.inv), alpha.bar) 
  WW                              <- crossprod(W)
  Sigma.gamma.inv                 <- solve(Sigma.gamma)
  Sigma.gamma.inv.times.gamma.bar <- crossprod(t(Sigma.gamma.inv), gamma.bar) 
  Zstar                           <- Z %x% diag(p)
  
  ### pick initial parameter values ###
  WX             <- cbind(W,X)  
  coefs          <- solve(crossprod(WX)) %*% crossprod(WX,y)
  coefs          <- matrix(coefs)
  alpha          <- coefs[1:k]
  beta           <- matrix(coefs[(k+1):(p+k)], p, J)
  Xbeta          <- X %*% beta[,1]
  gamma          <- matrix(0,p*q,1)
  Sigma.beta     <- diag(p)
  Sigma.beta.inv <- solve(Sigma.beta)
  sigma2         <- 0.5*var(y)
  count          <- 0






  ####################################
  ### The Gibbs Sampling Algorithm ###
  for (iter in 1:tot.iter){
    
    # sample  [alpha | everything else]
    ystar     <- y - Xbeta
    post.var  <- solve(Sigma.alpha.inv + WW/sigma2[1])
    post.mean <- crossprod(t(post.var), (Sigma.alpha.inv.times.alpha.bar +
                               crossprod(W, ystar)/sigma2[1]))
    CC        <- chol(post.var)
    alpha     <- crossprod(CC, rnorm(k)) + post.mean
    Walpha    <- crossprod(t(W), alpha)

 
    
    # sample  [beta | everything else]
    ystar     <- y - Walpha
    ZSigmaZ.sum    <- 0
    ZSigmabeta.sum <- 0
    for (j in 1:J){
      cid.j   <- cid.unique[j]
      X.j     <- X[cid==cid.j,]
      ystar.j <- ystar[cid==cid.j]      
      Zstar.j <- matrix(Z[j,],1,q) %x% diag(p)
      post.var  <- solve(Sigma.beta.inv + crossprod(X.j)/sigma2[1])
      beta.bar  <- crossprod(t(Zstar.j), gamma) 
      post.mean <- crossprod(t(post.var),
                             (crossprod(t(Sigma.beta.inv), beta.bar) +
                                        crossprod(X.j, ystar.j)/sigma2[1]))
      CC        <- chol(post.var)
      beta[,j]  <- crossprod(CC, rnorm(p)) + post.mean
      Xbeta[cid==cid.j]  <- crossprod(t(X.j), beta[,j])

      #        gammma  related quantities        #
      # calculated in this loop to improve speed #
      ZSigma  <- crossprod(Zstar.j, Sigma.beta.inv)
      ZSigmaZ.sum  <- ZSigmaZ.sum + crossprod(t(ZSigma), Zstar.j)
      ZSigmabeta.sum <- ZSigmabeta.sum + crossprod(t(ZSigma), beta[,j])
    }
    
    
    # sample  [gamma | everything else]
    post.var  <- solve(Sigma.gamma.inv + ZSigmaZ.sum)
    post.mean <- crossprod(t(post.var), (Sigma.gamma.inv.times.gamma.bar +
                               ZSigmabeta.sum))
    CC        <- chol(post.var)
    gamma     <- crossprod(CC, rnorm(p*q)) + post.mean

    
    # sample  [Sigma.beta | everything else]
    u <- as.vector(beta) - crossprod(t(Zstar), gamma) 
    u <- matrix(u,p,J)
    SSE <- crossprod(t(u))
    Sigma.beta.inv <- rwish(eta+J, solve(R.inv+SSE))
    Sigma.beta <- solve(Sigma.beta.inv)
    
    # sample  [sigma2 | everything else]
    e <- y - Walpha - Xbeta
    SSE <- crossprod(e)
    sigma2.inv <- rgamma(1, (nu + n)/2, 2/(delta+SSE))
    sigma2 <- 1/sigma2.inv

    
    # store draws
    if (iter > burnin && iter%%thin==0){
      count                     <- count + 1;
      alpha.stored[,count]      <- alpha
      gamma.stored[,count]      <- gamma
      sigma2.stored[,count]     <- sigma2
      Sigma.beta.stored[,count] <- as.vector(Sigma.beta)
      ypost.stored[,count]      <- Walpha + Xbeta + rnorm(n,0,sqrt(sigma2))
      if (storebeta)
        beta.stored[,count]     <- as.vector(beta)
    }


    # print intermediate results to screen
    if (iter%%50==0){
      cat("iteration = ", iter, "\n")
      cat("alpha = \n")
      print(alpha)
      cat("gamma= \n")
      print(gamma)
      cat("Sigma.beta = \n")
      print(Sigma.beta)
      cat("sigma2 = \n")
      print(sigma2)
      cat("\n\n")
    }

  }
  ###        end Gibbs loop      ###
  ##################################


  ### get end time ###
  end.time <- proc.time()
  tot.time <- end.time - start.time
  
  ### create hier2.output object and return ###
  hier2.output <- list(call=call,
                       alpha.draws=t(alpha.stored),
                       gamma.draws=t(gamma.stored),
                       sigma2.draws=t(sigma2.stored),
                       Sigma.beta.draws=t(Sigma.beta.stored),
                       beta.draws=t(beta.stored),
                       ypost.draws=t(ypost.stored),
                       alpha.bar=alpha.bar, Sigma.alpha=Sigma.alpha,
                       gamma.bar=gamma.bar,
                       Sigma.gamma=Sigma.gamma,
                       IWobs=IWobs, IWmean=IWmean, eta=eta, R=R, 
                       IGobs=IGobs, IGmean=IGmean, nu=nu, delta=delta,
                       burnin=burnin, gibbs=gibbs, thin=thin,
                       storebeta=storebeta, alpha.names=alpha.names,
                       gamma.names=gamma.names, tot.time=tot.time,
                       y=y, cid=cid, n=n, k=k, p=p, q=q)
  class(hier2.output) <- "hier2.output"

return(hier2.output)
}








#### summary function for hier2.output objects ####
summary.hier2.output <- function(obj,...){
  alpha.postmean   <- apply(obj$alpha.draws, 2, mean)
  alpha.postmedian <- apply(obj$alpha.draws, 2, quantile, 0.50)
  alpha.poststdev  <- sqrt(apply(obj$alpha.draws, 2, var))
  alpha.lower      <- apply(obj$alpha.draws, 2, quantile, 0.025)
  alpha.upper      <- apply(obj$alpha.draws, 2, quantile, 0.975)
  alpha.output     <- cbind(alpha.postmean, alpha.postmedian, alpha.poststdev,
                            alpha.lower, alpha.upper)
  if (!is.null(obj$alpha.names))
    dimnames(alpha.output)[[1]] <- obj$alpha.names
  dimnames(alpha.output)[[2]] <- list("post. mean", "post. median", "post. SD",
                                      "2.5 %tile", "97.5 %tile")

  gamma.postmean   <- apply(obj$gamma.draws, 2, mean)
  gamma.postmedian <- apply(obj$gamma.draws, 2, quantile, 0.50)
  gamma.poststdev  <- sqrt(apply(obj$gamma.draws, 2, var))
  gamma.lower      <- apply(obj$gamma.draws, 2, quantile, 0.025)
  gamma.upper      <- apply(obj$gamma.draws, 2, quantile, 0.975)
  gamma.output     <- cbind(gamma.postmean, gamma.postmedian, gamma.poststdev,
                            gamma.lower, gamma.upper)  
  if (!is.null(obj$gamma.names))
    dimnames(gamma.output)[[1]] <- obj$gamma.names
  dimnames(gamma.output)[[2]] <- list("post. mean", "post. median", "post. SD",
                                      "2.5 %tile", "97.5 %tile")


  Sigma.beta.postmean   <- apply(obj$Sigma.beta.draws, 2, mean)
  Sigma.beta.postmedian <- apply(obj$Sigma.beta.draws, 2, quantile, 0.50)
  Sigma.beta.poststdev  <- sqrt(apply(obj$Sigma.beta.draws, 2, var))
  Sigma.beta.lower      <- apply(obj$Sigma.beta.draws, 2, quantile, 0.025)
  Sigma.beta.upper      <- apply(obj$Sigma.beta.draws, 2, quantile, 0.975)
  Sigma.beta.output     <- cbind(Sigma.beta.postmean, Sigma.beta.postmedian,
                                 Sigma.beta.poststdev,
                                 Sigma.beta.lower, Sigma.beta.upper)
  Sigma.beta.names <- "junk"
  for (i in 1:obj$p){
    for (j in 1:obj$p){
        Sigma.beta.names <- c(Sigma.beta.names, paste("Sigma.beta[",j, ",",i,
                                                      "]", sep=""))

    }
  }

  Sigma.beta.names <- Sigma.beta.names[2:(obj$p*obj$p+1)]
  dimnames(Sigma.beta.output)[[1]] <- Sigma.beta.names
  dimnames(Sigma.beta.output)[[2]] <- list("post. mean", "post. median",
                                           "post. SD",
                                           "2.5 %tile", "97.5 %tile")

  sigma2.postmean   <- apply(obj$sigma2.draws, 2, mean)
  sigma2.postmedian <- apply(obj$sigma2.draws, 2, quantile, 0.50)
  sigma2.poststdev  <- sqrt(apply(obj$sigma2.draws, 2, var))
  sigma2.lower      <- apply(obj$sigma2.draws, 2, quantile, 0.025)
  sigma2.upper      <- apply(obj$sigma2.draws, 2, quantile, 0.975)
  sigma2.output     <- cbind(sigma2.postmean, sigma2.postmedian,
                             sigma2.poststdev,
                             sigma2.lower, sigma2.upper)  
  dimnames(sigma2.output)[[2]] <- list("post. mean", "post. median",
                                       "post. SD",
                                       "2.5 %tile", "97.5 %tile")


  cat("\n\n\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
  cat("The model is:\n")
  cat("y_{ij} = w'_{ij}alpha + x'_{ij}beta_{j} + epsilon_{ij}\n\n")
  cat("epsilon_{ij} ~ N(0, sigma2)\n\n")
  cat("and the following prior distributions:\n\n")
  cat("beta_{j}   ~ N(Zstar_{j}gamma, Sigma.beta)\n")
  cat("alpha      ~ N(alpha.bar,  Sigma.alpha)\n")
  cat("gamma      ~ N(gamma.bar,  Sigma.gamma)\n")
  cat("Sigma.beta ~ InvWish(eta, R)\n")
  cat("sigma2     ~ InvGamma(nu/2, delta/2)\n\n")
  cat("\nCall: ", deparse(obj$call), "\n\n\n\n")
  cat("@@@@@@@@@@@@@@@@@@@@@  SUMMARY OF PRIOR DENSITY @@@@@@@@@@@@@@@@@@@@@\n")
  cat("alpha normally distributed with mean\n ")
  print(obj$alpha.bar)
  cat("and variance\n")
  print(obj$Sigma.alpha)
  cat("\n")
  cat("gamma normally distributed with mean\n ")
  print(obj$gamma.bar)
  cat("and variance\n")
  print(obj$Sigma.gamma)
  cat("\n")
  cat("Sigma.beta follows an inverse Wishart distribution\n")
  cat("  with ", obj$eta, " degrees of freedom and inverse scale matrix\n") 
  print(obj$R)
  cat("\nThis corresponds to adding ", obj$eta, " pseudo-observations for\n")
  cat("  which the expected value of Sigma.beta is\n")
  print(obj$IWmean)
  cat("\n")

  cat("sigma2 follows an inverse gamma distribution with\n")
  cat("  shape ", obj$nu/2, "and scale ", obj$delta/2, "\n") 
  cat("\nThis corresponds to adding ", obj$IGobs, " pseudo-observations for\n")
  cat("  which the expected value of sigma2 is ", obj$IGmean, "\n")
  cat("\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
  cat("@@@@@@@@@@@@@@@@@@@@ SUMMARY OF POSTERIOR DENSITY @@@@@@@@@@@@@@@@@@@\n")
  cat("alpha:\n")
  print.default(alpha.output, print.gap=2, digits=4)
  cat("\ngamma:\n")
  print.default(gamma.output, print.gap=2, digits=4)
  cat("\nSigma.beta:\n")
  print.default(Sigma.beta.output, print.gap=2, digits=4)
  cat("\nsigma2:\n")
  print.default(sigma2.output, print.gap=2, digits=4)
  cat("\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
  cat("\n@@@@@@@@@@@@@@@@@@@@@@@@ MCMC PARAMETERS @@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
  cat("number of burnin sweeps             = ", obj$burnin, "\n")
  cat("total number of sweeps minus burnin = ", obj$gibbs, "\n")
  cat("thinning parameter                  = ", obj$thin, "\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
  cat("Total time elapsed = ", obj$tot.time[3]/60, " minutes\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
  
  
}








# function to carry out posterior predictive checks on 
#   hier2.output objects
#
# USAGE:
#
#   result <- postpred.hier2.output(obj, id, fun, plot=TRUE,
#                                   plot.rowsize=4, plot.colsize=4, ...)
#
# INPUT:
#
#   obj  an object of class hier2.output
#
#   id   a vector giving indicating how the observations in y (and
#        ypost) should be grouped to calculate the discrepancy measure.
#        For instance, id = 1:n where n is the number of observations in
#        y (and ypost) would indicate that the discrepancy measure should
#        be calculated for each observation. Similarly, id = obj$cid would
#        indicate that the discrepancy measure should be calculated for
#        each original data cluster defined by cid.
#
#   fun  a user-defined function that computes a discrepancy measure
#        using y (and elements of ypost).
#
#   plot should plots be sent to the graphics device? (TRUE or FALSE)
#
#   plot.rowsize  number of rows in histogram plots
#
#   plot.colsize  number of columns in histogram plots
#
#
# OUTPUT:
#
#  result  a list of posterior predictive p-values. In addition,
#          histograms of the posterior predictive discrepancy measures
#          superimposed on the observed data discepancy measures is
#          sent to the current graphics device.
#
#
# Kevin Quinn
# Assistant Professor
# Dept. of Political Science and CSSS
# Box 354322
# University of Washington
# Seattle, WA  98195-4322
# quinn@stat.washington.edu
# 
# 2/18/2001 
# Last updated 2/18/2001
#
postpred.hier2.output <- function(obj, id, fun, plot=TRUE, plot.rowsize=4,
                                  plot.colsize=4, ...){
  n <- nrow(obj$ypost)   # number of posterior predictive draws
  data.discrep <- aggregate(obj$y, list(ID=id), fun, ...) #obs. data discrep.
  unit.ids <- data.discrep[,1]
  data.discrep <- data.discrep[,2]
  J <- length(data.discrep)
  data.discrep <- matrix(data.discrep, J, 1)

  cat("\nCalculating posterior predictive discrepancy measures...\n\n")
  postpred.discrep <- matrix(NA, J, n) #postpred discrepancy  
  for (i in 1:n){
    hold <-  aggregate(obj$ypost[i,], list(ID=id), fun, ...)
    hold <- hold[,2]
    postpred.discrep[,i] <- hold
  }

  if (plot){
    ### create posterior predictive histograms ###
    par(mfrow=c(plot.rowsize,plot.colsize))
    for (j in 1:J){
      hist(postpred.discrep[j,], col=5, main=paste(unit.ids[j]))
      abline(v=data.discrep[j,],lwd=3,col=2)
      if ((j%%(plot.rowsize*plot.colsize)==0) && (j!=J)){
        X11()
        par(mfrow=c(plot.rowsize,plot.colsize))
      }
    }
  }

  
  cat("\nCalculating posterior predictive p-values...\n\n")
  ### calculate posterior predictive p-values ###
  data.discrep.mat <- matrix(data.discrep, J, n)
  for (j in 1:J){
    pval <- apply((postpred.discrep>data.discrep.mat), 1, mean)
  }

  
  # calculate posterior predictive coverages
  pred.cov.05 <- mean(pval<0.05)
  pred.cov.10 <- mean(pval<0.10)
  pred.cov.25 <- mean(pval<0.25)
  pred.cov.50 <- mean(pval<0.50)
  pred.cov.75 <- mean(pval>0.75)
  pred.cov.90 <- mean(pval>0.90)
  pred.cov.95 <- mean(pval>0.95)

  pred.cov <- c(pred.cov.05, pred.cov.10, pred.cov.25, pred.cov.50,
                pred.cov.75, pred.cov.90, pred.cov.95)
  pred.cov <- t(matrix(pred.cov))

  dimnames(pred.cov)[[2]] <- list(" fraction p<0.05 ", " fraction p<0.10 ",
                               " fraction p<0.25 ", " fraction p<0.50 ",
                               " fraction p>0.75 ", " fraction p>0.90 ",
                               " fraction p>0.95 ")
  dimnames(pred.cov)[[1]] <- list(" ")
  cat("\nNominal vs. Empirical Coverage Probabilities\n")
  print(t(pred.cov), print.gap=2, digits=4)
  cat("\n")
  

  # plot empirical coverage probabilities on nominal coverage probabilities
  points <- ppoints(pval)
  n <- length(points)
  p.vec <- matrix(NA,n,1)
  for (i in 1:n){
    p.vec[i] <- mean(pval<points[i])
  }
  par(mfrow=c(1,1), pty="s")
  X11()
  plot(points, p.vec, xlab="nominal coverage probabilities",
       ylab="empirical coverage probabilities")
  abline(0,1)
  

  return(pval) 
}







#
# rwish delivers a pseudo-random Wishart deviate
#
# USAGE:
#
#   A <- rwish(v, S)
#
# INPUT:
#
#   v    degrees of freedom
#
#   S    Scale matrix
#
# OUTPUT:
#
#  A     a pseudo-random Wishart deviate
#
# Based on code originally posted by Bill Venables to S-news
# 6/11/1998
#
# Kevin Quinn
# Assistant Professor
# Dept. of Political Science and CSSS
# Box 354322
# University of Washington
# Seattle, WA  98195-4322
# quinn@stat.washington.edu
# 
# 2/5/2001 
# Last updated 2/5/2001
#
rwish <- function(v, S) {
  if (!is.matrix(S))
    S <- matrix(S)
  if (nrow(S) != ncol(S)){
    stop(message="ERROR: S not square in rwish()\n\n")
  }
  if (v < nrow(S)){
    stop(message="ERROR: v is less than the dimension of S in rwish()\n\n")
  }
  p <- nrow(S)
  CC <- chol(S) 
  Z <- matrix(0, p, p)
  diag(Z) <- sqrt(rchisq(p, v:(v-p+1)))
  if(p > 1) {
    pseq <- 1:(p-1)
    Z[rep(p*pseq, pseq) + unlist(lapply(pseq, seq))] <- rnorm(p*(p-1)/2)
  }
  crossprod(Z %*% CC)
}




























