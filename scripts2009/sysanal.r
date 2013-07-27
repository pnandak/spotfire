# ======================================= #
# Systems Analysis Library of R Functions #
# ======================================= #



# created and maintained by 
# Peter Reichert
# EAWAG
# Duebendorf
# Switzerland
# reichert@eawag.ch



# Overview of functions
# =====================

# ode:            numerical integration of ordinary differential equations

# sde:            numerical integration of stochastic differential equations

# calc.pdf:       calculation of probability density function of a 
#                 multivariate normal or lognormal distribution

# randsamp:       random sampling from a multivariate normal or lognormal 
#                 distribution

# collind:        calculation of collinearity index

# comb:           calculation of index combinations

# ident:          calculation of identifiability measures from sensitivity 
#                 functions

# hist.weighted:  histogram from weighted sample

# pairs.nonpar:   scatterplot with linear and nonparametric regression



# libraries
# =========

library(stats)



# numerical integration of ordinary differential equations
# ========================================================

ode <- function(rhs,x.ini,par,t.out,dt.max=0,algorithm="euler")
{
   # -----------------------------------------------------------------------
   # This solver for a system of ordindary differential equations
   # was written for didactical purposes. It does not represent 
   # the state of the art in numerical integration techniques
   # but should rather demonstrate how the simplest integration
   # technique can be implemented for relatively simple use.
   # Check the package "odesolve" available from http://www.r-project.org
   # for professional solvers for ordinary differential equations
   #
   # Arguments:
   # rhs:       function returning the right hand side of the 
   #            system of differential equations as a function
   #            of the arguments x (state variables), t (time),
   #            and par (model parameters).
   # x.ini:     start values of the state variables.
   # par:       model parameters (transferred to rhs).
   # t.out:     set of points in time at which the solution 
   #            should be calculated; the solution at the first
   #            value in t is set to x.ini, the solution at subsequent
   #            values is calculated by numerical integration.
   # dt.max:    maximum time step; if the difference between points
   #            in time at which output has to be provided, t, is
   #            larger than dt.max, then this output interval is 
   #            divided into step of at most dt.max internally to
   #            improve the accuracy of the solution at the next 
   #            output point.
   # algorithm: right now, the only option is "euler"; it implements
   #            the simple explicit Euler algorithm of first order.
   #
   # Return Value:
   # matrix with results for state variables (columns) at all output
   # time steps t.out (rows).
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------
 
   # determine number of equations and number of time steps:
   num.eq <- length(x.ini)
   steps <- length(t.out)

   # define and initialize result matrix:
   x <- matrix(nrow=steps,ncol=num.eq)
   colnames(x) <- names(x.ini)
   rownames(x) <- t.out
   x[1,] <- x.ini

   # perform integration:
   if ( algorithm == "euler" )
   {
      for ( i in 2:steps )
      {
         if ( dt.max <= 0 || dt.max >= t.out[i]-t.out[i-1] )
         {
            # output interval <= dt.max (or dt.max zero):
            # perform a single step to the next output time:
            x[i,] <- x[i-1,] + (t.out[i]-t.out[i-1])*rhs(x[i-1,],t.out[i-1],par)
         }
         else
         {
            # output interval > dt.max:
            # perform multiple steps of maximum size dt.max:
            x[i,] <- x[i-1,]
            steps.internal <- ceiling((t.out[i]-t.out[i-1])/dt.max)
            dt <- (t.out[i]-t.out[i-1])/steps.internal
            for ( j in 1:steps.internal )
            {
               t.current <- t.out[i-1] + (j-1)*dt
               x[i,] <- x[i,] + dt*rhs(x[i,],t.current,par)
            }
         }
      }
   }
   else
   {
      stop(paste("ode: algorithm \"",algorithm,"\" not implemented",sep=""))
   }

   # return result matrix: 
   return(x)
}



# numerical integration of stochastic differential equations
# ==========================================================

sde <- function(rhs.drift,rhs.diffusion,x.ini,par,t.out,dt.max=0,
                algorithm="euler")
{
   # -----------------------------------------------------------------------
   # This solver for a system of stochastic ordindary differential 
   # equations was written for didactical purposes. It does not 
   # represent the state of the art in numerical integration techniques
   # but should rather demonstrate how the simplest integration
   # technique for stochastic differential equations can be implemented 
   # for relatively simple use. The solver calculates a single realisation
   # of the solution. Repeated calls are necessary in order to approximate
   # probability distributions of results by histograms.
   #
   # Arguments:
   # rhs.drift:     function returning the drift term of the system
   #                of stochastic differential equations as a function
   #                of the arguments x (state variables), t (time),
   #                and par (model parameters).
   # rhs.diffusion: function returning the diffusion term of the system
   #                of stochastic differential equations as a function
   #                of the arguments x (state variables), t (time),
   #                and par (model parameters).
   # x.ini:         start values of the state variables.
   # par:           model parameters (transferred to rhs).
   # t:             set of points in time at which the solution 
   #                should be calculated; the solution at the first
   #                value in t is set to x.ini, the solution at subsequent
   #                values is calculated by numerical integration.
   # dt.max:        maximum time step; if the difference between points
   #                in time at which output has to be provided, t, is
   #                larger than dt.max, then this output interval is 
   #                divided into step of at most dt.max internally to
   #                improve the accuracy of the solution at the next 
   #                output point.
   # algorithm:     right now, the only option is "euler"; it implements
   #                the simple explicit stochastic Euler algorithm of first 
   #                order.
   #
   # Return Value:
   # matrix with results for state variables (columns) at all output
   # time steps t.out (rows).
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------

   # determine number of equations and number of time steps:
   num.eq <- length(x0)
   steps <- length(t.out)

   # define and initialize result matrix:
   x <- matrix(nrow=steps,ncol=num.eq)
   colnames(x) <- names(x.ini)
   rownames(x) <- t.out
   x[1,] <- x.ini

   # perform integration:
   if ( algorithm == "euler" )
   {
      for ( i in 2:steps )
      {
         if ( dt.max <= 0 || dt.max >= t.out[i]-t.out[i-1] )
         {
            # output interval <= dt.max (or dt.max zero):
            # perform a single step to the next output time:
            x[i,] <- x[i-1,] + 
                     (t.out[i]-t.out[i-1])*rhs.drift(x[i-1,],t.out[i-1],par) +
                     sqrt(t.out[i]-t.out[i-1])*rhs.diffusion(x[i-1,],par)*rnorm(num.eq)
         }
         else
         {
            # output interval > dt.max:
            # perform multiple steps of maximum size dt.max:
            x[i,] <- x[i-1,]
            steps.internal <- ceiling((t.out[i]-t.out[i-1])/dt.max)
            dt <- (t.out[i]-t.out[i-1])/steps.internal
            for ( j in 1:steps.internal )
            {
               t.current <- t.out[i-1] + (j-1)*dt
               x[i,] <- x[i,] + 
                        dt*rhs.drift(x[i,],t.current,par) +
                        sqrt(dt)*rhs.diffusion(x[i,],t.current,par)*rnorm(num.eq)
            }
         }
      }
   }
   else
   {
      stop(paste("sde: algorithm \"",algorithm,"\" not implemented",sep=""))
   }

   # return result matrix:
   return(x)
}



# calculation of probability density function of a multivariate normal or lognormal distribution
# ==============================================================================================

calc.pdf <- function(x,mean=0,sd=1,corr=0,dist="lognormal")
{
   # -----------------------------------------------------------------------
   # This function calculates the value of the probability density function
   # of a multivariate normal or lognormal distribution.
   #
   # Arguments:
   # x:          point at which the value of the probability density
   #             function has to be evaluated
   # mean:       vector of means
   # sd:         vector of standard deviations
   # corr:       correlation matrix of the distribution
   # dist:       distribution type: "normal" or lognormal".
   #
   # Return Value:
   # value of the probability density function at x
   #
   #                                         Peter Reichert    Jan. 01, 2003
   #                                         last modification Jan. 03, 2003
   # -----------------------------------------------------------------------

   # consistency checks and initializations:
   num.par <- length(x)
   if ( length(mean) != num.par ) stop("calc.pdf: illegal dimension of means")
   if ( length(sd) != num.par )   stop("calc.pdf: illegal dimension of standard deviations")
   R <- diag(rep(1,num.par))
   if ( corr != 0 ) R <- corr
   if ( nrow(R) != num.par ) stop("calc.pdf: illegal dimension of correlation matrix")
   if ( ncol(R) != num.par ) stop("calc.pdf: illegal dimension of correlation matrix")


   if ( dist == "normal" )
   {
      # multivariate normal distribution:
      sigma <- diag(sd) %*% R %*% diag(sd)
      v <- x-mean
      v <- matrix(v,nrow=num.par,ncol=1) # ensure that v is a column vector
      pdf   <- 1/(2*pi)^(num.par/2) * 1/sqrt(det(sigma)) *
               exp( -0.5 * t(v) %*% solve(sigma) %*% (v) )
   }
   else
   {
      if ( dist == "lognormal" )
      {
         # multivariate lognormal distribution:
         if ( min(x) <= 0 )
         {
            pdf <- 0 
         }
         else
         {
            sdlog    <- sqrt(log(1+sd*sd/(mean*mean)))
            meanlog  <- log(mean) - sdlog*sdlog/2
            ln.sigma <- log( 1 + diag(sqrt(exp(sdlog*sdlog)-1)) %*% R %*% diag(sqrt(exp(sdlog*sdlog)-1)) )
            v <- log(x)-meanlog
            v <- matrix(v,nrow=num.par,ncol=1) # ensure that v is a column vector
            pdf <- 1/(2*pi)^(num.par/2) / sqrt(det(ln.sigma)) / prod(x) *
                   exp( -0.5 * t(v) %*% solve(ln.sigma) %*% v )
         }
      }
      else
      {
         stop("calc.pdf: unknown distribution type")
      }
   }

   # return result:
   return(pdf)
}



# random sampling from a multivariate normal or lognormal distribution
# ====================================================================

randsamp <- function(samp.size=100,mean=0,sd=1,corr=0,dist="lognormal")
{
   # -----------------------------------------------------------------------
   # This function calculates a random sample from a multivariate
   # normal or lognormal distribution.
   # This function is a simplified version of the program "randsamp"
   # available as part of the package UNCSIM at http://www.uncsim.eawag.ch
   #
   # Arguments:
   # samp.size:  sample size
   # mean:       vector of means
   # sd:         vector of standard deviations
   # corr:       correlation matrix of the distribution
   # dist:       distribution type: "normal" or lognormal".
   #
   # Return Value:
   # List of:
   # mean:       vector of means
   # sd:         vector of standard deviations
   # corr:       correlation matrix of the distribution
   # samp.size:  sample size
   # sample:     matrix of parameter samples (each row corresponds 
   #             to a draw)
   # pdf:        vector of values of probability density function at the
   #             sample points
   #
   #                                         Peter Reichert    Dec. 29, 2002
   #                                         last modification Jan. 01, 2003
   # -----------------------------------------------------------------------

   # consistency checks and initializations:
   num.par <- length(mean)
   if ( length(sd) != num.par ) stop("randsamp: illegal dimension of standard deviations")
   R <- diag(rep(1,num.par))
   if ( corr != 0 ) R <- corr
   if ( nrow(R) != num.par ) stop("randsamp: illegal dimension of correlation matrix")
   if ( ncol(R) != num.par ) stop("randsamp: illegal dimension of correlation matrix")

   # calculate sample from multivariate uniform distribution:
   samp <- runif(samp.size*num.par)
   dim(samp) <- c(samp.size,num.par)
   dens <- numeric(samp.size)

   # transform sample to multivariate normal or lognormal:
   if ( dist == "normal" )
   {
      # calculate transformation matrix and transform the sample:
      sigma <- diag(sd) %*% R %*% diag(sd)
      A <- t(chol(sigma))
      for ( i in 1:samp.size )
      {
         samp[i,] <- A %*% qnorm(samp[i,]) + mean
         dens[i]  <- calc.pdf(samp[i,],mean,sd,R,dist)
      }
   }
   else
   {
      if ( dist == "lognormal" )
      {
         # parameters of the log of the variable, calculate transformation matrix 
         # and transform the sample:
         sdlog    <- sqrt(log(1+sd*sd/(mean*mean)))
         meanlog  <- log(mean) - sdlog*sdlog/2
         ln.sigma <- log( 1 + diag(sqrt(exp(sdlog*sdlog)-1)) %*% R %*% diag(sqrt(exp(sdlog*sdlog)-1)) )
         ln.A <- t(chol(ln.sigma))
         for ( i in 1:samp.size )
         {
            samp[i,] <- exp(ln.A %*% qnorm(samp[i,]) + meanlog)
            dens[i]  <- calc.pdf(samp[i,],mean,sd,R,dist)
         }
      }
      else
      {
         stop("randsamp: unknown distribution type")
      }
   }

   # collect results:
   colnames(samp) <- names(mean)
   res <- list(
               mean      = mean,
               sd        = sd,
               corr      = R,
               samp.size = samp.size,
               sample    = samp,
               pdf       = dens
              )

   # return results:
   return(res)
}



# calculation of collinearity index (auxiliary function used in ident)
# ====================================================================

collind <- function(sen.scaled)
{
   # -----------------------------------------------------------------------
   # This function calculates the collinearity index from a scaled 
   # sensitivity matrix.
   #
   # Arguments:
   # sen:       matrix of model sensitivities (scaled partial derivatives
   #            of model outcomes with respect to model parameters:
   #            delta.par/scale dy/dpar); the columns of sen refer to
   #            different model parameters, the rows to different model
   #            outcomes.
   #
   # Return Value:
   # collinearity index (real number).
   #
   #                                         Peter Reichert    Dec. 27, 2002
   # -----------------------------------------------------------------------

   # normalize sensitivity functions:
   num.par <- ncol(sen.scaled)
   norms <- numeric(num.par)
   for ( i in 1:num.par )
   {
      norms[i] <- sqrt( sen.scaled[,i] %*% sen.scaled[,i] )
   }
   sen.norm <- sen.scaled %*% diag( 1/norms )

   # calculate collinearity index:
   collind <- 1/sqrt(min(eigen( t(sen.norm) %*% sen.norm )$values))

   # return result:
   return(collind)
}



# calculation of index combinations (auxiliary function used in ident)
# ====================================================================

comb <- function(n,p)
{
   # -----------------------------------------------------------------------
   # This function calculates all combination of subsets of length p out
   # of n indices.
   #
   # Arguments:
   # n:   number of indices.
   # p:   length of subset of indices.
   #
   # Return Value:
   # matrix with subsets of length p as rows.
   #
   #                                         Peter Reichert    Dec. 27, 2002
   # -----------------------------------------------------------------------

   # check input:
   if ( p > n ) stop("comb: illeal arguments (p>n)")

   # initialize array and auxiliary variables:
   num.comb <- choose(n,p)
   comb <- matrix(nrow=num.comb,ncol=p)
   ind <- 1:p
   pointer <- p

   # calculate index combinations:
   for ( i in 1:num.comb )
   {
      comb[i,] <- ind
      ind[pointer] <- ind[pointer]+1
      if ( ind[pointer] > n )
      {
         while ( pointer > 1 )
         {
            pointer <- pointer-1
            if ( ind[pointer] < n-(p-pointer) )
            {
               ind[pointer] <- ind[pointer]+1
               for ( j in (pointer+1):p )
               {
                  ind[j] <- ind[pointer]+j-pointer
               }
               pointer <- p
               break
            }
         }
      }
   }

   # return results:
   return(comb)
}



# calculation of identifiability measures
# =======================================

ident <- function(sen,delta.par=0,scale=0,max.subset.size=0)
{
   # -----------------------------------------------------------------------
   # This function calculates a parameter sensitivity ranking and 
   # collinearity indices for a series of parameter combinations
   # based on liniear sensitivity functions of a model, parameter
   # uncertainty ranges and scale factors of model results.
   # This function is a simplified version of the program "ident"
   # available at http://www.ident.eawag.ch
   #
   # Arguments:
   # sen:       matrix of model sensitivities (partial derivatives
   #            of model outcomes with respect to model parameters:
   #            dy/dpar); the columns of sen refer to different 
   #            model parameters, the rows to different model outcomes.
   # delta.par: model parameter uncertainty ranges (length equal to 
   #            the number of columns of sen); if zero, then all ranges
   #            are assumed to be unity.
   # scale:     scaling factors of model results (if zero, then all 
   #            scaling factors are assumed to be unity).
   #
   # Return Value:
   # List of delta.msqr, collind.
   #
   #                                         Peter Reichert    Dec. 27, 2002
   # -----------------------------------------------------------------------

   # determine number of model parameters:
   num.out <- nrow(sen)
   num.par <- ncol(sen)
   names.par <- colnames(sen)
   if ( length(names.par) != num.par ) names.par <- paste("par",1:num.par,sep="")
   if ( max.subset.size == 0 ) max.subset.size <- min(num.par,4)

   # apply parameter uncertainty ranges and scale factors if available:
   sen.scaled <- sen
   if ( length(delta.par) == num.par ) sen.scaled <- sen.scaled %*% diag(delta.par)
   if ( length(scale)     == num.out ) sen.scaled <- diag(1/scale) %*% sen.scaled

   # calculate sensitivity ranking:
   delta.msqr <- numeric(num.par)
   names(delta.msqr) <- names.par
   for ( i in 1:num.par )
   {
      delta.msqr[i] <- sqrt( t(sen.scaled[,i]) %*% sen.scaled[,i] ) / sqrt(num.out)
   }
   res <- list(delta.msqr=delta.msqr)

   if ( max.subset.size > 1 )
   {
      for ( i in 2:min(max.subset.size,num.par) )
      {
         ind <- comb(num.par,i)
         collind <- numeric(nrow(ind))
         par.set <- matrix(nrow=nrow(ind),ncol=i)
         for ( j in 1:nrow(ind) )
         {
            collind[j] <- collind(sen.scaled[,ind[j,]])
            for ( k in 1:i )
            {
               par.set[j,k] <- names.par[ind[j,k]]
            }
         }
         res[[paste("collind.",i,sep="")]] = cbind(par.set,collind)
      }
   }

   # return results:
   return(res)
}



# histogram from weighted sample:
# ===============================

hist.weighted <- function(xdata,xmin=0,xmax=0,nbin=10,weights=0)
{
   # -----------------------------------------------------------------------
   # This function calculates a curve describing a histogram for a weighted
   # sample.
   #
   # Arguments:
   # xdata:     array o data.
   # xmin:      lower bound to the range covered by the histogram.
   # xmax:      upper bound to the range covered by the histogram.
   # nbin:      number of histogram bins.
   # weights:   array of weights of the data points stored in xdata.
   #
   # Return Value:
   # List of:
   #   hist:       data frame of midpoints, counts and heights
   #   integral:   value of the integral
   #   curve:      data frame of x and y values of the histogram step line
   #   curve.mid:  data frame of x and y values of the histogram midpoint 
   #               line
   #
   #                                        Peter Reichert    March 27, 2002
   # -----------------------------------------------------------------------

   # number of data points, bin width and bin midpoints:
   if ( xmin >= xmax )
   { 
      xmin <- min(xdata)
      xmax <- max(xdata)
   }
   ndat <- length(xdata)
   dx   <- (xmax-xmin)/nbin
   xmid <- seq(xmin+0.5*dx,xmax-0.5*dx,length=nbin)

   # use equal weights if provided weights vector not length of data:
   w <- weights
   if ( length(w) != ndat ) { w <- rep(1/ndat,ndat) }

   # calculate weighted counts, bin heights, and integral:
   counts <- rep(0,nbin)
   for ( i in 1:ndat )
   {
      index <- floor( (xdata[i]-xmin)/(xmax-xmin)*nbin ) + 1
      if ( index >= 1 && index <= nbin )
      {
         counts[index] <- counts[index] + w[i]
      }
      else
      {
         if ( xdata[i] == xmax ) counts[nbin] <- counts[nbin] + w[i]
      }
   }
   counts   <- ndat*counts/sum(w)
   heights  <- counts/(ndat*dx)
   integral <- sum(counts)/ndat

   # construct histogram line:
   x.step           <- rep(0,2*nbin+2)
   y.step           <- rep(0,2*nbin+2)
   x.step[1]        <- xmin
   x.step[2*nbin+2] <- xmax
   for ( i in 1:nbin )
   {
      x.step[2*i]   <- xmin + (i-1)*dx
      x.step[2*i+1] <- xmin + i*dx
      y.step[2*i]   <- heights[i]
      y.step[2*i+1] <- heights[i]
   }

   # construct midpoint line:
   x.mid        <- seq(xmin-0.5*dx,xmax+0.5*dx,length=nbin+2)
   y.mid        <- rep(0,nbin+2)
   for ( i in 1:nbin )
   {
      y.mid[i+1] <- heights[i]
   }

   # return results
   res <- list(hist=data.frame(xmid=xmid,counts=counts,heights=heights),
               integral=integral,
               curve=data.frame(x=x.step,y=y.step),
               curve.mid=data.frame(x=x.mid,y=y.mid))
   return(res)
}



# scatterplot with linear and nonparametric regression
# ====================================================

pairs.nonpar <- function(data,span=0.75,...)
{
   # -----------------------------------------------------------------------
   # This function produces a scatterplot of the data with linear and
   # nonparametric regression lines.
   #
   # Arguments:
   # data:     matrix containing the data sets in the rows.
   # span:     smoothing parameter from loess.
   #
   # Return Value:
   #
   #                                        Peter Reichert    March 27, 2003
   # -----------------------------------------------------------------------

   library(modreg)
   panel.loess <- function(x,y,...)
   {
      x.tmp <- numeric(0)
      y.tmp <- numeric(0)
      i <- 1
      for ( j in 1:length(x) )
      {
         if ( !is.na(x[j]) & !is.na(y[j]) )
         {
            x.tmp[i] <- x[j]
            y.tmp[i] <- y[j]
            i <- i+1
         }
      }
      ind <- order(x.tmp)
      x.tmp <- x.tmp[ind]
      y.tmp <- y.tmp[ind]
      points(x.tmp,y.tmp)
      res.lm    <- lm   (formula=y.tmp~x.tmp)
      res.loess <- loess(formula=y.tmp~x.tmp,span=span)
      lines(x.tmp,predict(res.lm)   ,lty="solid",lwd=1)
      lines(x.tmp,predict(res.loess),lty="solid",lwd=2)
   }
   panel.hist <- function(x, ...)
   {
       usr <- par("usr"); on.exit(par(usr))
       par(usr = c(usr[1:2], 0, 1.5) )
       h <- hist(x, plot = FALSE)
       breaks <- h$breaks; nB <- length(breaks)
       y <- h$counts; y <- y/max(y)
       rect(breaks[-nB], 0, breaks[-1], y, ...)
   }
   return(pairs(data,diag.panel=panel.hist,panel=panel.loess,...))
}


# complete stepwise linear regression analysis
# ============================================

stepwise <- function(data,targetname=0,varnames=0)
{
   # -----------------------------------------------------------------------
   # This function produces results of a complete stepwise linear regression
   # analysis.
   #
   # Arguments:
   # data:        data frame of data.
   # targetname:  name of target variable (default: first variable).
   # varnames:    names of potential influence factors 
   # (default: variables 2:n)
   #
   # Return Value:
   # Matrix of model, sum of squares, r2
   #
   #                                        Peter Reichert      May 23, 2003
   # -----------------------------------------------------------------------

   # identify goal variable and influence factors:
   if ( targetname == 0 ) targetname <- names(data)[1]
   if ( varnames == 0 ) varnames <- names(data)[2:length(names(data))]

   # calculate number of influence factors:
   num.vars <- length(varnames)

   # calculate number of combinations of influence factors:
   num.comb <- 0
   for ( i in 1:num.vars ) num.comb <- num.comb + choose(num.vars,i)
   
   # calculate results:
   res <- matrix(nrow=num.comb,ncol=3)
   colnames(res) <- c("model","ss","r2")
   res <- data.frame(res)
   l <- 0
   for ( i in 1:num.vars )
   {
      inds <- comb(num.vars,i)
      for ( j in 1:nrow(inds) )
      {
         model <- paste(targetname,"~",varnames[inds[j,1]])
         if ( i > 1 )
         {
            for ( k in 2:i )
            {
               model <- paste(model,"+",varnames[inds[j,k]])
            }
         }
         #model <- paste(model,"-1")
         res.lm <- lm(as.formula(model),data=data)
         l <- l+1
         res[l,1] <- model
         res[l,2] <- sum(res.lm$residuals^2)
         res[l,3] <- 1-var(res.lm$residuals)/var(data[,targetname])
      }
   }
   
   # return results:
   return (res)
}




