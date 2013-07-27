# ========================================================== #
# R files for systems analysis of a simple degradation model #
# ========================================================== #



# created and maintained by 
# Peter Reichert
# EAWAG
# Duebendorf
# Switzerland
# reichert@eawag.ch



# This file contains didactical implementations of functions for 
# simulation and systems analysis of a model defined by the 
# differential equations
#
#   dX.H            S.S
#   ----  =  mu.H ------- X.H  -  b.H X.H
#    dt           K.S+S.S
#
#   dS.S      mu.H    S.S
#   ----  = - ----  ------- X.H
#    dt       Y.H   K.S+S.S
#
# These equations describe growth and decay of heterotrophic bacteria 
# (X.H) on a single substrate (S.S) in a batch reactor.
# The state variables and kinetic model parameters are addressed by name 
# instead of index in order to facilitate readability of the code.
# Parameters are mu.H, K.S, b.H, Y.H, X.H.ini, S.S.ini



# Overview of functions:
# ======================

# simpdeg.defarg:      specification of default arguments to simpdeg 
#                      functions:
#                      parameter values or means, 
#                      parameter standard deviations,
#                      output intervals,
#                      maximum internal integration time step

# simpdeg.rhs:         specification of the right hand side of the
#                      differential equations

# simpdeg.sim:         execution of a simulation for the model 
#                      (with optional plot of the results)

# simpdeg.montecarlo:  execution of a Monte Carlo simulation series 
#                      with a multivariate lognormal parameter 
#                      distribution

# simpdeg.gendat:      generation of synthetic data by adding samples
#                      from independent normal distributions to the
#                      results of the deterministic model

# simpdeg.fit:         perform a least-sqares parameter estimation
#                      of model parameters using measured data

# simpdeg.sensloc:     calculation of local sensitivity functions
#                      of model results with respect to model parameters

# simpdeg.sensreg:     calculation of regional sensitivity functions
#                      of model results with respect to model parameters

# simpdeg.ident:       calculation of sensitivity and collinearity 
#                      measures to assess model parameter identifiability

# simpdeg.markovchain: calculation of a Markov Chain to approximate the
#                      posterior distributions of model parameters



# load general functions from the systems analysis R library
# ==========================================================

source("sysanal.r")



# initialization of default arguments to simpdeg functions
# ========================================================

simpdeg.def.par.mean        <- c(  4   ,  5  , 0.4  , 0.67,   100   ,   50    )
simpdeg.def.par.sd          <- c(  0.8 ,  1  , 0.08 , 0.13,    20   ,   10    )
names(simpdeg.def.par.mean) <- c("mu.H","K.S", "b.H","Y.H","X.H.ini","S.S.ini")
names(simpdeg.def.par.sd)   <- names(simpdeg.def.par.mean)
simpdeg.def.X.H.sd          <- 2
simpdeg.def.S.S.sd          <- 1
simpdeg.def.t.out           <- seq(0,0.2,length=101)
simpdeg.def.t.out.dat       <- seq(0,0.2,length=21)
simpdeg.def.dt.max          <- (max(simpdeg.def.t.out)-min(simpdeg.def.t.out))/100
simpdeg.def.samp.size       <- 100



# specification of default arguments to simpdeg functions
# =======================================================

simpdeg.defarg <- function(
                           mu.H       = simpdeg.def.par.mean["mu.H"],
                           K.S        = simpdeg.def.par.mean["K.S"],
                           b.H        = simpdeg.def.par.mean["b.H"],
                           Y.H        = simpdeg.def.par.mean["Y.H"],
                           X.H.ini    = simpdeg.def.par.mean["X.H.ini"],
                           S.S.ini    = simpdeg.def.par.mean["S.S.ini"],
                           mu.H.sd    = simpdeg.def.par.sd["mu.H"],
                           K.S.sd     = simpdeg.def.par.sd["K.S"],
                           b.H.sd     = simpdeg.def.par.sd["b.H"],
                           Y.H.sd     = simpdeg.def.par.sd["Y.H"],
                           X.H.ini.sd = simpdeg.def.par.sd["X.H.ini"],
                           S.S.ini.sd = simpdeg.def.par.sd["S.S.ini"],
                           X.H.sd     = simpdeg.def.X.H.sd,
                           S.S.sd     = simpdeg.def.S.S.sd,
                           t.out      = simpdeg.def.t.out,
                           t.out.dat  = simpdeg.def.t.out.dat,
                           dt.max     = simpdeg.def.dt.max,
                           samp.size  = simpdeg.def.samp.size
                          )
{
   # -----------------------------------------------------------------------
   # Specification of default parameter values or means, parameter standard 
   # deviations, output intervals, and maximum internal integration time 
   # step.
   #
   # Arguments:
   # mu.H, K.S, b.H, Y.H, X.H.ini, S.S.ini:
   #               parameter values or means (optional)
   # mu.H.sd, K.S.sd, b.H.sd, Y.H.sd, X.H.ini.sd, S.S.ini.sd:
   #               parameter standard deviations (optional)
   # X.H.sd:       standard deviation of X.H measurements
   # S.S.sd:       standard deviation of s.S measurements
   # t.out:        array of output times
   # t.out.dat:    array of output times related to data points
   # dt.max:       maximum internal integration time step
   # samp.size:    sample size
   #
   # Return Value:
   # List of:
   # par:          new default parameter values
   # par.sd:       new default parmeter standard deviations
   # X.H.sd:       new default standard deviation of X.H measurements
   # S.S.sd:       new default standard deviation of s.S measurements
   # t.out:        new default output times
   # t.out.dat:    new default output times related to data points
   # dt.max:       new default maximum internal integration time step
   # samp.size:    new default sample size
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------

   # set default values:
   def.par.mean <- c(mu.H   ,K.S   ,b.H   ,Y.H   ,X.H.ini   ,S.S.ini   )
   def.par.sd   <- c(mu.H.sd,K.S.sd,b.H.sd,Y.H.sd,X.H.ini.sd,S.S.ini.sd)
   names(def.par.mean) <- names(simpdeg.def.par.mean)
   names(def.par.sd)   <- names(def.par.mean)
   def.X.H.sd          <- X.H.sd
   def.S.S.sd          <- S.S.sd
   def.t.out           <- t.out
   def.t.out.dat       <- t.out.dat
   def.dt.max          <- dt.max
   def.samp.size       <- samp.size

   # make new default default values globally available:
   assign("simpdeg.def.par.mean" ,def.par.mean ,envir=.GlobalEnv)
   assign("simpdeg.def.par.sd"   ,def.par.sd   ,envir=.GlobalEnv)
   assign("simpdeg.def.X.H.sd"   ,def.X.H.sd   ,envir=.GlobalEnv)
   assign("simpdeg.def.S.S.sd"   ,def.S.S.sd   ,envir=.GlobalEnv)
   assign("simpdeg.def.t.out"    ,def.t.out    ,envir=.GlobalEnv)
   assign("simpdeg.def.t.out.dat",def.t.out.dat,envir=.GlobalEnv)
   assign("simpdeg.def.dt.max"   ,def.dt.max   ,envir=.GlobalEnv)
   assign("simpdeg.def.samp.size",def.samp.size,envir=.GlobalEnv)

   # collect default values:
   res <- list(
               par       = simpdeg.def.par.mean,
               par.sd    = simpdeg.def.par.sd,
               X.H.sd    = simpdeg.def.X.H.sd,
               S.S.sd    = simpdeg.def.S.S.sd,
               t.out     = simpdeg.def.t.out,
               t.out.dat = simpdeg.def.t.out.dat,
               dt.max    = simpdeg.def.dt.max,
               samp.size = simpdeg.def.samp.size
              )

   # return default values:
   return (res)
}



# specification of the right hand side of the differential equations
# ==================================================================

simpdeg.rhs <- function(x,t,par.kin)
{
   # -----------------------------------------------------------------------
   # This function implements the right-hand side of the differential
   # equations of the model.
   #
   # Arguments:
   # x:        current value of state variables (named "X.H", "S.S")
   # t:        current time
   # par.kin:  value of kinetic model parameters (named "mu.H", "K.S", 
   #             "b.H", "Y.H")
   #
   # Return value:
   # Vector of values of temporal derivatives of state variables as required
   # by the integrator ode from sysanal.r.
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------

   # check number of equations and availability of components and parameters:
   # components: X.H, S.S
   # parameters: mu.H, K.S, b.H, Y.H
   num.eq <- length(x)
   if ( num.eq != 2 )
   {
      stop(paste("rhs.simpdeg: wrong number of equations:",num.eq,"instead of 2"))
   }
   if ( is.na(match("X.H",names(x))) )
   {
      stop("rhs.simpdeg: component \"X.H\" not available")
   }
   if ( is.na(match("S.S",names(x))) )
   {
      stop("rhs.simpdeg: component \"S.S\" not available")
   }
   if ( is.na(match("mu.H",names(par.kin))) )
   {
      stop("rhs.simpdeg: parameter \"mu.H\" not available")
   }
   if ( is.na(match("K.S",names(par.kin))) )
   {
      stop("rhs.simpdeg: parameter \"K.S\" not available")
   }
   if ( is.na(match("b.H",names(par.kin))) )
   {
      stop("rhs.simpdeg: parameter \"b.H\" not available")
   }
   if ( is.na(match("Y.H",names(par.kin))) )
   {
      stop("rhs.simpdeg: parameter \"Y.H\" not available")
   }

   # calculate right hand side:
   rhs <- numeric(num.eq)
   names(rhs) <- names(x)
   rhs["X.H"] <- par.kin["mu.H"]*x["S.S"]/(par.kin["K.S"]+x["S.S"])*x["X.H"] - par.kin["b.H"]*x["X.H"]
   rhs["S.S"] <- -par.kin["mu.H"]/par.kin["Y.H"]*x["S.S"]/(par.kin["K.S"]+x["S.S"])*x["X.H"]

   # return result:
   return(rhs)
}



# simulation of simple degradation model
# ======================================

simpdeg.sim <- function(
                        mu.H    = simpdeg.def.par.mean["mu.H"],
                        K.S     = simpdeg.def.par.mean["K.S"],
                        b.H     = simpdeg.def.par.mean["b.H"],
                        Y.H     = simpdeg.def.par.mean["Y.H"],
                        X.H.ini = simpdeg.def.par.mean["X.H.ini"],
                        S.S.ini = simpdeg.def.par.mean["S.S.ini"],
                        par     = 0,
                        t.out   = simpdeg.def.t.out,
                        dt.max  = simpdeg.def.dt.max,
                        plot    = TRUE
                       )
{
   # -----------------------------------------------------------------------
   # Perform a single simulation of the model.  
   #
   # Arguments:
   # mu.H, K.S, b.H, Y.H, X.H.ini, S.S.ini:
   #            parameter values used to overwrite the default values 
   #            (optional)
   # par:     array of parameter values (named "mu.H", "K.S", "b.H", 
   #            "Y.H", "X.H.ini", "S.S.ini") used to overwrite defaults 
   #            and explicit arguments (optional)
   # t.out:   array of output times used to overwrite defaults (optional)
   # dt.max:  maximum internal integration time step use to overwrite
   #            defaults (optional)
   # plot:    logical argument to specify if the simulation results
   #            should be plotted (optional; default TRUE)
   #
   # Return Value:
   # List of:
   # par:     parameter values used for the simulation
   # t:       output times
   # x:       matrix of calculated values of state variables (columns) at
   #            all output times (rows)
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------

   # initialize parameter values:
   act.par        <- c( mu.H , K.S , b.H , Y.H , X.H.ini , S.S.ini )
   names(act.par) <- c("mu.H","K.S","b.H","Y.H","X.H.ini","S.S.ini")
   if ( par != 0 ) act.par <- par

   # split parameters into kinetic and initial:
   par.kin <- act.par[1:4]
   names(par.kin) <- names(simpdeg.def.par.mean)[1:4]
   par.ini <- act.par[5:6]
   names(par.ini) <- c("X.H","S.S")

   # integrate differential equations:
   x <- ode(rhs=simpdeg.rhs,x.ini=par.ini,par=par.kin,t.out=t.out,dt.max=dt.max)

   # plot results if requested:
   if ( plot == TRUE )
   {
      def.par <- par(no.readonly=T)
      par(mfrow=c(1,2),xaxs="i",yaxs="i",mar=c(4.5,4.5,2,1.5)+0.1)
      plot(t.out,x[,"X.H"],xlab="t [d]",ylab="X.H [gCOD/m3]",type="l")
      plot(t.out,x[,"S.S"],xlab="t [d]",ylab="S.S [gCOD/m3]",type="l")
      par(def.par)
   }

   # collect results:
   res <- list(
               par = act.par,
               t   = t.out,
               x   = x
              )

   # return results:
   return(res)
}



# Monte Carlo simulation of simple degradation model
# ==================================================

simpdeg.montecarlo <- function(
                               par.mean  = simpdeg.def.par.mean,
                               par.sd    = simpdeg.def.par.sd,
                               par.corr  = 0,
                               t.out     = simpdeg.def.t.out,
                               dt.max    = simpdeg.def.dt.max,
                               samp.size = simpdeg.def.samp.size,
                               plot      = TRUE
                              )
{
   # -----------------------------------------------------------------------
   # Perform Monte Carlo simulation of the model with a multivariate
   # lognormal distribution of the parameters without dependence. 
   #
   # Arguments:
   # par.mean:    mean parameter values (named "mu.H", "K.S", "b.H", 
   #              "Y.H", "X.H.ini", "S.S.ini") used to overwrite defaults
   #              (optional)
   # par.sd:      parameter standard deviations (named "mu.H", "K.S",    
   #              "b.H", "Y.H", "X.H.ini", "S.S.ini") used to overwrite 
   #              defaults (optional)
   # par.corr:    correlation matrix of parameters (optional; default no
   #              correlations)
   # t.out:       array of output times used to overwrite defaults 
   #              (optional)
   # dt.max:      maximum internal integration time step use to overwrite
   #              defaults (optional)
   # samp.size:   sample size (optional; default 100)
   # plot:        logical argument to specify if the simulation results
   #              should be plotted (optional; default TRUE)
   #
   # Return Value:
   # List of:
   # par.mean:    mean parameter values used for the simulation
   # par.sd:      parameter standard deviations used for the simulation
   # par.corr:    correlation matrix of parameters
   # samp.size:   sample size
   # par:         parameter sample
   # t:           output times
   # x:           matrix of calculated values of state variables for all
   #              Monte Carlo runs (columns) at all output times (rows)
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------

   # draw parameter sample:
   par <- randsamp(samp.size=samp.size,mean=par.mean,sd=par.sd,corr=par.corr,
                   dist="lognormal")$sample

   # initialize result arrays:
   X.H <- matrix(nrow=length(t.out),ncol=samp.size)
   S.S <- matrix(nrow=length(t.out),ncol=samp.size)
   colnames(X.H) <- rep("X.H",samp.size)
   colnames(S.S) <- rep("S.S",samp.size)
   rownames(X.H) <- t.out
   rownames(S.S) <- t.out

   # perform simulations:
   for ( i in 1:samp.size )
   {
      tmp.res <- simpdeg.sim(par=par[i,],t.out=t.out,dt.max=dt.max,plot=FALSE)
      X.H[,i] <- tmp.res$x[,"X.H"]
      S.S[,i] <- tmp.res$x[,"S.S"]
   }

   # collect results:
   res <- list(
               par.mean  = par.mean,
               par.sd    = par.sd,
               par.corr  = par.corr,
               samp.size = samp.size,
               par       = par,
               t         = t.out,
               x         = cbind(X.H,S.S)
              )

   # plot results if requested:
   if ( plot == TRUE )
   {
      def.par <- par(no.readonly=T)
      par(mfrow=c(1,2),xaxs="i",yaxs="i",mar=c(4.5,4.5,2,1.5)+0.1)
      plot(numeric(0),numeric(0),type="n",
           xlim=c(min(t.out),max(t.out)),ylim=c(0,1.1*max(X.H)),
           xlab="t [d]",ylab="X.H [gCOD/m3]")
      for ( i in 1:samp.size )
      {
         lines(t.out,X.H[,i])
      }
      plot(numeric(0),numeric(0),type="n",
           xlim=c(min(t.out),max(t.out)),ylim=c(0,1.1*max(S.S)),
           xlab="t [d]",ylab="S.S [gCOD/m3]")
      for ( i in 1:samp.size )
      {
         lines(t.out,S.S[,i])
      }
      par(def.par)
   }

   # return results:
   return(res)
}



# generation of simulated data for simple degradation model
# =========================================================

simpdeg.gendat <- function(
                           mu.H       = simpdeg.def.par.mean["mu.H"],
                           K.S        = simpdeg.def.par.mean["K.S"],
                           b.H        = simpdeg.def.par.mean["b.H"],
                           Y.H        = simpdeg.def.par.mean["Y.H"],
                           X.H.ini    = simpdeg.def.par.mean["X.H.ini"],
                           S.S.ini    = simpdeg.def.par.mean["S.S.ini"],
                           par        = 0,
                           X.H.sd     = simpdeg.def.X.H.sd,
                           S.S.sd     = simpdeg.def.S.S.sd,
                           t.out      = simpdeg.def.t.out.dat,
                           dt.max     = simpdeg.def.dt.max,
                           write.data = FALSE,
                           plot       = TRUE
                          )
{
   # -----------------------------------------------------------------------
   # Generate synthetic data.
   #
   # Arguments:
   # mu.H, K.S, b.H, Y.H, X.H.ini, S.S.ini:
   #              parameter values owerwriting the default values (optional)
   # sd.X.H, sd.S.S:
   #              standard deviation of the measurement process (normal,
   #              mean zero)
   # t.out:       array of output times (optional)
   # dt.max:      maximum internal integration time step use to overwrite
   #              defaults (optional)
   # write.data:  locigal argument to specify if the synthetic sample
   #              should be written to the file "simpdeg.dat"
   # plot:        logical argument to specify if the fit results
   #              should be plotted (optional; default TRUE)
   #
   # Return Value:
   # matrix containing time in the first column and synthetic data points
   # for state variables in succeeding columns.
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------

   # initialize parameter values:
   act.par <- c(mu.H,K.S,b.H,Y.H,X.H.ini,S.S.ini)
   names(act.par) <- c("mu.H","K.S","b.H","Y.H","X.H.ini","S.S.ini")
   if ( par != 0 ) act.par <- par

   # perform simulation:
   x <- simpdeg.sim(par=act.par,t.out=t.out,dt.max=dt.max,plot=FALSE)$x

   # add measurement noise:
   x[,"X.H"] <- x[,"X.H"] + rnorm(n=length(t.out),mean=0,sd=X.H.sd)
   x[,"X.H"] <- ifelse(x[,"X.H"]<0,0,x[,"X.H"])
   x[,"S.S"] <- x[,"S.S"] + rnorm(n=length(t.out),mean=0,sd=S.S.sd)
   x[,"S.S"] <- ifelse(x[,"S.S"]<0,0,x[,"S.S"])

   # combine results with time vector:
   res <- cbind(t.out,x)

   # write data file if requested:
   if ( write.data == TRUE )
   {
      write.table(res,file="simpdeg.dat",row.names=FALSE,col.names=TRUE,sep="\t")
   }

   # plot results if requested:
   if ( plot == TRUE )
   {
      par(mfrow=c(1,2),xaxs="i",yaxs="i",mar=c(4.5,4.5,2,1.5)+0.1)
      plot(t.out,x[,"X.H"],xlab="t [d]",ylab="X.H [gCOD/m3]")
      plot(t.out,x[,"S.S"],xlab="t [d]",ylab="S.S [gCOD/m3]")
   }

   # return results:
   return(res)
}



# generation of simulated data for simple degradation model
# =========================================================

model.function <- function(mu.H,K.S,b.H,Y.H,X.H.ini,S.S.ini,
                           t.out,dt.max,data.sel,weight.S.S=1)
{
   # -----------------------------------------------------------------------
   # Calculation of model results corresponding to data for fit.
   #
   # Arguments:
   # mu.H, K.S, b.H, Y.H, X.H.ini, S.S.ini:
   #              parameter values
   # t.out:       array of output times (optional)
   # dt.max:      maximum internal integration time step use to overwrite
   #              defaults (optional)
   # data.sel:    either "all" (default), "X.H.only" or "S.S.only"; 
   #              indicating which data to be used for the fit (data for
   #              X.H and S.S, data for S.H only, or data for S.S only)
   # weight.S.S:  weight of S.S relative to X.H (optional, default 2)
   #
   # Return Value:
   # Array of model results.
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------

   res.x <- simpdeg.sim(mu.H=mu.H,K.S=K.S,b.H=b.H,Y.H=Y.H,
                        X.H.ini=X.H.ini,S.S.ini=S.S.ini,
                        t.out=t.out,dt.max=dt.max,plot=FALSE)$x
   if ( data.sel == "X.H.only" )
   {
      return(res.x[,"X.H"])
   }
   else
   {
      if ( data.sel == "S.S.only" )
      {
         return(res.x[,"S.S"]*weight.S.S)
      }
      else
      {
         return(c(res.x[,"X.H"],res.x[,"S.S"]*weight.S.S))
      }
   }
}


simpdeg.fit <- function(
                        mu.H       = simpdeg.def.par.mean["mu.H"],
                        K.S        = simpdeg.def.par.mean["K.S"],
                        b.H        = simpdeg.def.par.mean["b.H"],
                        Y.H        = simpdeg.def.par.mean["Y.H"],
                        X.H.ini    = simpdeg.def.par.mean["X.H.ini"],
                        S.S.ini    = simpdeg.def.par.mean["S.S.ini"],
                        fitpars    = c("mu.H","K.S","b.H","Y.H","X.H.ini","S.S.ini"),
                        data       = "simpdeg.dat", 
                        data.sel   = "all",
                        weight.S.S = simpdeg.def.X.H.sd/simpdeg.def.S.S.sd,
                        dt.max     = simpdeg.def.dt.max,
                        trace      = FALSE,
                        plot       = TRUE
                       )
{
   # -----------------------------------------------------------------------
   # Perform a least squares (frequentist) parameter estimation of a 
   # subset of the 6 model parameters mu.H, K.S, b, Y.H, X.H.ini,
   # S.S.ini based on all data, data for X.H only or data for S.S only.
   #
   # Arguments:
   # mu.H, K.S, b.H, Y.H, X.H.ini, S.S.ini:
   #              start values for the fit owerwriting the default values
   #              (optional)
   # fitpars:     names of parameters to be fitted (optional); 
   #              default c("mu.H","K.S","b.H","Y.H","X.H.ini","S.S.ini")
   #              all parameters; alternatively, a subset can be selected
   # data:        data frame with variables "t", "X.H" and "S.S" 
   #              (optional)
   # data.sel:    either "all" (default), "X.H.only" or "S.S.only"; 
   #              indicating which data to be used for the fit (data for
   #              X.H and S.S, data for S.H only, or data for S.S only)
   # weight.S.S:  weight of S.S relative to X.H (optional, default 2)
   # dt.max:      maximum internal integration time step use to overwrite
   #              defaults (optional)
   # trace:       logical argument indicating if steps of the iteration
   #              process should be listed (optional; default FALSE)
   # plot:        logical argument to specify if the simulation results
   #              should be plotted (optional; default TRUE)
   #
   # Return Value:
   # output of nonlinear regression function nls.
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------

   # read data:
   data.simpdeg <- read.table(data,header=TRUE)
  
   # select data:
   fitdata <- numeric(0)
   if ( data.sel == "X.H.only" )
   {
      fitdata <- data.simpdeg$X.H  
   }
   else
   {
      if ( data.sel == "S.S.only" )
      {
         fitdata <- data.simpdeg$S.S*weight.S.S 
      }
      else
      {
         fitdata <- c(data.simpdeg$X.H,data.simpdeg$S.S*weight.S.S) 

      }
   }

   # select parameters to be fitted:
   start.full <- list(mu.H=mu.H,K.S=K.S,b.H=b.H,Y.H=Y.H,X.H.ini=X.H.ini,S.S.ini=S.S.ini)
   start.sel  <- list()
   for ( i in 1:length(fitpars) ) start.sel[[fitpars[i]]] <- start.full[[fitpars[i]]]

   # perform parameter estimation and return summary of results:
   res <- nls(
              fitdata ~ model.function(mu.H,K.S,b.H,Y.H,X.H.ini,S.S.ini,
                                       data.simpdeg$t,dt.max,data.sel,weight.S.S),
              start=start.sel,
              trace=trace
             )

   # plot results if requested:
   if ( plot == TRUE )
   {
      estimates <- summary(res)$parameters[,1]
      fitpars   <- names(estimates)
      if ( !is.na(match("mu.H",   fitpars)) ) mu.H    <- estimates["mu.H"]
      if ( !is.na(match("K.S",    fitpars)) ) K.S     <- estimates["K.S"]
      if ( !is.na(match("b.H",    fitpars)) ) b.H     <- estimates["b.H"]
      if ( !is.na(match("Y.H",    fitpars)) ) Y.H     <- estimates["Y.H"]
      if ( !is.na(match("X.H.ini",fitpars)) ) X.H.ini <- estimates["X.H.ini"]
      if ( !is.na(match("S.S.ini",fitpars)) ) S.S.ini <- estimates["S.S.ini"]
      t.out <- seq(0,max(data.simpdeg$t),length=101)
      sim.res <- simpdeg.sim(mu.H=mu.H,K.S=K.S,b.H=b.H,Y.H=Y.H,X.H.ini=X.H.ini,S.S.ini=S.S.ini,
                             t=t.out,plot=FALSE)
      def.par <- par(no.readonly=T)
      par(mfrow=c(1,2),xaxs="i",yaxs="i",mar=c(4.5,4.5,2,1.5)+0.1)
      plot(numeric(0),numeric(0),type="n",
           xlim=c(0,max(t.out)),ylim=c(0,1.1*max(data.simpdeg$X.H)),
           xlab="t [d]",ylab="X.H [gCOD/m3]")
      lines(sim.res$t,sim.res$x[,"X.H"])
      points(data.simpdeg$t,data.simpdeg$X.H)
      plot(numeric(0),numeric(0),type="n",
           xlim=c(0,max(t.out)),ylim=c(0,1.1*max(data.simpdeg$S.S)),
           xlab="t [d]",ylab="S.S [gCOD/m3]")
      lines(sim.res$t,sim.res$x[,"S.S"])
      points(data.simpdeg$t,data.simpdeg$S.S)
      par(def.par)
   }

   # return results:
   return(res)
}



# calculation of linear (local) sensitivity functions of simple degradation model
# ===============================================================================

simpdeg.sensloc <- function(
                            mu.H      = simpdeg.def.par.mean["mu.H"],
                            K.S       = simpdeg.def.par.mean["K.S"],
                            b.H       = simpdeg.def.par.mean["b.H"],
                            Y.H       = simpdeg.def.par.mean["Y.H"],
                            X.H.ini   = simpdeg.def.par.mean["X.H.ini"],
                            S.S.ini   = simpdeg.def.par.mean["S.S.ini"],
                            par       = 0,
                            t.out     = simpdeg.def.t.out,
                            dt.max    = simpdeg.def.dt.max,
                            delta.rel = 0.01,
                            plot      = TRUE
                           )
{
   # -----------------------------------------------------------------------
   # Calculate local sensitivity functions of model results with respect
   # to all model parameters.
   #
   # Arguments:
   # par:         array of parameter values (named "mu.H", "K.S", "b.H", 
   #              "Y.H", "X.H.ini", "S.S.ini") used to overwrite defaults 
   #              (optional)
   # t.out:       array of output times used to overwrite defaults 
   #              (optional)
   # dt.max:      maximum internal integration time step use to overwrite
   #              defaults (optional)
   # delta.rel:   relative change in parameter value to approximate the 
   #              derivative by a finite difference approximation
   # plot:        logical argument to specify if the simulation results
   #              should be plotted (optional; default TRUE)
   #
   # Return Value:
   # List of:
   # par:         parameter values used for the base simulation
   # t:           output times
   # x:           matrix of calculated values of state variables (columns)
   #              at all output times (rows) at base parameter values
   # sen:         list of sensitivity coefficients for X.H and S.S; each
   #              of the elements of the list contains a matrix of
   #              sensitivity coefficients of the outputs for the variable
   #              at all output times (rows) for all parameters (cols)
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------

   # initialize parameter values:
   act.par <- c(mu.H,K.S,b.H,Y.H,X.H.ini,S.S.ini)
   names(act.par) <- c("mu.H","K.S","b.H","Y.H","X.H.ini","S.S.ini")
   if ( par != 0 ) act.par <- par

   # calculate absolute sensitivity functions:
   num.par <- length(act.par)
   num.res <- length(t.out)
   res.base <- simpdeg.sim(par=act.par,t.out=t.out,dt.max=dt.max,plot=FALSE)
   sen.X.H <- matrix(nrow=num.res,ncol=num.par)
   colnames(sen.X.H) <- names(act.par)
   rownames(sen.X.H) <- t.out
   sen.S.S <- matrix(nrow=num.res,ncol=num.par)
   colnames(sen.S.S) <- names(act.par)
   rownames(sen.S.S) <- t.out
   for ( i in 1:num.par )
   {
      delta <- delta.rel*act.par[i]
      act.par[i] <- act.par[i]+delta
      sen.par.i <- (simpdeg.sim(par=act.par,t.out=t.out,dt.max=dt.max,plot=FALSE)$x-res.base$x)/delta
      sen.X.H[,i] <- sen.par.i[,"X.H"]
      sen.S.S[,i] <- sen.par.i[,"S.S"]
      act.par[i] <- act.par[i]-delta
   }
   sen <- list(X.H=sen.X.H,S.S=sen.S.S)

   # collect results:
   res <- list(
               par = act.par,
               t   = t.out,
               x   = res.base$x,
               sen = sen
              )

   # plot results if requested:
   if ( plot == TRUE )
   {
      num.eq <- length(names(sen))
      def.par <- par(no.readonly=T)
      par(mfrow=c(num.par,num.eq),xaxs="i",yaxs="i",mar=c(2.5,4.5,2,1.5)+0.1)
      for ( j in 1:num.par )
      {
         for ( i in 1:num.eq )
         {
            plot(t.out,
                 act.par[j]*sen[[i]][,j],
                 type = "l",
                 main=paste("Local sensitivity of ",colnames(res.base$x)[i]," to ",names(act.par)[j],sep=""),
                 xlab=paste("t [d]",sep=""),
                 ylab=paste("s_a,r ",colnames(res.base$x)[i]," [gCOD/m3]",sep=""))
         }
      }
      par(def.par)
   }

   # return results:
   return(res)
}



# calculation of regional sensitivity coefficients of simple degradation model
# ============================================================================

simpdeg.sensreg <- function(
                            par.mean  = simpdeg.def.par.mean,
                            par.sd    = simpdeg.def.par.sd,
                            par.corr  = 0,
                            t.out     = simpdeg.def.t.out,
                            dt.max    = simpdeg.def.dt.max,
                            samp.size = simpdeg.def.samp.size,
                            plot      = TRUE
                           )
{
   # -----------------------------------------------------------------------
   # Calculate regional sensitivity functions of model results with respect
   # to all model parameters.
   #
   # Arguments:
   # par.mean:    mean parameter values (named "mu.H", "K.S", "b.H", 
   #              "Y.H", "X.H.ini", "S.S.ini") used to overwrite defaults
   #              (optional)
   # par.sd:      parameter standard deviations (named "mu.H", "K.S", 
   #              "b.H", "Y.H", "X.H.ini", "S.S.ini") used to overwrite 
   #              defaults (optional)
   # t.out:       array of output times used to overwrite defaults 
   #              (optional)
   # dt.max:      maximum internal integration time step use to overwrite
   #              defaults (optional)
   # samp.size:   sample size (optional; default 100)
   # plot:        logical argument to specify if the simulation results
   #              should be plotted (optional; default TRUE)
   #
   # Return Value:
   # List of:    
   # par.mean:    mean parameter values used for the Monte Carlo 
   #              simulations
   # par.sd:      parameter standard deviations used for the Monte Carlo
   #              simulations
   # samp.size:   sample size
   # par:         parameter sample used for Monte Carlo simulations
   # t:           output times
   # reg.coeff:   list of regression coefficients for X.H and S.S; each
   #              of the elements of the list contains a matrix of
   #              regression coefficients of the outputs for the variable
   #              at all output times (rows) for all parameters (cols)
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------

   # calculate Monte Carlo sample:
   res.mc <- simpdeg.montecarlo(par.mean=par.mean,par.sd=par.sd,par.corr=par.corr,
                                t.out=t.out,dt.max=dt.max,samp.size=samp.size,plot=FALSE)

   # construct data set for linear regression:
   resnames.X.H <- paste("X.H.",t.out,sep="")
   resnames.S.S <- paste("S.S.",t.out,sep="")
   ind.X.H <- ifelse(colnames(res.mc$x)=="X.H",TRUE,FALSE)
   ind.S.S <- ifelse(colnames(res.mc$x)=="S.S",TRUE,FALSE)
   res.X.H <- t(res.mc$x[,ind.X.H])
   res.S.S <- t(res.mc$x[,ind.S.S])
   colnames(res.X.H) <- resnames.X.H
   colnames(res.S.S) <- resnames.S.S
   rownames(res.X.H) <- NULL
   rownames(res.S.S) <- NULL
   fitdata <- data.frame(cbind(res.mc$par,res.X.H,res.S.S))

   # construct model equations for linear regressions of all available results:
   num.par <- length(par.mean)
   model.rhs <- names(par.mean)[1]
   for ( i in 2:num.par ) model.rhs <- paste(model.rhs,names(par.mean)[i],sep="+")

   # perform linear regressions and extract regression coefficients:
   num.res <- length(resnames.X.H)
   reg.coeff.X.H <- matrix(nrow=num.res,ncol=num.par)
   colnames(reg.coeff.X.H) <- names(par.mean)
   rownames(reg.coeff.X.H) <- t.out
   reg.coeff.S.S <- matrix(nrow=num.res,ncol=num.par)
   colnames(reg.coeff.S.S) <- names(par.mean)
   rownames(reg.coeff.S.S) <- t.out
   r2.X.H <- numeric(num.res)
   r2.S.S <- numeric(num.res)
   for ( i in 1:num.res )
   {
      model <- paste(resnames.X.H[i],model.rhs,sep="~")
      res.lm <- lm(as.formula(model),data=fitdata)
      reg.coeff.X.H[i,] <- coefficients(res.lm)[2:(num.par+1)]
      r2.X.H[i] <- summary(res.lm)$r.squared
      model <- paste(resnames.S.S[i],model.rhs,sep="~")
      res.lm <- lm(as.formula(model),data=fitdata)
      reg.coeff.S.S[i,] <- coefficients(res.lm)[2:(num.par+1)]
      r2.S.S[i] <- summary(res.lm)$r.squared
   }

   # collect results:
   res <- list(
               par.mean  = par.mean,
               par.sd    = par.sd,
               samp.size = samp.size,
               par       = res.mc$par,
               t         = t.out,
               reg.coeff = list(X.H=reg.coeff.X.H,S.S=reg.coeff.S.S),
               r2        = list(X.H=r2.X.H,S.S=r2.S.S)
              )

   # plot results if requested:
   if ( plot == TRUE )
   {
      def.par <- par(no.readonly=T)
      par(mfrow=c((num.par+1),2),xaxs="i",yaxs="i",mar=c(2.5,4.5,2,1.5)+0.1)
      for ( i in 1:num.par )
      {
         plot(t.out,
              par.mean[i]*reg.coeff.X.H[,i],
              type = "l",
              main=paste("Regional sensitivity of X.H to ",names(par.mean)[i],sep=""),
              xlab=paste("t [d]",sep=""),
              ylab="Delta X.H [gCOD/m3]")
         plot(t.out,
              par.mean[i]*reg.coeff.S.S[,i],
              type = "l",
              main=paste("Regional sensitivity of S.S to ",names(par.mean)[i],sep=""),
              xlab=paste("t [d]",sep=""),
              ylab="Delta S.S [gCOD/m3]")
      }
      plot(t.out,r2.X.H,ylim=c(0,1),type="l",main="R2 X.H",xlab="t [d]",ylab="R2")
      plot(t.out,r2.S.S,ylim=c(0,1),type="l",main="R2 S.S",xlab="t [d]",ylab="R2")
      par(def.par)
   }

   # return results:
   return(res)
}



# calculation of quantitative identifiability measures of simple degradation model
# ================================================================================

simpdeg.ident <- function(
                          mu.H          = simpdeg.def.par.mean["mu.H"],
                          K.S           = simpdeg.def.par.mean["K.S"],
                          b.H           = simpdeg.def.par.mean["b.H"],
                          Y.H           = simpdeg.def.par.mean["Y.H"],
                          X.H.ini       = simpdeg.def.par.mean["X.H.ini"],
                          S.S.ini       = simpdeg.def.par.mean["S.S.ini"],
                          par           = 0,
                          scale.X.H     = simpdeg.def.par.mean["X.H.ini"],
                          scale.S.S     = simpdeg.def.par.mean["S.S.ini"],
                          delta.par.rel = 0.1,
                          data.sel      = "all",
                          t.out         = simpdeg.def.t.out.dat,
                          dt.max        = simpdeg.def.dt.max,
                          delta.rel     = 0.01,
                          plot          = TRUE
                         )
{
   # -----------------------------------------------------------------------
   # Calculate sensitivity index and collinearity indices for all
   # parameter subsets
   #
   # Arguments:
   # mu.H, K.S, b, Y.H, X.H.ini, S.S.ini:
   #                 parameter values used to overwrite the default  
   #                 values (optional)
   # par:            array of parameter values (named "mu.H", "K.S",  
   #                 "b.H", "Y.H", "X.H.ini", "S.S.ini") used to overwrite  
   #                 defaults and explicit arguments (optional)
   # scale.X.H, scale.S.S:
   #                 scaling parameters for sensitivity functions
   # delta.par.rel:  scaling parameter for sensitivity functions
   # data:           either "all" (default), "X.H.only" or "S.S.only"; 
   #                 indicating which data to be used for the analysis 
   #                 (data for X.H and S.S, data for S.H only, or data 
   #                 for S.S only)
   # t.out:          array of output times used to overwrite defaults 
   #                 (optional)
   # dt.max:         maximum internal integration time step use to 
   #                 overwrite defaults (optional)
   # delta.rel:      relative change in parameter value to approximate the 
   #                 derivative by a finite difference approximation
   # plot:           logical argument to specify if the sensitivity 
   #                 functions should be plotted (optional; default TRUE)
   # 
   # Return Value:
   # List of:
   # delta.msqr:     sensitivity ranking of parameters
   # collind.all:    collinearity index of all parameters
   # collind.n:      collinearity index of all subsets of parameters
   #                 (various elements with differing values of n that
   #                 indicate the size of the subsets)
   #
   # -----------------------------------------------------------------------

   # initialize parameter values:
   act.par        <- c( mu.H , K.S , b.H , Y.H , X.H.ini , S.S.ini )
   names(act.par) <- c("mu.H","K.S","b.H","Y.H","X.H.ini","S.S.ini")
   if ( par != 0 ) act.par <- par

   # calculate sensitivity functions:
   res.sensloc <- simpdeg.sensloc(par=act.par,t.out=t.out,dt.max=dt.max,
                                  delta.rel=delta.rel,plot=plot)

   # select sensitivity functions and define scaling factors:
   if ( data.sel == "X.H.only" )
   {
      sen <- res.sensloc$sen$X.H
      scale <- rep(scale.X.H,length(t.out))
   }
   else
   {
      if ( data.sel == "S.S.only" )
      {
         sen <- res.sensloc$sen$S.S
         scale <- rep(scale.S.S,length(t.out))
      }
      else
      {
         sen <- rbind(res.sensloc$sen$X.H,res.sensloc$sen$S.S)
         scale <- c(rep(scale.X.H,length(t.out)),rep(scale.S.S,length(t.out)))
      }
   }

   # define parameter uncertainty ranges:
   delta.par <- delta.par.rel*res.sensloc$par

   # calculate identifiability measures:
   res <- ident(sen=sen,delta.par=delta.par,scale=scale,max.subset.size=6)

   # return results:
   return(res)
}



# Bayesian inference with Markov chain Monte Carlo
# ================================================

model.logpostpdf <- function(mu.H     = simpdeg.def.par.mean["mu.H"],
                             K.S      = simpdeg.def.par.mean["K.S"],
                             b.H      = simpdeg.def.par.mean["b.H"],
                             Y.H      = simpdeg.def.par.mean["Y.H"],
                             X.H.ini  = simpdeg.def.par.mean["X.H.ini"],
                             S.S.ini  = simpdeg.def.par.mean["S.S.ini"],
                             par      = 0,
                             X.H.sd   = simpdeg.def.X.H.sd,
                             S.S.sd   = simpdeg.def.S.S.sd,
                             dt.max   = simpdeg.def.dt.max,
                             data,
                             data.sel = "all",
                             pri.mean = simpdeg.def.par.mean,
                             pri.sd   = simpdeg.def.par.sd,
                             pri.corr = 0,
                             pri.dist = "lognormal")
{
   # -----------------------------------------------------------------------
   # Calculation of the log of an expression proportional to the  posterior
   # distribution.
   #
   # Arguments:
   # mu.H, K.S, b, Y.H, X.H.ini, S.S.ini:
   #                 parameter values used to overwrite the default  
   #                 values (optional)
   # par:            array of parameter values (named "mu.H", "K.S",  
   #                 "b.H", "Y.H", "X.H.ini", "S.S.ini") used to overwrite  
   #                 defaults and explicit arguments (optional)
   # X.H.sd, S.S.sd: standard deviations of the values of X.H and S.S
   # dt.max:         maximum internal integration time step use to 
   #                 overwrite defaults (optional)
   # data:           data frame with variables "t", "X.H" and "S.S"
   # data.sel:       either "all" (default), "X.H.only" or "S.S.only"; 
   #                 indicating which data to be used for the fit (data for
   #                 X.H and S.S, data for S.H only, or data for S.S only)
   # pri.mean:       mean values of the prior distributions for all 
   #                 parameters
   # pri.sd:         standard deviations of the prior distributions for all 
   #                 parameters
   # pri.corr:       correlation matrix of the prior distribution of all 
   #                 parameters (optional)
   # pri.dist:       distribution type of the prior ("normal" or 
   #                 "lognormal") (optional)
   #
   # Return Value:
   # Value proportional to the posterior probability density function
   # (log of prior times likelihood)
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------

   act.par        <- c( mu.H , K.S , b.H , Y.H , X.H.ini , S.S.ini )
   names(act.par) <- c("mu.H","K.S","b.H","Y.H","X.H.ini","S.S.ini")
   if ( par != 0 ) act.par <- par
   res.x <- simpdeg.sim(par=act.par,t.out=data$t,dt.max=dt.max,plot=FALSE)$x
   num.out <- length(data$t)
   resid.X.H <- (res.x[,"X.H"]-data$X.H)/X.H.sd
   resid.S.S <- (res.x[,"S.S"]-data$S.S)/S.S.sd
   resid.X.H <- matrix(resid.X.H,nrow=num.out,ncol=1) # ensure that resid.X.H is a column
   resid.S.S <- matrix(resid.S.S,nrow=num.out,ncol=1) # ensure that resid.S.S is a column
   if ( data.sel == "X.H.only" )
   {
      chi2 <- t(resid.X.H) %*% resid.X.H 
   }
   else
   {
      if ( data.sel == "S.S.only" )
      {
         chi2 <- t(resid.S.S) %*% resid.S.S
      }
      else
      {
         chi2 <- t(resid.X.H) %*% resid.X.H + t(resid.S.S) %*% resid.S.S
      }
   }
   loglikeli  <- -chi2
   logpripdf  <- log(calc.pdf(x=act.par,mean=pri.mean,sd=pri.sd,
                              corr=pri.corr,dist=pri.dist))
   logpostpdf <- logpripdf + loglikeli
   return(logpostpdf)
}


simpdeg.markovchain <- function(
                                par.ini   = simpdeg.def.par.mean,
                                pri.mean  = simpdeg.def.par.mean,
                                pri.sd    = simpdeg.def.par.sd,
                                pri.corr  = 0,
                                pri.dist  = "lognormal",
                                X.H.sd    = simpdeg.def.X.H.sd,
                                S.S.sd    = simpdeg.def.S.S.sd,
                                jmp.sd    = simpdeg.def.par.sd/20,
                                jmp.corr  = 0,
                                dt.max    = simpdeg.def.dt.max,
                                data      = 0,
                                data.sel  = "all",
                                samp.size = simpdeg.def.samp.size,
                                plot      = TRUE,
                                nbin      = 15
                               )
{
   # -----------------------------------------------------------------------
   # calculation of a Markov chain of the posterior distribution
   #
   # Arguments:
   # par.ini:        parameter values to start the chain
   # pri.mean:       mean values of the prior distributions for all 
   #                 parameters
   # pri.sd:         standard deviations of the prior distributions for all 
   #                 parameters
   # pri.corr:       correlation matrix of the prior distribution of all 
   #                 parameters (optional)
   # pri.dist:       distribution type of the prior ("normal" or 
   #                 "lognormal") (optional)
   # X.H.sd, S.S.sd: standard deviations of the values of X.H and S.S
   # jmp.sd:         standard deviations of the jump distribution
   # jmp.corr:       correlation matrix of the jump distribution
   # data:           data frame with variables "t", "X.H" and "S.S" 
   #                 (optional)
   # data.sel:       either "all" (default), "X.H.only" or "S.S.only"; 
   #                 indicating which data to be used for the fit (data for
   #                 X.H and S.S, data for S.H only, or data for S.S only)
   # samp.size:      length of the Markov chain to be calculated
   # plot:           logical argument to specify if the simulation results
   #                 should be plotted (optional; default TRUE)
   # nbin:           number of bins of the histograms of marginal 
   #                 distributions of the posterior
   #
   # Return Value:
   # List of:
   # samp.size:      lenght of the the Markov chain returned
   # accept.freq:    ratio of accepted steps to total steps
   # par:            matrix of parameter sets of the chain (each row 
   #                 corresponds to one parameter set)
   #
   #                                        Peter Reichert    Dec.  22, 2002
   #                                        last modification March 27, 2002
   # -----------------------------------------------------------------------

   # read data:
   if ( data == 0 )
   {
      data.simpdeg <- read.table("simpdeg.dat",header=TRUE)
   }
   else
   {
      data.simpdeg = 0
   }

   # calculate Markov chain:
   par <- matrix(nrow=samp.size,ncol=length(par.ini))
   colnames(par) <- names(par.ini)
   logpostpdf.old <- model.logpostpdf(par=par.ini,X.H.sd=X.H.sd,S.S.sd=S.S.sd,
                                      dt.max=dt.max,data=data.simpdeg,data.sel=data.sel,
                                      pri.mean=pri.mean,pri.sd=pri.sd,
                                      pri.corr=pri.corr,pri.dist=pri.dist)
   par[1,] = par.ini
   num.accept <- 0
   for ( i in 2:samp.size )
   {
      par.new <- par[i-1,] +
                 randsamp(samp.size=1,mean=rep(0,length(par.ini)),
                          sd=jmp.sd,corr=jmp.corr,dist="normal")$sample
      logpostpdf.new <- model.logpostpdf(par=par.new,X.H.sd=X.H.sd,S.S.sd=S.S.sd,
                                         dt.max=dt.max,data=data.simpdeg,data.sel=data.sel,
                                         pri.mean=pri.mean,pri.sd=pri.sd,
                                         pri.corr=pri.corr,pri.dist=pri.dist)
      r <- exp(logpostpdf.new-logpostpdf.old)
      if ( runif(n=1,min=0,max=1) <= r )
      {
         par[i,] <- par.new
         logpostpdf.old <- logpostpdf.new
         num.accept <- num.accept+1
      }
      else
      {
         par[i,] <- par[i-1,]
      }
   }

   # collect results:
   res <- list(
               samp.size   = samp.size,
               accept.freq = num.accept/samp.size,
               par         = par
              )

   # plot results if requested:
   if ( plot == TRUE )
   {
      num.par <- ncol(par)
      def.par <- par(no.readonly=T)
      par(mfrow=c(3,2),xaxs="i",yaxs="i",mar=c(4.5,4.5,2,1.5)+0.1)
      for ( i in 1:num.par )
      {
         if ( pri.dist == "normal" )
         {
            mean  <- pri.mean[i]
            sd    <- pri.sd[i]
            pri.x <- seq(mean-3*sd,mean+3*sd,length=101)
            pri.y <- dnorm(pri.x,mean,sd)
         }
         else
         {
            if ( pri.dist == "lognormal" )
            {
               mean    <- pri.mean[i]
               sd      <- pri.sd[i]
               sdlog   <- sqrt(log(1+sd*sd/(mean*mean)))
               meanlog <- log(mean) - sdlog*sdlog/2
               pri.x   <- seq(exp(meanlog-3*sdlog),exp(meanlog+3*sdlog),length=101)
               pri.y   <- dlnorm(pri.x,meanlog,sdlog)
            }
            else
            {
               pri.x <- pri.mean[i]
               pri.y <- 0
            }
         }
         xmin <- min(par[,i])
         xmax <- max(par[,i])
         hist.curve <- hist.weighted(par[,i],xmin=xmin,xmax=xmax,nbin=nbin)$curve
         plot(numeric(0),numeric(0),type="n",
              xlim=c(min(pri.x),max(pri.x)),ylim=c(0,1.1*max(hist.curve$y,pri.y)),
              xlab=names(pri.mean)[i],ylab="pdf")
         lines(hist.curve)
         lines(pri.x,pri.y,lty="dashed")
      }
      par(def.par)
   }

   # return results:
   return(res)
}


