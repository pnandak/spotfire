######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  1-4-06                                                      #
# PURPOSE: Simulate data from an ARFIMA model and fit model          #
# NOTES:                                                             #
#                                                                    #
######################################################################


arfima.sim.data<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\chapter5\\x_arfima.txt", header=TRUE, sep = "")
head(arfima.sim.data)
x<-arfima.sim.data$x

#Package containing the ARFIMA functions (this needs to be downloaded from the R website)
library(fracdiff)  


################################################################################
# Example of simulating ARFIMA data

  #Note the defaults for this function are shown a few lines below.  I have included some here just to
  #  remind students that they can be changed.  
  set.seed(9121)
  x.ex<-fracdiff.sim(n = 500, ar = c(0.2), ma = NULL, d = 0.3, rand.gen = rnorm, sd = 1, mu = 0)
  names(x.ex)
  x.ex$series[1:5]
  
  #DEFAULTS
  #fracdiff.sim(n = 500, ar = NULL, ma = NULL, d, rand.gen = rnorm, innov = rand.gen(n+q, ...),
  #           n.start = NA, allow.0.nstart = FALSE, ..., mu = 0.)

  par(mfrow = c(1,2))
  acf(x = x.ex$series, lag.max = 50, type = "correlation", main = "Estimated ACF for x.ex$series data", xlim = c(1,50), ylim = c(-1,1))
  pacf(x = x.ex$series, lag.max = 50, main = "Estimated PACF for x.ex$series data", xlim = c(1,50), ylim = c(-1,1))
  par(mfrow = c(1,1))
  
  

################################################################################
# Plot data and find ACF and PACF - I am using the originally simulated series read in 
#   at the beginning of the program.  

  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = "red", lwd = 1 , 
       main = "Plot of x_arfima.txt data", panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")

  par(mfrow = c(1,2))
  acf(x = x, lag.max = 50, type = "correlation", main = "Estimated ACF for x_arfima.txt data", xlim = c(1,50), ylim = c(-1,1))
  pacf(x = x, lag.max = 50, main = "Estimated PACF for x_arfima.txt data", xlim = c(1,50), ylim = c(-1,1))
  par(mfrow = c(1,1))
  
    
############################################################################
# Fit model

  mean(x) 
  x.adj<-x-mean(x)

  mod.fit<-fracdiff(x = x.adj, nar = 1,  M = 100) #use nma = 1 for 1 MA if needed
  mod.fit
  summary(mod.fit)
  vcov(mod.fit)
  names(mod.fit)


#################################################################################
# Model building

  mod.fit<-fracdiff(x = x.adj, M = 100) 
  summary(mod.fit)

  pi.vec<-numeric(length(x.adj))
  pi.vec[1]<--mod.fit$d/(0+1)

  w<-numeric(length(x.adj))
  w[1]<-x.adj[1]
  for(j in 2:(length(x.adj))) {
    #Need to move subscript one back to match (5.7) p. 274
    pi.vec[j]<-((j-1-mod.fit$d)*pi.vec[j-1])/(j-1+1)
    w[j]<-x.adj[j] + sum(pi.vec[1:(j-1)]*rev(x.adj[1:(j - 1)]))
  }

  pi.vec[1:5]
  all<-data.frame(x.adj, w, pi.vec)
  head(all, n = 10) 


  par(mfrow = c(1,2))
  acf(x = w, lag.max = 100, type = "correlation", main = "Est. ACF for residuals", ylim = c(-1,1),
      panel.first=grid(col = "gray", lty = "dotted"))
  pacf(x = w, lag.max = 100, main = "Est. PACF for residuals", ylim = c(-1,1),
      panel.first=grid(col = "gray", lty = "dotted"))
  par(mfrow = c(1,1))
  
  
  #Perhaps starting the residuals at t = 101 (since M = 100) would be better
  par(mfrow = c(1,2))
  acf(x = w[-(1:100)], lag.max = 100, type = "correlation", main = "Est. ACF for residuals", ylim = c(-1,1),
      panel.first=grid(col = "gray", lty = "dotted"))
  pacf(x = w[-(1:100)], lag.max = 100, main = "Est. PACF for residuals", ylim = c(-1,1),
      panel.first=grid(col = "gray", lty = "dotted"))
  par(mfrow = c(1,1))



  ##########################################
  #Work with AR(1) parameter added to model
  
  mod.fit<-fracdiff(x = x.adj, nar = 1,  M = 100) 
  summary(mod.fit)

  pi.vec<-numeric(length(x.adj))
  pi.vec[1]<--mod.fit$d/(0+1)

  w<-numeric(length(x.adj))
  w[1]<-x.adj[1]

  for (j in 2:(length(x))) {  
    pi.vec[j]<-(j-1-mod.fit$d)*pi.vec[j-1]/(j-1+1)
  }

  w[2]<-x.adj[2] + pi.vec[1]*x.adj[1] - mod.fit$ar*x.adj[1] 

  for (j in 3:(length(x))) {  
    w[j]<-x.adj[j]+sum(pi.vec[1:(j-1)]*rev(x.adj[1:(j-1)])) - 
           mod.fit$ar*x.adj[j-1] - mod.fit$ar*sum(pi.vec[1:(j-2)]*rev(x.adj[1:(j-2)])) 
  }

  all<-data.frame(x.adj, w, pi.vec)
  head(all, n = 10) 


  par(mfrow = c(1,2))
  acf(x = w, lag.max = 100, type = "correlation", main = "Est. ACF for residuals", ylim = c(-1,1),
      panel.first=grid(col = "gray", lty = "dotted"))
  pacf(x = w, lag.max = 100, main = "Est. PACF for residuals", ylim = c(-1,1),
      panel.first=grid(col = "gray", lty = "dotted"))
  par(mfrow = c(1,1))
  
  
  #Perhaps starting the residuals at t = 101 (since M = 100) would be better
  par(mfrow = c(1,2))
  acf(x = w[-(1:100)], lag.max = 100, type = "correlation", main = "Est. ACF for residuals", ylim = c(-1,1),
      panel.first=grid(col = "gray", lty = "dotted"))
  pacf(x = w[-(1:100)], lag.max = 100, main = "Est. PACF for residuals", ylim = c(-1,1),
      panel.first=grid(col = "gray", lty = "dotted"))
  par(mfrow = c(1,1))













































#
