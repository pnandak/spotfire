######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  1-4-06                                                      #
# PURPOSE: ARFIMA model for Varve data                               #
# NOTES:                                                             #
#                                                                    #
######################################################################


varve<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\Shumway_Stoffer_web_info\\Data\\varve.dat", 
                  header = FALSE, col.names = "x", sep = "")
head(varve)
x<-varve$x


################################################################################
# Plot data and find ACF and PACF  

  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = "red", lwd = 1 , 
       main = "Glacial varve thickness data", panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")
  plot(x = log(x), ylab = expression(log(x[t])), xlab = "t", type = "l", col = "red", lwd = 1 , 
       main = "Glacial varve thickness data - log transformed", panel.first=grid(col = "gray", lty = "dotted"))
  points(x = log(x), pch = 20, col = "blue")

  par(mfrow = c(1,2))
  acf(x = log(x), lag.max = 50, type = "correlation", main = "Est. ACF for log trans. data", xlim = c(1,50), ylim = c(-1,1))
  pacf(x = log(x), lag.max = 50, main = "Est. PACF for log trans. data", xlim = c(1,50), ylim = c(-1,1))
  par(mfrow = c(1,1))
  
    
############################################################################
# Fit model

  mean(log(x)) 
  log.x.adj<-log(x)-mean(log(x))

  mod.fit<-fracdiff(x = log.x.adj) 
  summary(mod.fit)
 
  #With M = 30 like in Shumway and Stoffer (2006)
  mod.fit<-fracdiff(x = log.x.adj, M = 30) 
  summary(mod.fit)


##########################################################################
# Find pi's

  pi.vec<-numeric(length(log.x.adj))
  pi.vec[1]<--mod.fit$d/(0+1)

  w<-numeric(length(log.x.adj))
  w[1]<-log.x.adj[1]
  for(j in 2:(length(log.x.adj))) {
  #Need to move subscript one back to match (5.7) p. 274
    pi.vec[j]<-((j-1-mod.fit$d)*pi.vec[j-1])/(j-1+1)
    w[j]<-log.x.adj[j] + sum(pi.vec[1:(j-1)]*rev(log.x.adj[1:(j - 1)]))
  }

  pi.vec[1:5]
  all<-data.frame(log.x.adj, w, pi.vec)
  head(all, n = 10) 

  par(mfrow = c(1,2))
  acf(x = w, lag.max = 100, type = "correlation", main = "Est. ACF for residuals", ylim = c(-1,1),
      panel.first=grid(col = "gray", lty = "dotted"))
  pacf(x = w, lag.max = 100, main = "Est. PACF for residuals", ylim = c(-1,1),
      panel.first=grid(col = "gray", lty = "dotted"))
  par(mfrow = c(1,1))
  
  
  #Perhaps starting the residuals at t = 31 (since M = 30) would make the above plots exactly the same 
  #  as those in Shumway and Stoffer (2006).
  par(mfrow = c(1,2))
  acf(x = w[-(1:30)], lag.max = 100, type = "correlation", main = "Est. ACF for residuals", ylim = c(-1,1),
      panel.first=grid(col = "gray", lty = "dotted"))
  pacf(x = w[-(1:30)], lag.max = 100, main = "Est. PACF for residuals", ylim = c(-1,1),
      panel.first=grid(col = "gray", lty = "dotted"))
  par(mfrow = c(1,1))





#
