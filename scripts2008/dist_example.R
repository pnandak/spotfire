############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-19-03                                                           #
# UPDATE: 6-26-04                                                          #
# PURPOSE: Chapter 9 material                                              #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################

xr<-c(2, 3.4)
yr<-c(4, 5)
sqrt(t(xr - yr) %*% (xr - yr))


############################################################################
# Simple plot

  #Square plot
  par(pty = "s")
  xpoints<-c(2, 4, 6, 4)
  ypoints<-c(3.4, 5, 3, 2)

  obs<-data.frame(X1 = xpoints, X2 = ypoints)

  plot(x = obs$X1, y = obs$X2, type = "p", xlab = expression(X[1]), ylab = expression(X[2]), 
       pch = 16, col = "blue", cex = 5, xlim = c(0,7), ylim = c(0,7), panel.first=grid(col = "gray", lty = "dotted")) 
   
  for (i in 1:3) {
    for (j in (i+1):4) {
      segments(x0 = obs$X1[i], y0 = obs$X2[i], x1 = obs$X1[j], y1 = obs$X2[j], lty = 1, lwd = 1, col = 2)
      distance<-sqrt(t(c(obs$X1[i], obs$X2[i]) - c(obs$X1[j], obs$X2[j])) %*% (c(obs$X1[i], obs$X2[i]) - c(obs$X1[j], obs$X2[j])))
      text(x = (obs$X1[i]+obs$X1[j])/2, y = (obs$X2[i]+obs$X2[j])/2, labels = round(distance,2))
      }
     }
   text(x = obs$X1, y = obs$X2, labels = 1:4, col = "white", cex = 2)
   
####################################################################### 
# Find all distances

dist(x = obs, method = "euclidean")

library(cluster)
daisy(x = obs, metric = "euclidean")


#
