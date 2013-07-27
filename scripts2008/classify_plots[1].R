############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-17-03                                                           #
# PURPOSE: Create plots in R for Chapter 7                                 #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################


#I first exported an ASCII text file from SAS which contained the density,
#  weight, class, and _INTO_

wheat<-read.table(file = "C:\\chris\\UNL\\STAT873\\Chapter 7\\class_wheat.txt", header=TRUE)
#Notice how R changes the name of _INTO_ to X_INTO_


par(pty="s")

#Original obs.
plot(x = wheat$skwt, y = wheat$skden, xlab="Weight", ylab="Density", type="n", 
     main = "Density vs. Weight \n Original observations", panel.first=grid(col="gray", lty="dotted"))
points(x = wheat$skwt[wheat$type1=="Healthy"], y = wheat$skden[wheat$type1=="Healthy"], pch = 1, col = 1, cex = 1)
points(x = wheat$skwt[wheat$type1=="Sprout"], y = wheat$skden[wheat$type1=="Sprout"], pch = 2, col = 2, cex = 1)
points(x = wheat$skwt[wheat$type1=="Scab"], y = wheat$skden[wheat$type1=="Scab"], pch = 5, col = 3, cex = 1)
legend(locator(1), legend=c("Healthy", "Sprout", "Scab"),bg = "white", pch = c(1,2,5), col=c(1,2,3)) 


#Classified obs.
plot(x = wheat$skwt, y = wheat$skden, xlab="Weight", ylab="Density", type="n", 
     main = "Density vs. Weight \n Classified observations", panel.first=grid(col="gray", lty="dotted"))
points(x = wheat$skwt[wheat$X_INTO_=="Healthy"], y = wheat$skden[wheat$X_INTO_=="Healthy"], pch = 1, col = 1, cex = 1)
points(x = wheat$skwt[wheat$X_INTO_=="Sprout"], y = wheat$skden[wheat$X_INTO_=="Sprout"], pch = 2, col = 2, cex = 1)
points(x = wheat$skwt[wheat$X_INTO_=="Scab"], y = wheat$skden[wheat$X_INTO_=="Scab"], pch = 5, col = 3, cex = 1)
legend(locator(1), legend=c("Healthy", "Sprout", "Scab"),bg = "white", pch = c(1,2,5), col=c(1,2,3)) 


#Overlaid 
plot(x = wheat$skwt, y = wheat$skden, xlab="Weight", ylab="Density", type="n", 
     main = "Density vs. Weight \n Classified (large points) overlaid on the original (small points) observations", 
     panel.first=grid(col="gray", lty="dotted"))
points(x = wheat$skwt[wheat$type1=="Healthy"], y = wheat$skden[wheat$type1=="Healthy"], pch = 1, col = 1, cex = 0.75)
points(x = wheat$skwt[wheat$type1=="Sprout"], y = wheat$skden[wheat$type1=="Sprout"], pch = 2, col = 2, cex = 0.75)
points(x = wheat$skwt[wheat$type1=="Scab"], y = wheat$skden[wheat$type1=="Scab"], pch = 5, col = 3, cex = 0.75)

points(x = wheat$skwt[wheat$X_INTO_=="Healthy"], y = wheat$skden[wheat$X_INTO_=="Healthy"], pch = 1, col = 1, cex = 1.5)
points(x = wheat$skwt[wheat$X_INTO_=="Sprout"], y = wheat$skden[wheat$X_INTO_=="Sprout"], pch = 2, col = 2, cex = 1.5)
points(x = wheat$skwt[wheat$X_INTO_=="Scab"], y = wheat$skden[wheat$X_INTO_=="Scab"], pch = 5, col = 3, cex = 1.5)

legend(locator(1), legend=c("Healthy", "Sprout", "Scab"),bg = "white", pch = c(1,2,5), col=c(1,2,3)) 




#
