#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  2-24-06                                                    #
# UPDATE:                                                           #
# PURPOSE: Plot comparing all of the nonparametric boot. intervals  #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#Method              Lower   Upper
##################################
#BCa                 56.5    232.4
#Percentile          45.8    193.0
#Basic               23.2    170.3
#Studentized         45.0    303.4
#Basic trans.        60.5    254.9
#Studentized trans.  50.0    335.2

#method<-c("BCa", "Percentile", "Basic", "Studentized", "Basic trans.", "Studentized trans.")
method<-c("BCa", "Percent.", "Basic", "Stud.", "Basic trans.", "Stud. trans.")
lower<-c(56.5, 45.8, 23.2, 45.0, 60.5, 50.0)
upper<-c(232.4, 193.0, 170.3, 303.4, 254.9, 335.2)

ci<-data.frame(method, lower, upper)
ci


win.graph(width = 10, height = 8, pointsize = 11) 
stripchart(lower ~ method, vertical = FALSE, xlim = c(0, 400), col = "red", 
           pch = "(", main = "Nonparametric bootstrap C.I.s", xlab = expression(theta), ylab = "Method")  
stripchart(upper ~ method, vertical = FALSE, col = "red", pch = ")", add = TRUE)  
grid(nx = NA, ny = NULL, col="gray", lty="dotted")


library(boot)
abline(v = mean(aircondit$hours), col = "darkblue", lwd = 4)




#
