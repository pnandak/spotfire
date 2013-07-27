############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-4-06                                                            #
# PURPOSE: Draw a t-distribution for the sales and adverstising example    #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################


#This changes a default graphics "par"ameter value - see help(par) for more on it
#  "r" is the default which causes the y-axis to be 4% larger (both directions) than the minimum value being plotted
par(yaxs = "i") 

#t-distribution - note that dt() finds f(t) for a t-distribution.  
curve(expr = dt(x = x, df = 3), from = -5, to = 5, ylab = "f(t)", xlab = "t", lty = "solid", lwd = 2, col = "black",
      main = "t-distribution")

#Add critical values
segments(x0 = qt(p = 1-0.05/2, df = 3), y0 = 0, x1 = qt(p = 1-0.05/2, df = 3), y1 = dt(x = qt(p = 1-0.05/2, df = 3), df = 3),
         col = "blue", lwd = 2)
segments(x0 = qt(p = 0.05/2, df = 3), y0 = 0, x1 = qt(p = 0.05/2, df = 3), y1 = dt(x = qt(p = 0.05/2, df = 3), df = 3),
         col = "blue", lwd = 2)
         
#Center line
segments(x0 = 0, y0 = 0, x1 = 0, y1 = dt(x = 0, df = 3), col = "black")

#Labels
text(x = 0, y = 0.1, label = "Don't reject Ho")
text(x = -4, y = 0.075, label = "Reject Ho")
text(x = 4, y = 0.075, label = "Reject Ho")
    
#Draw arrows
arrows(x0 = -4, y0 = 0.06, x1 =-3.4, y1 = dt(x = -4, df = 3)/1.5, length = 0.1)
arrows(x0 =  4, y0 = 0.06, x1 = 3.4, y1 = dt(x =  4, df = 3)/1.5, length = 0.1)
         
#Test statistic location
segments(x0 = 3.66, y0 = 0, x1 = 3.66, y1 = dt(x =  3.66, df = 3)-0.002, col = "red", lwd = 2)

#Legend
legend(x = -4.8, y = 0.15, legend = c("Critical values", "Test statistic"), col = c("blue", "red"), 
       cex = 0.75, lty = c(1,1), lwd = c(2,2))
