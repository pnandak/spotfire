#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-8-02                                                     #
# UPDATE: 1-11-03, 12-6-06                                          #
# PURPOSE: Find binomial cdf                                        #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#N=5, pi=0.6
pdf<-dbinom(x = 0:5, size = 5, prob = 0.6) 
pdf

cdf<-pbinom(q = 0:5, size = 5, prob = 0.6) 

#Make the printing look a little nicer
save<-data.frame(Y = 0:5, prob = round(pdf,4), cdf = round(cdf,4))
save 

plot(x = save$Y, y = save$cdf, type = "s", xlab = "y", ylab = "Probability", lwd = 2, 
     col = "blue", panel.first=grid(col = "gray"), main = 
     expression(paste("CDF of a binomial distribution for N=5, ", pi == 0.6)))
abline(h = 0)
segments(x0 = 5, y0 = 1, x1 = 10, y1 = 1, lwd = 2, col = "blue")
segments(x0 = -5, y0 = 0, x1 = 0, y1 = 0, lwd = 2, col = "blue")
segments(x0 = 0, y0 = 0, x1 = 0, y1 = dbinom(0, 5, 0.6), lwd = 2, col = "blue")
