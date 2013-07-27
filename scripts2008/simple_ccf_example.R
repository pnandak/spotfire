######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  12-19-06                                                    #
# PURPOSE: CCF example                                               #
#                                                                    #
# NOTES:                                                             #
######################################################################


x<-c(1, 2, 3, 4, 5, 6)
y<-c(2, 3, 5, 6, 8, 9)

gamma.x<-acf(x = x, type = "covariance", plot = FALSE)
gamma.x
gamma.y<-acf(x = y, type = "covariance", plot = FALSE)
gamma.y

win.graph(width = 8, height = 6, pointsize = 10)  #Opens up wider plot window than the default (good for time series plots)

x.y.acf<-acf(x = cbind(x,y), type = "correlation")
x.y.acf

x.y.ccf<-ccf(x = x, y = y, type = "correlation")
x.y.ccf
