#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-20-06                                                   #
# PURPOSE: Non-stationarity in mean                                 #
#                                                                   #
# NOTES:                                                            #
#####################################################################


library(RODBC)
z<-odbcConnectExcel("C:\\chris\\UNL\\STAT_time_series\\chapter2\\nonstat.mean.xls")
nonstat.mean<-sqlFetch(z, "Sheet1")
close(z)

head(nonstat.mean)
tail(nonstat.mean)



#One way to do plot
win.graph(width = 8, height = 6, pointsize = 10) #width = 6 is default so this makes it wider
plot(x = nonstat.mean$x, ylab = expression(x[t]), xlab = "t (time)", type = "l", col = "red", 
        main = "Nonstationary time series", panel.first=grid(col = "gray", lty = "dotted"))
points(x = nonstat.mean$x, pch = 20, col = "blue")

acf(x = nonstat.mean$x, type = "correlation", main = "Plot of the ACF")

#Find first differences
first.diff<-diff(x = nonstat.mean$x, lag = 1, differences = 1) 
first.diff[1:5]
nonstat.mean$x[2] - nonstat.mean$x[1]
nonstat.mean$x[3] - nonstat.mean$x[2]

plot(x = first.diff, ylab = expression(x[t]-x[t-1]), xlab = "t (time)", type = "l", col = "red", 
        main = "First differences", panel.first=grid(col = "gray", lty = "dotted"))
points(x = first.diff, pch = 20, col = "blue")

acf(x = first.diff, type = "correlation", main = "Plot of the ACF for first differences")

#Put both x_t and x_t - x_t-1 in one set
x<-ts(data = nonstat.mean$x)
x.diff1<-ts(data = first.diff, start = 2)
ts.intersect(x, x.diff1)





lag.plot(x, lags=4, layout=c(2,2), do.lines = FALSE)

















#
