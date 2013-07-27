#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-18-06                                                   #
# PURPOSE: Chapter 1 Sunspots                                       #
#                                                                   #
# NOTES:                                                            #
#####################################################################


sunspots.data<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\chapter1\\sunspots.csv", header=TRUE, sep = ",")
head(sunspots.data)
tail(sunspots.data)

win.graph(width = 8, height = 6, pointsize = 10)
plot(x = sunspots.data$Sunspots, ylab = "Number of sunspots", xlab = "t (time)", type = "l", col = "red", 
     main = "Sunspots per year from 1784 to 1983", panel.first=grid(col = "gray", lty = "dotted"))
points(x = sunspots.data$Sunspots, pch = 20, col = "blue")

plot(y = sunspots.data$Sunspots, x = sunspots.data$Year, ylab = "Number of sunspots", xlab = "Year", type = "l", col = "red", 
     main = "Sunspots per year from 1784 to 1983", panel.first=grid(col = "gray", lty = "dotted"))
points(y = sunspots.data$Sunspots, x = sunspots.data$Year, pch = 20, col = "blue")

#Convert to an object of class "ts"
x<-ts(sunspots.data$Sunspots, start = 1784, frequency = 1)
class(x)
class(sunspots.data$Sunspots)
x
plot.ts(x = x, ylab = expression(paste(x[t], " (Number of sunspots)")), xlab = "t (year)", type = "o", col = "red",
        main = "Sunspots per year from 1784 to 1983")
#type = "b" also works for "both" points and lines, but it leaves spaces between the points and lines
