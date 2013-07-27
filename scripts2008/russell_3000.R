#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-17-06                                                   #
# PURPOSE: Chapter 1 Russell 3000 index                             #
#                                                                   #
# NOTES:                                                            #
#####################################################################


library(RODBC)
z<-odbcConnectExcel("C:\\chris\\UNL\\STAT_time_series\\chapter1\\russell.xls")
russell<-sqlFetch(z, "Sheet1")
close(z)

head(russell)
tail(russell)


#One way to do plot
win.graph(width = 8, height = 6, pointsize = 10) #width = 6 is default so this makes it wider
plot(x = russell$"Value Without Dividends", ylab = "Russell 3000 Index", xlab = "t (time)", type = "l", col = "red", 
        main = "Russell 3000 Index from 6/1/1995 to 12/31/1997", panel.first=grid(col = "gray", lty = "dotted"))
points(x = russell$"Value Without Dividends", pch = 20, col = "blue")


#Another way to do plot with actual dates
plot(y = russell$"Value Without Dividends", x = as.Date(russell$Date), xlab = "Time", type = "l", col = "red", 
        main = "Russell 3000 Index from 6/1/1995 to 12/31/1997", ylab = "Russell 3000 Index", xaxt = "n")
axis.Date(side = 1, at = seq(from = as.Date("1995/6/1"), to = as.Date("1997/12/31"), by = "months"), 
         labels = format(x = seq(from = as.Date("1995/6/1"), to = as.Date("1997/12/31"), by = "months"), 
         format = "%b%y"), las = 2)  #las changes orientation of labels
points(y = russell$"Value Without Dividends", x = as.Date(russell$Date), pch = 20, col = "blue")
#Create own gridlines
abline(v = as.Date(c("1995/7/1", "1996/1/1", "1996/7/1", "1997/1/1", "1997/7/1")),
       lty = "dotted", col = "lightgray")
abline(h = seq(from = 600, to = 1000, by = 100), lty = "dotted", col = "lightgray")

















#
