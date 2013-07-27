#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-17-06                                                   #
# PURPOSE: Chapter 1 OSU data                                       #
#                                                                   #
# NOTES:                                                            #
#####################################################################


library(RODBC)
z<-odbcConnectExcel("C:\\chris\\UNL\\STAT_time_series\\chapter1\\osu_enroll.xls")
osu.enroll<-sqlFetch(z, "Sheet1")
close(z)

head(osu.enroll)
tail(osu.enroll)


#One way to do plot
win.graph(width = 8, height = 6, pointsize = 10) #width = 6 is default so this makes it wider
plot(x = osu.enroll$Enrollment, ylab = "OSU Enrollment", xlab = "t (time)", type = "l", col = "red", 
        main = "OSU Enrollment from Fall 1989 to Fall 2002", panel.first=grid(col = "gray", lty = "dotted"))
points(x = osu.enroll$Enrollment, pch = 20, col = "blue")


#Another way to do plot with actual dates
plot(y = osu.enroll$Enrollment, x = as.Date(osu.enroll$date), xlab = "Time", ylab = "OSU Enrollment", type = "l", col = "red", 
        main = "OSU Enrollment from Fall 1989 to Fall 2002")
points(y = osu.enroll$Enrollment, x = as.Date(osu.enroll$date), pch = 20, col = "blue")
#Create own gridlines
abline(v = as.Date(c("1990/1/1", "1992/1/1", "1994/1/1", "1996/1/1", "1998/1/1", "2000/1/1", "2002/1/1")),
       lty = "dotted", col = "lightgray")
abline(h = c(10000, 15000, 20000), lty = "dotted", col = "lightgray")
#There may be better ways to work with actual dates.  Students: If you find a way, please let me know!


#More complicated plot
plot(y = osu.enroll[osu.enroll$Semester == "Fall",]$Enrollment, x = osu.enroll[osu.enroll$Semester == "Fall",]$t, 
     ylab = "OSU Enrollment", xlab = "t (time)", col = "blue", main = "OSU Enrollment from Fall 1989 to Fall 2002", 
      panel.first=grid(col = "gray", lty = "dotted"), pch = 1, type = "o", ylim = c(0,
      max(osu.enroll$Enrollment)))
lines(y = osu.enroll[osu.enroll$Semester == "Spring",]$Enrollment, x = osu.enroll[osu.enroll$Semester == "Spring",]$t, 
      col = "red", type = "o", pch = 2)
lines(y = osu.enroll[osu.enroll$Semester == "Summer",]$Enrollment, x = osu.enroll[osu.enroll$Semester == "Spring",]$t, 
      col = "darkgreen", type = "o", pch = 3)
legend(x = locator(1),legend=c("Fall", "Spring", "Summer"), pch = c(1,2,3), lty = c(1,1,1), col=c("blue", "red", "darkgreen"), bty="n")


#Maybe could use the its package for irregular time series to help with dates?
#library(its)


#Section 1.6
x<-osu.enroll$Enrollment
rho.x<-acf(x = x, type = "correlation", main = "OSU Enrollment series")
rho.x
rho.x$acf[1:9]
  
