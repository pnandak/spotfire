######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  12-19-06                                                    #
# PURPOSE: Reproduce example 1.25 of Shumway and Stoffer (2006)      #
#                                                                    #
# NOTES:                                                             #
######################################################################

soi<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\Shumway_Stoffer_web_info\\Data\\soi.dat", 
                header=FALSE, col.names = "soi.var")
rec<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\Shumway_Stoffer_web_info\\Data\\recruit.dat", 
                header=FALSE, col.names = "rec.var")
head(soi)
head(rec)

win.graph(width = 8, height = 6, pointsize = 10)  #Opens up wider plot window than the default (good for time series plots)
soi.rec.acf<-acf(x = cbind(soi,rec), type = "correlation", lag = 50)
soi.rec.acf
ccf(x = soi, y = rec, type = "correlation", main = "Part of Figure 1.14", lag = 50)
ccf(x = rec, y = soi, type = "correlation", main = "Reverse", lag = 50)



#Also could convert data to a ts class first
soi.rec<-ts.intersect(soi = ts(soi), rec = ts(rec))
acf(soi.rec)















#
