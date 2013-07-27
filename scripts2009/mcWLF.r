# ***********************************************************
# R Scripts to produce the NVD part of the Vulnerability Dashboard
# ***********************************************************
rm(list=ls())
# Load up the standard visualization libraries for Flash
source("mcLib.r")

# Required libraries
library(gplots)  # For outputting images
library(gdata)   # For drop.levels
library(stats)
library(lattice)
# library(Rcmdr)   # For Levene test for homogeneity of variance

# Set up environment
setEnv("public-metrics/","galois")
primecolor<-"#6699CC"

# Ingest and massage the NVD data sets:
#      vuln - A dataset with year, quarter, month, day, dayIndex, and cvssIndex columns
# source("getWLF.r")

# Dataset = Vulns after 2005
START_YEAR<-2005
ds<-as.data.frame(as.matrix(vuln[as.numeric(as.character(vuln$year)) > START_YEAR,]))
attach(ds)
names(ds)

# This is just cvss from getNVD.r

# YOU CAN START HERE if you JUST sourced mcNVD.r
ds<-cvss

# Make an xref between day and dayIndex
DayIndex<-as.numeric(as.character(levels(ds$dayIndex)))
Day<-levels(ds$day)
xref<-cbind(Day,DayIndex)

# Compute NVD Workload Factors:
counts.wlf<-table(vuln$dayIndex,vuln$cvssIndex,dnn=c("dayIndex","cvss"))

# day1 is dayIndex of the first record after 2005
day1<-min(as.numeric(as.character(ds$dayIndex)))
# The very first record that has dayIndex = 1 expressed in time format
# dayN is the dayIndex of the last record added to this data set
dayN<-max(as.numeric(as.character(ds$dayIndex)))

total.low<-c(day1:dayN)
total.med<-c(day1:dayN)
total.high<-c(day1:dayN)
wkf.date<-Day
wkf.value<-c(day1:dayN)
for (i in (day1:dayN)) {
   n<-i+1-day1
   total.low[n]<-sum(counts.wlf[as.numeric(row.names(counts.wlf)) > (i-30) &
                                as.numeric(row.names(counts.wlf)) <= i,2])
   total.med[n]<-sum(counts.wlf[as.numeric(row.names(counts.wlf)) > (i-30) &
                                as.numeric(row.names(counts.wlf)) <= i,3])
   total.high[n]<-sum(counts.wlf[as.numeric(row.names(counts.wlf)) > (i-30) &
                                as.numeric(row.names(counts.wlf)) <= i,1])
   foo<-xref[as.numeric(as.character(DayIndex)) == i, 1 ]
   if (length(foo)!=0) wkf.date[n]<-foo[1]

   # If there were no vuln's published on a date then it will not appear in DayIndex
   # So we need to compute the date to go along with this dayIndex.
   # We do this by adding one day to the previous day.
   if (length(foo)==0) {
       # Get the date from the previous element in the wkf.date timeseries.
       # Add one day to get a new date.
       # Then convert back to characters
       temp<-seq(as.Date(wkf.date[n-1]), by="day", length=2)[2]
       temp<-trunc(temp, units="days")
       wkf.date[n]<-as.character(temp)
   }
   wkf.value[n]<-(total.high[n]+(total.med[n]/5)+(total.low[n]/20))/30
}
# *******************************************************************
# Create amstock chart for NVD Workload Factor
# *******************************************************************
  metricName<-"WLF"
  fileName<-paste(dataPath,metricName,"-ComparisonData.csv",sep="")
  yvalues<-cbind(total.low,total.med,total.high,wkf.value)
  printEventCSVData(wkf.date,yvalues,metricName,fileName)

# Print starter dataset
#  fileName<-paste(dataPath,metricName,"-ComparisonData.csv",sep="")
#  yvalues<-cbind(volume,means[,1])
#  printEventCSVData(wkf.date,yvalues,metricName,fileName)

# Print HTML file
  flashObj<-"amstock"
  chartTitle<-"Workload Factor Index"
  htmlFileName<-paste(htmlPath,metricName,"-Comparison.html",sep="")
  dataFileName<-paste(dataRelPath,metricName,"-ComparisonData.csv",sep="")
  settingsFileName<-paste(settingsRelPath,metricName,"-ComparisonSettings.xml",sep="")
  amPrintHTML(flashObj,chartTitle,metricName,amChartsPathStock,dataFileName,settingsFileName,htmlFileName)

# ----------------- THE END ------------------------------





