# ***************************************************
#            getNVD.r
# ***************************************************
# This R script reads vuln data from the NVD
# and computes the Workload Factor for display
# on a stock chart
#
# The timeDate library conflicts with the MySQL
# library so I am not using it here.
# ***************************************************

# Required libraries
library(gplots)     # For outputting imagem
library(gdata)      # For drop.levels
library(stats)

# library(timeDate)
# dt<-"1947-06-13"
# myFC<-"America/NewYork"
# DT<-timeDate(dt, format="%Y-%m-%d", FinCenter=myFC)
# DT
# DT + 3600*24
# foo<-DT + 3600*24
# as.character(foo)

library(RMySQL)     # For db access

# ----------------------------------------------------
# Set up filenames
# ----------------------------------------------------
# Filenames and directories to write html,data, and settings files
topDir<-"/home/ean/Desktop/amCharts/rsa-metrics/"
htmlPath<-paste(topDir,"html/",sep="")
cssPath<-paste(topDir,"css/",sep="")
dataPath<-paste(topDir,"data/",sep="")
settingsPath<-paste(topDir,"settings/",sep="")

amChartsPath<-paste("../../../amCharts/",sep="")
amChartsPathHist<-paste(amChartsPath,"amcolumn/",sep="")
amChartsPathLine<-paste(amChartsPath,"amline/",sep="")
amChartsPathXY<-paste(amChartsPath,"amxy/",sep="")
amChartsPathStock<-paste(amChartsPath,"amstock/",sep="")

# File paths to use inside html and settings files
# These are all RELATIVE to topDir/html - for use in html scripts
cssRelPath<-"../css/"
dataRelPath<-"../data/"
settingsRelPath<-"../settings/"

# Color 
primecolor<-"#6699CC"
# ----------------------------------------------------
# Get NVD data
# ----------------------------------------------------
mgr<-dbManager("MySQL")
con<-dbConnect(mgr, group="nvd")

rs2<-dbExecStatement(con, "select * from vuln;")
r2<-fetch(rs2,n = -1)

# Close out the MySQL connection
close(rs2)
close(con)
unload(mgr)

m<-format(Sys.time(),"%m")
y<-format(Sys.time(),"%Y")
q<-as.integer((as.integer(m) - 1)/3 + 1)
CurrentMON<-paste(y,m,sep="-")
CurrentQTR<-paste(y,q,sep="-")

# Set Exclusions for calculations
excludeQTR<-c(CurrentQTR,"Na","NA","na","Na-NA","?",NULL)
excludeMON<-c(CurrentMON,"Na","NA","na","Na-NA","?",NULL)

# Create time dimension columns for createDate: year, quarter, and month
year<-strftime(strptime(as.character(r2[,3]), format="%Y-%m-%d"), format="%Y")
qtr<-as.integer(((as.integer(strftime(strptime(as.character(r2[,3]), format="%Y-%m-%d"), format="%m")))-1)/3 + 1)
quarter<-paste(year,qtr,sep="-")
month<-strftime(strptime(as.character(r2[,3]), format="%Y-%m-%d"), format="%Y-%m")

day<-strftime(strptime(as.character(r2[,3]), format="%Y-%m-%d"), format="%Y-%m-%d")
dayIndex<-c(1:length(r2$published))
dayIndex[1]<-1
cvssIndex<-c(1:length(r2$published))
cvssIndex[1]<-"HIGH"
# Note: Per Steve Christey, NVD web site classifies severity as follows:  
# 0-3.9 is LOW, 
# 4-6.9 is MED, 
# 7-10 is HIGH
for (i in c(2:length(r2$published))) {
   dayIndex[i]<-1 + trunc(as.numeric( difftime(r2[i,3], r2[1,3], units="days") ))
   score<-as.numeric(as.character(r2$cvss_score[i]))
   if (score >= 0 && score < 4.0)   cvssIndex[i]<-"LOW"
   if (score >= 4.0 && score < 7.0) cvssIndex[i]<-"MED"
   if (score >= 7.0 && score <=10)  cvssIndex[i]<-"HIGH"
}
vuln<-as.data.frame(cbind(year,quarter,month,day,dayIndex,cvssIndex))
attach(vuln)
table(vuln$year)

# Dataset = Vulns after 2005
START_YEAR<-2005
ds<-as.data.frame(as.matrix(vuln[year > START_YEAR,]))
attach(ds)
names(ds)

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
