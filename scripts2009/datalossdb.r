# ***********************************************************
# R Scripts to produce the DatalossDB Dashboard
# ***********************************************************
rm(list=ls())
# Load up the standard visualization libraries for Flash
source("rsaLib.r")
source("htmLib.r")
source("dldbChartsLib.r")

# Required libraries
library(gplots)  # For outputting images
library(gdata)   # For drop.levels
library(stats)
library(Rcmdr)   # For Levene test for homogeneity of variance

# Set up environment
setEnv("public-metrics/")
primecolor<-"#6699CC"

# Ingest and massage the DataLossDB data set
# Return two datasets:
#   events = raw data from DLDOS.
#   ds = massaged data after 2005
source("getDLDB.r")

# ************************************************************************
# CELL:  Create html summary of datasets:  Summary:Cell[1,2]
# ************************************************************************
# SetUp
metricName<-"DataSetsUsed"
chartTitle<-"Some Metrics"
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd")
evenColor<-"#FFFFFF"
oddColor<-"#FFFFCC"

# Derive
allRecords<-dim(events)[1]
dsRecords<-dim(ds)[1]
logRecords<-dim(ds.logTA)[1]
header<-c("DataSet","events","ds","ds.logTA")
rowCount<-c("# Rows",allRecords,dsRecords,logRecords)
rowsOmitted<-c("# Rows Omitted",0,allRecords-dsRecords, allRecords-logRecords)
pctOmitted<-c("% Rows Omitted",0,
                 100*round((allRecords-dsRecords)/allRecords,2),
                 100*round((allRecords-logRecords)/allRecords,2))
colCount<-c("# Columns",dim(events)[2],dim(ds)[2],dim(ds.logTA)[2])
values<-rbind(header,rowCount,rowsOmitted,pctOmitted,colCount)

# Deliver
top10PrintHTML(chartTitle,metricName,htmlFileName,values,rowStyles,evenColor,oddColor)

# ************************************************************************
# CELL:  Create html Top Breached by Breach Count:  Summary:Cell[2,1]
# ************************************************************************
# SetUp
chartTitle<-"Breaches Per Company"
metricName<-"BreachCount-Top10"
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven",
            "rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven")
evenColor<-"#FFFFFF"
oddColor<- "#FFC1C1"

# Derive
company.bc<-as.data.frame.table(table(events$Company))
attach(company.bc)
temp<-company.bc[Freq > 4,]
attach(temp)
Company<-as.character(temp$Var1)
BreachCount<-as.numeric(temp$Freq)

top.companies<-cbind(Company,BreachCount)
bc.shame<-top.companies[ order(-BreachCount),]
header<-c("Company","BreachCount")
values<-rbind(header,bc.shame)

# Deliver
top10PrintHTML(chartTitle,metricName,htmlFileName,values,rowStyles,evenColor,oddColor)

# ************************************************************************
# CELL:  Create html Top Breached by Breach Impact:  Summary:Cell[2,2]
# ************************************************************************
# SetUp
metricName<-"BreachImpact-Top10"
chartTitle<-"Breach Impact Per Company"
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd",
            "rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven")
evenColor<-"#FFFFFF"
oddColor<- "#FFC1C1"

# Derive
temp<-as.data.frame.table(tapply(events$TotalAffected,events$Company,sum))
Company<-as.character(temp$Var1)
TotalAffected<-round((as.numeric(temp$Freq)/1000000),0)
company.ta<-cbind(Company,TotalAffected)
ordered.ta<-company.ta[ order(-TotalAffected),]
ta.shame<-ordered.ta[1:10,]
header<-c("Company", "TotalAffected(MM)")
values<-rbind(header,ta.shame)

# Deliver
top10PrintHTML(chartTitle,metricName,htmlFileName,values,rowStyles,evenColor,oddColor)


# ************************************************************************
# CELL:  TrendLines: BreachCounts
# Law and Breach Disclosure : Cell[,2]
# ************************************************************************
# SetUp
metricName<-"BreachCountBySegment-ALL"
ylabel<-"BreachCount/Mon"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366",
               "#659933","#CC99CC","#333333","99CC00", "#FF3300","#3366CC",
               "#00CC33","#FFCC00","#FF0099","#003366","#659933","#CC99CC",
               "#333333","99CC00")

#Derive
months.seg<-table(events$month,events$Segment,exclude=excludeMON)
Total<-table(events$month,exclude=excludeMON)
values<-cbind(months.seg,Total)
Months<-rownames(values)
graphNames<-colnames(values)

#Deliver
legacyLineWidget()

# Print XML data file
fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
linePrintXMLData(Months, values, graphNames, graphColors, fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
linePrintSettings(metricName,ylabel,fileName)

# Print HTML file
flashObj<-"amline"
chartTitle<-paste("2000 to ", date())
amChartsPathXY<-paste(amChartsPath,"amline/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

# ***************************************************************************
# CELL SET:  SEGMENT ANALYSIS
# *************************************************************************
# *************************************************************************
# CELL:  Histogram:  BreachCountPercent by Segment 
# *************************************************************************
# SetUp
metricName<-"BreachCountPctBySegment"
verticalLabel<-"Market Segments"
horizontalLabel<-"% BreachCount"

# Derive
SegTotals<-table(ds.logTA$Segment)
Total<-sum(SegTotals)
y<-100*round(SegTotals/Total,2)
graphNames<-rownames(SegTotals)
x<-labels(y)[[1]]

# Print XML data file
fileName<-paste(dataPath,metricName,"-HistData.xml",sep="")
graphNames<-c(1:length(x))
graphColors<-c(1:length(x))
for (i in (1:length(x))) {
   graphNames[i]<-metricName
   graphColors[i]<-primecolor
}
legendFlag<-"false"
legendTitle<-"NONE"
colPrintXMLData(metricName,graphNames,graphColors,legendTitle,x,y,fileName)

# Print Settings file
fileName<-paste(settingsPath,metricName,"-HistSettings.xml",sep="")
rowPrintSettings(metricName,verticalLabel,horizontalLabel,legendFlag,fileName)

# Print HTML file
flashObj<-"amcolumn"
chartTitle<-paste("2006 to ", date())
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# ************************************************************************
# CELL:  TrendLines:  BreachCount by Segment
# ************************************************************************
# SetUp
metricName<-"BreachCountBySegment"
ylabel<-"BreachCount/Mon"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00",
               "#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00")

# Derive
months.seg<-table(ds.logTA$month,ds.logTA$Segment,exclude=excludeMON)
Total<-table(ds.logTA$month,exclude=excludeMON)
values<-cbind(months.seg,Total)
Months<-rownames(values)
graphNames<-colnames(values)

#Deliver
# Print XML data file
fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
linePrintXMLData(Months, values, graphNames, graphColors, fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
linePrintSettings(metricName,ylabel,fileName)

# Print HTML file
flashObj<-"amline"
chartTitle<-paste("2006 to ", date())
amChartsPathXY<-paste(amChartsPath,"amline/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

# *************************************************************************
# CELL:  Histogram:  BreachImpactPercent by Segment
# *************************************************************************
# SetUp
metricName<-"BreachImpactPctBySegment"
verticalLabel<-"Market Segments"
horizontalLabel<-"% BreachImpact (TotalAffected)"

# Derive
TA<-as.numeric(as.character(ds.logTA$TotalAffected))
SegTotals<-tapply(TA, ds.logTA$Segment, sum)
Total<-sum(SegTotals)
y<-100*round(SegTotals/Total,2)
x<-labels(y)[[1]]

# Print XML data file
fileName<-paste(dataPath,metricName,"-HistData.xml",sep="")
graphNames<-c(1:length(x))
graphColors<-c(1:length(x))
for (i in (1:length(x))) {
   graphNames[i]<-metricName
   graphColors[i]<-primecolor
}
legendFlag<-"false"
legendTitle<-"NONE"
colPrintXMLData(metricName,graphNames,graphColors,legendTitle,x,y,fileName)

# Print Settings file
fileName<-paste(settingsPath,metricName,"-HistSettings.xml",sep="")
rowPrintSettings(metricName,verticalLabel,horizontalLabel,legendFlag,fileName)

# Print HTML file
flashObj<-"amcolumn"
chartTitle<-paste("2006 to ", date())
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# ************************************************************************
# CELL:  TrendLines:  BreachImpact by Segment
# ************************************************************************
# SetUp
metricName<-"BreachImpactBySegment"
ylabel<-"Log(TotalAffected)/Mon"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00",
               "#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00")

# Derive
TA<-as.numeric(as.character(ds.logTA$TotalAffected))
months.seg<-tapply(TA, list(ds.logTA$month, ds.logTA$Segment), sum)
Total<-tapply(TA, ds.logTA$month, sum)
values<-cbind(months.seg,Total)
values<-log(values,10)
Months<-rownames(values)
graphNames<-colnames(values)

#Deliver
# Print XML data file
fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
linePrintXMLData(Months, values, graphNames, graphColors, fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
linePrintSettings(metricName,ylabel,fileName)

# Print HTML file
flashObj<-"amline"
chartTitle<-paste("2006 to ", date())
amChartsPathXY<-paste(amChartsPath,"amline/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

# *************************************************************************
# CELL SET : BREACH TYPE ANALYSIS
# *************************************************************************
# *************************************************************************
# CELL:  Histogram:  BreachCountPercent by BreachType
# *************************************************************************
# SetUp
metricName<-"BreachCountPctByType"
verticalLabel<-"Breach Types"
horizontalLabel<-"% BreachCount"

# Derive
Totals.BT<-table(ds.logTA$BT,exclude=excludeMON)
Total<-sum(Totals.BT)
y<-100*round(Totals.BT/Total,2)
x<-labels(y)[[1]]
graphNames<-colnames(values)
x<-labels(y)[[1]]

# Deliver
# Print XML data file
fileName<-paste(dataPath,metricName,"-HistData.xml",sep="")
graphNames<-c(1:length(x))
graphColors<-c(1:length(x))
for (i in (1:length(x))) {
   graphNames[i]<-metricName
   graphColors[i]<-primecolor
}
legendFlag<-"false"
legendTitle<-"NONE"
colPrintXMLData(metricName,graphNames,graphColors,legendTitle,x,y,fileName)

# Print Settings file
fileName<-paste(settingsPath,metricName,"-HistSettings.xml",sep="")
rowPrintSettings(metricName,verticalLabel,horizontalLabel,legendFlag,fileName)

# Print HTML file
flashObj<-"amcolumn"
chartTitle<-paste("2006 to ", date())
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# ************************************************************************
# CELL:  TrendLines:  BreachCount by Breach Type
# ************************************************************************
# SetUp
metricName<-"BreachCountByType"
ylabel<-"BreachCount/Mon"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00",
               "#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00")

# Derive
months.bt<-table(ds.logTA$month,ds.logTA$BT,exclude=excludeMON)
Total<-table(ds.logTA$month,exclude=excludeMON)
values<-cbind(months.bt,Total)
Months<-rownames(values)
graphNames<-colnames(values)

#Deliver
# Print XML data file
fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
linePrintXMLData(Months, values, graphNames, graphColors, fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
linePrintSettings(metricName,ylabel,fileName)

# Print HTML file
flashObj<-"amline"
chartTitle<-paste("2006 to ", date())
amChartsPathXY<-paste(amChartsPath,"amline/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

# *************************************************************************
# CELL:  Histogram:  BreachImpactPercent by Breach Type
# *************************************************************************
# SetUp
metricName<-"BreachImpactPctByType"
verticalLabel<-"BreachTypes"
horizontalLabel<-"% BreachImpact(TotalAffected)"

# Derive
TA<-as.numeric(as.character(ds.logTA$TotalAffected))
TypeTotals<-tapply(TA, ds.logTA$BT, sum)
Total<-sum(TypeTotals)
y<-100*round(TypeTotals/Total,2)
x<-labels(y)[[1]]

# Print XML data file
fileName<-paste(dataPath,metricName,"-HistData.xml",sep="")
graphNames<-c(1:length(x))
graphColors<-c(1:length(x))
for (i in (1:length(x))) {
   graphNames[i]<-metricName
   graphColors[i]<-primecolor
}
legendFlag<-"false"
legendTitle<-"NONE"
colPrintXMLData(metricName,graphNames,graphColors,legendTitle,x,y,fileName)

# Print Settings file
fileName<-paste(settingsPath,metricName,"-HistSettings.xml",sep="")
rowPrintSettings(metricName,verticalLabel,horizontalLabel,legendFlag,fileName)

# Print HTML file
flashObj<-"amcolumn"
chartTitle<-paste("2006 to ", date())
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# ************************************************************************
# CELL:  TrendLines:  BreachImpact by Breach Type
# ************************************************************************
# SetUp
metricName<-"BreachImpactByType"
ylabel<-"Log(TotalAffected)/Mon"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00",
               "#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00")

# Derive
TA<-as.numeric(as.character(ds.logTA$TotalAffected))
months.BT<-tapply(TA, list(ds.logTA$month, ds.logTA$BT), sum)
Total<-tapply(TA, ds.logTA$month, sum)
values<-cbind(months.BT,Total)
values<-log(values,10)
Months<-rownames(values)
graphNames<-colnames(values)

#Deliver
# Print XML data file
fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
linePrintXMLData(Months, values, graphNames, graphColors, fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
linePrintSettings(metricName,ylabel,fileName)

# Print HTML file
flashObj<-"amline"
chartTitle<-paste("2006 to ", date())
amChartsPathXY<-paste(amChartsPath,"amline/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

# *************************************************************************
# CELL SET:  DATALOST TYPE ANALYSIS
# *************************************************************************
# *************************************************************************
# CELL:  Histogram:  BreachCountPercent by DataLost
# *************************************************************************
# SetUp
metricName<-"BreachCountPctByDataLost"
verticalLabel<-"DataLost Types"
horizontalLabel<-"% BreachCount"

# Derive
# Get subtotals by DataLost (e.g. ACC, CCN, etc)
# Calculate number of breachs in which various types of data were lost
# Note that since breaches have more than one type of lost data, the percenters will total to more than 100.
# The mc dataset is the ds.logTA dataset with DataLost expanded...one row per type of DataLost

foo<-mc[mc$Origin=="MC",]
foo<-drop.levels(foo)
values<-table(foo$DataLost,exclude=excludeMON)
total<-dim(ds.logTA)[1]
y<-100*round(values/total,2)
x<-rownames(values)

# Print XML data file
fileName<-paste(dataPath,metricName,"-HistData.xml",sep="")
graphNames<-c(1:length(x))
graphColors<-c(1:length(x))
for (i in (1:length(x))) {
   graphNames[i]<-metricName
   graphColors[i]<-primecolor
}
legendFlag<-"false"
legendTitle<-"NONE"
colPrintXMLData(metricName,graphNames,graphColors,legendTitle,x,y,fileName)

# Print Settings file
fileName<-paste(settingsPath,metricName,"-HistSettings.xml",sep="")
rowPrintSettings(metricName,verticalLabel,horizontalLabel,legendFlag,fileName)

# Print HTML file
flashObj<-"amcolumn"
chartTitle<-paste("2006 to ", date())
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# ************************************************************************
# CELL:  TrendLines:  BreachCount by DataLost
# ************************************************************************
# SetUp
metricName<-"BreachCountByDataLost"
ylabel<-"BreachCount/Mon"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00",
               "#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00")

# Derive
foo<-mc[mc$Origin=="MC",]
foo<-drop.levels(foo)
values<-table(foo$month,foo$DataLost,exclude=excludeMON)
Months<-rownames(values)
graphNames<-colnames(values)

#Deliver
# Print XML data file
fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
linePrintXMLData(Months, values, graphNames, graphColors, fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
linePrintSettings(metricName,ylabel,fileName)

# Print HTML file
flashObj<-"amline"
chartTitle<-paste("2006 to ", date())
amChartsPathXY<-paste(amChartsPath,"amline/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

# *************************************************************************
# CELL:  Histogram:  BreachImpactPercent by DataLost
# *************************************************************************
# SetUp
metricName<-"BreachImpactPctByDataLost"
verticalLabel<-"Market Segments"
horizontalLabel<-"% BreachImpact (TotalAffected)"

# Derive
TA<-as.numeric(as.character(ds.logTA$TotalAffected))
DataLostTotals<-tapply(TA, ds.logTA$DL, sum)
Total<-sum(SegTotals)
y<-100*round(SegTotals/Total,2)
x<-labels(y)[[1]]

# Print XML data file
fileName<-paste(dataPath,metricName,"-HistData.xml",sep="")
graphNames<-c(1:length(x))
graphColors<-c(1:length(x))
for (i in (1:length(x))) {
   graphNames[i]<-metricName
   graphColors[i]<-primecolor
}
legendFlag<-"false"
legendTitle<-"NONE"
colPrintXMLData(metricName,graphNames,graphColors,legendTitle,x,y,fileName)

# Print Settings file
fileName<-paste(settingsPath,metricName,"-HistSettings.xml",sep="")
rowPrintSettings(metricName,verticalLabel,horizontalLabel,legendFlag,fileName)

# Print HTML file
flashObj<-"amcolumn"
chartTitle<-paste("2006 to ", date())
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# ************************************************************************
# CELL:  TrendLines:  BreachImpact by DataLost
# ************************************************************************
# SetUp
metricName<-"BreachImpactByDataLost"
ylabel<-"Log(TotalAffected)/Mon"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00",
               "#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00")

# Derive
TA<-as.numeric(as.character(foo$TotalAffected))
foo<-mc[mc$Origin=="MC",]
foo<-drop.levels(foo)
values<-tapply(TA, list(foo$month, foo$DataLost), sum)
values<-log(values,10)
Months<-rownames(values)
graphNames<-colnames(values)

#Deliver
# Print XML data file
fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
linePrintXMLData(Months, values, graphNames, graphColors, fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
linePrintSettings(metricName,ylabel,fileName)

# Print HTML file
flashObj<-"amline"
chartTitle<-paste("2006 to ", date())
amChartsPathXY<-paste(amChartsPath,"amline/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

# *************************************************************************
# CELL SET:  SOURCE ANALYSIS
# *************************************************************************
# *************************************************************************
# CELL:  Histogram:  BreachCountPercent by Source
# *************************************************************************
# SetUp
metricName<-"BreachCountPctBySource"
verticalLabel<-"Breach Types"
horizontalLabel<-"% BreachCount"

# Derive
Totals.SRC<-table(ds.logTA$SRC,exclude=excludeMON)
Total<-sum(Totals.SRC)
y<-100*round(Totals.SRC/Total,2)
graphNames<-colnames(values)
x<-labels(y)[[1]]

# Print XML data file
fileName<-paste(dataPath,metricName,"-HistData.xml",sep="")
graphNames<-c(1:length(x))
graphColors<-c(1:length(x))
for (i in (1:length(x))) {
   graphNames[i]<-metricName
   graphColors[i]<-primecolor
}
legendFlag<-"false"
legendTitle<-"NONE"
colPrintXMLData(metricName,graphNames,graphColors,legendTitle,x,y,fileName)

# Print Settings file
fileName<-paste(settingsPath,metricName,"-HistSettings.xml",sep="")
rowPrintSettings(metricName,verticalLabel,horizontalLabel,legendFlag,fileName)

# Print HTML file
flashObj<-"amcolumn"
chartTitle<-paste("2006 to ", date())
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# ************************************************************************
# CELL:  TrendLines:  BreachCount by Source
# ************************************************************************
# SetUp
metricName<-"BreachCountBySource"
ylabel<-"BreachCount/Mon"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00",
               "#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00")

# Derive
months.SRC<-table(ds.logTA$month,ds.logTA$SRC,exclude=excludeMON)
Total<-table(ds.logTA$month,exclude=excludeMON)
values<-cbind(months.SRC,Total)
Months<-rownames(values)
graphNames<-colnames(values)

#Deliver
# Print XML data file
fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
linePrintXMLData(Months, values, graphNames, graphColors, fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
linePrintSettings(metricName,ylabel,fileName)

# Print HTML file
flashObj<-"amline"
chartTitle<-paste("2006 to ", date())
amChartsPathXY<-paste(amChartsPath,"amline/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

# *************************************************************************
# CELL:  Histogram:  BreachImpactPercent by Source
# *************************************************************************
# SetUp
metricName<-"BreachImpactPctBySource"
verticalLabel<-"Breach Sources"
horizontalLabel<-"% BreachImpact (TotalAffected)"

# Derive
TA<-as.numeric(as.character(ds.logTA$SRC))
SourceTotals<-tapply(TA, ds.logTA$SRC, sum)
Total<-sum(SourceTotals)
y<-100*round(SourceTotals/Total,2)
x<-labels(y)[[1]]

# Print XML data file
fileName<-paste(dataPath,metricName,"-HistData.xml",sep="")
graphNames<-c(1:length(x))
graphColors<-c(1:length(x))
for (i in (1:length(x))) {
   graphNames[i]<-metricName
   graphColors[i]<-primecolor
}
legendFlag<-"false"
legendTitle<-"NONE"
colPrintXMLData(metricName,graphNames,graphColors,legendTitle,x,y,fileName)

# Print Settings file
fileName<-paste(settingsPath,metricName,"-HistSettings.xml",sep="")
rowPrintSettings(metricName,verticalLabel,horizontalLabel,legendFlag,fileName)

# Print HTML file
flashObj<-"amcolumn"
chartTitle<-paste("2006 to ", date())
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# ************************************************************************
# CELL:  TrendLines:  BreachImpact by Source
# ************************************************************************
# SetUp
metricName<-"BreachImpactBySource"
ylabel<-"Log(TotalAffected)/Mon"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00",
               "#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00")

# Derive
TA<-as.numeric(as.character(ds.logTA$TotalAffected))
values<-tapply(TA, list(ds.logTA$month, ds.logTA$SRC), sum)
values<-log(values,10)
Months<-rownames(values)
graphNames<-colnames(values)

#Deliver
# Print XML data file
fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
linePrintXMLData(Months, values, graphNames, graphColors, fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
linePrintSettings(metricName,ylabel,fileName)

# Print HTML file
flashObj<-"amline"
chartTitle<-paste("2006 to ", date())
amChartsPathXY<-paste(amChartsPath,"amline/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

