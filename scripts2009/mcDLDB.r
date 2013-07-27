# ***********************************************************
# R Scripts to produce the DatalossDB Dashboard
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

# Ingest and massage the DataLossDB data set
# Return two datasets:
#   events = raw data from DLDOS.
#   ds = massaged data after 2005
#   ds.logTA = ds such that TA > 0
#   mc = ds.logTA replicated for multiple DL types
source("getDLDB.r")

# ************************************************************************
# CELL:  Table: Summary of datasets:  
# ************************************************************************
# SetUp
metricName<-"DLDB-DataSetsUsed"
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd")
evenColor<-"#FFFFFF"
oddColor<-"#FFFFCC"

# Derive
allRecords<-dim(events)[1]
dsRecords<-dim(ds)[1]
logRecords<-dim(ds.logTA)[1]
header<-c("DataSet","events","ds","ds.logTA")
comment<-c("Description",
           "DataLossDB",
           "Year > 2005",
           "Year < 2005 & TotalAffected > 0")
rowCount<-c("# Rows",allRecords,dsRecords,logRecords)
rowsOmitted<-c("# Rows Omitted",0,allRecords-dsRecords, allRecords-logRecords)
pctOmitted<-c("% Rows Omitted",0,
                 100*round((allRecords-dsRecords)/allRecords,2),
                 100*round((allRecords-logRecords)/allRecords,2))
colCount<-c("# Columns",dim(events)[2],dim(ds)[2],dim(ds.logTA)[2])
values<-rbind(header,comment,rowCount,rowsOmitted,pctOmitted,colCount)

# Deliver
top10PrintHTML(metricName,htmlFileName,values,rowStyles,evenColor,oddColor)

# ************************************************************************
# CELL:  Table: Top Breached by Breach Count:  Summary:Cell[2,1]
# ************************************************************************
# SetUp
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
top10PrintHTML(metricName,htmlFileName,values,rowStyles,evenColor,oddColor)

# ************************************************************************
# CELL:  Table:  Top Breached by Breach Impact:  Summary:Cell[2,2]
# ************************************************************************
# SetUp
metricName<-"BreachImpact-Top10"
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
top10PrintHTML(metricName,htmlFileName,values,rowStyles,evenColor,oddColor)


# ************************************************************************
# CELL:  Histogram:  Breach Count Distribution
# ************************************************************************
# SetUp
fileName<-"images/BreachCountPerMon-Hist.png"
title<-paste("DatalossDB: Histogram of BreachCount/Month\nds.logTA: 1Q06 to ",date())
xlabel<-"Ranges of BreachCount/Month"
ylabel<-"# Months"

# Derive
values<-table(ds.logTA$month, exclude=excludeMON)
bc.min<-min(values)
bc.mean<-mean(values)
bc.max<-max(values)
bc.sd<-sqrt(var(values))
bc.stdErr<-sqrt((var(values)/length(values)))

parms<-hist(values,plot=F)
x.max<-max(parms$breaks)
h.max<-max(parms$counts)+4
area<-sum(parms$counts)*(x.max/length(parms$mids))

# Deliver
png(file=fileName,bg="white",width=350,height=350)
hist(values,xlab=xlabel, ylab=ylabel, main=title, labels=TRUE, ylim=c(0,h.max), col="#6699CC")
xv<-seq(bc.min,bc.max,0.1)
yv<-dnorm(xv,mean=bc.mean,sd=bc.sd)*area
lines(xv,yv,col="red",lwd=3)
dev.off()

# ************************************************************************
# CELL:  Histogram:  Breach Impact Distribution
# ************************************************************************
# SetUp
fileName<-"images/BreachImpact-Hist.png"
title<-paste("DatalossDB: log10(TotalAffected) Histogram\nds.logTA: 1Q06 to ",date())
xlabel<-"log10(TotalAffected)/Breach"
ylabel<-"# Breaches"

# Derive
logy<-as.numeric(as.character(ds.logTA$logTA))
ta.min=min(logy)
ta.mean=mean(logy)
ta.max=max(logy)
ta.sd=sqrt(var(logy))

parms<-hist(logy,plot=F)
x.max<-max(parms$breaks)
area<-sum(parms$counts)*(x.max/length(parms$mids))

png(file=fileName,width=350,height=350,bg="white")
hist(logy, col="#6699CC", xlab=xlabel, ylab=ylabel, main=title)
xv<-seq(0,x.max,0.1)
yv<-dnorm(xv,mean=ta.mean,sd=ta.sd)*area
lines(xv,yv,col="red",lwd=3)
dev.off()

# ************************************************************************
# CELL:  TrendLines: BreachCounts - ALL
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
legacyLineWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, values, graphNames, graphColors, ylabel) 

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

# Deliver
legacyColWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, x, y, horizontalLabel, verticalLabel) 

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
legacyLineWidget(dataPath,dataRelPath,settingsPath,settingsRelPath,htmlPath,
                 amChartsPathXY,metricName, Months, values, graphNames, 
                 graphColors, ylabel) 

# ************************************************************************
# CELL:  WhiskerPlots:  BreachCount by Segment
# ************************************************************************
# SetUp
fileName<-"images/BreachCountPerMonBySegment.png"
xlabel<-"Segment"
ylabel<-"BreachCount/Month"
title<-paste("DatalossDB: BreachCount/Mon by Segment\nds.logTA: 1Q06 to ",date())

# Derive
values<-as.data.frame(table(ds.logTA$month,ds.logTA$Segment,exclude=excludeMON))
names(values)<-c("Month","Segment","BreachCount")

# Deliver
png(file=fileName ,bg="white",width=350,height=350)
boxplot(values$BreachCount ~ values$Segment,notch=T,xlab=xlabel ,ylab=ylabel,
     main=title, col="#6699CC")
dev.off()

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

# Deliver
legacyColWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, x, y, horizontalLabel, verticalLabel) 

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
legacyLineWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, values, graphNames, graphColors, ylabel) 

# ************************************************************************
# CELL:  WhiskerPlots:  BreachImpactPerMonth by Segment
# ************************************************************************
# SetUp
fileName<-"images/BreachImpactPerMonBySegment.png"
xlabel<-"Segment"
ylabel<-"log10(TotalAffected)"
title<-paste("DatalossDB: BreachImpact/Mon by Segment\nds.logTA: 1Q06 to ",date())

# Derive
lastMonth<-excludeMON[1]
foo<-ds.logTA[ ds.logTA$month != lastMonth, ]
logy<-as.numeric(as.character(foo$logTA))

# Deliver
png(file=fileName,bg="white",width=350,height=350)
boxplot(split(logy,factor(foo$Segment)), notch=T, 
        main=title, ylab=ylabel,border="#0000CC", col="#6699CC")
dev.off()

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
legacyColWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, x, y, horizontalLabel, verticalLabel) 

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
legacyLineWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, values, graphNames, graphColors, ylabel) 

# ************************************************************************
# CELL:  WhiskerPlots:  BreachCount by BreachType
# ************************************************************************
# SetUp
fileName<-"images/BreachCountPerMonByBreachType.png"
xlabel<-"BreachType"
ylabel<-"BreachCount/Month"
title<-paste("DatalossDB: BreachCount/Mon by BreachType\nds.logTA: 1Q06 to ",date())

# Derive
values<-as.data.frame(table(ds.logTA$month,ds.logTA$BT,exclude=excludeMON))
names(values)<-c("Month","BreachType","BreachCount")

# Deliver
png(file=fileName,bg="white",width=900,height=350)
boxplot(values$BreachCount ~ values$BreachType,notch=T,
     xlab=xlabel,ylab=ylabel,main=title,col="#6699CC")
dev.off()


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

# Deliver
legacyColWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, x, y, horizontalLabel, verticalLabel) 

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
legacyLineWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, values, graphNames, graphColors, ylabel) 

# ************************************************************************
# CELL:  WhiskerPlots:  BreachImpactPerMonth by BreachType
# ************************************************************************
# SetUp
fileName<-"images/BreachImpactPerMonByBreachType.png"
xlabel<-"BreachType"
ylabel<-"log10(TotalAffected)"
title<-paste("DatalossDB: BreachImpact/Mon by BreachType\nds.logTA: 1Q06 to ",date())

# Derive
lastMonth<-excludeMON[1]
foo<-ds.logTA[ ds.logTA$month != lastMonth, ]
logy<-as.numeric(as.character(foo$logTA))

# Deliver
png(file=fileName,bg="white",width=900,height=350)
boxplot(split(logy,factor(foo$BT)), notch=T,
        main=title, ylab=ylabel,border="#0000CC", col="#6699CC")
dev.off()

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

# Deliver
legacyColWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, x, y, horizontalLabel, verticalLabel) 

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
legacyLineWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, values, graphNames, graphColors, ylabel) 

# ************************************************************************
# CELL:  WhiskerPlots:  BreachCount by DataLost
# ************************************************************************
# SetUp
fileName<-"images/BreachCountPerMonByDataLost.png"
xlabel<-"DataLost"
ylabel<-"BreachCount/Month"
title<-paste("DatalossDB: BreachCount/Mon by DataLost\nds.logTA: 1Q06 to ",date())

# Derive
foo<-mc[mc$Origin=="MC",]
foo<-drop.levels(foo)
values<-as.data.frame(table(foo$month,foo$DataLost), exclude=excludeMON)
names(values)<-c("Month","BreachType","BreachCount")

# Deliver
png(file=fileName,bg="white",width=900,height=350)
boxplot(values$BreachCount ~ values$BreachType,notch=T,
     xlab=xlabel,ylab=ylabel,main=title,col="#6699CC")
dev.off()

# *************************************************************************
# CELL:  Histogram:  BreachImpactPercent by DataLost
# *************************************************************************
# SetUp
metricName<-"BreachImpactPctByDataLost"
verticalLabel<-"DataLost Types"
horizontalLabel<-"% BreachImpact (TotalAffected)"

# Derive
foo<-mc[mc$Origin=="MC",]
foo<-drop.levels(foo)
TA<-as.numeric(as.character(foo$TotalAffected))
values<-tapply(TA,foo$DataLost,sum)
Total<-sum(values)
y<-100*round(values/Total,2)
x<-labels(y)[[1]]

# Deliver
legacyColWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, x, y, horizontalLabel, verticalLabel) 

# ************************************************************************
# CELL:  TrendLines:  BreachImpact by DataLost
# ************************************************************************
# SetUp
metricName<-"BreachImpactByDataLost"
ylabel<-"Log(TotalAffected)/Mon"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00",
               "#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00")

# Derive
foo<-mc[mc$Origin=="MC",]
foo<-drop.levels(foo)
TA<-as.numeric(as.character(foo$TotalAffected))
values<-tapply(TA, list(foo$month, foo$DataLost), sum)
values<-log(values,10)
Months<-rownames(values)
graphNames<-colnames(values)

#Deliver
legacyLineWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, values, graphNames, graphColors, ylabel) 

# ************************************************************************
# CELL:  WhiskerPlots:  BreachImpactPerMonth by DataLost
# ************************************************************************
# SetUp
fileName<-"images/BreachImpactPerMonByDataLost.png"
xlabel<-"DataLost"
ylabel<-"log10(TotalAffected)"
title<-paste("DatalossDB: BreachImpact/Mon by DataLost\nds.logTA: 1Q06 to ",date())

# Derive
foo1<-mc[mc$Origin=="MC",]
foo1<-drop.levels(foo1)
lastMonth<-excludeMON[1]
foo<-foo1[ foo1$month != lastMonth, ]
logy<-as.numeric(as.character(foo$logTA))

# Deliver
png(file=fileName,bg="white",width=900,height=350)
boxplot(split(logy,factor(foo$DataLost)), notch=T,
        main=title, ylab=ylabel,border="#0000CC", col="#6699CC")
dev.off()

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

# Deliver
legacyColWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, x, y, horizontalLabel, verticalLabel) 

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

# Deliver
legacyLineWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, values, graphNames, graphColors, ylabel) 

# ************************************************************************
# CELL:  WhiskerPlots:  BreachCount by Source
# ************************************************************************
# SetUp
fileName<-"images/BreachCountPerMonBySource.png"
xlabel<-"Source"
ylabel<-"BreachCount/Month"
title<-paste("DatalossDB: BreachCount/Mon by Source\nds.logTA: 1Q06 to ",date())

# Derive
values<-as.data.frame(table(ds.logTA$month,ds.logTA$SRC,exclude=excludeMON))
names(values)<-c("Month","Source","BreachCount")

# Deliver
png(file=fileName ,bg="white",width=350,height=350)
boxplot(values$BreachCount ~ values$Source,notch=T,xlab=xlabel ,ylab=ylabel,
     main=title, col="#6699CC")
dev.off()


# *************************************************************************
# CELL:  Histogram:  BreachImpactPercent by Source
# *************************************************************************
# SetUp
metricName<-"BreachImpactPctBySource"
verticalLabel<-"Breach Sources"
horizontalLabel<-"% BreachImpact (TotalAffected)"

# Derive
TA<-as.numeric(as.character(ds.logTA$TotalAffected))
SourceTotals<-tapply(TA, ds.logTA$SRC, sum)
Total<-sum(SourceTotals)
y<-100*round(SourceTotals/Total,2)
x<-labels(y)[[1]]

# Deliver
legacyColWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, x, y, horizontalLabel, verticalLabel) 

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
legacyLineWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                 metricName, Months, values, graphNames, graphColors, ylabel) 

# ************************************************************************
# CELL:  WhiskerPlots:  BreachImpactPerMonth by Source
# ************************************************************************
# SetUp
fileName<-"images/BreachImpactPerMonBySource.png"
xlabel<-"Source"
ylabel<-"log10(TotalAffected)"
title<-paste("DatalossDB: BreachImpact/Mon by Source\nds.logTA: 1Q06 to ",date())

# Derive
lastMonth<-excludeMON[1]
foo<-ds.logTA[ ds.logTA$month != lastMonth, ]
logy<-as.numeric(as.character(foo$logTA))

# Deliver
png(file=fileName,bg="white",width=350,height=350)
boxplot(split(logy,factor(foo$SRC)), notch=T,
        main=title, ylab=ylabel,border="#0000CC", col="#6699CC")
dev.off()

# **************************************************************************
# CELL:  Pairwise Independence Heatmaps (ChiSquare)
# CELL       Segment by SRC
# CELL       Segment by BT
# CELL       Segment by Year
# CELL       SRC by BT
# CELL       SRC by Year
# CELL       BT by Year
# **************************************************************************
# SetUp
chi.classes<-  c("none","conf95","conf99","conf999")
# toDoList Cols: metricName, ds.logTA$dim1, ds.logTA$dim2, result$dim1, result$dim2
toDoList<-               c("BreachCountInd-SegSrc", 7,26,  2,3)
toDoList<-rbind(toDoList,c("BreachCountInd-BTSeg" ,25, 7,  1,2))
toDoList<-rbind(toDoList,c("BreachCountInd-YrSeg" ,1 , 7,  2,4))
toDoList<-rbind(toDoList,c("BreachCountInd-BTSrc" ,25,26,  1,3))
toDoList<-rbind(toDoList,c("BreachCountInd-YrSrc" , 1,26,  3,4))
toDoList<-rbind(toDoList,c("BreachCountInd-BTYr"  ,25, 1,  1,4))

sum.results<-matrix(0,nrow=4,ncol=4, byrow=TRUE,
             dimnames=list(c("BreachType","Segment","Source","Year"),
                           c("BreachType","Segment","Source","Year")))
sum.styles<-matrix(0,nrow=4,ncol=4)
# Make a heatmap for each item in the toDoList
N<-dim(toDoList)[1]
for (k in (1:N)) { 
    metricName<-toDoList[k,1]
    i<-as.numeric(toDoList[k,2])
    j<-as.numeric(toDoList[k,3])
    si<-as.numeric(toDoList[k,4])
    sj<-as.numeric(toDoList[k,5])

    # Derive
    chiResults<-twoWay(ds.logTA[,j],ds.logTA[,i])
    chi.values<-chiResults$ChiTable
    chiStat<-sum(chi.values)
    R<-dim(chi.values)[1]
    C<-dim(chi.values)[2]

    # ChiSquare Cutpoints
    df<-(R-1)*(C-1)
    chi95<-qchisq(0.95,df)
    chi99<-qchisq(0.99,df)
    chi999<-qchisq(0.999,df)
    chiCell95<-chi95/(C*R)
    chiCell99<-chi99/(C*R)
    chiCell999<-chi999/(C*R)

    chi.cutpoints<-c( 0, chiCell95,  chiCell99,  chiCell999, max(chi.values))
    chi.values<-round(chi.values,2)
    chi.styles<-symnum(chi.values, cutpoints=chi.cutpoints,symbols=chi.classes, 
              legend=FALSE,abbr.colnames=FALSE,lower.triangular=FALSE)
    chi.delta<-chiResults$Observed[1:R,1:C] - chiResults$Expected

    # Load up values in summary table
    score<-sum(chi.values)
    sum.results[si,sj]<-score
    sum.results[sj,si]<-score
    sum.results[si,si]<-0
    sum.results[sj,sj]<-0
    # Set summary cell color
    sum.cutpoints<-c(0,chi95,chi99,chi999,100000000) 
    if (     0  <= score && score < chi95 )  { style<-chi.classes[1] }
    if ( chi95  <= score && score < chi99 )  { style<-chi.classes[2] }
    if ( chi99  <= score && score < chi999 ) { style<-chi.classes[3] }
    if ( chi999 <= score ) { style<-chi.classes[4] }
    sum.styles[si,sj]<-style
    sum.styles[sj,si]<-style

    # Deliver
    # Print out heatmap 
    htmlFileName<-paste(htmlPath,metricName,"-chi.html",sep="")
    hmPrintHTML(metricName,htmlFileName,chi.delta,chi.styles)
}

# Write out summary table
metricName<-"BreachCountInd-Summary"
N<-dim(sum.styles)[1]
for (i in (1:N)) { sum.styles[i,i] = "na" }

htmlFileName<-paste(htmlPath,metricName,"-chi.html",sep="")
# dataFileName<-paste(dataRelPath,metricName,"-chiData.xml",sep="")
hmPrintHTML(metricName,htmlFileName,sum.results,sum.styles)

# ***********************************************************************
# NOTE: BT vs DL Independence Heatmap:  NOT AVAIL.  Fails ChiSquare rqts
# ***********************************************************************

# ***********************************************************************
# CELL: Correlation Heatmaps:  BreachCount/Mon  
# CELL      Segment by Src
# CELL      Segment by BT
# CELL      Source by BT 
# ***********************************************************************
# SetUp
cor.classes<-  c("noc","low","med","hi","vhi")
cor.cutpoints<-c( 0.3,  0.6,  0.75,  0.9)

chi.classes<-  c("none","conf95","conf99","conf999")
toDoList<-               c("BreachCountCor-SegSrc",7,26)
toDoList<-rbind(toDoList,c("BreachCountCor-SegBT" ,7,25))
toDoList<-rbind(toDoList,c("BreachCountCor-SrcBT" ,26,25))

# Make a heatmap for each item in the toDoList
N<-dim(toDoList)[1]
for (k in (1:N)) {
    metricName<-toDoList[k,1]
    i<-as.numeric(toDoList[k,2])
    j<-as.numeric(toDoList[k,3])

    # Derive
    months.one<-table(ds.logTA$month,ds.logTA[,j],exclude=excludeMON)
    months.two<-table(ds.logTA$month,ds.logTA[,i],exclude=excludeMON)

    cor.values<-round(cor(months.one,months.two),2)
    cor.styles<-symnum(cor.values,
          cutpoints<-cor.cutpoints,symbols=cor.classes,corr=TRUE,
          legend=FALSE,abbr.colnames=FALSE,lower.triangular=FALSE)

    # Deliver
    # Print out heatmap
    htmlFileName<-paste(htmlPath,metricName,"-cor.html",sep="")
    hmPrintHTML(metricName,htmlFileName,cor.values,cor.styles)
}

# ***********************************************************************
# CELL: Correlation Heatmap:  BreachCount/Mon BT vs DL
# ***********************************************************************
# SetUp
metricName<-"BreachCountCor-BTvsDL"
# CSS classes
cor.classes<-  c("noc","low","med","hi","vhi")
cor.cutpoints<-c( 0.3,  0.6,  0.75,  0.9)

# Derive
months.bt<-table(ds.logTA$month,ds.logTA$BT,exclude=excludeMON)

foo<-mc[mc$Origin=="MC",]
foo<-drop.levels(foo)
months.dl<-table(foo$month,foo$DataLost,exclude=excludeMON)

cor.values<-round(cor(months.bt,months.dl),2)
cor.styles<-symnum(cor.values,
          cutpoints<-cor.cutpoints,symbols=cor.classes,corr=TRUE,
          legend=FALSE,abbr.colnames=FALSE,lower.triangular=FALSE)

# Deliver
# Print out heatmap
htmlFileName<-paste(htmlPath,metricName,"-cor.html",sep="")
hmPrintHTML(metricName,htmlFileName,cor.values,cor.styles)

# ***********************************************************************
# CELL: Small Multiples: Histograms  logTA by BT
# ***********************************************************************
# histogram( ~  logy |  ds.logTA$BT, type="density")

# ***********************************************************************
# CELL: Small Multiples: Histograms  logTA by Segment * SRC 
# ***********************************************************************
# histogram( ~  logy |  ds.logTA$Segment * ds.logTA$SRC, type="percent")

# ***********************************************************************
# CELL: Small Multiples: Histograms  BreachCount Segment BC by BT
# ***********************************************************************
# histogram( ~  ds.logTA$Segment |  ds.logTA$BT)
# histogram( ~  ds.logTA$BT |  ds.logTA$Segment)

# ********************************
#   THE END
# ********************************
