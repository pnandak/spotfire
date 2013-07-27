# Required libraries
library(gplots)  # For outputting images
library(gdata)   # For drop.levels
library(stats)
library(RMySQL)

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

# -----------------------------------------------------------
# Get Vuln-Prod Raw Data
# -----------------------------------------------------------
library(RMySQL)
mgr<-dbManager("MySQL")
con<-dbConnect(mgr, group="nvd")

rs<-dbExecStatement(con, "select * from rawCVSS;")
r<-fetch(rs,n = -1)

# Close out the MySQL connection
close(rs)
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
year<-strftime(strptime(as.character(r[,3]), format="%Y-%m-%d"), format="%Y")
qtr<-as.integer(((as.integer(strftime(strptime(as.character(r[,3]), format="%Y-%m-%d"), format="%m")))-1)/3 + 1)
quarter<-paste(year,qtr,sep="-")
month<-strftime(strptime(as.character(r[,3]), format="%Y-%m-%d"), format="%Y-%m")

ds<-cbind(year,quarter,month,r)
attach(ds)

# Take only data after 2005
vulnProd<-as.data.frame(as.matrix(ds [ year > 2005, ]))
rm(ds)
attach(vulnProd)
names(vulnProd)
cvss<-as.numeric(as.character(vulnProd$cvssScore))

# -----------------------------------------------------------
# Get Breach Data
# -----------------------------------------------------------
# DatalossDB Data Source File
rawFile<-"http://datalossdb.org/exports/dataloss.csv"
e<-read.csv(rawFile, sep=",", header=F, skip=0, comment.char="", na.strings="?", blank.lines.skip=T)

names(e)<-c("Date","Company","Country","Segment","SubSegment","BreachType","DataLost","Source","ThirdParty","ThirdPartyName","TotalAffected","RefPage","UID","XREF","StockSymbol","DataRecovered","ConsumerLawSuit","ArrestProsecution")

# Create time dimension columns: year, quarter, and month
year<-strftime(strptime(as.character(e[,1]), format="%Y-%m-%d"), format="%Y")
qtr<-as.integer(((as.integer(strftime(strptime(as.character(e[,1]), format="%Y-%m-%d"), format="%m")))-1)/3 + 1)
quarter<-paste(year,qtr,sep="-")
month<-strftime(strptime(as.character(e[,1]), format="%Y-%m-%d"), format="%Y-%m")
events<-cbind(year,quarter,month,e)
attach(events)
names(events)

# Dataset = Breaches after 2005
START_YEAR<-2005
ds<-as.data.frame(as.matrix(events[year > START_YEAR,]))
detach(events)
attach(ds)
names(ds)

# -------------------------------------------------------
# BreachCountByTypePerQuarter
# BreachCountByDataLostPerQuarter
# Quarterly Breach Count by BreachType and DataLost
# -------------------------------------------------------
# Create a dataset (BreachType, DataLost, Quarter, Freq) where Freq > 0.
# Note:  Freq is a count of breaches by BreachType and Quarter or
#        Freq is a count of breaches by DataLost and Quarter

q.bt<-table(ds$quarter,ds$BreachType,exclude=excludeQTR)
q.dl<-table(ds$quarter,ds$DataLost,exclude=excludeQTR)

# This is to get the list of quarters without the ones excluded in the above table calculations
temp<-as.data.frame(table(ds$quarter,ds$BreachType,exclude=excludeQTR))
Quarters<-levels(temp$Var1)

# Consolidating BreachTypes into a shorter list
Disp<-q.bt[,1]+q.bt[,2]+q.bt[,3]+q.bt[,4]+q.bt[,5]
Email<-q.bt[,6]
Fraud<-q.bt[,7]+q.bt[,8]+q.bt[,9]
Hack<-q.bt[,10]+q.bt[,11]+q.bt[,12]
Lost<-q.bt[,13]+q.bt[,14]+q.bt[,15]+q.bt[,16]+q.bt[,17]+q.bt[,18]+q.bt[,19]
Mail<-q.bt[,20]
Stolen<-q.bt[,21]+q.bt[,22]+q.bt[,23]+q.bt[,24]+q.bt[,25]+
        q.bt[,26]+q.bt[,27]+q.bt[,28]+q.bt[,29]+q.bt[,30]
Unk<-q.bt[,31]
Virus<-q.bt[,32]
Web<-q.bt[,33]+q.bt[,34]+q.bt[,35]

# Just to check the calc's below that use grep
# q.ACC<-q.dl[,2]+q.dl[,3]+q.dl[,4]+q.dl[,5]+q.dl[,7]+q.dl[,9]+q.dl[,25]+q.dl[,26]+q.dl[,29]+q.dl[,38]+q.dl[,48]+q.dl[,51]+q.dl[,52]+q.dl[,53]
# q.ACC

# Get subtotals by DataLost (e.g. ACC, CCN, etc)
# Calculate number of breachs in which various types of data were lost
# grep function returns a list of integers, each of which denotes a column where the string "ACC" appears -- neat huh?
q.ACC<-q.dl[,grep("ACC",levels(ds$DataLost))]
ACC<-q.ACC[,1]
for (i in (2:length(q.ACC[1,]))) { ACC<-ACC+q.ACC[,i] }

q.CCN<-q.dl[,grep("CCN",levels(ds$DataLost))]
CCN<-q.CCN[,1]
for (i in (2:length(q.CCN[1,]))) { CCN<-CCN+q.CCN[,i] }

q.DOB<-q.dl[,grep("DOB",levels(ds$DataLost))]
DOB<-q.DOB[,1]
for (i in (2:length(q.DOB[1,]))) { DOB<-DOB+q.DOB[,i] }

q.EMA<-q.dl[,grep("EMA",levels(ds$DataLost))]
EMA<-q.EMA[,1]
for (i in (2:length(q.EMA[1,]))) { EMA<-EMA+q.EMA[,i] }

q.FIN<-q.dl[,grep("FIN",levels(ds$DataLost))]
FIN<-q.ACC[,1]
for (i in (2:length(q.ACC[1,]))) { FIN<-FIN+q.FIN[,i] }

q.MED<-q.dl[,grep("MED",levels(ds$DataLost))]
MED<-q.MED[,1]
for (i in (2:length(q.MED[1,]))) { MED<-MED+q.MED[,i] }

q.MISC<-q.dl[,grep("MISC",levels(ds$DataLost))]
MISC<-q.MISC[,1]
for (i in (2:length(q.MISC[1,]))) { MISC<-MISC+q.MISC[,i] }

q.NAA<-q.dl[,grep("NAA",levels(ds$DataLost))]
NAA<-q.NAA[,1]
for (i in (2:length(q.NAA[1,]))) { NAA<-NAA+q.NAA[,i] }

q.SSN<-q.dl[,grep("SSN",levels(ds$DataLost))]
SSN<-q.SSN[,1]
for (i in (2:length(q.SSN[1,]))) { SSN<-SSN+q.SSN[,i] }

# --------------------------------------------------
# BreachCountByTypePerQuarter
# BreachCountByDataLostPerQuarter
# --------------------------------------------------
quarter.bt<-as.data.frame(cbind(Disp,Email,Fraud,Hack,Lost,Mail,Stolen,Unk,Virus,Web))
quarter.dl<-as.data.frame(cbind(ACC,CCN,DOB,EMA,FIN,MED,MISC,NAA,SSN))

# ----------------------------------------------------
# High Impact Confidentialty Quarterly VulnProd counts
hi.c<-vulnProd[ cvssConfidentialityImpact == "COMPLETE", ]
attach(hi.c)
conf<-table(hi.c$quarter,exclude=excludeQTR)

hi.i<-vulnProd[ cvssIntegrityImpact == "COMPLETE", ]
attach(hi.i)
integ<-table(hi.i$quarter,exclude=excludeQTR) 

hi.a<-vulnProd[ cvssAvailabilityImpact == "COMPLETE", ]
attach(hi.a)
avail<-table(hi.a$quarter,exclude=excludeQTR) 

quarter.cvss<-as.data.frame(cbind(conf,integ,avail))

# ----------------------------------------------------------------
#  Heatmaps: BreachCountTrendByType vs VulnProdCountTrendByImpact
# ----------------------------------------------------------------
metricName<-"BVC-VulnImpactVsBreachType"
#                          RED       ORANGE    YELLOW   L-YELL     GREY      GREY      L-YELL   YELLOW    L-GREEN   D-GREEN
# RED-GREEN  hm.colors<-c("#FF0000","#FF3300","#FFFF00","#FFFF99","#CCCCCC","#CCCCCC","#FFFF99","#FFFF00","#00FF00","#009900")
# R STANDARD hm.cutpoints<-c( -.95,    -0.90,     -0.8,     -0.6,     -0.3,      0.3,      0.6,      0.8,      0.9,     0,95)
# CSS classes
hm.classes<-  c("noc","low","med","hi","vhi")
hm.cutpoints<-c( 0.3,  0.6,  0.8,  0.9)
hm.values<-round(cor(quarter.cvss,quarter.bt),2)
hm.styles<-symnum(hm.values,
          cutpoints<-hm.cutpoints,symbols=hm.classes,corr=TRUE,
          legend=FALSE,abbr.colnames=FALSE,lower.triangular=FALSE)

# Visualize bfc Heatmap for Type vs DataLost
# Print out hm-TypeXDataLost.html
chartTitle<-"2005 to Present"
htmlFileName<-paste(htmlPath,metricName,"-hm.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-hmData.xml",sep="")
cssFileName<-paste(cssRelPath,"heatmap.css",sep="")
hmPrintHTML(chartTitle,metricName,htmlFileName,cssFileName,hm.values,hm.styles)

metricName<-"BVC-VulnImpactVsDataLost"
#                          RED       ORANGE    YELLOW   L-YELL     GREY      GREY      L-YELL   YELLOW    L-GREEN   D-GREEN
# RED-GREEN  hm.colors<-c("#FF0000","#FF3300","#FFFF00","#FFFF99","#CCCCCC","#CCCCCC","#FFFF99","#FFFF00","#00FF00","#009900")
# R STANDARD hm.cutpoints<-c( -.95,    -0.90,     -0.8,     -0.6,     -0.3,      0.3,      0.6,      0.8,      0.9,     0,95)
# CSS classes
hm.classes<-  c("noc","low","med","hi","vhi")
hm.cutpoints<-c( 0.3,  0.6,  0.8,  0.9)
hm.values<-round(cor(quarter.cvss,quarter.dl),2)
hm.styles<-symnum(hm.values,
          cutpoints<-hm.cutpoints,symbols=hm.classes,corr=TRUE,
          legend=FALSE,abbr.colnames=FALSE,lower.triangular=FALSE)

# Visualize bfc Heatmap for Type vs DataLost
# Print out hm-TypeXDataLost.html
chartTitle<-"2005 to Present"
htmlFileName<-paste(htmlPath,metricName,"-hm.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-hmData.xml",sep="")
cssFileName<-paste(cssRelPath,"heatmap.css",sep="")
hmPrintHTML(chartTitle,metricName,htmlFileName,cssFileName,hm.values,hm.styles)


# THE END

