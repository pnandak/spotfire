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
#      cvss - A set of CVSS scores after 2005 for each vulnerability in the NVD
#      vuln - All CVSS scores
#      vp     A set of records, one for each vendor-product-version and vulnerability
#      vv     A table with quarter, vendor, vulnID, freq columns
source("getNVD.r")

# ****************************************************************************************
# CELL:  Data Sets Stats
# ****************************************************************************************
# SetUp
metricName<-"NVD-DataSetStats"
dsDescr<-paste("All vendors, products, versions, and vulnerabilities in NVD as of ",date())
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven",
            "rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven")
evenColor<-"#FFFFFF"
oddColor<- "#CCFFCC"

# Derive
vpStats<-vpDataSetStats(vp)
values<-vpStats$dsStats

# Deliver
dsStatsPrintHTML(metricName,dsDescr,vpStats$rowCount,vpStats$colCount,vpStats$nVuln,
                 vpStats$nVend,values,htmlFileName,rowStyles,evenColor,oddColor)


# ****************************************************************************************
# CELL:  Top 10 Lists based on Vuln Count
#        Vendor Top 10 List 
#        Vendor Bottom 10 List
# ****************************************************************************************
# SetUp
metricName<-"NVD-VulnCount-VendorTop10"
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven",
            "rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven")
evenColor<-"#FFFFFF"
oddColor<- "#9AFF9A"

# Derive both lists using vvf table from MySQL
vendor.qtr<-as.data.frame.table(table(vvf$quarter,vvf$vendor,exclude=excludeQTR))
names(vendor.qtr)<-c("Quarter","Vendor","Freq")

nn<-c(1:length(levels(vendor.qtr$Quarter)))
vendorName<-levels(vendor.qtr$Vendor)
nvendor<-length(vendorName)
vulnCount<-c(1:nvendor)
intercept<-c(1:nvendor)
gradient<-c(1:nvendor)
for (i in 1:nvendor) {
    points<-vendor.qtr[vendor.qtr$Vendor == vendorName[i],]
    vulnCount[i]<-sum(points$Freq)
    fitted.vendor<-coef(lm(points$Freq ~ nn))
    intercept[i]<-fitted.vendor[1]
    gradient[i]<-fitted.vendor[2]
}
gradient<-round(gradient,2)
intercept<-round(intercept,2)
vendor.trend<-cbind(vendorName,gradient,vulnCount)

ordered.vt<-vendor.trend[ order(gradient,vendorName),]
trend.fame<-ordered.vt[1:10,]
header<-c("Vendor","Gradient","Vuln Count")
values<-rbind(header,trend.fame)

# Deliver
top10PrintHTML(metricName,htmlFileName,values,rowStyles,evenColor,oddColor)

# SetUp
metricName<-"NVD-VulnCount-VendorBottom10"
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven",
            "rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven")
evenColor<-"#FFFFFF"
oddColor<- "#FFC1C1"

# Derive (mini)
ordered.vt<-vendor.trend[ order(-gradient,vendorName),]
trend.shame<-ordered.vt[1:10,]
header<-c("Vendor","Gradient","Vuln Count")
values<-rbind(header,trend.shame)

# Deliver
top10PrintHTML(metricName,htmlFileName,values,rowStyles,evenColor,oddColor)

# ****************************************************************************************
# CELL:  Top 10 Lists based on Vuln-Prod Count
#        Vendor Top 10 List 
#        Vendor Bottom 10 List
# ****************************************************************************************
# SetUp
metricName<-"NVD-ProdVulnCount-VendorTop10"
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven",
            "rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven")
evenColor<-"#FFFFFF"
oddColor<- "#9AFF9A"

# Derive both lists
vendor.qtr<-as.data.frame.table(table(vp$quarter,vp$vendor,exclude=excludeQTR))
names(vendor.qtr)<-c("Quarter","Vendor","Freq")
nn<-c(1:length(levels(vendor.qtr$Quarter)))
vendorName<-levels(vendor.qtr$Vendor)
nvendor<-length(vendorName)
vulnCount<-c(1:nvendor)
intercept<-c(1:nvendor)
gradient<-c(1:nvendor)
for (i in 1:nvendor) {
    points<-vendor.qtr[vendor.qtr$Vendor == vendorName[i],]
    vulnCount[i]<-sum(points$Freq)
    fitted.vendor<-coef(lm(points$Freq ~ nn))
    intercept[i]<-fitted.vendor[1]
    gradient[i]<-fitted.vendor[2]
}
gradient<-round(gradient,0)
intercept<-round(intercept,2)
vendor.trend<-cbind(vendorName,gradient,vulnCount)

ordered.vt<-vendor.trend[ order(gradient,vendorName),]
trend.fame<-ordered.vt[1:10,]
header<-c("Vendor","Gradient","Vuln-Prod Count")
values<-rbind(header,trend.fame)

# Deliver
top10PrintHTML(metricName,htmlFileName,values,rowStyles,evenColor,oddColor)

# SetUp
metricName<-"NVD-ProdVulnCount-VendorBottom10"
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven",
            "rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven")
evenColor<-"#FFFFFF"
oddColor<- "#FFC1C1"

# Derive (mini)
ordered.vt<-vendor.trend[ order(-gradient,vendorName),]
trend.shame<-ordered.vt[1:10,]
header<-c("Vendor","Gradient","Vuln-Prod Count")
values<-rbind(header,trend.shame)

# Deliver
top10PrintHTML(metricName,htmlFileName,values,rowStyles,evenColor,oddColor)

# ****************************************************************************************
# CELL:  VulnCountPerQuarter for Selected Vendors
# ****************************************************************************************
# SetUp
metricName<-"NVD-VendorVulnCountPerQtr-Trend"
ylabel<-"VendorVulnCount/Qtr"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366",
               "#659933","#CC99CC","#333333","99CC00", "#FF3300","#3366CC",
               "#00CC33","#FFCC00","#FF0099","#003366","#659933","#CC99CC",
               "#333333","99CC00")

# Derive from vvf: A table with cols: quarter, vendor, vulnID, Freq
bar<-table(vvf$quarter,vvf$vendor)
tmp<-as.data.frame.table(bar)
vendor.qtr<-replace(tmp, is.na(tmp), 0)
names(vendor.qtr)<-c("Quarter","Vendor","Freq")
attach(vendor.qtr)

Quarters<-levels(vendor.qtr$Quarter)
Apache<-vendor.qtr[Vendor=="apache",3]
Apple<-vendor.qtr[Vendor=="apple",3]
Cisco<-vendor.qtr[Vendor=="cisco",3]
HP<-vendor.qtr[Vendor=="hp",3]
IBM<-vendor.qtr[Vendor=="ibm",3]
Linux<-vendor.qtr[Vendor=="linux",3]
Microsoft<-vendor.qtr[Vendor=="microsoft",3]
Mozilla<-vendor.qtr[Vendor=="mozilla",3]
Novell<-vendor.qtr[Vendor=="novell",3]
Oracle<-vendor.qtr[Vendor=="oracle",3]
PHP<-vendor.qtr[Vendor=="php",3]
Sun<-vendor.qtr[Vendor=="sun",3]

quarter.vc<-as.data.frame(cbind(Apache,Apple,Cisco,HP,IBM,Linux,Microsoft,Mozilla,Novell,Oracle,PHP,Sun))
graphNames<-names(quarter.vc)

# Deliver
legacyLineWidget(dataPath,dataRelPath,settingsPath,settingsRelPath,htmlPath,
                 amChartsPathXY, metricName, Quarters, quarter.vc, graphNames, 
                 graphColors, ylabel)

# ****************************************************************************************
# CELL:  ProdVulnCountPerQuarter for Selected Vendors
# ****************************************************************************************
# SetUp
metricName<-"NVD-VendorProdVulnCountPerQtr-Trend"
ylabel<-"ProdVulnCount/Qtr"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366",
               "#659933","#CC99CC","#333333","99CC00", "#FF3300","#3366CC",
               "#00CC33","#FFCC00","#FF0099","#003366","#659933","#CC99CC",
               "#333333","99CC00")

# Derive
# Obtain VulnCount per quarter timeseries for some interesting vendors
vendor.qtr<-as.data.frame.table(table(vp$quarter,vp$vendor,exclude=excludeQTR))
names(vendor.qtr)<-c("Quarter","Vendor","Freq")
attach(vendor.qtr)

Quarters<-levels(vendor.qtr$Quarter)
Apache<-vendor.qtr[Vendor=="apache",3]
Apple<-vendor.qtr[Vendor=="apple",3]
Cisco<-vendor.qtr[Vendor=="cisco",3]
HP<-vendor.qtr[Vendor=="hp",3]
IBM<-vendor.qtr[Vendor=="ibm",3]
Linux<-vendor.qtr[Vendor=="linux",3]
Microsoft<-vendor.qtr[Vendor=="microsoft",3]
Mozilla<-vendor.qtr[Vendor=="mozilla",3]
Novell<-vendor.qtr[Vendor=="novell",3]
Oracle<-vendor.qtr[Vendor=="oracle",3]
PHP<-vendor.qtr[Vendor=="php",3]
Sun<-vendor.qtr[Vendor=="sun",3]

quarter.vc<-as.data.frame(cbind(Apache,Apple,Cisco,HP,IBM,Linux,Microsoft,Mozilla,Novell,Oracle,PHP,Sun))
graphNames<-names(quarter.vc)

# Deliver
legacyLineWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                     metricName, Quarters, quarter.vc, graphNames, graphColors, ylabel) 

# ************************************************************************************************
#                            THE END
# ************************************************************************************************

