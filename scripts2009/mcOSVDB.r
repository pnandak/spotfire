# ***********************************************************
# R Scripts to produce the OSVDB part of the Vulnerability Dashboard
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

# Ingest and massage the OSVDB data sets:
#      ovuln - A set of records for each vulnerability in the OSVDB
#      ovp     A set of records, one for each vendor-product-version and vulnerability
source("getOSVDB.r")

# ****************************************************************************************
# CELL:  DataSets Statistics
# ****************************************************************************************
# SetUp
metricName<-"OSVDB-DataSetStats"
dsDescr<-paste("All vendors, products, versions, and vulnerabilities in OSVDB as of ",date())
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven",
            "rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven")
evenColor<-"#FFFFFF"
oddColor<- "#CCFFCC"

# Derive
vpStats<-vpDataSetStats(ovp)
values<-vpStats$dsStats

# Deliver
dsStatsPrintHTML(metricName,dsDescr,vpStats$rowCount,vpStats$colCount,
                 vpStats$nVuln,vpStats$nVend,values,
                 htmlFileName,rowStyles,evenColor,oddColor)

# ****************************************************************************************
# CELL:  Top 10 Lists based on Vuln Count
#        Vendor Top 10 List 
#        Vendor Bottom 10 List
# ****************************************************************************************
# SetUp
metricName<-"OSVDB-VulnCount-VendorTop10"
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven",
            "rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven")
evenColor<-"#FFFFFF"
oddColor<- "#9AFF9A"

# Derive both lists using the vvf table
vendor.qtr<-as.data.frame.table(table(vvf$quarter,vvf$vendor))
names(vendor.qtr)<-c("Quarter","Vendor","Freq")
attach(vendor.qtr)

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

# OSVDB Selected Vendor Name Repair Hack
trend.fame[grep("Mozilla",as.character(trend.fame[,1])),1]<-"Mozilla"
trend.fame[grep("Oracle",as.character(trend.fame[,1])),1]<-"Oracle"
trend.fame[grep("Apple",as.character(trend.fame[,1])),1]<-"Apple"
trend.fame[grep("International Business Machines",as.character(trend.fame[,1])),1]<-"IBM"
trend.fame[grep("Cisco",as.character(trend.fame[,1])),1]<-"Cisco"
trend.fame[grep("FreeBSD",as.character(trend.fame[,1])),1]<-"FreeBSD"
trend.fame[grep("Hitachi",as.character(trend.fame[,1])),1]<-"Hitachi"
trend.fame[grep("Microsoft",as.character(trend.fame[,1])),1]<-"Microsoft"
trend.fame[grep("Hewlett-Packard",as.character(trend.fame[,1])),1]<-"HP"
trend.fame[grep("Sun Microsystems",as.character(trend.fame[,1])),1]<-"Sun"
trend.fame[grep("Silicon Graphics",as.character(trend.fame[,1])),1]<-"SGI"
trend.fame[grep("phpMyAdmin",as.character(trend.fame[,1])),1]<-"phpMyAdmin"
trend.fame[grep("Sambar",as.character(trend.fame[,1])),1]<-"Sambar"
trend.fame[grep("Association for Progressive Communications",as.character(trend.fame[,1])),1]<-"APC"

header<-c("Vendor","Gradient","Vuln Count")
values<-rbind(header,trend.fame)

# Deliver
top10PrintHTML(metricName,htmlFileName,values,rowStyles,evenColor,oddColor)

# SetUp
metricName<-"OSVDB-VulnCount-VendorBottom10"
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven",
            "rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven")
evenColor<-"#FFFFFF"
oddColor<- "#FFC1C1"

# Derive (mini)
ordered.vt<-vendor.trend[ order(-gradient,vendorName),]
trend.shame<-ordered.vt[1:10,]

# OSVDB Selected Vendor Name Repair Hack
trend.shame[grep("Mozilla",as.character(trend.shame[,1])),1]<-"Mozilla"
trend.shame[grep("Oracle",as.character(trend.shame[,1])),1]<-"Oracle"
trend.shame[grep("Apple",as.character(trend.shame[,1])),1]<-"Apple"
trend.shame[grep("International Business Machines",as.character(trend.shame[,1])),1]<-"IBM"
trend.shame[grep("Cisco",as.character(trend.shame[,1])),1]<-"Cisco"
trend.shame[grep("FreeBSD",as.character(trend.shame[,1])),1]<-"FreeBSD"
trend.shame[grep("Hitachi",as.character(trend.shame[,1])),1]<-"Hitachi"
trend.shame[grep("Microsoft",as.character(trend.shame[,1])),1]<-"Microsoft"
trend.shame[grep("Hewlett-Packard",as.character(trend.shame[,1])),1]<-"HP"
trend.shame[grep("Sun Microsystems",as.character(trend.shame[,1])),1]<-"Sun"
trend.shame[grep("Silicon Graphics",as.character(trend.shame[,1])),1]<-"SGI"
trend.shame[grep("PHP Project Management ERP",as.character(trend.shame[,1])),1]<-"PGP ERP"

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
metricName<-"OSVDB-ProdVulnCount-VendorTop10"
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven",
            "rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven")
evenColor<-"#FFFFFF"
oddColor<- "#9AFF9A"

# Derive both lists using the vvf table
vendor.qtr<-as.data.frame.table(table(ovp$quarter,ovp$vendor,exclude=excludeQTR))
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

# OSVDB Selected Vendor Name Repair Hack
trend.fame[grep("Mozilla",as.character(trend.fame[,1])),1]<-"Mozilla"
trend.fame[grep("Oracle",as.character(trend.fame[,1])),1]<-"Oracle"
trend.fame[grep("Apple",as.character(trend.fame[,1])),1]<-"Apple"
trend.fame[grep("International Business Machines",as.character(trend.fame[,1])),1]<-"IBM"
trend.fame[grep("Cisco",as.character(trend.fame[,1])),1]<-"Cisco"
trend.fame[grep("FreeBSD",as.character(trend.fame[,1])),1]<-"FreeBSD"
trend.fame[grep("Hitachi",as.character(trend.fame[,1])),1]<-"Hitachi"
trend.fame[grep("Microsoft",as.character(trend.fame[,1])),1]<-"Microsoft"
trend.fame[grep("Hewlett-Packard",as.character(trend.fame[,1])),1]<-"HP"
trend.fame[grep("Sun Microsystems",as.character(trend.fame[,1])),1]<-"Sun"
trend.fame[grep("Silicon Graphics",as.character(trend.fame[,1])),1]<-"SGI"
trend.fame[grep("Association for Progressive Communications",as.character(trend.fame[,1])),1]<-"APC"
trend.fame[grep("phpMyAdmin",as.character(trend.fame[,1])),1]<-"phpMyAdmin"
trend.fame[grep("Sambar",as.character(trend.fame[,1])),1]<-"Sambar"

header<-c("Vendor","Gradient","Vuln-Prod Count")
values<-rbind(header,trend.fame)

# Deliver
top10PrintHTML(metricName,htmlFileName,values,rowStyles,evenColor,oddColor)

# SetUp
metricName<-"OSVDB-ProdVulnCount-VendorBottom10"
htmlFileName<-paste(htmlPath,metricName,".html",sep="")
rowStyles=c("rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven",
            "rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven","rowOdd","rowEven")
evenColor<-"#FFFFFF"
oddColor<- "#FFC1C1"

# Derive (mini)
ordered.vt<-vendor.trend[ order(-gradient,vendorName),]
trend.shame<-ordered.vt[1:10,]

trend.shame[grep("Mozilla",as.character(trend.shame[,1])),1]<-"Mozilla"
trend.shame[grep("Oracle",as.character(trend.shame[,1])),1]<-"Oracle"
trend.shame[grep("Apple",as.character(trend.shame[,1])),1]<-"Apple"
trend.shame[grep("International Business Machines",as.character(trend.shame[,1])),1]<-"IBM"
trend.shame[grep("Cisco",as.character(trend.shame[,1])),1]<-"Cisco"
trend.shame[grep("FreeBSD",as.character(trend.shame[,1])),1]<-"FreeBSD"
trend.shame[grep("Hitachi",as.character(trend.shame[,1])),1]<-"Hitachi"
trend.shame[grep("Microsoft",as.character(trend.shame[,1])),1]<-"Microsoft"
trend.shame[grep("Hewlett-Packard",as.character(trend.shame[,1])),1]<-"HP"
trend.shame[grep("Sun Microsystems",as.character(trend.shame[,1])),1]<-"Sun"
trend.shame[grep("Silicon Graphics",as.character(trend.shame[,1])),1]<-"SGI"
trend.shame[grep("PHP Project Management ERP",as.character(trend.shame[,1])),1]<-"PGP ERP"

header<-c("Vendor","Gradient","Vuln-Prod Count")
values<-rbind(header,trend.shame)

# Deliver
top10PrintHTML(metricName,htmlFileName,values,rowStyles,evenColor,oddColor)

# ****************************************************************************************
# CELL:  VulnCountPerQuarter for Selected Vendors
# ****************************************************************************************
# SetUp
metricName<-"OSVDB-VendorVulnCountPerQtr"
ylabel<-"VendorVulnCount/Qtr"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366",
               "#659933","#CC99CC","#333333","99CC00", "#FF3300","#3366CC",
               "#00CC33","#FFCC00","#FF0099","#003366","#659933","#CC99CC",
               "#333333","99CC00")

# Derive both lists using vvf
bar<-table(vvf$quarter,vvf$vendor)
tmp<-as.data.frame.table(bar)
vendor.qtr<-replace(tmp, is.na(tmp), 0)
names(vendor.qtr)<-c("Quarter","Vendor","Freq")
attach(vendor.qtr)

Quarters<-levels(vendor.qtr$Quarter)
Apache<-vendor.qtr[Vendor=="Apache Software Foundation",3]
Apple<-vendor.qtr[Vendor=="Apple Computer, Inc.",3]
Cisco<-vendor.qtr[Vendor=="Cisco Systems, Inc.",3]
HP<-vendor.qtr[Vendor=="Hewlett-Packard Development Company, L.P.",3]
IBM<-vendor.qtr[Vendor=="International Business Machines Corporation",3]
Linux<-vendor.qtr[Vendor=="Linux",3]
Microsoft<-vendor.qtr[Vendor=="Microsoft Corporation",3]
Mozilla<-vendor.qtr[Vendor=="Mozilla Organization",3]
Novell<-vendor.qtr[Vendor=="Novell, Inc.",3]
Oracle<-vendor.qtr[Vendor=="Oracle Corporation",3]
PHP<-vendor.qtr[Vendor=="The PHP Group",3]
Sun<-vendor.qtr[Vendor=="Sun Microsystems, Inc.",3]

quarter.vc<-as.data.frame(cbind(Apache,Apple,Cisco,HP,IBM,Linux,
                                Microsoft,Mozilla,Novell,Oracle,PHP,Sun))

quarter.vc<-as.data.frame(cbind(Apache,Apple,Cisco,HP,IBM,Linux,Microsoft,Mozilla,Novell,Oracle,PHP,Sun))
graphNames<-names(quarter.vc)

# Deliver
legacyLineWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                     metricName, Quarters, quarter.vc, graphNames, graphColors, ylabel)

# ****************************************************************************************
# CELL:  ProdVulnCountPerQuarter for Selected Vendors
# ****************************************************************************************
# SetUp
metricName<-"OSVDB-VendorProdVulnCountPerQtr"
ylabel<-"ProdVulnCount/Qtr"
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366",
               "#659933","#CC99CC","#333333","99CC00", "#FF3300","#3366CC",
               "#00CC33","#FFCC00","#FF0099","#003366","#659933","#CC99CC",
               "#333333","99CC00")

# Derive
# Obtain VulnCount per quarter timeseries for some interesting vendors
vendor.qtr<-as.data.frame.table(table(ovp$quarter,ovp$vendor,exclude=excludeQTR))
names(vendor.qtr)<-c("Quarter","Vendor","Freq")
attach(vendor.qtr)

Quarters<-levels(vendor.qtr$Quarter)
Apache<-vendor.qtr[Vendor=="Apache Software Foundation",3]
Apple<-vendor.qtr[Vendor=="Apple Computer, Inc.",3]
Cisco<-vendor.qtr[Vendor=="Cisco Systems, Inc.",3]
HP<-vendor.qtr[Vendor=="Hewlett-Packard Development Company, L.P.",3]
IBM<-vendor.qtr[Vendor=="International Business Machines Corporation",3]
Linux<-vendor.qtr[Vendor=="Linux",3]
Microsoft<-vendor.qtr[Vendor=="Microsoft Corporation",3]
Mozilla<-vendor.qtr[Vendor=="Mozilla Organization",3]
Novell<-vendor.qtr[Vendor=="Novell, Inc.",3]
Oracle<-vendor.qtr[Vendor=="Oracle Corporation",3]
PHP<-vendor.qtr[Vendor=="The PHP Group",3]
Sun<-vendor.qtr[Vendor=="Sun Microsystems, Inc.",3]


quarter.vc<-as.data.frame(cbind(Apache,Apple,Cisco,HP,IBM,Linux,Microsoft,Mozilla,Novell,Oracle,PHP,Sun))
graphNames<-names(quarter.vc)

# Deliver
legacyLineWidget(dataPath, dataRelPath, settingsPath, settingsRelPath, htmlPath, amChartsPathXY,
                     metricName, Quarters, quarter.vc, graphNames, graphColors, ylabel) 
# ************************************************************************************************
#                 THE END
# ************************************************************************************************

