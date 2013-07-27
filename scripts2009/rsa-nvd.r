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
# ----------------------------------------------------
# Get NVD data
# ----------------------------------------------------
mgr<-dbManager("MySQL")
con<-dbConnect(mgr, group="nvd")
rs<-dbExecStatement(con, "select * from raw;")
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

# --------------------------------------------------
# Data cleansing
# --------------------------------------------------
# Replace empty product names with "NotSpecified"  (osvdb needs this)
ds[ product=="",8 ]<-"NotSpecified"

# ---------------------------------------------------
# database stats
# ---------------------------------------------------
# Number of vuln-prod's
dim(ds)[1]

# Distribution:  Vulns Per Vendor-Product-Version
tab<-paste(ds$vendor,ds$product,ds$version,sep="::")
VulnsPerVPV<-table(tab)
summary(as.numeric(VulnsPerVPV))

# Distribution: Vendor-Product-Version per Vulnerability
col<-paste(ds$vendor,ds$product,ds$version,sep="::")
tab<-cbind(ds$vulnID,col)
VPVPerVuln<-table(tab[,1])
summary(as.numeric(VPVPerVuln))

# Distribution:  Product-Versions Per Vendor
col<-paste(ds$product,ds$version,sep="::")
tab<-cbind(ds$vendor,col)
PVPerVendor<-table(tab[,1])
summary(as.numeric(PVPerVendor))

# Distribution:  Versions Per Vendor-Product 
col<-paste(ds$vendor,ds$product,sep="::")
tab<-cbind(col,ds$Version)
VerPerVendorProd<-table(tab[,1])
summary(as.numeric(VerPerVendorProd))

# Distribution:  Products Per Vendor
tab<-as.data.frame(VerPerVendorProd)
names(tab)<-c("PV","Freq")
N<-length(tab[,1])
ven<-c(1:N)
prod<-c(1:N)
for (i in (1:N))  {
   ven[i]<-unlist(strsplit(as.character(tab[i,1]),"::"))[[1]]
   prod[i]<-unlist(strsplit(as.character(tab[i,1]),"::"))[[2]]
}
tab<-cbind(ven,prod)
ProdPerVendor<-table(tab[,1])
summary(as.numeric(ProdPerVendor))

# ----------------------------------------------------
# Vulnerability compute logic
# ----------------------------------------------------
# Vuln count per quarter
# Determine sample size
count.quarter<-table(ds$quarter,exclude=excludeQTR, dnn="Quarter")
count.quarter
png(file="images/NVD-ProdVulnCountPerQuarter-ALL.png",bg="white",width=900,height=350)
plot(count.quarter,"b",
     main="NVD: Prod-Vuln Count per  Quarter\nAll Data",
     ylab="# Prod-Vulns")
dev.off()

# Take only data after 2005
ds<-as.data.frame(as.matrix(ds[ year > 2005, ]))
attach(ds)
names(ds)

count.1Q06<-table(ds$quarter,exclude=excludeQTR, dnn="Quarter")
count.1Q06

png(file="images/NVD-ProdVulnCountPerQuarter.png",bg="white",width=950,height=300)
plot(count.1Q06,"b",
     main="NVD: Prod-Vuln Count per Quarter\nJan 2006 to Present",
     ylab="# Prod-Vulns")
abline(lsfit(1:length(count.1Q06),count.1Q06),col="red", lwd=3, lty=2)
dev.off()

nn<-c(1:length(count.1Q06))
coef(lm(count.1Q06 ~ nn))
model<-lm(count.1Q06 ~ nn)
summary.lm(model)
summary.aov(model)

# Check "goodness" of model.  Get four plots.  See RSA presentation
# and both stat books on leverage
plot(model)

# Key parameters
vc.min<-min(count.1Q06)
vc.mean<-mean(count.1Q06)
vc.max<-max(count.1Q06)
vc.sd<-sqrt(var(count.1Q06))
vc.stdErr<-sqrt((var(count.1Q06)/length(count.1Q06)))
rbind(vc.min,vc.mean,vc.max,vc.sd)

parms<-hist(count.1Q06,plot=F)
x.min<-min(parms$breaks)
x.max<-max(parms$breaks)
area=sum(parms$counts)*(x.max/length(parms$mids))
y.max<-max(parms$counts)+1

# Summary plot
png(file="images/NVD-QtrProdVulnDist.png",bg="white",width=300,height=350)
boxplot(count.1Q06, xlab="Quarterly Totals: 1Q06 to Present", 
   ylab="# Prod-Vulns",
   main="NVD Dist of Prod-Vulns/Qtr \n1Q06 to Present",
   col="#6699CC", notch=F)
dev.off()

png(file="images/NVD-ProdVulnHistQtr.png",bg="white",width=350,height=350)
hist(count.1Q06,xlab="Ranges of Prod-Vuln Counts/Quarter", ylab="# Quarters",
   main="NVD: Histogram of Prod-Vulns/Qtr \n1Q06 to Present",
   labels=TRUE,xlim=c(x.min,x.max),ylim=c(0,y.max),
   col="#6699CC")
xv<-seq(x.min,x.max,((x.max - x.min)/100))
yv<-dnorm(xv,mean=vc.mean,sd=vc.sd)*area
lines(xv,yv,col="red",lwd=3)
dev.off()

confidence<-c(.999,.995,.99,.95,.90)
K<-length(confidence)
cint<-1 - (1-confidence)/2
df<-length(count.1Q06) - 1
low<-c(1:K)
high<-c(1:K)
tval<-c(1:K)
for (i in (1:K)) {
   tval[i]<-qt(cint[i],df)
   low[i]<-vc.mean -  (tval[i]*vc.stdErr)
   high[i]<-vc.mean + (tval[i]*vc.stdErr)
}
ans<-cbind(confidence*100,low, high)
ans
vc.stdErr
count.1Q06

# -----------------------------------------------------------
# Top Ten Lists
# -----------------------------------------------------------
vendor.qtr<-as.data.frame.table(table(ds$quarter,ds$vendor,exclude=excludeQTR))
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
vendor.trend<-cbind(gradient,intercept,vendorName,vulnCount)

ordered.vt<-vendor.trend[ order(gradient,intercept,vendorName),]
trend.fame<-ordered.vt[1:10,]

ordered.vt<-vendor.trend[ order(-gradient,-intercept,vendorName),]
trend.shame<-ordered.vt[1:10,]

png(file="images/NVD-ShameList-Qtr.png", width=510, height=350, bg="white")
cols <- c("black","#CC0000","black", "#CC0066","black","#CC0066","black","#CC0066","black","#CC0066")
textplot(trend.shame, valign="top", halign="left",
          col.data=matrix(cols, nrow=length(cols), byrow=FALSE, ncol=4),
          show.colnames=TRUE, show.rownames=FALSE,rmar=1)
title(main="NVD Prod-Vuln Gradient: Least Improved Vendors\nQuarterly from Jan 2006")
dev.off()

png(file="images/NVD-FameList-Qtr.png", width=520, height=350, bg="white")
cols <- c("black","#006600","black", "#006600","black","#006600","black","#006600","black","#006600")
textplot(trend.fame, valign="top", halign="left",
          col.data=matrix(cols, nrow=length(cols), byrow=FALSE, ncol=4),
          show.colnames=TRUE, show.rownames=FALSE,rmar=1)
title(main="NVD Prod-Vuln Gradient: Most Improved Vendors \nQuarterly from Jan 2006")
dev.off()

# Inter Quartile Range = 3Q - 1Q
iqr<-summary(gradient)[5] - summary(gradient)[2]
gradient.low<-summary(gradient)[2] - 1.5*iqr
gradient.hi <-summary(gradient)[5] + 1.5*iqr

# Filter out the outliers
ds.trend.shame<-as.data.frame(vendor.trend)
vt.good<-ds.trend.shame[ gradient > gradient.low & gradient < gradient.hi, ]

png(file="images/NVD-GradientHist.png", width=300, height=350, bg="white")
hist(as.numeric(as.character(vt.good$gradient)),col="#6699CC",
     xlab="Prod-Vendor Quarterly Gradient Ranges", ylab="# Vendors",
     main="NVD Gradient Histogram \nFrom 1Jan06 with Outliers Omitted")
dev.off()

# Obtain VulnCount per quarter timeseries for some interesting vendors
vendor.qtr<-as.data.frame.table(table(ds$quarter,ds$vendor,exclude=excludeQTR))
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

quarter.vc<-as.data.frame(cbind(Apache,Apple,Cisco,HP,IBM,Linux,
                                Microsoft,Mozilla,Novell,Oracle,PHP,Sun))

# --------------------------------------------------------------------------------------
# VulnCount:  VCbyVendorPerQuarter
# --------------------------------------------------------------------------------------
# Print XML data file
metricName<-"NVD-VCbyVendorPerQuarter"
yLabel<-metricName
graphNames<-names(quarter.vc)
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00",
               "#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00")
fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
linePrintXMLData(Quarters, quarter.vc, graphNames, graphColors, fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
linePrintSettings(metricName,yLabel,fileName)

# Print HTML file
flashObj<-"amline"
chartTitle<-"Quarterly"
amChartsPathXY<-paste(amChartsPath,"amline/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

# End viz of Quarterly VC timeline for selected vendors

# THE END
