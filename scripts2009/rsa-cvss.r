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
# Vulnerability Scoring and Impact Metrics
# -----------------------------------------------------------
library(RMySQL)
mgr<-dbManager("MySQL")
con<-dbConnect(mgr, group="nvd")

rs<-dbExecStatement(con, "select * from rawCVSS;")
r<-fetch(rs,n = -1)
close(rs)

rs2<-dbExecStatement(con, "select * from vuln;")
r2<-fetch(rs2,n = -1)
close(rs2)

# Close out the MySQL connection
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

# ds is Vuln-Prod records
ds<-cbind(year,quarter,month,r)
attach(ds)

year<-strftime(strptime(as.character(r2[,3]), format="%Y-%m-%d"), format="%Y")
qtr<-as.integer(((as.integer(strftime(strptime(as.character(r2[,3]), format="%Y-%m-%d"), format="%m")))-1)/3 + 1)
quarter<-paste(year,qtr,sep="-")
month<-strftime(strptime(as.character(r2[,3]), format="%Y-%m-%d"), format="%Y-%m")

# ds2 is Vuln records
ds2<-cbind(year,quarter,month,r2)

# Take only data after 2005
vulnProd<-ds [ as.numeric(as.character(ds$year)) > 2005, ]
vulnProd<-drop.levels(vulnProd)
attach(vulnProd)
names(vulnProd)

vuln<-ds2 [ as.numeric(as.character(ds2$year)) > 2005, ]
vuln<-drop.levels(vuln)
attach(vuln)
names(vuln)

# Are vulnerabilities getting worse?
cvss<-as.numeric(as.character(vulnProd$cvssScore))
max.CVSS=max(cvss)
min.CVSS=min(cvss)
mean.CVSS=mean(cvss)
sd.CVSS=sqrt(var(cvss))
rbind(min.CVSS,mean.CVSS,max.CVSS,sd.CVSS)

parms<-hist(cvss,plot=F)
x.max=max(parms$breaks)
x.min=min(parms$breaks)
area=sum(parms$counts)*(x.max/length(parms$mids))

png(file="images/NVD-CVSS-ProdVulnHist.png",width=300,height=350,bg="white")
hist(cvss, col="#6699CC", xlab="CVSS Score",
     ylab="# Product-Vulnerabilities", 
     main="NVD: CVSS Score Prevalence \nJan 2006 to Present")
xv<-seq(0,x.max,0.1)
yv<-dnorm(xv,mean=mean.CVSS,sd=sd.CVSS)*area
lines(xv,yv,col="red",lwd=3)
dev.off()

cvssVuln<-as.numeric(as.character(vuln$cvss_score))
max.CVSS=max(cvssVuln)
min.CVSS=min(cvssVuln)
mean.CVSS=mean(cvssVuln)
sd.CVSS=sqrt(var(cvssVuln))
rbind(min.CVSS,mean.CVSS,max.CVSS,sd.CVSS)

png(file="images/NVD-CVSS-VulnHist.png",width=300,height=350,bg="white")
hist(cvssVuln, col="#6699CC", xlab="CVSS Score",
     ylab="# Vulnerabilities", 
     main="NVD: CVSS Score Prevalence \nJan 2006 to Present")
xv<-seq(0,x.max,0.1)
yv<-dnorm(xv,mean=mean.CVSS,sd=sd.CVSS)*area
lines(xv,yv,col="red",lwd=3)
dev.off()

png(file="images/NVD-YearCVSSVulnBoxPlot.png",bg="white")
boxplot(
       split(cvssVuln,vuln$year),
       notch=T,
       main="NVD: CVSS Score by Year\nVulns: Jan 2006 to Present",
       ylab="CVSS Score",
       border="#0000CC", col="#6699CC")
legendText<-"Calculation based on vulnerabilities"
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()

png(file="images/NVD-YearCVSSProdVulnBoxPlot.png",bg="white")
boxplot(
       split(cvss,vulnProd$year),
       notch=T,
       main="NVD: CVSS Score by Year\nProd-Vulns: Jan 2006 to Present",
       ylab="CVSS Score",
       border="#0000CC", col="#6699CC")
legendText<-"Calculation based on prod-vulnss"
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()

# -----------------------------------------------------------
# Sparklines for CVSS Score as a function of Year and Impact
# -----------------------------------------------------------
vulnProd2<-drop.levels(vulnProd)
cvss<-as.numeric(as.character(vulnProd2$cvssScore))

# Confidentiality
library(lattice)
foo<-as.data.frame(cbind(as.numeric(cvss),
                         as.character(vulnProd2$year),
                         as.character(vulnProd2$cvssConfidentialityImpact)))
foo1<-tapply(cvss,foo[,-1],mean)
foo2<-as.data.frame(foo1)
yearLevels<-levels(vulnProd2$year)
yearLevels<-c(yearLevels,yearLevels,yearLevels)
foo3<-as.data.frame(stack(foo2))
foo4<-cbind(foo3,yearLevels)
names(foo4)
names(foo4)<-c("meanCVSS","confImpact","year")

png(file="images/NVD-CVSS-YrImpactConf.png",height=300,width=900,bg="white")
gTB<-xyplot(meanCVSS ~ year | confImpact, data=foo4,
   layout=c(3,1), aspect=.5, type="l",lwd="3",
   main="CVSS Trend for Confidentiality Impact\nNVD: From 1Jan06",
   col="#6699CC", xlab="Year", ylab="Mean CVSS Score")
print(gTB,split=c(1,1,1,1))
dev.off()

# Integrity
foo<-as.data.frame(cbind(as.numeric(cvss),
                         as.character(vulnProd$year),
                         as.character(vulnProd$cvssIntegrityImpact)))
foo1<-tapply(cvss,foo[,-1],mean)
foo2<-as.data.frame(foo1)
foo3<-as.data.frame(stack(foo2))
foo4<-cbind(foo3,yearLevels)
names(foo4)
names(foo4)<-c("meanCVSS","integImpact","year")

png(file="images/NVD-CVSS-YrImpactInteg.png",height=300,width=900,bg="white")
gTB<-xyplot(meanCVSS ~ year | integImpact, data=foo4,
   layout=c(3,1), aspect=.5, type="l",lwd="3",
   main="CVSS Trend for Integrity Impact\nNVD: From 1Jan06",
   col="#6699CC", xlab="Year", ylab="Mean CVSS Score")
print(gTB,split=c(1,1,1,1))
dev.off()

# Availability
foo<-as.data.frame(cbind(as.numeric(cvss),
                         as.character(vulnProd$year),
                         as.character(vulnProd$cvssAvailabilityImpact)))
foo1<-tapply(cvss,foo[,-1],mean)
foo2<-as.data.frame(foo1)
foo3<-as.data.frame(stack(foo2))
foo4<-cbind(foo3,yearLevels)
names(foo4)
names(foo4)<-c("meanCVSS","availImpact","year")

png(file="images/NVD-CVSS-YrImpactAvail.png",height=300,width=900,bg="white")
gTB<-xyplot(meanCVSS ~ year | availImpact, data=foo4,
   layout=c(3,1), aspect=.5, type="l",lwd="3",
   main="CVSS Trend for Availability Impact\nNVD: From 1Jan06",
   col="#6699CC", xlab="Year", ylab="Mean CVSS Score")
print(gTB,split=c(1,1,1,1))
dev.off()

confVulnProd<-vulnProd[ cvssConfidentialityImpact == "COMPLETE" ,]

# CVSS Score Distribution vs Impact
# Confidentiality
png(file="images/NVD-ConfImpactBoxPlot.png",height=350, width=300, bg="white")
boxplot(
       split(cvss,factor(vulnProd$cvssConfidentialityImpact)),
       notch=T,
       main="NVD: CVSS Score by Conf Impact\nJan 2006 to Present",
       ylab="CVSS Score", xlab="Confidentiality Impact",
       border="#0000CC", col="#6699CC")
dev.off()

# Integrity
png(file="images/NVD-IntegImpactBoxPlot.png",height=350, width=300, bg="white")
boxplot(
       split(cvss,factor(vulnProd$cvssIntegrityImpact)),
       notch=T,
       main="NVD: CVSS Score by Integ Impact\nJan 2006 to Present",
       ylab="CVSS Score", xlab="Integrity Impact",
       border="#0000CC", col="#6699CC")
dev.off()

# Availability
png(file="images/NVD-AvailImpactBoxPlot.png",height=350, width=300, bg="white")
boxplot(
       split(cvss,factor(vulnProd$cvssAvailabilityImpact)),
       notch=T,
       main="NVD: CVSS Score by Avail Impact\nJan 2006 to Present",
       ylab="CVSS Score", xlab="Availability Impact",
       border="#0000CC", col="#6699CC")
dev.off()

# ------------------ THE END -------------------------
