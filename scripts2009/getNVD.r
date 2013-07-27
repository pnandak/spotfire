# ***************************************************
#            getNVD.r
# ***************************************************
# This R script reads vuln data from the NVD
# and computes the metric fro display in the
# NVD dashboard.
#
# Two datasets delivered:
#     cvss:  All vulnerability records in NVD with cvss scores
#     vp: All vuln-vendor-prod-ver records in NVD (the raw table)
#
# The timeDate library conflicts with the MySQL
# library so I am not using it here.
# ***************************************************

# Required libraries
library(RMySQL)     # For db access

# ----------------------------------------------------
# Get NVD dataset: cvss
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

# --------------------------------------------------
# Adding a day and cvssIndex attributes so we can comput WLF
# --------------------------------------------------
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

# Dataset = Vulns after 2005
START_YEAR<-2005
cvss<-as.data.frame(as.matrix(vuln[year > START_YEAR,]))
attach(cvss)

# ----------------------------------------------------
# Get NVD raw prod-vuln data in dataset: vp
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

raw<-cbind(year,quarter,month,r)
attach(raw)

# Data cleansing
# Replace empty product names with "NotSpecified"  (osvdb needs this)
raw[ product=="",8 ]<-"NotSpecified"

# Dataset = Vuln Prods after 2005
START_YEAR<-2005
vp<-as.data.frame(as.matrix(raw[as.numeric(as.character(raw$year)) > START_YEAR,]))
attach(vp)

# ----------------------------------------------------
# Get NVD vv table in dataset: vv
# ----------------------------------------------------
# This table is created via SQL and has the following columns:
#           id,
#           year,
#           quarter,
#           vendor,
#           vulnID,
#           freq
# ....derived from the raw table

mgr<-dbManager("MySQL")
con<-dbConnect(mgr, group="nvd")

vvs<-dbExecStatement(con, "select * from vv where year > 2005;")
vv<-fetch(vvs,n = -1)

# Close out the MySQL connection
close(vvs)
close(con)
unload(mgr)

# Data cleansing
# Replace empty product names with "NotSpecified"  (osvdb needs this)
vv[ vv$vendor=="",4 ]<-"NotSpecified"

Quarter<-paste(vv$year,vv$quarter,sep="-")
foo<-cbind(Quarter,vv$vendor,vv$vulnID,vv$freq)
vvf<-as.data.frame(foo)
names(vvf)<-c("quarter","vendor","vulnID","Freq")
attach(vvf)

rm(vv, foo)
