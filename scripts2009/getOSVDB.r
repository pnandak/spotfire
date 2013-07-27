# ***************************************************
#            getOSVDB.r
# ***************************************************
# This R script reads vuln data from the OSVDB
# and computes metrics for display in the OSVDB
# dashboard
#
# Two datasets delivered:
#     ovuln:  All vulnerability records in OSVDB
#     ovp: All vuln-vendor-prod-ver records in OSVDB (the raw table)
#
# The timeDate library conflicts with the MySQL
# library so I am not using it here.
# ***************************************************

# Required libraries
library(RMySQL)     # For db access

# ----------------------------------------------------
# Get OSVDB dataset: ovuln
# ----------------------------------------------------
mgr<-dbManager("MySQL")
con<-dbConnect(mgr, group="osvdb")

ors2<-dbExecStatement(con, "select * from vulnerabilities;")
or2<-fetch(ors2,n = -1)

# Close out the MySQL connection
close(ors2)
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
year<-strftime(strptime(as.character(or2[,3]), format="%Y-%m-%d"), format="%Y")
qtr<-as.integer(((as.integer(strftime(strptime(as.character(or2[,3]), format="%Y-%m-%d"), format="%m")))-1)/3 + 1)
quarter<-paste(year,qtr,sep="-")
month<-strftime(strptime(as.character(or2[,3]), format="%Y-%m-%d"), format="%Y-%m")

ovuln<-cbind(year,quarter,month,or2)
attach(ovuln)

# ----------------------------------------------------
# Get OSVDB dataset: ovp
# ----------------------------------------------------
mgr<-dbManager("MySQL")
con<-dbConnect(mgr, group="osvdb")
ors<-dbExecStatement(con, "select * from raw;")
or<-fetch(ors,n = -1)

# Close out the MySQL connection
close(ors)
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
year<-strftime(strptime(as.character(or[,3]), format="%Y-%m-%d"), format="%Y")
qtr<-as.integer(((as.integer(strftime(strptime(as.character(or[,3]), format="%Y-%m-%d"), format="%m")))-1)/3 + 1)
quarter<-paste(year,qtr,sep="-")
month<-strftime(strptime(as.character(or[,3]), format="%Y-%m-%d"), format="%Y-%m")

ovp<-as.data.frame(cbind(year,quarter,month,or))
attach(ovp)

# --------------------------------------------------
# Data cleansing
# --------------------------------------------------
# Replace empty product names with "NotSpecified"  (osvdb needs this)
ovp[ ovp$product=="",8 ]<-"NotSpecified"
ovp[ ovp$vendor=="",7 ]<-"NotSpecified"

# ---------------------------------------------------
rm(or,ors,or2,ors2)

# ----------------------------------------------------
# Get OSVDB vv table in dataset: vv
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
con<-dbConnect(mgr, group="osvdb")

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

