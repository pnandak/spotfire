# ***************************************************
#            getWLF.r
# ***************************************************
# This R script reads vuln data from the NVD
# and computes the Workload Factor for display
# on a stock chart
#
# The timeDate library conflicts with the MySQL
# library so I am not using it here.
# ***************************************************

library(RMySQL)     # For db access

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

# ----------------- THE END ------------------------------
