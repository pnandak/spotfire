# ***************************************************
#            getDLDB.r 
# ***************************************************
# This R script reads raw datalossDB data from the 
# Open Security Foundation web site
# ***************************************************

# ----------------------------------------------------
# R Logic for DatalossDB:  Populate three datasets:
#   events:  All data augmented with time dimension columns
#   ds: Data starting with 2006.  Subset of events.
#   ds.logTA:  Subset of ds where TA>0 with logTA column appended
#   Note:  If the data gets voluminous, then we should delete e
# ----------------------------------------------------
# DatalossDB Data Source File
rawFile<-"http://datalossdb.org/exports/dataloss.csv"
e<-read.csv(rawFile, sep=",", header=F, skip=0, comment.char="", na.strings="?", blank.lines.skip=T)
#            V1     V2        V3        V4        V5           V6           V7         V8       V9         V10            V11             V12       V13   V14    V15           V16             V17               V18
names(e)<-c("Date","Company","Country","Segment","SubSegment","BreachType","DataLost","Source","ThirdParty","ThirdPartyName","TotalAffected","RefPage","UID","XREF","StockSymbol","DataRecovered","ConsumerLawSuit","ArrestProsecution") 

# Set some parameters
#   CurrentMON:  The current month whose data is incomplete
#   CurrentQTR:  The current quarter whose data is incomplete
m<-format(Sys.time(),"%m")
y<-format(Sys.time(),"%Y")
q<-as.integer((as.integer(m) - 1)/3 + 1)
CurrentMON<-paste(y,m,sep="-")
CurrentQTR<-paste(y,q,sep="-")

# Set Exclusions for calculations
excludeQTR<-c(CurrentQTR,"Na","NA","na","Na-NA","?",NULL)
excludeMON<-c(CurrentMON,"Na","NA","na","Na-NA","?",NULL)

e<-read.csv(rawFile, sep=",", header=F, skip=0, comment.char="", na.strings="?", blank.lines.skip=T)
#            V1     V2        V3        V4        V5           V6           V7         V8       V9         V10            V11             V12       V13   V14    V15           V16             V17               V18
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
attach(ds)
names(ds)

# **************************************************************************
# Clean up ds data
# **************************************************************************
ds2<-ds
# Clean up Segment data
ds2$Segment[grep("^Biz/",ds$Segment)]<-"Biz"
ds2$Segment[grep("^Edu/",ds$Segment)]<-"Edu"
ds2$Segment[grep("^Gov/",ds$Segment)]<-"Gov"
ds2$Segment[grep("^Med/",ds$Segment)]<-"Med"

# Clean up Source data but retain original
InOut<-ds2$Source
ds2<-cbind(ds2,InOut)
ds2$InOut[grep("^Inside",ds2$InOut)]<-"Inside"
ds2$InOut[grep("^Outside",ds2$InOut)]<-"Outside"

ds2$SubSegment[grep("^Data/",ds$SubSegment)]<-"Data"
ds2$SubSegment[grep("^Fed/",ds$SubSegment)]<-"Fed"
ds2$SubSegment[grep("^Fin/",ds$SubSegment)]<-"Fin"
ds2$SubSegment[grep("^Ind/",ds$SubSegment)]<-"Ind"
ds2$SubSegment[grep("^Media/",ds$SubSegment)]<-"Media"
ds2$SubSegment[grep("^Med/",ds$SubSegment)]<-"Med"
ds2$SubSegment[grep("^State/",ds$SubSegment)]<-"State"
ds2$SubSegment[grep("^Tech/",ds$SubSegment)]<-"Tech"
ds<-drop.levels(ds2)

ds.NZ<-ds[ as.numeric(as.character(TotalAffected)) > 0, ]
logTA<-log(as.numeric(as.character(ds.NZ$TotalAffected)),10)
ds.logTA<-cbind(ds.NZ,logTA)
attach(ds.logTA)

# Reclassify Type by More or Less Mitigatable
MitigationType<-as.character(ds.logTA$BreachType)
MitigationType[grep("^Disposal",MitigationType)]<-"More"
MitigationType[grep("^Email",   MitigationType)]<-"More"
MitigationType[grep("^Fraud",   MitigationType)]<-"Less"
MitigationType[grep("^Hack",    MitigationType)]<-"Less"
MitigationType[grep("^Lost",    MitigationType)]<-"More"
MitigationType[grep("^Missing", MitigationType)]<-"More"
MitigationType[grep("^Snail",   MitigationType)]<-"Less"
MitigationType[grep("^Stolen",  MitigationType)]<-"Less"
MitigationType[grep("^Virus",   MitigationType)]<-"Less"
MitigationType[grep("^Web",     MitigationType)]<-"Less"

BT<-as.character(ds.logTA$BreachType)
BT[grep("^Disposal",  BT)]<-"Disposal"
BT[grep("^Email",     BT)]<-"Email"
BT[grep("^Fraud",     BT)]<-"Fraud"
BT[grep("^Hack",      BT)]<-"Hack"
BT[grep("^Lost",      BT)]<-"Lost"
BT[grep("^Missing",   BT)]<-"Missing"
BT[grep("^Snail",     BT)]<-"Snail"
BT[grep("^Stolen",    BT)]<-"Stolen"
BT[grep("^Virus",     BT)]<-"Virus"
BT[grep("^Web",       BT)]<-"Web"

# Clean up original Source data (shorten labels for graphs)
SRC<-as.character(ds.logTA$Source)
SRC[grep("Inside-Accidental",SRC)]<-"In-Acc"
SRC[grep("Inside-Malicious", SRC)]<-"In-Mal"

ds.logTA<-drop.levels(cbind(ds.logTA,MitigationType,BT,SRC))
attach(ds.logTA)

rm(ds2,e,ds.NZ,InOut,MitigationType)
rm(y,q,qtr,m,logTA)
rm(year,quarter,month)
attach(ds)

# The end of setting up the DataLossDB dataset

# Create file for each dataset
# write.table(ds.logTA, file = "dsLogTA.csv", sep = ",", col.names = NA, qmethod = "double")

# Create file mc-dldb.csv that expands breaches with multiple datalost types into separated records
Origin<-"DLDB"
mc<-cbind(ds.logTA[1,],Origin)
N<-dim(ds.logTA)[1]
for (i in (1:N)) {
  Origin<-"DLDB"
  breach<-cbind(ds.logTA[i,],Origin)
  if ( i > 1 ) { mc<-rbind(mc,breach) }
  dlstring<-unlist(strsplit(as.character(breach$DataLost),"/"))
  M<-length(dlstring)
  if (M > 0) {
    for (j in (1:length(dlstring))) {
      breach$DataLost<-dlstring[j]
      breach$Origin<-"MC"
      mc<-rbind(mc,breach)
    }
  }
}
# write.table(mc, file = "mc-dldb.csv", sep = ",", col.names = NA, qmethod = "double")
