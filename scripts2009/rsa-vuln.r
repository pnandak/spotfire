# Required libraries
library(gplots)  # For outputting images
library(gdata)   # For drop.levels
library(stats)

# Breach count per quarter
# Determine sample size
count.quarter<-table(events[,2],exclude=excludeQTR, dnn="Quarter")
count.quarter

# Take only data after 2005
ds<-as.data.frame(as.matrix(events[ year > 2005, ]))
attach(ds)
names(ds)

count.1Q06<-table(ds$quarter,exclude=excludeQTR, dnn="Quarter")
count.1Q06

png(file="images/DLDB-BreachCountPerQuarter.png",bg="white",width=900,height=350)
plot(count.1Q06,"b",
     main="DatalossDB: Breach Count per  Quarter\nJan 2006 to Present",
     ylab="# Breaches Reported")
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
bc.min<-min(count.1Q06)
bc.mean<-mean(count.1Q06)
bc.max<-max(count.1Q06)
bc.sd<-sqrt(var(count.1Q06))
bc.stdErr<-sqrt((var(count.1Q06)/length(count.1Q06)))

# Summary plot
png(file="images/DLDB-QtrBreachDist.png",bg="white",width=300,height=350)
boxplot(count.1Q06, xlab="Quarterly Totals: 1Q06 to Present", 
   ylab="# Breaches",
   main="Distribution of Breaches/Quarter \n1Q06 to Present",
   col="#6699CC", notch=F)
dev.off()

png(file="images/DLDB-BreachHistQtr.png",bg="white",width=300,height=350)
hist(count.1Q06,xlab="Ranges of Breach/Quarter Counts", ylab="# Quarters",
   main="Frequency Histogram of Breaches/Quarter \n1Q06 to Present",
   labels=TRUE,
   col="#6699CC")
xv<-seq(80,150,0.1)
yv<-dnorm(xv,mean=bc.mean,sd=bc.sd)*120
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
   low[i]<-bc.mean -  (tval[i]*bc.stdErr)
   high[i]<-bc.mean + (tval[i]*bc.stdErr)
}
ans<-cbind(confidence*100,low, high)
ans
bc.stdErr
count.1Q06

# Are breaches getting worse?
TA<-as.numeric(as.character(ds2$TotalAffected))
max.TA=max(TA)
min.TA=min(TA)
mean.TA=mean(TA)
sd.TA=sqrt(var(TA))
rbind(min.TA,mean.TA,max.TA,sd.TA)

ds3<-cbind(ds2,TA)
attach(ds3)
ds4<-ds3[TA > 0,]
removed<-dim(ds3)[1] - dim(ds4)[1]
attach(ds4)
impact<-ds4$TA

logy<-as.numeric(rapply(as.list(impact),log,classes="numeric",how="replace",base=10))
ta.min=min(logy)
ta.mean=mean(logy)
ta.max=max(logy)
ta.sd=sqrt(var(logy))
rbind(ta.min,ta.mean,ta.max,ta.sd)

parms<-hist(logy,plot=F)
x.max=max(parms$breaks)
area=sum(parms$counts)*(x.max/length(parms$mids))

png(file="images/DLDB-TA-Hist.png",width=300,height=350,bg="white")
hist(logy, col="#6699CC", xlab="log10(TotalAffected/Breach)",
     ylab="# Breaches", 
     main="Frequency Histogram of log10(TotalAffected) \nJan 2006 to Present")
xv<-seq(0,x.max,0.1)
yv<-dnorm(xv,mean=ta.mean,sd=ta.sd)*area
lines(xv,yv,col="red",lwd=3)
dev.off()

png(file="images/DLDB-SegmentImpactBoxPlot.png",bg="white")
boxplot(
       split(logy,factor(ds4$Segment)),
       notch=T,
       main="DataLossDB: TotalAffected by Segment\nJan 2006 to Present",
       ylab="log10(TotalAffected/Breach)",
       border="#0000CC", col="#6699CC")
legendText<-paste("Number of breaches with TotalAffected=0: ",removed,"
All were removed in calculations for this graph.")
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()

png(file="images/DLDB-YearImpactBoxPlot.png",bg="white")
boxplot(
       split(logy,factor(ds4$year)),
       notch=T,
       main="DataLossDB: TotalAffected by Year\nJan 2006 to Present",
       ylab="log10(TotalAffected/Breach)",
       border="#0000CC", col="#6699CC")
legendText<-paste("Number of breaches with TotalAffected=0: ",removed,"
All were removed in calculations for this graph.")
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()

png(file="images/DLDB-QtrImpactBoxPlot.png",bg="white")
boxplot(
       split(logy,factor(ds4$quarter)),
       notch=T,
       main="DataLossDB: TotalAffected by Quarter\nJan 2006 to Present",
       ylab="log10(TotalAffected/Breach)",
       border="#0000CC", col="#6699CC")
legendText<-paste("Number of breaches with TotalAffected=0: ",removed,"
All were removed in calculations for this graph.")
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()

# Insider vs Outsider Breach Trends and Projections
source.count<-table(ds$quarter,ds$Source, exclude=excludeQTR, dnn="Yr-Qtr")

# inside Breaches are the sum of several categories:
#     Inside
#     Inside-Accidental
#     Inside-Malicious
#     Total
inside<-source.count[,1]+source.count[,2]+source.count[,3]
outside<-source.count[,4]
unknown<-source.count[,5]
total<-table(ds$quarter,exclude=excludeQTR,dnn="Quarter")
N<-length(total)

png(file="images/DLDB-VolumeBySource-Trend.png",bg="white")
plot(total,"p", main="DataLossDB: Breach Volume by Source\nJan 2006 to Present", 
     pch=21,bg="white", ylab="# Breaches Reported",ylim=c(0,200))
abline(lsfit(1:N,total),lty=2,col="black",lwd=2)
points(1:N,inside,col="red",pch=24,bg="red")
points(1:N,outside,col="blue",pch=22,bg="blue")
abline(lsfit(1:N,inside),lty=2,col="red",lwd=2)
abline(lsfit(1:N,outside),lty=2,col="blue",lwd=2)
ltext<-c("Total","Outside","Inside")
legend(1,200,ltext, pch=c(21,24,22),
    col=c("black","blue","red"),
    pt.bg=c("white","blue","red"),
    text.col=c("black","blue","red"))
dev.off()

primecolor="#6699CC"

# **************************************************
# BreachCountbySegment Histogram
# Distribution of Breaches by Segment starting in 2005
# **************************************************
# Compute histogram value for Segment
y<-cbind(table(ds2$Segment,exclude=c("Edu/Gov","Gov/Biz","Biz/Gov","Biz/Med","Biz/Edu","Med/Biz","Med/Edu","Med/Gov")))
x<-labels(y)[[1]]
metricName<-"BCbySegment"

# Print XML data file
fileName<-paste(dataPath,metricName,"-HistData.xml",sep="")
graphNames<-c(1:length(x))
graphColors<-c(1:length(x))
for (i in (1:length(x))) {
   graphNames[i]<-metricName
   graphColors[i]<-primecolor
}
legendFlag<-"false"
legendTitle<-"NONE"
colPrintXMLData(metricName,graphNames,graphColors,legendTitle,x,y,fileName)

# Print Settings file
fileName<-paste(settingsPath,metricName,"-HistSettings.xml",sep="")
verticalLabel<-"Market Segments"
horizontalLabel<-"% BreachCount"
rowPrintSettings(metricName,verticalLabel,horizontalLabel,legendFlag,fileName)

# Print HTML file
flashObj<-"amcolumn"
chartTitle<-"2005 to Present"
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# **************************************************
# Company's with 1, 2, 3, ... breaches since DLDOS started
# **************************************************
ds.count.month<-table(events$month, exclude=excludeMON, dnn="Month")

company.bc<-as.data.frame.table(table(events$Company))
attach(company.bc)
maxBC<-max(company.bc$Freq)
freqHist<-hist(company.bc$Freq,breaks=c(0:maxBC),plot=FALSE)
BreachesReported<-c(1:maxBC)
NumberOfCompanies<-freqHist$counts
bc.counts<-cbind(BreachesReported,NumberOfCompanies)

library(gplots)
png(file="images/DLDB-MB-Table.png", width=510, height=350, bg="white")
cols <- c("black")
textplot(bc.counts, valign="top", halign="left",
    col.data=matrix(cols, nrow=dim(bc.counts)[1], byrow=FALSE, ncol=2),
    show.colnames=TRUE, show.rownames=FALSE,rmar=1)
title(main=paste("Multiple Breach Distribution\nDataLossDB as of ",date()))
dev.off()

temp<-company.bc[Freq > 4,]
attach(temp)
Company<-as.character(temp$Var1)
BreachCount<-as.numeric(temp$Freq)

top.companies<-cbind(Company,BreachCount)
bc.shame<-top.companies[ order(-BreachCount),]

library(gplots)

library(gplots)
png(file="images/DLDB-BC-ShameList.png", width=510, height=350, bg="white")
cols <- c("black")
textplot(bc.shame, valign="top", halign="left",
    col.data=matrix(cols, nrow=dim(bc.shame)[1], byrow=FALSE, ncol=2),
    show.colnames=TRUE, show.rownames=FALSE,rmar=1)
title(main=paste("Most Breached Companies\nDataLossDB as of ",date()))
dev.off()

temp<-as.data.frame.table(tapply(events$TotalAffected,events$Company,sum))
Company<-as.character(temp$Var1)
TotalAffected<-round((as.numeric(temp$Freq)/1000000),2)
company.ta<-cbind(Company,TotalAffected)
ordered.ta<-company.ta[ order(-TotalAffected),]
ta.shame<-ordered.ta[1:10,]

png(file="images/DLDB-TA-ShameList.png", width=510, height=400, bg="white")
cols <- c("black")
textplot(ta.shame, valign="top", halign="left",
    col.data=matrix(cols, nrow=dim(ta.shame)[1], byrow=FALSE, ncol=2),
    show.colnames=TRUE, show.rownames=FALSE,rmar=1)
title(main=paste("TotalAffected in Millions: Top 10\nDataLossDB as of ",date()))
dev.off()

# **************************************************
# BreachCountBySubSegment
# **************************************************
# Later with Mondrian ...

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


metricName<-"BCbyType"
t<-stack(quarter.bt)
bt<-tapply(t$values,t$ind,sum)

# Print XML data file
# Bars all the same color and have the same name
fileName<-paste(dataPath,metricName,"-HistData.xml",sep="")
graphNames<-c(1:length(x))
graphColors<-c(1:length(x))
for (i in (1:length(bt))) {
   graphNames[i]<-metricName
   graphColors[i]<-primecolor
}
legendFlag<-"false"
legendTitle<-"NONE"
colPrintXMLData(metricName,graphNames,graphColors,legendTitle,names(bt),bt,fileName)

# Print Settings file
fileName<-paste(settingsPath,metricName,"-HistSettings.xml",sep="")
xLabel<-"Type Categories"
yLabel<-"% Breach Count"
rowPrintSettings(metricName,xLabel,yLabel,legendFlag,fileName)

# Print HTML file
flashObj<-"amcolumn"
amChartsPathHist<-paste(amChartsPath,"amcolumn/",sep="")
chartTitle<-"2005 to Present"
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# -------------------------------------------------------
# Visualize BreachCountByDataLost as Histogram
# BreachCountByDataLost
# Total Breaches by DataLost: 2006 to Present
# -------------------------------------------------------
metricName<-"BCbyDataLost"
t<-stack(quarter.dl)
dl<-tapply(t$values,t$ind,sum)

# Print XML data file
fileName<-paste(dataPath,metricName,"-HistData.xml",sep="")
graphNames<-c(1:length(x))
graphColors<-c(1:length(x))
for (i in (1:length(bt))) {
   graphNames[i]<-metricName
   graphColors[i]<-primecolor
}
legendFlag<-"false"
legendTitle<-"NONE"
colPrintXMLData(metricName,graphNames,graphColors,legendTitle,names(dl),dl,fileName)

# Print Settings file
fileName<-paste(settingsPath,metricName,"-HistSettings.xml",sep="")
verticalLabel<-"DataLost Categories"
horizontalLabel<-"% Breach Count"
rowPrintSettings(metricName,verticalLabel,horizontalLabel,legendFlag,fileName)

# Print HTML file
flashObj<-"amcolumn"
amChartsPathHist<-paste(amChartsPath,"amcolumn/",sep="")
chartTitle<-"2005 to Present"
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# ----------------------------------------------------------------
#  Heatmap: BreachCountTrendByType vs BreachCountTrendbyDataLost
# ----------------------------------------------------------------
metricName<-"BCT-TypeVsDataLost"
#                          RED       ORANGE    YELLOW   L-YELL     GREY      GREY      L-YELL   YELLOW    L-GREEN   D-GREEN
# RED-GREEN  hm.colors<-c("#FF0000","#FF3300","#FFFF00","#FFFF99","#CCCCCC","#CCCCCC","#FFFF99","#FFFF00","#00FF00","#009900")
# R STANDARD hm.cutpoints<-c( -.95,    -0.90,     -0.8,     -0.6,     -0.3,      0.3,      0.6,      0.8,      0.9,     0,95)
# CSS classes
hm.classes<-  c("noc","low","med","hi","vhi")
hm.cutpoints<-c( 0.3,  0.6,  0.8,  0.9)
hm.values<-round(cor(quarter.bt,quarter.dl),2)
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

