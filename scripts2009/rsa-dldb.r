rm(list=ls())
# Load up the standard visualization libraries for Flash
source("rsaLib.r")
source("hmLib.r")
source("dldbChartsLib.r")

# Required libraries
library(gplots)  # For outputting images
library(gdata)   # For drop.levels
library(stats)
library(Rcmdr)   # For Levene test for homogeneity of variance

# Set up environment
setEnv("rsa-metrics/")

# Ingest and massage the DataLossDB data set
# Return two datasets:  
#   events = raw data from DLDOS.  
#   ds = massaged data after 2005
source("getDLDB.r")

# ************************************************************************
# ANALYSIS OF event DATASET
# ************************************************************************
# Breach count per quarter from the beginning
# Determine sample size
count.quarter<-table(events[,2],exclude=excludeQTR, dnn="Quarter")
count.month<-table(events[,3],exclude=excludeMON, dnn="Month")

count.quarter
count.month

png(file="images/DLDB-BreachCountPerMonth-ALL.png",bg="white",width=900,height=350)
plot(count.month,"b",
     main=paste("DatalossDB: BreachCount/Month\nALL to ",date()),
     ylab="BreachCount")
dev.off()

png(file="images/DLDB-BreachCountPerQuarter-ALL.png",bg="white",width=900,height=350)
plot(count.quarter,"b",
     main=paste("DatalossDB: BreachCount/Quarter\nALL to ",date()),
     ylab="BreachCount")
dev.off()

# ************************************************************************
# ANALYSIS of ds DATASET
# ************************************************************************
# How many companies are in each Segment
foo<-as.data.frame(table(ds$Company,ds$Segment))
names(foo)<-c("Company","Segment","Freq")
attach(foo)
foo<-foo[ Freq > 0, ]
bsum<-tapply(foo$Freq,foo$Segment,sum)
# Company count per segment
cc<-table(foo$Segment)
# Company percent per segment
cp<-round(cc/sum(cc),2)
# Mean breach count per company
bmean<-round(tapply(foo$Freq,foo$Segment,mean),2)

count.1Q06<-table(ds$quarter,exclude=excludeQTR, dnn="Quarter")
count.1Q06

count.1M06<-table(ds$month, exclude=excludeMON, dnn="Month")
count.1M06

# One big whisker to show distribution of quarterly BreachCounts
png(file="images/DLDB-QtrBreachDist.png",bg="white",width=350,height=350)
boxplot(count.1Q06, ylab="BreachCount",
   main=paste("DatalossDB: Distribution of BreachCount/Qtr\n1Q06 to ",date()),
   col="#6699CC", notch=F)
dev.off()

# One big whisker to show distribution of monthly BreachCounts
png(file="images/DLDB-MonBreachDist.png",bg="white",width=350,height=350)
boxplot(count.1M06, ylab="BreachCount",
   main=paste("DatalossDB: Distribution of BreachCount/Mon\n1Q06 to ",date()),
   col="#6699CC", notch=F)
dev.off()

summary(as.numeric(count.1M06))

# A whisker plot for Breaches/Mon for each level of InOut
foo<-table(ds$month,ds$InOut)
m.in<-foo[,1]
m.out<-foo[,2]
m.unk<-foo[,3]
m.all<-c(m.in,m.out,m.unk)
N<-length(m.in)
label<-factor(c(rep("Inside",N),rep("Outside",N),rep("Unknown",N)))
png(file="images/DLDB-BreachCountPerMonBySource.png",bg="white",width=350,height=350)
boxplot(m.all~label,notch=T,xlab="Source",ylab="BreachCount/Month",
     main=paste("DatalossDB: BreachCount/Mon by Source\n1Q06 to ",date()),
     col="#6699CC")
dev.off()

# Are Source and BreachCount/Month independent?
twoWay(label,m.all)

chisq.test(m.all)

png(file="images/DLDB-BreachCountPerQuarter.png",bg="white",width=900,height=350)
plot(count.1Q06,"b",
     main=paste("DatalossDB: BreachCount/Qtr\n1Q06 to ",date()),
     ylab="BreachCount")
dev.off()

png(file="images/DLDB-BreachCountPerQuarter-Reg.png",bg="white",width=900,height=350)
plot(count.1Q06,"b",
     main=paste("DatalossDB: BreachCount/Qtr\n1Q06 to ",date()),
     ylab="BreachCount")
abline(lsfit(1:length(count.1Q06),count.1Q06),col="red", lwd=3, lty=2)
dev.off()

png(file="images/DLDB-BreachCountPerMonth.png",bg="white",width=900,height=350)
plot(count.1M06,"b",
     main=paste("DatalossDB: BreachCount/Month\n1Q06 to ",date()),
     ylab="BreachCount")
dev.off()

summary(as.numeric(count.1M06))

png(file="images/DLDB-BreachCountPerMonth-Reg.png",bg="white",width=900,height=350)
plot(count.1M06,"b",
     main=paste("DatalossDB: BreachCount/Month\n1Q06 to ",date()),
     ylab="BreachCount")
abline(lsfit(1:length(count.1M06),count.1M06),col="red", lwd=3, lty=2)
dev.off()

summary(as.numeric(count.1M06))

# Key parameters
bc.min<-min(count.1M06)
bc.mean<-mean(count.1M06)
bc.max<-max(count.1M06)
bc.sd<-sqrt(var(count.1M06))
bc.stdErr<-sqrt((var(count.1M06)/length(count.1M06)))

parms<-hist(count.1M06,plot=F)
x.max<-max(parms$breaks)
h.max<-max(parms$counts)+4
area<-sum(parms$counts)*(x.max/length(parms$mids))

png(file="images/DLDB-BreachHistMon.png",bg="white",width=350,height=350)
hist(count.1M06,xlab="Ranges of BreachCount/Month", ylab="# Months",
   main=paste("DatalossDB: Histogram of BreachCount/Month\n1Q06 to ",date()),
   labels=TRUE, ylim=c(0,h.max),
   col="#6699CC")
xv<-seq(bc.min,bc.max,0.1)
yv<-dnorm(xv,mean=bc.mean,sd=bc.sd)*area
lines(xv,yv,col="red",lwd=3)
dev.off()

# Is this distribution normal?
skew(count.1M06)
kurtosis(count.1M06)
shapiro.test(count.1M06)

confidence<-c(.999,.995,.99,.95,.90)
K<-length(confidence)
cint<-1 - (1-confidence)/2
df<-length(count.1M06) - 1
low<-c(1:K)
high<-c(1:K)
tval<-c(1:K)
for (i in (1:K)) {
   tval[i]<-qt(cint[i],df)
   low[i]<-bc.mean -  (tval[i]*bc.stdErr)
   high[i]<-bc.mean + (tval[i]*bc.stdErr)
}
ans<-round(cbind(confidence*100,low, high),2)
ans
round(bc.stdErr,2)
count.1M06

# Model Checking
nn<-c(1:length(count.1M06))
coef(lm(count.1M06 ~ nn))
model<-lm(count.1M06 ~ nn)
summary.lm(model)
summary.aov(model)

# Check "goodness" of model.  Get four plots.  See RSA presentation
# and both stat books on leverage
plot(model)

# Need three CR-LF's for above plot call




# Key parameters
bc.min<-min(count.1Q06)
bc.mean<-mean(count.1Q06)
bc.max<-max(count.1Q06)
bc.sd<-sqrt(var(count.1Q06))
bc.stdErr<-sqrt((var(count.1Q06)/length(count.1Q06)))

# Summary plot
png(file="images/DLDB-QtrBreachDist.png",bg="white",width=350,height=350)
boxplot(count.1Q06, xlab="Quarterly Totals: 1Q06 to Present", 
   ylab="BreachCount",
   main=paste("DatalossDB: Distribution of BreachCount/Qtr\n1Q06 to ",date()),
   col="#6699CC", notch=F)
dev.off()

png(file="images/DLDB-BreachHistQtr.png",bg="white",width=350,height=350)
hist(count.1Q06,xlab="Ranges of BreachCount/Qtr", ylab="# Quarters",
   main=paste("DatalossDB: Histogram of BreachCount/Qtr\n1Q06 to ",date()),
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

# *********************************************************************
# Impact Analysis:  TA and log10(TA)
# *********************************************************************

# Homogeneity of Variances for Several Groups
l.yr<- levene.test(ds.logTA$logTA,ds.logTA$year)
l.seg<-levene.test(ds.logTA$logTA,ds.logTA$Segment)
l.src<-levene.test(ds.logTA$logTA,ds.logTA$Source)
l.io<- levene.test(ds.logTA$logTA,ds.logTA$InOut)
l.bt<- levene.test(ds.logTA$logTA,ds.logTA$BreachType)
l.mt<- levene.test(ds.logTA$logTA,ds.logTA$iMitigationType)
l.cs<- levene.test(ds.logTA$logTA,ds.logTA$ClientServer)

# Normality testing for Several Groups
y2009<-ds.logTA [ year == "2009", 23 ]
y2008<-ds.logTA [ year == "2008", 23 ]
y2007<-ds.logTA [ year == "2007", 23 ]
y2006<-ds.logTA [ year == "2006", 23 ]

# ----------------------------------------------
# Normal distribution testing
# ---------------------------------------------
png(file="images/DLDB-BreachHistQtr.png",bg="white",width=350,height=350)
qqmath(~ logTA | InOut, aspect = "xy", data = ds.logTA,
      prepanel = prepanel.qqmathline,
      panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
      })
dev.off()

# Year
k<-length(levels(year))
png(file="images/DLDB-BreachQQYear.png",bg="white",width=350,height=350)
qqmath(~ logTA | year, data=ds.logTA,main="Year",
      aspect=.75, layout=c(1,k), index.cond=list(1:k),
      prepanel = prepanel.qqmathline,
      panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
     })
dev.off()

# InOut
k<-length(levels(InOut))
png(file="images/DLDB-BreachQQInOut.png",bg="white",width=350,height=350)
qqmath(~ logTA | InOut, data=ds.logTA, main="InOut",
      aspect=1, layout=c(k,1), index.cond=list(1:k),
      prepanel = prepanel.qqmathline,
      panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
     })
dev.off()

# MitigationType
png(file="images/DLDB-BreachQQMT.png",bg="white",width=350,height=350)
k<-length(levels(MitigationType))
qqmath(~ logTA | MitigationType, data=ds.logTA, main="MitigationType",
      aspect=1, layout=c(k,1), index.cond=list(1:k),
      prepanel = prepanel.qqmathline,
      panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
     })
dev.off()

# ClientServer
k<-length(levels(ClientServer))
png(file="images/DLDB-BreachQQCS.png",bg="white",width=350,height=350)
qqmath(~ logTA | ClientServer, data=ds.logTA, main="ClientServer",
      aspect=1, layout=c(k,1), index.cond=list(1:k),
      prepanel = prepanel.qqmathline,
      panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
     })
dev.off()

# Are breaches getting worse?
TA<-as.numeric(as.character(ds$TotalAffected))
max.TA=max(TA)
min.TA=min(TA)
mean.TA=mean(TA)
sd.TA=sqrt(var(TA))
rbind(min.TA,mean.TA,max.TA,sd.TA)

# logy<-as.numeric(rapply(as.list(impact),log,classes="numeric",how="replace",base=10))
logy<-as.numeric(as.character(ds.logTA$logTA))
ta.min=min(logy)
ta.mean=mean(logy)
ta.max=max(logy)
ta.sd=sqrt(var(logy))
summary(logy)
skew(logy)
kurtosis(logy)
removed<-dim(ds)[1] - dim(ds.logTA)[1]

# Get a Q-Q Plot to Determine if this is a normal distribution
png(file="images/DLDB-TA-QQPlot.png",width=350,height=350,bg="white")
qqnorm(logy, ylab="log10(TotalAffected) Quantiles", col="#6699FF")
qqline(logy, col="#FF0000", lwd=2)
legendText<-paste("ds.log10TA
Removed: ", removed)
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()

parms<-hist(logy,plot=F)
x.max=max(parms$breaks)
area=sum(parms$counts)*(x.max/length(parms$mids))

png(file="images/DLDB-logTAHist.png",width=350,height=350,bg="white")
hist(logy, col="#6699CC", xlab="log10(TotalAffected/Breach)",
     ylab="BreachCount", 
     main=paste("DatalossDB: log10(TotalAffected) Histogram\n1Q06 to ",date()))
xv<-seq(0,x.max,0.1)
yv<-dnorm(xv,mean=ta.mean,sd=ta.sd)*area
lines(xv,yv,col="red",lwd=3)
dev.off()

summary(as.numeric(as.character(ds$TotalAffected)))
summary(as.numeric(as.character(ds.logTA$logTA)))
sqrt(var(as.numeric(as.character(ds$TotalAffected))))
sqrt(var(as.numeric(as.character(ds.logTA$logTA))))
skew(as.numeric(as.character(ds.logTA$logTA)))
kurtosis(as.numeric(as.character(ds.logTA$logTA)))
shapiro.test(as.numeric(as.character(ds.logTA$logTA)))

png(file="images/DLDB-SegmentImpactBoxPlot.png",bg="white",width=350,height=350)
boxplot(
     split(logy,factor(ds.logTA$Segment)),
     notch=T,
     main=paste("DataLossDB: log10(TotalAffected) by Segment\n1Q06 to ",date()),
     ylab="log10(TotalAffected)",
     border="#0000CC", col="#6699CC")
legendText<-paste("Number of breaches with TotalAffected=0: ",removed,"
All were removed in calculations for this graph.")
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()

png(file="images/DLDB-YearImpactBoxPlot.png",bg="white",width=350,height=350)
boxplot(
   split(logy,factor(ds.logTA$year)),
   notch=T,
   main=paste("DataLossDB: log10(TotalAffected) by Year\n1Q06 to ",date()),
   ylab="log10(TotalAffected)",
   border="#0000CC", col="#6699CC")
legendText<-paste("Number of breaches with TotalAffected=0: ",removed,"
All were removed in calculations for this graph.")
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()

png(file="images/DLDB-QtrImpactBoxPlot.png",bg="white",width=900,height=350)
boxplot(
   split(logy,factor(ds.logTA$quarter)),
   notch=T,
   main=paste("DataLossDB: log10(TotalAffected) by Qtr\n1Q06 to ",date()),
   ylab="log10(TotalAffected)",
   border="#0000CC", col="#6699CC")
legendText<-paste("Number of breaches with TotalAffected=0: ",removed,"
All were removed in calculations for this graph.")
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()

png(file="images/DLDB-SourceImpactBoxPlot.png",bg="white",width=350,height=350)
boxplot(
   split(logy,factor(ds.logTA$Source)),
   notch=T, names=c("Inside","In-Acc","In-Mal","Outside","Unknown"),
   main=paste("DataLossDB: log10(TotalAffected) by Qtr\n1Q06 to ",date()),
   ylab="log10(TotalAffected)",
   border="#0000CC", col="#6699CC")
legendText<-paste("Number of breaches with TotalAffected=0: ",removed,"
All were removed in calculations for this graph.")
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()

png(file="images/DLDB-InOutImpactBoxPlot.png",bg="white",width=350,height=350)
boxplot(
   split(logy,factor(ds.logTA$InOut)),
   notch=T, 
   main=paste("DataLossDB: log10(TotalAffected) by InOut\n1Q06 to ",date()),
   ylab="log10(TotalAffected)",
   border="#0000CC", col="#6699CC")
legendText<-paste("Number of breaches with TotalAffected=0: ",removed,"
All were removed in calculations for this graph.")
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()


png(file="images/DLDB-CSImpactBoxPlot.png",bg="white",width=350,height=350)
boxplot(
   split(logy,factor(ds.logTA$ClientServer)),
   notch=T, 
   main=paste("DataLossDB: log10(TotalAffected) by ClientServer\n1Q06 to ",date()),
   ylab="log10(TotalAffected)",
   border="#0000CC", col="#6699CC")
legendText<-paste("Number of breaches with TotalAffected=0: ",removed,"
All were removed in calculations for this graph.")
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()

png(file="images/DLDB-MTImpactBoxPlot.png",bg="white",width=350,height=350)
boxplot(
   split(logy,factor(ds.logTA$MitigationType)),
   notch=T, 
   main=paste("DataLossDB: log10(TotalAffected) by Mitigation\n1Q06 to ",date()),
   ylab="log10(TotalAffected)",
   border="#0000CC", col="#6699CC")
legendText<-paste("Number of breaches with TotalAffected=0: ",removed,"
All were removed in calculations for this graph.")
legend("bottomright",legendText,bty="n",text.col="#FF0000")
dev.off()


# ***************************************************************
# ANOVA:  TA as a function of other factors
# ***************************************************************
# Function to plot a bar graph with error bars (an alternative to whisker plots)
error.bars<-function(yv,z,nn) {
  xv<-barplot(yv,ylim=c(0,(max(yv)+max(z))), names=nn, ylab=deparse(substitute(yv)))
  g<-(max(xv)-min(xv))/50
  for (i in 1:length(xv)) {
     lines(c(xv[i],xv[i]),    c(yv[i]+z[i],yv[i]-z[i]))
     lines(c(xv[i]-g,xv[i]+g),c(yv[i]+z[i],yv[i]+z[i]))
     lines(c(xv[i]-g,xv[i]+g),c(yv[i]-z[i],yv[i]-z[i]))
  }
}

# logTA as a function of Source
model.Source<-aov(logTA,Source, data=ds.logTA)

se<-1.49/table(ds.logTA$Source)
labels<-as.character(levels(ds.logTA$Source))

ybar<-as.vector(tapply(ds.logTA$logTA,ds.logTA$Source,mean))
error.bars(ybar,se,labels)

# logTA as a function of various factors 
model.Year<-aov(logTA ~ year, data=ds.logTA)
summary.lm(model.Year)
model.Segment<-aov(logTA ~ Segment, data=ds.logTA)
summary.lm(model.Segment)
model.Source<-aov(logTA ~ Source, data=ds.logTA)
summary.lm(model.Source)
model.InOut<-aov(logTA ~ InOut, data=ds.logTA)
summary.lm(model.InOut)
model.MT<-aov(logTA ~ MitigationType, data=ds.logTA)
summary.lm(model.MT)
model.CS<-aov(logTA ~ ClientServer, data=ds.logTA)
summary.lm(model.CS)
model.BT<-aov(logTA ~ BreachType, data=ds.logTA)
summary.lm(model.BT)
model.DL<-aov(logTA ~ DataLost, data=ds.logTA)
summary.lm(model.DL)

# ====================================================

# logTA as a function of Segment and Source
ds6<-drop.levels(ds.logTA)
means.SBT<-tapply(ds6$logTA,list(ds6$Source,ds6$BreachType),mean)
means.SBT
model.SBT<-aov(ds6$logTA ~ ds6$Source*ds6$BreachType)

# logTA as a function of Segment and Source
ds6<-drop.levels(ds6)
means.SDL<-tapply(ds6$logTA,list(ds6$Source,ds6$DataLost),mean)
means.SDL
model.SDL<-aov(ds6$logTA ~ ds6$Source*ds6$DataLost)

# Monthly: Insider vs Outsider Breach Trends and Projections
source.count<-table(ds$month,ds$Source, exclude=excludeMON, dnn="Yr-Mon")

# inside Breaches are the sum of several categories:
#     Inside
#     Inside-Accidental
#     Inside-Malicious
#     Total
inside<-source.count[,1]+source.count[,2]+source.count[,3]
outside<-source.count[,4]
unknown<-source.count[,5]
total<-table(ds$month,exclude=excludeMON,dnn="Month")
N<-length(total)

# Visualize as a static image
png(file="images/DLDB-MonVolumeBySource-Trend.png",bg="white",width=900,height=350)
plot(total,"p",pch=21,bg="white",
   main=paste("DataLossDB: BreachCount/Month by Source\n1Q06 to ",date()),
   ylab="BreachCount",ylim=c(0,max(total)))
abline(lsfit(1:N,total),lty=2,col="black",lwd=2)
points(1:N,inside,col="red",pch=24,bg="red")
points(1:N,outside,col="blue",pch=22,bg="blue")
abline(lsfit(1:N,inside),lty=2,col="red",lwd=2)
abline(lsfit(1:N,outside),lty=2,col="blue",lwd=2)
ltext<-c("Total","Outside","Inside")
legend(1,60,ltext, pch=c(21,24,22),
    col=c("black","blue","red"),
    pt.bg=c("white","blue","red"),
    text.col=c("black","blue","red"))
dev.off()

nn<-c(1:length(total))
fitted.total<-coef(lm(total ~ nn))
fitted.inside<-coef(lm(inside ~ nn))
fitted.outside<-coef(lm(outside ~ nn))

# x Coordinates for two points on trend line
x<-levels(ds$month)
yvalues<-as.data.frame(cbind(total,inside,outside))
tx<-c(x[1], x[length(x)])
y.total<-c(fitted.total[1],(fitted.total[1]+length(x)*fitted.total[2]))
y.inside<-c(fitted.inside[1],(fitted.inside[1]+length(x)*fitted.inside[2]))
y.outside<-c(fitted.outside[1],(fitted.outside[1]+length(x)*fitted.outside[2]))

tvalues<-cbind(tx,y.total,y.inside,y.outside)
gradients<-c(fitted.total[2],fitted.inside[2],fitted.outside[2])

# Quarterly: Insider vs Outsider Breach Trends and Projections
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

# Visualize as a static image
png(file="images/DLDB-VolumeBySource-Trend.png",bg="white",width=900,height=350)
plot(total,"p",pch=21,bg="white", 
   main=paste("DataLossDB: BreachCount/Qtr by Source\n1Q06 to ",date()), 
   ylab="BreachCount",ylim=c(0,200))
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

# -------------------------------------------------
#  Scatter Plot with Trend Line:  BC by Source per Month
# -------------------------------------------------
primecolor="#6699CC"

# Visualize as a flash trend line with monthly BC totals
# Gradient and intercept calculations
#   BreachCountTrendBySource
# -------------------------------------------------
# Compute y-intercept and gradients for the trend lines
source.count<-table(ds$month,ds$Source, exclude=excludeMON, dnn="Yr-Mon")

# inside Breaches are the sum of several categories:
#     Inside
#     Inside-Accidental
#     Inside-Malicious
#     Total
inside<-source.count[,1]+source.count[,2]+source.count[,3]
outside<-source.count[,4]
unknown<-source.count[,5]
total<-table(ds$month,exclude=excludeMON,dnn="Month")
N<-length(total)

nn<-c(1:length(total))
fitted.total<-coef(lm(total ~ nn))
fitted.inside<-coef(lm(inside ~ nn))
fitted.outside<-coef(lm(outside ~ nn))

# x Coordinates for two points on trend line
x<-levels(ds$month)
yvalues<-as.data.frame(cbind(total,inside,outside))
tx<-c(x[1], x[length(x)])
y.total<-c(fitted.total[1],(fitted.total[1]+length(x)*fitted.total[2]))
y.inside<-c(fitted.inside[1],(fitted.inside[1]+length(x)*fitted.inside[2]))
y.outside<-c(fitted.outside[1],(fitted.outside[1]+length(x)*fitted.outside[2]))

tvalues<-cbind(tx,y.total,y.inside,y.outside)
gradients<-c(fitted.total[2],fitted.inside[2],fitted.outside[2])

# Print XML data file with no YAH graph
yah<-0
metricName<-"MonthlyBreachCount"
graphNames<-c("total","inside","outside")
graphColors<-c("#70DE05","#999999","#BB0000")
graphBullets<-c("triangle_down","round","triangle_up")
dateFormats<-c("YYYY-MM","MM","YYYY-MM")
ylabel<-"BreachCount/Mon"
fileName<-paste(dataPath,metricName,"-ScatterData.xml",sep="")
xyPrintXMLData(yah,graphNames,graphColors,graphBullets,x,yvalues,tvalues,gradients,fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-ScatterSettings.xml",sep="")
xyPrintSettings(metricName,dateFormats,ylabel,fileName)

# Print HTML file
flashObj<-"amxy"
chartTitle<-paste("BC/Mon: 1Q06 to ",date())
amChartsPathXY<-paste(amChartsPath,"amxy/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-Scatter.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-ScatterData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-ScatterSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

# **************************************************
# BreachCountbySegment Histogram
# Distribution of Breaches by Segment starting in 2005
# **************************************************
# Some summary stats:
table(ds$Segment)

# Compute histogram value for Segment
y<-cbind(table(ds$Segment,exclude=c("Edu/Gov","Gov/Biz","Biz/Gov","Biz/Med","Biz/Edu","Med/Biz","Med/Edu","Med/Gov")))
x<-labels(y)[[1]]
metricName<-"BreachCountPctBySegment"

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
chartTitle<-paste("2006 to ", date())
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# **************************************************
# IMPACT METRICS
# **************************************************
# -------------------------------------------------*
# Company's with 1, 2, 3, ... breaches since DLDOS started
# -------------------------------------------------*
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
title(main=paste("DatalossDB: BreachCount/Company Distribution\nAll DB as of ",date()))
dev.off()

# ----------------------------------------------------
# Top 10 Lists:  BASED ON event DATASET
# ----------------------------------------------------
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
title(main=paste("DatalossDB: Most Breached Companies\nAll DB as of ",date()))
dev.off()

temp<-as.data.frame.table(tapply(events$TotalAffected,events$Company,sum))
Company<-as.character(temp$Var1)
TotalAffected<-round((as.numeric(temp$Freq)/1000000),0)
company.ta<-cbind(Company,TotalAffected)
ordered.ta<-company.ta[ order(-TotalAffected),]
ta.shame<-ordered.ta[1:10,]

png(file="images/DLDB-TA-ShameList.png", width=510, height=400, bg="white")
cols <- c("black")
textplot(ta.shame, valign="top", halign="left",
    col.data=matrix(cols, nrow=dim(ta.shame)[1], byrow=FALSE, ncol=2),
    show.colnames=TRUE, show.rownames=FALSE,rmar=1)
title(main=paste("DatalossDB: Companies with Highest Impact Breaches\nTotalAffected in Millions\nAll DB as of ",date()))
dev.off()

# ******************** END TOP 10 LISTS *********************************************
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

# --------------------------------------------------------------------------
# Flash Trend lines
# --------------------------------------------------------------------------
# Breach Type Trends
# Print XML data file
metricName<-"BreachCountByType"
ylabel<-"BreachCount/Qtr"
graphNames<-names(quarter.bt)
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00",
               "#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00")
fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
linePrintXMLData(Quarters, quarter.bt, graphNames, graphColors, fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
linePrintSettings(metricName,ylabel,fileName)

# Print HTML file
flashObj<-"amline"
chartTitle<-paste("2006 to ", date())
amChartsPathXY<-paste(amChartsPath,"amline/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

# DataLost Trends
# Print XML data file
metricName<-"BreachCountByDataLost"
ylabel<-"Breach Count"
graphNames<-names(quarter.dl)
graphColors<-c("#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00",
               "#FF3300","#3366CC","#00CC33","#FFCC00","#FF0099","#003366","#659933","CC99CC","#333333","99CC00")
fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
linePrintXMLData(Quarters, quarter.dl, graphNames, graphColors, fileName)

# Print Settings file (Note the same settings file works for both historgrams)
fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
linePrintSettings(metricName,ylabel,fileName)

# Print HTML file
flashObj<-"amline"
chartTitle<-paste("2006 to ", date())
amChartsPathXY<-paste(amChartsPath,"amline/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)

# ---------------------------------------------------------------------------
# Flash histograms 
# ---------------------------------------------------------------------------
metricName<-"BreachCountPctByType"
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
xlabel<-"Type Categories"
ylabel<-"% Breach Count"
rowPrintSettings(metricName,xlabel,ylabel,legendFlag,fileName)

# Print HTML file
chartTitle<-paste("206 to ", date())
flashObj<-"amcolumn"
amChartsPathHist<-paste(amChartsPath,"amcolumn/",sep="")
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# -------------------------------------------------------
# Visualize BreachCountByDataLost as Histogram
# BreachCountByDataLost
# Total Breaches by DataLost: 2006 to Present
# -------------------------------------------------------
metricName<-"BreachCountPctByDataLost"
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
chartTitle<-paste("2006 to ", date())
htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
amPrintHTML(flashObj,chartTitle,metricName,amChartsPathHist,dataFileName,settingsFileName,htmlFileName)

# ----------------------------------------------------------------
#  Heatmap: BreachCountTrendByType vs BreachCountTrendbyDataLost
# ----------------------------------------------------------------
metricName<-"BCTrend-TypeVsDataLost"
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

