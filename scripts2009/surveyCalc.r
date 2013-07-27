# ***************************************************
#             surveyCalc.r
# ***************************************************
# This R script reads raw survey data from a file, performs
# computations and prints files needed for amCharts to 
# visualized results as flash "quantlets"
#
# This script depends upon a collection of functions defined
# in the libMetricsViz.r file.
# ***************************************************
#  
# **********   Notes on HISTOGRAM CHARTS
# x$breaks gives the breakpoints for the bars
# x$counts gives the counts per bar
# range(x$breaks) gives the range of values on the x-axis from the lowest to highest bar
# length(x$breaks) gives the number of bars
# max(x$breaks) gives tha highest break value
# sum(x$counts)
# *************************************************** 
# Filenames and directories to write html,data, and settings files
topDir<-"/home/ean/Desktop/amCharts/survey-metrics/"
htmlPath<-paste(topDir,"html/",sep="")
dataPath<-paste(topDir,"data/",sep="")

settingsPath<-paste(topDir,"settings/",sep="")
rawFile<-paste(topDir,"raw/survey-raw2.csv",sep="")

# Read a raw file with values from survey in a values column.  One row for each customer and date.
raw<-read.csv(rawFile,sep=";", header=T)
attach(raw)
metrics<-names(raw)      # Metric names start at item number 4
d<-levels(date)          # Capture date labels
# File paths to use inside html and settings files
amChartsPath<-paste("../../../amCharts/",sep="")

# These are all RELATIVE to topDir/html
amChartsPathCol<-paste(amChartsPath,"amcolumn/",sep="")
amChartsPathXY<-paste(amChartsPath,"amxy/",sep="")
amChartsPathLine<-paste(amChartsPath,"amline/",sep="")
amChartsPathStock<-paste(amChartsPath,"amstock/",sep="")
dataRelPath<-"../data/"
settingsRelPath<-"../settings/"

# For each metric: generate data, settings, and html files
for (m in (4:length(metrics))) {

# **************************************************
# Histograms 
# **************************************************
metricName<-metrics[m]
primecolor<-"#999999"
yahcolor<-"#6699CC"
nbreaks<-5
# Compute histograms: Quintiles
h<-hist(raw[,m], breaks=nbreaks, plot=FALSE)

# Compute number of bars to plot
nbars<-length(h$breaks)-1

# Generate x-axis labels
graphNames<-c(1:nbars)
graphColors<-c(1:nbars)
x<-c(1:nbars)
for (i in (1:nbars)) {
     x[i]<-paste(h$breaks[i],"-",h$breaks[i+1],sep="")
     graphColors[i]<-primecolor
     graphNames[i]<-metricName
}
y<-h$counts  
 
# Print XML data file
  legendFlag<-"true"
  legendTitle<-"All Submitted Data" 
  fileName<-paste(dataPath,metricName,"-HistData.xml",sep="")
  colPrintXMLData(metricName,graphNames,graphColors,legendTitle,x,y,fileName) 

# Print Settings file 
  fileName<-paste(settingsPath,metricName,"-HistSettings.xml",sep="")
  xLabel<-paste(metricName,": Value Ranges",sep="")
  yLabel<-"% Weekly Reports"
  colPrintSettings(metricName,xLabel,yLabel,legendFlag,fileName)

# Print HTML file
  flashObj<-"amcolumn"
  chartTitle<-"YTD"
  htmlFileName<-paste(htmlPath,metricName,"-Hist.html",sep="")
  dataFileName<-paste(dataRelPath,metricName,"-HistData.xml",sep="")
  settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
  amPrintHTML(flashObj,chartTitle,metricName,amChartsPathCol,dataFileName,settingsFileName,htmlFileName)  

# ***************** YAH Histogram Data for each metric
  value<-raw[518,m]      # User #8's most current data point in the raw dataset
  yahbar<-yahInterval(h,value)
  graphNames[yahbar]<-"YAH"
  graphColors[yahbar]<-yahcolor
  legendFlag<-"true"
  legendTitle<-"YouAreHere"
# Print XML data file
  fileName<-paste(dataPath,metricName,"-HistYahData.xml",sep="")
  colPrintXMLData(metricName,graphNames,graphColors,legendTitle,x,y,fileName) 

# Print Settings file -- Settings file is the same for both histogram and yah histogram charts
  fileName<-paste(settingsPath,metricName,"-HistYahSettings.xml",sep="")
  colPrintSettings(metricName,xLabel,yLabel,legendFlag,fileName)

# Print HTML file
  flashObj<-"amcolumn"
  chartTitle<-"YAH YTD"
  htmlFileName<-paste(htmlPath,metricName,"-HistYah.html",sep="")
  dataFileName<-paste(dataRelPath,metricName,"-HistYahData.xml",sep="")
  settingsFileName<-paste(settingsRelPath,metricName,"-HistSettings.xml",sep="")
  amPrintHTML(flashObj,chartTitle,metricName,amChartsPathCol,dataFileName,settingsFileName,htmlFileName)  

# **************************************************
# Trend Lines
# **************************************************
  # Compute min,mean,max,sum 
  max<-tapply(raw[,m],date,max)
  min<-tapply(raw[,m],date,min)
  mean<-tapply(raw[,m],date,mean)
  sum<-tapply(raw[,m],date,sum)  
  bands<-as.data.frame(cbind(min,mean,max))

# Print XML data file
  graphNames<-c("min","mean","max")
  graphColors<-c("#70DE05","#999999","#BB0000")
  fileName<-paste(dataPath,metricName,"-TrendLineData.xml",sep="")
  linePrintXMLData(d,bands,graphNames,graphColors,fileName)

# Print Settings file (Note the same settings file works for both historgrams)
  yLabel<-paste(metricName,": Weekly Units",sep="")
  fileName<-paste(settingsPath,metricName,"-TrendLineSettings.xml",sep="")
  linePrintSettings(metricName,yLabel,fileName)

# Print HTML file
  flashObj<-"amline"
  chartTitle<-"YTD"
  htmlFileName<-paste(htmlPath,metricName,"-TrendLine.html",sep="")
  dataFileName<-paste(dataRelPath,metricName,"-TrendLineData.xml",sep="")
  settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineSettings.xml",sep="")
  amPrintHTML(flashObj,chartTitle,metricName,amChartsPathLine,dataFileName,settingsFileName,htmlFileName)  

# ****************************************************************************
#  Set up dummy yah data for trend and scatter charts
# ****************************************************************************
  yahValues<-raw[seq(8,520,10),m]           #  User #8's data series from the raw set
  yahBands<-as.data.frame(cbind(min,mean,max,yahValues))
  graphNames<-c("min","mean","max","yah")
  graphColors<-c("#70DE05","#999999","#BB0000","#6699CC")
  fileName<-paste(dataPath,metricName,"-TrendLineYahData.xml",sep="")
  linePrintXMLData(d,yahBands,graphNames,graphColors,fileName)

# Print Settings file -- Settings file is the same for both histogram and yah histogram charts
  yLabel<-paste(metricName,": Weekly Units",sep="")
  fileName<-paste(settingsPath,metricName,"-TrendLineYahSettings.xml",sep="")
  linePrintSettings(metricName,yLabel,fileName)

# Print HTML file
  chartTitle<-"YAH YTD"
  htmlFileName<-paste(htmlPath,metricName,"-TrendLineYah.html",sep="")
  dataFileName<-paste(dataRelPath,metricName,"-TrendLineYahData.xml",sep="")
  settingsFileName<-paste(settingsRelPath,metricName,"-TrendLineYahSettings.xml",sep="")
  amPrintHTML(flashObj,chartTitle,metricName,amChartsPathLine,dataFileName,settingsFileName,htmlFileName)  

# **************************************************
# Scatter Plots 
# **************************************************
# Compute y-intercept and gradients for trend lines in the scatter plots
  nn<-c(1:52)
  fitted.min<-coef(lm(min ~ nn))
  fitted.mean<-coef(lm(mean ~ nn))
  fitted.max<-coef(lm(max ~ nn))

  td<-c(d[1],d[length(d)])                 # x coord's
  miny<-c(fitted.min[1],(fitted.min[1]+length(d)*fitted.min[2]))
  meany<-c(fitted.mean[1],(fitted.mean[1]+length(d)*fitted.mean[2]))
  maxy<-c(fitted.max[1],(fitted.max[1]+length(d)*fitted.max[2]))
  tvalues<-cbind(td,miny,meany,maxy)
  gradients<-c(fitted.min[2],fitted.mean[2],fitted.max[2])

# Print XML data file
  yah<-0
  graphNames<-c("min","mean","max")
  graphColors<-c("#70DE05","#999999","#BB0000")
  graphBullets<-c("triangle_down","round","triangle_up")
  fileName<-paste(dataPath,metricName,"-ScatterData.xml",sep="")
  xyPrintXMLData(yah,graphNames,graphColors,graphBullets,d,bands,tvalues,gradients,fileName)

# Print Settings file (Note the same settings file works for both historgrams)
  fileName<-paste(settingsPath,metricName,"-ScatterSettings.xml",sep="")
  dateFormats<-c("YYYY-MM-DD","DD","YYYY-MM-DD")
  yLabel<-paste(metricName,": Units",sep="")
  xyPrintSettings(metricName,dateFormats,yLabel,fileName)

# Print HTML file
  flashObj<-"amxy"
  chartTitle<-"YTD"
  htmlFileName<-paste(htmlPath,metricName,"-Scatter.html",sep="")
  dataFileName<-paste(dataRelPath,metricName,"-ScatterData.xml",sep="")
  settingsFileName<-paste(settingsRelPath,metricName,"-ScatterSettings.xml",sep="")
  amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)  

# ****************************************************************************
# YAH Scatter Chart 
# ****************************************************************************
  yah<-1
  fitted.yah<-coef(lm(yahValues ~ nn))
  yahy<-c(fitted.yah[1],(fitted.yah[1]+length(d)*fitted.yah[2]))
  ttvalues<-cbind(td,miny,meany,maxy,yahy)
  tgradients<-c(fitted.min[2],fitted.mean[2],fitted.max[2],fitted.yah[2])

# Print XML data file
  graphNames<-c("min","mean","max","yah")
  graphColors<-c("#70DE05","#999999","#BB0000","#6699CC")
  graphBullets<-c("triangle_down","round","triangle_up","square")
  fileName<-paste(dataPath,metricName,"-ScatterYahData.xml",sep="")
  xyPrintXMLData(yah,graphNames,graphColors,graphBullets,d,yahBands,ttvalues,tgradients,fileName)

# Print Settings file
  fileName<-paste(settingsPath,metricName,"-ScatterYahSettings.xml",sep="")
  dateFormats<-c("YYYY-MM-DD","DD","YYYY-MM-DD")
  yLabel<-paste(metricName,": Units",sep="")
  xyPrintSettings(metricName,dateFormats,yLabel,fileName)

# Print HTML file
  chartTitle<-"YAH YTD"
  htmlFileName<-paste(htmlPath,metricName,"-ScatterYah.html",sep="")
  dataFileName<-paste(dataRelPath,metricName,"-ScatterYahData.xml",sep="")
  settingsFileName<-paste(settingsRelPath,metricName,"-ScatterYahSettings.xml",sep="")
  amPrintHTML(flashObj,chartTitle,metricName,amChartsPathXY,dataFileName,settingsFileName,htmlFileName)  

}     # end of for all metrics loop

# **************************************************
# Comparison Chart
# Note:  The settings file is not automatically generated
# Note:  The HTML and data files are automatically generated
# **************************************************
# Build datasets from raw data:  Incident Metrics Only and hardcoded
volume<-tapply(raw[,4],date,sum)          # 4 is Security Incident Count
means<-tapply(raw[,4],date,mean)
for (m in (5:length(metrics))) {
    mean<-tapply(raw[,m],date,mean)
    means <-cbind(means,mean)
}
means<-as.data.frame(means)
names(means)<-names(raw)[4:length(raw)]

for (i in (1:length(means[1,])))  {
  metricName<-names(means)[i]
  fileName<-paste(dataPath,metricName,"-ComparisonData.csv",sep="")
  yvalues<-cbind(volume,means[,i])
  printEventCSVData(d,yvalues,metricName,fileName)
}

# Print starter dataset 
  metricName<-"SecurityIncident"
  fileName<-paste(dataPath,metricName,"-ComparisonData.csv",sep="")
  yvalues<-cbind(volume,means[,1])
  printEventCSVData(d,yvalues,metricName,fileName)

# Print HTML file
  flashObj<-"amstock"
  metricName<-"SurveyMetrics"
  chartTitle<-"Survey Metrics Comparison Chart"
  htmlFileName<-paste(htmlPath,metricName,"-Comparison.html",sep="")
  dataFileName<-paste(dataRelPath,metricName,"-ComparisonData.csv",sep="")
  settingsFileName<-paste(settingsRelPath,metricName,"-ComparisonSettings.xml",sep="")
  amPrintHTML(flashObj,chartTitle,metricName,amChartsPathStock,dataFileName,settingsFileName,htmlFileName)  

# ***********************************************************
# Correlation heat map
# ***********************************************************
metricName<-"surveyMetricCorrelation"
cssRelPath<-"../css/"
means<-tapply(raw[,4],date,mean)
for (m in (5:length(metrics))) {
    mean<-tapply(raw[,m],date,mean)
    means <-cbind(means,mean)
}
means<-as.data.frame(means)
names(means)<-names(raw)[4:length(raw)]

# Compute pair-wise correlation coeficients
hm.classes<-  c("noc","low","med","hi","vhi")
hm.cutpoints<-c( 0.3,  0.6,  0.8,  0.9)
hm.values<-round(cor(means),2)
hm.styles<-symnum(hm.values,
          cutpoints<-hm.cutpoints,symbols=hm.classes,corr=TRUE,
          legend=FALSE,abbr.colnames=FALSE,lower.triangular=FALSE)

# Visualize Heatmap for Pairwise metric correlation
# Print out html file
chartTitle<-"All Survey Data"
htmlFileName<-paste(htmlPath,metricName,"-hm.html",sep="")
dataFileName<-paste(dataRelPath,metricName,"-hmData.xml",sep="")
cssFileName<-paste(cssRelPath,"heatmap.css",sep="")
hmPrintHTML(chartTitle,metricName,htmlFileName,cssFileName,hm.values,hm.styles)

# *************************************************************
# Survey Statistics
# *************************************************************
stats<-c("metric","min","mean","max","sd")
for (m in (4:length(metrics))) {
   mins<-tapply(raw[,m],date,min)
   means<-tapply(raw[,m],date,mean)
   maxes<-tapply(raw[,m],date, max)
   sds<-round(tapply(raw[,m],date,sd),2)

   stat<-c(names(raw)[m],mins[length(mins)],means[length(means)],maxes[length(maxes)],sds[length(sds)])
   stats<-rbind(stats,stat) 
}
