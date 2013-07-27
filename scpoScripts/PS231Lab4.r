#Lab 4
#Basic R graphics

#=========================
#R Graphics
#=========================
#Tools covered---
#pie chart
#barchart

#histogram
#boxplot

#Plot/scatterplot
#print multiple plots on a single page
#plot overlay

#qqplot
#line graph
#===========================


library(foreign)

#let's use the 2002 NES dataset
nesda<-read.dta("C:/Users/Iris/Documents/PS231A/03740-0001-Data.dta", convert.factor=FALSE)
dim(nesda) #check dimension
names(nesda) #check out the var names

attach(nesda)

#~~~~~~~~~~~~~~~~~~~~~
#pie chart
#~~~~~~~~~~~~~~~~~~~~~

#LET'S CHECK OUT THE REGIONS WHERE THE RESPONDENTS LIVE
#have to organize the variable into a table first
region<-table(V021203)
pie(region, labels=c("NE", "North", "South", "West"))           #use labels to add category names


#~~~~~~~~~~~~~~~~~~~~~
#bar graph
#~~~~~~~~~~~~~~~~~~~~~

barplot(region)      #note the V021203 has to be in table format
barplot(region, names=c("NE", "North", "South", "West"))
barplot(region, names=c("NE", "North", "South", "West"), main="Regions")    #use main= to add a title to the graph


#~~~~~~~~~~~~~~~~~~~~~
#HISTOGRAM
#~~~~~~~~~~~~~~~~~~~~~
#LET'S CHECK OUT V025043, POST-ELECTION GWB FT SCORE
hist(V025043)   #show frequency
hist(V025043, freq=F)     #show probability
hist(V025043, col="red")  #fill the bars with red color
hist(V025043, density=3)  #to shade the bars with line
hist(V025043, nclass=5)   #collapse into 5 classes
hist(V025043, breaks=c(0,25,50,75,100))   #assign break intervals


hist(V025043)
abline(h=100, col="red", lwd=5)  #add a red horizontal line at freq=100
                                 #use lwd= to change the width of the line
hist(V025043)
abline(v=80, col="red", lwd=5)  #add a red vertical line at x-value=80

#to overlay a density line on top of historgram
#need to remove NA in order to use density()
V025043b<-V025043[!is.na(V025043)]
hist(V025043b, freq=F) #NEED TO change the y-axis to display probabilities 
lines(density(V025043b), col="red", lwd=2)
lines(density(V025043b), col="blue", lwd=2, lty=10)   #change to dotted line


#~~~~~~~~~~~~~~~~~~~~~
#BOX PLOT
#~~~~~~~~~~~~~~~~~~~~~

#box plot
boxplot(V023010)
boxplot(V023010, xlab=c("GWBft"))

#do boxplots for multiple pre-election FT scores
ftscore<-nesda[, 130:140]
boxplot(ftscore)
boxplot(ftscore, names=c("GWB", "Cheney", "Gore", "Lieberman", "Nader", "Clinton", "Powell", "Ashcroft", "Jesse Jackson", "Laura Bush", "Hillary Clinton"), main="Pre-Election FT SCores")


#~~~~~~~~~~~~~~~~~~~~~
#SCATTER PLOT
#~~~~~~~~~~~~~~~~~~~~~

#simple plot pre and post-election GWB FT 
plot(V025043, V023010)
#to dress up the graph
#to add a title to the plot, use main= 
#use xlab and ylab to label x and y-axis
#use xlim and ylim to change the range of x and y-axis
plot(V025043, V023010, main="Pre & Post Election GWB FT", xlab="pre-election", ylab="post-election", xlim=c(0,120), ylim=c(0,120))


plot(V025043, V023010, pch=10) #use pch= to choose diff type of points
plot(V025043, V023010, col="green") #use col= to choose color
plot(V025043, V023010, cex=1.2) #use cex= to choose size/font


#WANT TO EXAMINE: DOES THE PRE- AND POST-ELECTION PATTERN VARY BY REGION?
#ONE WAY IS TO EXAMINE PLOTS SIDE BY SIDE
table(V021203, V023010)
#plot GWB in North
par(mfrow=c(1,2))     #use this to have the plots side-by-side
north<-nesda[V021203==1,]       #subset the respondents who live in the NORTH
plot(north$V023010, north$V025043)   #plot their pre- and post- election GWB scores
abline(0,1)                          #add 45 degree line

south<-nesda[V021203==3,]       #subset respondents in the SOUTH
plot(south$V023010, south$V025043)
abline(0,1)

#ANOTHER WAY IS TO OVERLAY THE TWO PLOTS (ONE ON TOP OF ANOTHER)
par(mfrow=c(1,1))                  #back to one graph per page
plot(north$V023010, north$V025043, col="blue", pch=1, xlab="Pre-Election FT", ylab="Post-Election FT")
points(south$V023010, south$V025043, col="red", pch=2)      #POINTS() is for overlay

#~~~~~~~~~~~~~~~~~~~~~
#QQPLOT
#~~~~~~~~~~~~~~~~~~~~~

#QQPLOT MAY BE USED TO REPLACE A SCATTER PLOT
qqplot(V023010, V025043)
abline(0,1) #add a 45-degree line


#~~~~~~~~~~~~~~~~~~~~~
#LINE GRAPH
#~~~~~~~~~~~~~~~~~~~~~

#LET'S CREATE AN EXAMPLE WITH CONTINUOUS VARIABLE
#LOOK AT THE CPI (CONSUMER PRICE INDEX) FROM 1997 TO 2007
year<-1997:2007
cpi<-c(160.5,163,166.6,172.2,177.1,179.8,183.96,188.9,195.3,201.6,207.34)
demodata<-data.frame(cbind(year, cpi))

plot(demodata$year, demodata$cpi, type='l')

#~~~~~~~~~~~~~~~~~~~~~
#MORE FANCY OPTIONS for LINE GRAPH 
#~~~~~~~~~~~~~~~~~~~~~
### the 'type' argument to the plot() function has profound effects.
plot(x= 1:100,y =sin(1:100/10))

par(mfrow=c(2,3))
plot(x=1:100,y=sin(1:100/10),type='s')    #connect points with "step"
plot(1:100,sin(1:100/10),type='l')        #connect points with "line"
plot(1:100,sin(1:100/10),type='b')        #just points
plot(1:100,sin(1:100/10),type='p')        #just points
plot(1:100,sin(1:100/10),type='o')        #points and line
plot(1:100,sin(1:100/10),type='h')        #shade area under curve

#~~~~~~~~~~~~~~~~~~~~
#QUICK NOTE
#~~~~~~~~~~~~~~~~~~~~

#for lines()
#lty=line type
#lwd=line width
#col=color 
#type="b" or pch= to change type of symbol (e.g. *,+)
#for a list of symbols, see
#http://cran.r-project.org/doc/contrib/Paradis-rdebuts_en.pdf
#Chapter 4: Graphics with R  (p.44)
#For a nice reference site:
#http://freshmeat.net/articles/view/2237/