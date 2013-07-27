### R LECTURE NUMBER 2: INTRODUCTION TO GRAPHICAL FUNCTIONS ###

load("RgraphExamples.RData") #load example dataset

ls()


##########################			HIGH LEVEL PLOTTING FUNCTIONS			##########################         


###THE PLOT FUNCTION IS THE SIMPLEST, MOST FLEXIBLE, AND MOST IMPORTANT GRAPHICAL OPTION ###


# IF ONE CONTINUOUS VECTOR IS GIVEN, then x axis values are defined by the index of the element
plot(y) 

###PLOT FUNCTION DEFAULT ARGUMENTS CAN BE OVERRIDDEN

#add labels
plot(y,xlab="Sample Number",ylab="# Bacteria",main="Number of Bacteria found in several cultures of Milk",sub="This is a subheading---isn't it neat?") 


###SEVERAL DIFFERENT PLOTTING TYPES ARE SPECIFIED--UTILITY WILL DEPEND ON DATA

plot(y,type="p") #points (default)
plot(y,type="l") #lines 
plot(y,type="b") #points and lines
plot(y,type="h")
plot(y[1:100],type="h") #histogram-like for first 100 points 
plot(y[1:100],type="s") #stairsteps for first 100 points

###DATA MINIPULATION FUNCTIONS CAN BE INTEGRATED INTO PLOT COMMANDS JUST LIKE ELSEWHARE
plot(sort(y)) #ascending sort of vector y

#BIVARIATE GRAPHS CAN BE PLOTTED -- OUTPUT WILL DEPEND ON THE DATA TYPE

#Scatterplot for two continuous variables
plot(x,y)

#Alternative Syntax, same outcome
plot(y~x) 

#maybe a change of scale?
plot(log(y)~x) 

#adding labels
plot(x,y,xlab="Minutes left on table",ylab=" Number of Bacteria",main="Bacterial Consequences of Leaving out Milk")  



###Color can easily be modified

plot(x,y,xlab="Minutes left on table",ylab=" # Bacteria",main="Bacterial Consequences of Leaving out Milk",col="red") 
plot(x,y,xlab="Minutes left on table",ylab=" # Bacteria",main="Bacterial Consequences of Leaving out Milk",col=colors()[552])

#many default colors
colors()
plot(x,y,xlab="Minutes left on table",ylab=" # Bacteria",main="Bacterial Consequences of Leaving out Milk",col="sienna") 


#VECTORS OF COLOR NAMES CAN BE INPUT RATHER THAN A SCALAR--THIS VECTOR WILL RECYCLE IF SMALLER THAN x and y 

#small vector recycled
plot(x,y,col=c("blue","red","darkgoldenrod4"))

#full length vector created for defining color
toxic<-matrix("blue",nrow=length(y)); toxic[y>5000]<-"red"
plot(x,y,col=toxic) #specify colors as a previously calculated vector

#using conditional statements, it is not necessary to create a vector
plot(x,y,col=ifelse(y > 5000 ,"red","black"))


###PLOTTING CHARACTER CAN BE MODIFIED

#pch defines characters some characters correspond to numbers (see handout)
plot(1:25,rep(0,25),pch=1:25) 

#or you can manually specify the character
plot(x[60:100],y[60:100],pch="*") 



############### 			LOW LEVEL PLOTTING COMMANDS ADD INFORMATION TO EXISTING PLOTS       #####################

#familiar x-y scatterplot, this time accessing data from a data frame (same data as x,y above)
plot(impact$minutes,impact$bacteria,xlab="time (minutes)",ylab="# bacteria",main="bacterial growth in milk with time")

#lines() adds a series of line segment x-y coordinates, in this case from a smoothing function
lines(lowess(impact$minutes,impact$bacteria),col="red",lwd=3)

#points() adds a series of new points lactose-loving British bacteria
points(impact$minutes,impact$bacGB,pch=5)

#add another smoothed line for new data
lines(lowess(impact$minutes,impact$bacGB),col="orange",lwd=3)


#add text at specified x-y coordinates 
text(25,10000,"British Bacteria")
text(25,11000,"Highly Adapted")

#vectors also work
text(c(80,80),c(12000,11000),c("Underachieving","American Bacteria"),col=c("blue","darkgreen"))

#arrows work similarly--start and end coordinates are arguments, x1,y1,x2,y2
arrows(30,10500,45,10500)
arrows(80,10500,80,9000)


### NEW PLOT -- minutes versus tastiness -- example of mathematically-defined  lines
plot(impact$minutes,impact$tastiness,main="Milk tastiness with time",ylab="tastiness",xlab="time (minutes)")

#horizontal/vertical straight lines - provide value for x or y axis
abline(h=50) #h for horizontal value
abline(v=60)

###lines defined by regression models

#simple linear regression
abline(lm(impact$tastiness~impact$minutes),col="red")

#line with slope b, and intercept y -- abline(y,b)
abline(0,1,col="blue",lty=2)

###PLOT FUNCTION GRAPHICS WILL DEPEND ON DATA TYPE (e.g. numeric versus factor)


## FACTOR on X axis
# plot(x) with a factor variable produces counts
plot(xfac)

#scatterplot plot(x,y) produces boxplots 
plot(xfac,y,col=c("darkblue","blue","white","red","darkred"),xlab="time",ylab="number of bacteria")


## DATAFRAME

#when x is a dataframe, plot(x) plots all rows and columns in a scatterplot matrix

plot(suspectinfo)


#coplots allow one to specify subplots based on factors
coplot(suspectinfo$caffeinedep~suspectinfo$cupstea | suspectinfo$Classlist)




#???????????????????????????????????	            PRACTICAL # 1  	                ?????????????????????????????????????
#
#     Your boss, the narcissistic inspector Hap Mapp, is desparate to show progress on the case. "Give me something," he says, 
#     "to impress a know-nothing journalist." Use your near-infinite knowledge of R plotting functions and the dataset 
#     'suspectinfo' to generate a snazzy graphic. 
#
#	hint -- names(suspectinfo) will give the list of variables in the dataset, each can be referenced by using the $ sign,
# 	thus variable caffienedep in data frame suspectinfo can be referenced as suspectinfo$caffienedep. One also can use the 
# 	detach(suspectinfo) command to "free" all of the datavectors from the data frame.
#
#????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????




######################### 	 A BRIEF SURVEY OF OTHER GRAPHICAL  FUNCTIONS		#########################


###HISTOGRAM FUNCTION TAKES A CONTINUOUS VECTOR, many similar arguments to plot
hist(x,main="sampled times in minutes")
hist(y,main="bacteria growth",col="purple")

# freq argument toggles density versus count histograms
hist(suspectinfo$overachiever,main="distribution of overachievers in sample",freq=F) 

#########################   	 LOW LEVEL PLOTTING FUNCTION STILL CAN ADD TO EXISTING PLOTS          ###########################

#add density plot to histogram
lines(density(suspectinfo$overachiever),col="aquamarine") 

#increase the line width and replot 
lines(density(suspectinfo$overachiever),col="aquamarine",lwd=10) 


### NORMAL QQ PLOTS
qqnorm(suspectinfo$overachiever)


### BOXPLOTS

boxplot(y) # STANDARD BOXPLOT

# LOW LEVEL PLOTTING COMMAND--ADDS UNIVARIATE LOCATIONS OF INDIVIDUAL DATA VALUES: side tells R which position on the box
rug(y,side=2)  

#multiple boxplots will be created if dataframe of multiple vectors is given
boxplot(as.data.frame(cbind(x,z)),col=c("firebrick","dodgerblue"))

#add a rugplot for fun---x on left, z on right
rug(x,side=2,col="firebrick"); rug(z,side=4,col="dodgerblue")



###BARPLOTS
bpobject<-barplot(as.numeric(suspectinfo[1:10,6]),ylab="cups of tea per day ",main="cups of tea consumed by first 10 VIPBGERS",ylim=c(-5,10))
barplot(as.numeric(suspectinfo[1:10,6]),ylab="cups of tea per day ",main="cups of tea consumed by first 10 VIPBGERS",ylim=c(-5,10))

text(bpobject,-1,suspectinfo[1:10,1],srt=90)



###3D plots ---take a z square matrix

image(tableEvi$xcup,tableEvi$ycup,tableEvi$zcup,main="density of Milk on Break Room Table Corner")

contour(tableEvi$xcup,tableEvi$ycup,tableEvi$zcup)

filled.contour(tableEvi$xcup,tableEvi$ycup,tableEvi$zcup)

persp(tableEvi$xcup,tableEvi$ycup,tableEvi$zcup,phi=60)

persp(tableEvi$xcup,tableEvi$ycup,tableEvi$zcup,phi=60,col="lightblue",main="milk density distribution")



############ CHANGING PAR VALUES WILL HAVE "PERMANENT" EFFECTS ON PARAMETER VALUES

par()

#change background color
par("bg"="khaki")

#font size
par("font"=2)

#character type
par("pch"=5)

#number of plots per device (doesn't work for all plot types!)
par("mfcol"=c(3,2))

######RANDOM GRAPHS FROM THE PRACTICAL
plot(x,y,col=c("blue","red","darkgoldenrod4"))
plot(impact$minutes,impact$bacteria,xlab="time (minutes)",ylab="# bacteria",main="bacterial growth in milk with time")
hist(suspectinfo$caffeinedep,freq=F);lines(density(suspectinfo$caffeinedep),col=colors()[26])
image(tableEvi$xcup,tableEvi$ycup,tableEvi$zcup,main="density of Milk on Break Room Table Corner")
boxplot(as.data.frame(cbind(x,z)),col=c("firebrick","dodgerblue"));rug(x,side=2,col="firebrick"); rug(z,side=4,col="dodgerblue")
plot(sort(y)) #ascending sort of vector y




#?????????????????????             		      PRACTICAL # 2                             ??????????????????????????????????/
#
#         Your forensic psychologist has discovered that individuals with both high lactose tolerance and caffeine dependence
#         have an increased liability to malevolant behavior towards milk. In conjunction with the physical evidence and   
#         other information, is it possible to identify the culprit?
#
#         
#
#???????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????