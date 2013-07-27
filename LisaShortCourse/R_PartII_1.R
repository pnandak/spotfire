###############################################################
##        LISA Short Course Series
##        Introduction to R, Part II:
##        Andy Hoegh & Caleb King, LISA Collaborators
##        Department of Statistics
##        Some content developed by Sai Wang
##        Feb 16, 2012
################################################################
##                 Outline of talk
##
##  Section III:   Basic Statistical Analysis
##  Section IV:    Advanced Programming
###########################################################################

###########################################################################
## Section III:   Basic Statistical Analysis
###########################################################################
#
##  1. T-test
# Developed by William Gosset, an employee of Guinness Brewery

# Still use the 'mtcars' data set. Consider comparing mpg of cars between 
# automatic and manual transmission.
attach(mtcars)
#################################################################
# 1.1 One sample t-test
# Research Question (one sample test): Is the mean of a population different from the null hypothesis?

# Consider testing whether the average mpg of cars in the sample is different from
# 23 mpg (the mpg of my first car, a Plymouth Colt Vista)
# ALWAYS plot data first
boxplot(mpg,ylab='mpg')
stripchart(mpg,method='jitter',vertical=T,add=T)
abline(h=23,col="Red",lwd=2)
abline(h=mean(mpg),lwd=2)
title("MPG of 1974 Motor Trend Cars",sub='Red Line = 1993 Plymouth Colt Vista \n Black Line=Mean Motor Trend Cars')
# Data is highly variable, but mean appears to be less than 23 mpg

# Statistical procedure to test whether mean of mtcars is different from 23 mpg
?t.test
t.test(mpg,mu=23)
# Results suggest rejecting the null hypothesis - that the mean is equal to 23 - 
# p-value of .01033

#################################################################
# 1.2 Two sample t-test
# Research Question (two sample test): Are the means of two populations different?

# Next consider whether the average mpg of automatic cars is different from manual
boxplot(mpg~am,xlab='Transmission',ylab='mpg',names=c('auto','manual'))
stripchart(mpg~am,method="jitter",vertical=T,add=T)
title("Average MPG by Transmission Type")
# MGP appears to be higher for manual transmission

# Statistical procedure to test whether the mean mpg is different for two transmission types
t.test(mpg~am) # Default is Welch t-test with unequal variance assumption
t.test(mpg~am,var.equal=T) # T-test with equal variance assumption
# Welch (unequal variance assumption) is more conservative, with respect to rejecting the null hypothesis
# generally use Welch unless there is strong evidence to support equal variance
#Both tests provide evidence to reject the null hypothesis - equal mpg for both transmission types
# Welch p-value 0.001

# Suppose data in two vectors rather than data frame
mpg.a = mpg[am==0];    mpg.m = mpg[am==1]
t.test(mpg.a,mpg.m)
# identical results

############################################################################
# 1.3 Sample Size Calculation for Power or Margin of Error
# Research Question: How many observations are needed for a given power or sample size
# Power = probability rejecting null when null is false

# Compute power for a given sample size, difference between means - delta, & standard deviation
power.t.test(n=20, delta=2, sd=2) # power

# Compute minimum sample size for given power, difference between means, and standard deviation
power.t.test(delta=2,sd=2,power=.8) # min sample size to give power of 0.8

################################################################################
# 1.4 Paired T-test
# T-test for data that is inherently paired or linked
# Research Question: Given the paired structure of the data are the means of two 
# sets of observations significantly different?

# Consider the following example: a study was conducted to generate electricity from wave power at sea
# Two different procedures were tested for a variety of wave types with one of each type tested on every wave
# The question of interest is whether bending stress differs for the two mooring methods
# Data obtained from HSAUR Package
method1=c(2.23,2.55,7.99,4.09,9.62,1.59,8.98,.82,10.83,1.54,10.75,5.79,5.91,5.79,5.5,9.96,1.92,7.38)
method2=c(1.82,2.42,8.26,3.46,9.77,1.4,8.88,.87,11.2,1.33,10.32,5.87,6.44,5.87,5.3,9.82,1.69,7.41)

# Plot the differences of the two populations
par(mfcol=c(1,1))
diff=method1-method2
boxplot(diff,ylab='Difference in Stress')
stripchart(diff,method='jitter',vertical=T,add=T)
abline(h=0,col="Red",lwd=3)
title("Difference in Bending Stress \n Method 1 - Method 2")

# Statistical test for difference between two observations
t.test(method1,method2,paired=T)
t.test(diff) #Alternatively test whether the difference is equal to zero - Same Results

#################################################################
# 1.5 Normality Assumption
# Assumption of normal distribution underpins t-test

# Assess normality visually
par(mfcol=c(1,2))
# For large sample size histograms are useful, beware of how the data is "binned"
# paricularly in smaller sample sizes (see mpg.m)
hist(mpg.a)
hist(mpg.m)

# Normal quantile-quantile Plot
# compares expected quantiles from normal distribution with observed quantiles
qqnorm(mpg.a)    # normal quantile-quantile plot
qqnorm(mpg.m)

# Statistical Test for Normality
# Null distribution: data came from a normal distribution
shapiro.test(mpg.a)  # Shapiro-Wilk Normality Test
shapiro.test(mpg.m)

#####################################################################
# 1.6 Wilcoxon Signed Rank Statistic:
# Non-parametric Alternative when Normality Assumption Not Satisified

#Consider Barley Yields from 2 consecutive years
#Data available in MASS package, immer dataset
Yield1=c(81,105.4,119.7,109.7,98.3,146.6,142,150.7,191.5,145.7,82.3,77.3,78.4,131.3,
         89.6,119.8,121.4,124,140.8,124.8,98.8,89,69.1,89.3,104.1,86.9,77.1,78.9,101.8,96)
Yield2=c(80.7,82.3,80.4,87.2,84.2,100.4,115.5,112.2,147.7,108.1,103.1,105.1,116.5,139.9,
         129.6,98.9,61.9,96.2,125.5,75.7,66.4,49.9,96.7,61.9,80.3,67.7,66.7,67.4,91.8,94.1)

par(mfcol=c(1,1))
# Plot average yields for the two years
boxplot(Yield1,Yield2,xlab='Year',ylab='Yield Amount',names=c('1931','1932'))
stripchart(Yield1,method="jitter",add=T,vertical=T,at=1,col="darkorange3")
stripchart(Yield2,method="jitter",add=T,vertical=T,at=2,col="darkred")

title("Barley Yield by Year")

# Assess normality visually
par(mfcol=c(1,2))
hist(Yield1, col="darkorange3")
qqnorm(Yield1,col="darkred") # notice the lack of a straight line

# Statistical Test for Normality
# Null distribution: data came from a normal distribution
shapiro.test(Yield1) # reject null

# Need to use a non-parametric alternative to test whether the two distributions are the same
wilcox.test(Yield1,Yield2)
# p=.04, reject null that the two distributions are the same
# paired version also available

##  END of 1. T-test
################

################
##  2. ANOVA - (ANalysis Of VAriance)
# ANOVA is used to compare means of more than two groups.

# 2.1 One-Way ANOVA
# One-way ANOVA: consider comparing mpg for 3 cyl levels
# First plot data
par(mfcol=c(1,1))
boxplot(mpg~cyl,xlab='cyl',ylab='mpg')
stripchart(mpg~cyl,method="stack",vertical=T,add=T)
title("Comparison of MPG by # of Cylinders")
# One-way ANOVA:
# Tests null hypothesis that at least one factor is different
a.1= aov(mpg~factor(cyl))
summary(a.1)
# Use contrasts to test differences between treatment pairs
# pairwise comparisons: Tukey Procedure controls multiple comparison problem
TukeyHSD(a.1)
plot(TukeyHSD(a.1))

# one-way anova without equal var assumption
oneway.test(mpg~cyl) 

#There is also a non-parametric version of ANOVA known as Kruskal-Wallis Test
kruskal.test(mpg~factor(cyl))

# 2.2 Two-way ANOVA: consider comparing mpg~cyl*am
# First plot data
par(mfrow=c(1,2))
#Boxplots and Stripcharts visualize Main Effects of Factors
boxplot(mpg~cyl,subset=am==0,ylim=c(10,35),xlab='cyl',main='automatic')
stripchart(mpg~cyl,subset=am==0,method="jitter", vertical=T,add=T)
boxplot(mpg~cyl,subset=am==1,ylim=c(10,35),xlab='cyl',main='manual')
stripchart(mpg~cyl,subset=am==1,method="jitter", vertical=T,add=T)

#Interaction Plot 
par(mfrow=c(1,1))
interaction.plot(cyl, am, mpg, type="b", col=c(1:3),leg.bty="o", 
                 leg.bg="beige", lwd=2, pch=c(18,24,22), xlab="Number of Cylinders", 
                 ylab="Mean Miles Per Gallon", main="Interaction Plot")

# Two-way ANOVA:
a.2 = aov(mpg~factor(am)*factor(cyl)) # main effects and intaction
summary(a.2) # results based on type I SS (Sequential-Variation explained 
# relative to other factors in the model)
drop1(a.2, ~., test="F") 	# type III SS and F Tests
summary(aov(mpg~factor(am)+factor(cyl))) # main effects only


# pairwise comparisons
TukeyHSD(a.2)
plot(TukeyHSD(a.2,'factor(am):factor(cyl)'))

##  END of 2. ANOVA
################


##############################################################################
##  3. Regression
# Consider the 'stackloss' dataset, which contains operational data of a plant 
# for the oxidation of ammonia to nitric acid. Read the descriptions about this 
# dataset by typing ?stackloss in R. 
?stackloss
attach(stackloss)

#Plot relationship between stack loss and air flow
par(mfrow=c(1,1))
plot(Air.Flow,stack.loss)

# Fit simple linear regression model: stackloss vs. Water temp
r.1 = lm(stack.loss~Air.Flow)
abline(r.1,lwd=2)
title("Regression Stack Loss vs. Air Flow")

# Using 'plot()' on fitted model from 'lm()' produces nice diagnostic plots.
par(mfrow=c(2,2))
plot(r.1)

summary(r.1) # Summary of analysis
anova(r.1) # Type I ANOVA table

# Plot all data
plot(stackloss)

r.2 = lm(stack.loss~Air.Flow + Water.Temp + Acid.Conc., data=stackloss)

# Using 'plot()' on fitted model from 'lm()' produces nice diagnostic plots.
par(mfrow=c(2,2))
plot(r.2)

summary(r.2) # Summary of analysis
anova(r.2) # Type I ANOVA table
drop1(r.2,~.,test='F') # Type III ANOVA table
# Use 'predict()' on fitted model to get prediction intervals
predict(r.2,interval='confidence',level=.9) # 90% confidence interval
predict(r.1, newdata=data.frame(Air.Flow=65,Water.Temp=22,Acid.Conc.=80),    
        interval='prediction') # 95% prediction interval for arbitrary point


################
##  4. Logistic Regression
# Data from Faraway Library: Example from Extending the Linear Model with R, Julian Faraway
# Data can be found in faraway library, Orings
# The 1986 crash of the space shuttle Challenger was linked to failure of O-ring seals in the
# rocket engines. Data was collected on the 23 previous shuttle missions. The launch temperature
# on the day of the crash was 31F. Damage= number of damaged orings out of 6.
#install faraway package: Tools->Install Packages -> (may need to select a CRAN mirror)->specify faraway
library(faraway) #load faraway package
temp=c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,75,76,76,78,79,81)
damage=c(5,1,1,1,0,0,0,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,0)

#Plot the number of probability of damaged oring against launch temperature
par(mfrow=c(1,1))
plot(damage/6~temp,xlim=c(25,85),ylim=c(0,1),xlab="Temperature", ylab="Prob of Damage")

#add line for 31 degrees launch temperature
abline(v=31,col='navy',lwd=3)

# Fit naive linear model: Normality violated, damage/6 restricted to [0,1]
linmod <- lm(damage/6~temp)
summary(linmod)
abline(linmod,col="green", lwd=2)      

# Fit logistic regression model
logitmod <- glm(cbind(damage,6-damage)~temp,family=binomial)
summary(logitmod)

# Plot the results of logistic fit: log(p)/(log(1-p)) = beta0+beta1*time
t<- seq (25,85,1)
lines(t,ilogit(11.6630-.2162*t),col='red',lwd=2)

# Predict probability of failure of one oring at 31 degrees, the launch temperature
failure=ilogit(11.6630-.2162*31);failure

#Hence probability that all fail is failure*failure*failure*failure*failure*failure
failure^6

##  END of Section III:   Basic Statistical Analysis
################################################################

###########################################################################
## Section IV: Advanced Programming
###########################################################################
####################################################################
##  5. Conditional Execution and Loops

######## 5.1. Conditional Execution with if...else... statement
# Format of if...else...:
##  if (condition) {
##  	commands to be executed when condition is TRUE
##  } else {
## 		commands to be executed when condition is FALSE
##	} # the else statement is optional 
## Example:
set.seed(123)
v.1 = runif(1) # generates a random number in (0,1)
if (v.1 >= 0.5) print('Head') else print('Tail')
## do this again
v.1 = runif(1) # generates a random number in (0,1)
if (v.1 >= 0.5) print('Head') else print('Tail')

######## 5.2. 'for()' loops
# 'for' loops are useful if you know exactly how many times to iterate  
for (i in 1:10) {
  # Put commands to run for each value of i inside the '{ }'
  print(i)
}

# You can specify a vector of values for i to loop over.    
v.2 = c('blue','red','yellow')
for (i in v.2) {
  print(i)
} 

######## 5.3. 'while()' loops
# A 'while()' loop will continue to iterate until the condition becomes FALSE.
v.3 = 0; niter = 0 # intial value of v.3 and iteration counter
while (v.3 <= 0.9) {
  v.3 = runif(1)
  niter = niter+1
  cat('iter:',niter,'   v.3=',v.3,'\n')
}
# Be careful with your condition expression, there must be some command inside
# '{ }' to change the condition for the while loop to escape.

##  END of 5. Conditional Execution and Loops
################

################################
##  6. Functions

## Format of function(one or more parameters){
##		put operations to be performed here
##	}
quad=function(x,a=0){
  return((x-a)^2)
}
a=c(1,2,3)
sum(quad(a,mean(a)))
## Functions can be as simple or as complex as needed.
## They are very useful for combining multiple operations into one
## single 'operation'
scatter.c = function(x,y,z,obs=F) {
  n = length(x)
  classes = sort(unique(z))
  nclass = length(classes)
  cols = rainbow(nclass+1)
  z.col = rep(0,n)
  z.pch = rep(0,n)
  for (i in 1:nclass) {
    z.col[z==classes[i]] = cols[i]
    z.pch[z==classes[i]] = i
  }
  plot(x,y,type='n',xlab='',ylab='')
  points(x,y,col=z.col,pch=z.pch)
  if (obs) text(x,y,c(1:n),pos=1,col=z.col,cex=1)
}

## Many of the default functions in R were created using this 
## framework.
sd

##  END of 6. Functions
################

################################
##  7. Data From the Internet

######## 7.1. Data Stored as Txt Files
## Identify NOAA FTP Site Directory
datadir <-"ftp://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/Heating degree Days/Monthly City/";
## Set NOAA Dataset Parameters 
colz <- c("state","city","call","hdd","d2","d3","d4","d5","d6","d7")
w <- c(3, 17, 3, 5, 6, 6, 7, 7, 5, 7);
## Initialize HDD dataframe 
hdd.df <- data.frame("year" = "","month" = "", "state" = "",
                     "city" = "", "hdd" = "")
## Download all HDD datasets as .txt files
## Datasets will be saved in the directory listed in the
## 'setwd()' statement.
## NOTE: This loop downloads only the city-level data, but
## may be easily modified to download the regional data

yr <- 2010;

month <- c("Jan","Feb","Mar","Apr",
           "May","Jun","Jul","Aug",
           "Sep","Oct","Nov","Dec");

for (y in yr) {
  
  for (m in month){
    
    tmpname <- paste(m," ",y,".txt",sep="")
    tmppath <- paste(datadir,y,"/",tmpname,sep="")
    
    download.file(tmppath,tmpname);
    
  }
  
}

closeAllConnections()

######## 7.2. Data Scraping/Final Summary
## The following serves as an example of 'data scraping' as well
## as final summarization of all that you have learned in R

## These are some packages that will be needed.
library(XML)
library(gdata)

## These are the vectors containing the file names associated with
## each team separated by league. They will be used to access
## the online data. 
teamAL=c("BAL","BOS","CHW","CLE","DET","KCR","LAA","MIN","NYY","OAK","SEA","TBR","TEX","TOR")
teamNL=c("ARI","ATL","CHC","CIN","COL","FLA","HOU","LAD","MIL","NYM","PHI","PIT","SDP","SFG","STL","WSN")

## This is the basic function to 'scrape' the data from the website.
getData=function(x,output){
  url=paste("http://www.baseball-reference.com/teams/",x,"/2011-schedule-scores.shtml",sep="")
  doc=htmlTreeParse(url,useInternalNodes=T)
  tables=getNodeSet(doc,"//table")
  #some data cleaning is needed
  v=unlist(xpathApply(doc,"//table[@cellspacing='5']",xmlValue))
  v=v[4]
  v=gsub("OpponentSplitWLRSRAWP","",v)
  v=gsub("    ",",",v)
  v=gsub("   ",",",v)
  v=gsub("  ",",",v)
  v=gsub(" ",",",v)
  #these lines separate the data into individual lines ready to 
  #be setup as a dataframe
  q=trim(strsplit(v,"\n"))
  r=unlist(q)
  for(i in 1:length(r)){
    y=trim(unlist(strsplit(r[i],",")))
    cat(paste(y,collapse=","),"\n",file=output,append=T)
  }
}

## This function serves to compile the online data one league at 
## a time
LeagueData=function(vector,league){
  for(i in 1:length(vector)){
    getData(vector[i],paste(league,"_",vector[i],".txt",sep=""))
  }}

## Once the data has been scraped, it still needed some cleaning
## before it was ready to be accessed
cleanData=function(vector,infile){
  # This first section deals with an issue that occured when
  # a team had no losses. The line was being read incorrectly
  # so the dataset had to be outputted to a file to be manually
  # corrected.
  s=readLines(infile)
  t=count.fields(infile,sep=",")
  flag=0
  for(k in 1:length(t)){
    if(t[k]<6){
      r=paste(read.fwf(textConnection(s[k]),c(nchar(s[k])-6,6),as.is=TRUE),collapse=",")
      s[k]=r
      flag=flag+1}
  }
  if(flag>0){
    u=paste("t",infile,sep="")
    w=unlist(s)
    for(i in 1:length(w)){
      y=trim(unlist(strsplit(w[i],",")))
      cat(paste(y,collapse=","),"\n",file=u,append=T)
    }
    q=read.table(u,sep=",")
  }
  else{
    q=read.table(infile,sep=",")
  }
  # This section removes those teams that were not in the same
  # league as the team for which this dataset corresponds to
  w=q[1]
  b=c()
  k=1
  for(i in 1:dim(w)[1]){
    mark=0
    for(j in 1:length(vector)){
      if(identical(vector[j],as.character(w[i,]))==FALSE){
        mark=mark+1}
    }
    if(mark==length(vector)){
      b[k]=i
      k=k+1}
  }
  if(length(b)>0){
    q=q[-b,]
    return(q)}
  else{
    return(q)}
}

## These next two functions create matrices that records the wins and number of games for each team in a league for each matchup
WinMatrix=function(vector,league){
  W=matrix(0,nrow=length(vector),ncol=length(vector))
  for(i in 1:length(vector)){
    q=cleanData(vector,paste(league,"_",vector[i],".txt",sep=""))
    r=q[1]
    for(j in 1:length(vector)){
      mark1=0
      mark2=0
      for(k in 1:dim(r)[1]){
        if(identical(vector[j],as.character(r[k,]))==TRUE){
          mark1=k}
        else{
          mark2=mark2+1}}
      if(mark1>0){
        W[i,j]=(q[mark1,2]-q[mark1,3])}
      else if(mark2==dim(r)[1]){
        W[i,j]=0}}}
  return(W)}

GameMatrix=function(vector,league){
  G=matrix(0,nrow=length(vector),ncol=length(vector))
  for(i in 1:length(vector)){
    s=cleanData(vector,paste(league,"_",vector[i],".txt",sep=""))
    u=s[1]
    for(j in 1:length(vector)){
      mark1=0
      mark2=0
      for(k in 1:dim(u)[1]){
        if(identical(vector[j],as.character(u[k,]))==TRUE){
          mark1=k}
        else{
          mark2=mark2+1}}
      if(mark1>0){
        G[i,j]=(s[mark1,2]+s[mark1,3])}
      else if(mark2==dim(u)[1]){
        G[i,j]=0}}}
  return(G)}

LeagueData(teamAL,"AL")
LeagueData(teamNL,"NL")

AW=WinMatrix(teamAL,"AL")
AG=GameMatrix(teamAL,"AL")

NW=WinMatrix(teamNL,"NL")
NG=GameMatrix(teamNL,"NL")

## These functions correspond to the rating system being used
bvec=function(m){
  n=length(m[1,])
  p=length(m[,1])
  b=matrix(0,p,1)
  for(i in 1:n){
    b[i]=1+sum(m[i,])/2}
  return(b)}

Colleym=function(m){
  n=length(m[1,])
  p=length(m[,1])
  c=matrix(0,n,p)
  for(i in 1:p){
    for(j in 1:n){
      if(i==j){
        c[i,j]=2+sum(m[i,])}
      else{
        c[i,j]=-m[i,j]}
    }
  }
  return(c)}

ratCol=function(c,b){
  return(solve(c)%*%b)}

rcolN=ratCol(Colleym(NG),bvec(NW))
rcolA=ratCol(Colleym(AG),bvec(AW))

## Finally, the data is written out to a csv file for each league
write.csv(rcolN,file='ratingColl_Nat.csv')
write.csv(rcolA,file='ratingColl_Amer.csv')
