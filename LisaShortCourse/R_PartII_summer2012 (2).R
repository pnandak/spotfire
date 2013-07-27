###############################################################
##        LISA Short Course Series
##        Introduction to R, Part II:
##        Andy Hoegh, LISA Collaborator
##        Department of Statistics
##        July 19, 2012
################################################################
##                 Outline of talk
##
##  Section III:   Graphing
##  Section IV:    Statistical Analysis
##  Section V:     Advanced Graphics: ggplot2
###########################################################################


################################################################
##  Section III:   Graphing

# High-level plotting functions create a new plot on the graphics device, 
# possibly with axes, labels, titles and so on. 

# Low-level plotting functions add more information to an existing plot, 
# such as extra points, lines and labels. 

# The mtcars dataset will be used in examples
attach(mtcars) # now we can directly use component names

################
## 1. High-level Graphing Functions
# High-level plotting functions create a new plot on the graphics device, 
# possibly with axes, labels, titles and so on. 

#### 1.1. 'plot()'
plot(wt,mpg) # if inputs are 2 vectors, scatterplot of 1st on X, and 2nd on Y
plot(mpg~wt) # you can also use foumula expression
plot(mpg~factor(cyl)) # if 'y~factor', boxplot for each level
plot(factor(cyl)) # if input is a factor, bar chart of # of obs in each level
plot(mtcars) # if input is a data.frame, scatterplot matrix
pairs(cbind(mpg,wt,hp,cyl)) # pairs()

#### 1.2. other high-level plotting functions
hist(mpg,freq=F) # histogram
boxplot(mpg) # boxplot
boxplot(mpg~cyl,col=rainbow(3)) # you can also use formula
barplot(cyl_freq, horiz=T) # barplot for freq. data, horiz=T / F
qqnorm(mpg) # Normal Q-Q plot
curve(.5*(x-3)^2+log(x),from=1,to=4) # draw function curve
# Some high-level plots can be overlayed with 'add=TRUE' argument.
curve(sqrt(x),from=1,to=4,add=T,col='red')

#### 1.3 Modifying Default Plots
# Optional arguments to high-level plotting functions:
plot(mpg~wt) # default plot
# add titles
plot(mpg~wt,xlab='Weight',ylab='Miles per Gallon',main='add a title',
     sub='also a subtitle if you want',xlim=c(1,6),ylim=c(10,40))

# You can control graphing parameters within a function argument.
# These work for both high-level and low-level graphing functions.
# Some commonly used parameters:
#     'col=' color, 'pch=' point symbol, 'cex=' magnification ratio
#     'lwd=' line width, 'lty=' line type
# ?par for more details. 
# http://www.statmethods.net/advgraphs/parameters.html
plot(mpg~wt,col='red',pch=3,cex=.8) # color, point style, 80% of default size
curve(37.285-5.344*x,1.5,5.5,add=T,lwd=2,lty=2,col='blue')

# To place multiple plots in the same graph window
par(mfrow=c(1,2)) # 1 row 2 cols by row
plot(mpg~factor(cyl))
plot(mpg~wt)
par(mfrow=c(1,1)) # reset default window
##  END of 1. High-level Graphing Functions
################

################
##  2. Low-level Graphing Functions
# Low-level plotting functions add more information to an existing plot, such
# as extra points, lines and labels. Low-level plotting ftns must be used
# after a plot beging created by a high-level ftn.

# A list of commonly used low-level graphing functions:
## points(x,y) 
## lines(x,y) 
## text(x,y,labels,...) 
## abline(a,b); abline(h=y); abline(v=x) 
## legend(x,y,legend,...) 
## title(main,sub) 
## axis(side,...) 

# Example:
# First run the following codes to make arbitrary data 
n = 50;  set.seed(12345)
x = runif(n,0,4) 
e = rnorm(n,0,.2)
e[50] = 1.5 # add an outlier at the 50th obs 
y = 1+.5*x+e # true model
fit = lm(y~x) # SLR fit    
# Now use high-level ftn 'plot()' to create a basic plot
plot(x,y)
# Now use low-level ftn to add extra info
points(x[50],y[50],col='red')
points(2,3)
text(x[50],y[50],'outlier',col='red',pos=1)
lines(0:4,1+.5*0:4,col='blue',lwd=2)
abline(fit$coefficients[1],fit$coefficients[2],col='red',lwd=2,lty=2)
title(main='Simple linear regression')
legend('bottomright',legend=c('True model','SLR fit'),col=c('blue','red'),lty=1:2,lwd=2)

##  END of Section III:   Graphing
################################################################



###########################################################################
## Section IV:   Basic Statistical Analysis
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

# load pwr library for additional power calculations, including Anova
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
# paricularly in smaller sample sizes 
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
summary(a.2) 
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
anova(r.1) # ANOVA table

# Plot all data
plot(stackloss)

r.2 = lm(stack.loss~Air.Flow + Water.Temp + Acid.Conc., data=stackloss)

# Using 'plot()' on fitted model from 'lm()' produces nice diagnostic plots.
par(mfrow=c(2,2))
plot(r.2)

summary(r.2) # Summary of analysis
anova(r.2) # Type I ANOVA table
# Use 'predict()' on fitted model to get prediction intervals
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

##  END of Section IV:   Basic Statistical Analysis
################################################################

################################################################
##  Section V:   Advanced Graphing


################
## 1. GGPLOT2
## Developed by Hadley Wickham, loosely based on Wilkinson's Grammar of Graphics
## Very popular for creating high quality images: http://had.co.nz/ggplot2/ 
## Excellent tutorial: http://www.ceb-institute.org/bbs/wp-content/uploads/2011/09/handout_ggplot2.pdf
install.packages("ggplot2")
library("ggplot2")

# Heat Map
d <- ggplot(diamonds, aes(x = x, y = y)) + xlim(4,10) + ylim(4,10)
d + geom_bin2d()

# Box Plots
install.packages("plyr")
library(plyr)
m <- ggplot(movies, aes(y = votes, x = rating, group = round_any(rating, 0.5)))
m + geom_boxplot() + coord_trans(y = "log10")

# Dot Plots
ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")

# Density Plots
ggplot(diamonds_small, aes(depth, fill = cut)) +
  geom_density(alpha = 0.2) + xlim(55, 70)

# Maps
install.packages("maps")
library(maps)
if (require("maps")) {
  states <- map_data("state")
  arrests <- USArrests
  names(arrests) <- tolower(names(arrests))
  arrests$region <- tolower(rownames(USArrests))
  
  choro <- merge(states, arrests, sort = FALSE, by = "region")
  choro <- choro[order(choro$order), ]
  qplot(long, lat, data = choro, group = group, fill = assault,
        geom = "polygon")
  qplot(long, lat, data = choro, group = group, fill = assault / murder,
        geom = "polygon")
}

# Contour Plots
install.packages('reshape2')
library(reshape2) # for melt
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")
v <- ggplot(volcano3d, aes(x, y, z = z))
v + geom_tile(aes(fill = z)) + stat_contour()


################
## 2. GoogleVis 

# Bubble Plot
data(Fruits)
M <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
plot(M)

demo(WorldBank)

# Geo Plots
require(stats)
data(quakes)
head(quakes)
quakes$latlong<-paste(quakes$lat, quakes$long, sep=":")

G7 <- gvisGeoChart(quakes, "latlong", "depth", "mag",
                   options=list(displayMode="Markers", region="009",
                                colorAxis="{colors:['red', 'grey']}",
                                backgroundColor="lightblue"))
plot(G7)

# Hurricane Andrew Example
data(Andrew)

AndrewGeoMap <- gvisGeoMap(Andrew, locationvar='LatLong', numvar='Speed_kt',
                           hovervar='Category',
                           options=list(width=800,height=400,
                                        region='US', dataMode='Markers'))

AndrewMap <- gvisMap(Andrew, 'LatLong' , 'Tip',
                     options=list(showTip=TRUE, showLine=TRUE,
                                  enableScrollWheel=TRUE,
                                  mapType='hybrid', useMapTypeControl=TRUE,
                                  width=800,height=400))

AndrewTable <- gvisTable(Andrew,options=list(width=800))

## Combine the outputs into one page:

AndrewVis <- gvisMerge(AndrewGeoMap, AndrewMap)

plot(AndrewVis)
##  END of Section V:   Advanced Graphics
################################################################

