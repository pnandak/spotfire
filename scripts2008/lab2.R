##############################################################################
# QERM 514 Lab 2 (4/05/07) Script file
# The purpose of this lab is to:
#  - provide an example of testing for lack of fit using ANOVA
#  - use tapply() to peform functions on groups of data
#  - learn some of the dummy variables coding systems in S+
#  - identify points on a plot interactively
##############################################################################
setwd("c://courses//5142007")
# For the first I've typed in the example from the notes about 
#  the electrical impulses coming from fish.  Download this file
#  from the website and read it into your current session. 

im.freq.df<-read.table("elecfish.dat", header=T)
	
# NOTE: the results from the analysis below don't exactly match the 
#  results from the class notes...  I may have mis-entered some of the data,
#  but anyway, the results are close and you should get the idea of 
#  what's going on...

# Create a ordinary linear model regressing Freq as a function of Temp 
im.freq.lm<-lm(Freq~Temp, data=im.freq.df)		
summary(im.freq.lm)				# Look at the summary output
anova(im.freq.lm)					# and look at the ANOVA output


# Here's another way to calculate the means (grouped by temp) for this data.
#  Note that the last parameter of tapply() can be any function that can be 
#  applied to the grouped data (such as min, max, var, etc.)
tapply(im.freq.df$Freq, im.freq.df$Temp, mean)

# ALso, you can use as.factor to fit a model using Temp as a categorical data containing 7 levels

group.lm<-lm(Freq~as.factor(Temp), data=im.freq.df)		
summary(group.lm)				# Look at the summary output
anova(group.lm)					# and look at the ANOVA output

# Note the values of the coefficients (intercept), what should they be?
# We know that for each level of Temp, the fitted value in this linear model should be the mean of the several y 
# which shares the same Temp level. 
# how do we expect this?


# IMPORTANT WARNING MESSAGE: 
# ALways Check "what dummy variable coding is this using by the software?!"   To see type:
group.lm$contrasts
# This isn't the expected coding scheme and instead is what's known as 
#  Helmert (i think that's all you need to know for now about that), but 
#  its easier to use the 'treatment' coding.  This can be changed globally
#  and then you won't need to deal with it again this session...


#  If you are using R, the default coding IS contr.treatment!!!! So no need to change!


# First type:
options()
# which will cause a long list of global options to be shown.  Find the 
#  $contrasts line.  Now type:
options(contrasts=c("contr.treatment", "contr.poly")) 
# this will set the default for unordered factor data to 'treatment'
# So, recreate the linear model...
group.lm<-lm(Freq~as.factor(Temp), data=im.freq.df)		
summary(group.lm)				# Look at the summary output
anova(group.lm)					# and look at the ANOVA output
group.lm$contrasts


# Produce plots of the data and the residuals
# First, setup a split window plot screen
par(mfrow=c(1,2))

#  Create a scatter plot of the data with a different.
#  plotting character  
plot(im.freq.df$Temp, im.freq.df$Freq, xlab="Temperature", 
	ylab="Frequency", pch=4)
abline(im.freq.lm)				# add the regression line
# Create a plot of the residuals
plot(im.freq.lm$fit, im.freq.lm$res, xlab="Fitted Values", ylab="Residuals")
abline(h=0)

# OR directly use 
par(mfrow=c(1,1))
plot(im.freq.lm)

# Do the residuals look like they're normally distributed?!?
# S-Plus provides an interactive approach to identifying points on plots.  
#  With the previous plot still shown, call:

plot(im.freq.lm$fit, im.freq.lm$res, xlab="Fitted Values", ylab="Residuals")
identify(im.freq.lm$fit, im.freq.lm$res, row.names(im.freq.df))

# Then, use the mouse to identify a point by left clicking on it (the point 
#  number should be displayed on the plot).  To quit the interactive session
#  press ESC.  This function works on most plots, just make sure its parameters
#  are the same as were passed to the initial plot function.

# If you want to label them all at once
text(im.freq.lm$fit, im.freq.lm$res, row.names(im.freq.df))
