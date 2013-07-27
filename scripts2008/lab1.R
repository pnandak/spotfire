# Qerm514 Computing Introduction 1 (3/29/07) Script file
# - Introduction to R
# - Set working directory
# - Import data 
# - Asking for Help
# - Easy plot
# - Simple Linear Model
######################################################################
#Introductory statistical computing with R or S+

#S is a statistical and graphical programming language developed at Bell Labs over the past 30 years.

#S-PLUS is a commercial version of S with many additional features. It is sold by Insightful Co., a Seattle company. The University has a site license, and you can buy copies from the University bookstore for $125.

#R is a free implementation of a language very similar to S. You can download it from http://www.r-project.org.
############################################################
#Set working directory
##################################
# set working directory to C:\\courses\\5142007

setwd("C:\\courses\\5142007")

# check working directory

getwd()
#####################################
#Reading data
  #o Text files
  #o Web pages
#Much more information is in the Data Import/Export manual.

#Reading text data
#The easiest format has variable names in the first row and fields separated by spaces.

#use
crab.df <- read.table("C:\\courses\\5142007\\mcgraw.data.txt", header=F)
# Or simply
crab.df <- read.table("mcgraw.data.txt", header=F)

#to read the data from the file "mcgraw.data.txt" into the data frame carb.df.
#"crab.df" is a data frame, which is a list of variables of the same length, but can be 
# of different types (numeric, character or logical) 

#Syntax notes
 #o Spaces in commands don't matter (except for readability), but Capitalisation Does Matter.
 #o TRUE (and FALSE) are logical constants

#Have a look at the data

crab.df 

# There is no column names to the dataset, so we need to names the two columns
names(crab.df)<-c("density", "entrainment")
#Syntax notes
#c() is a function that makes a single vector from its arguments.
#names is a function that accesses the variable names of a data frame

#Have a look at the data again

crab.df

###################################################################
#Sometimes columns are separated by commas (or tabs).
#Especially for Excel file, save as ".csv" file.

#Use
bird.df <- read.table("bird.csv", header=TRUE, sep=",")
#or in R
bird.df <- read.csv("bird.csv")



#################################################################
#Reading data directly from the web
#With R, not S-plus, files for read.table can live on the web
island.df<-read.table("http://students.washington.edu/zhh/qerm514/island.area.spp.txt", header=T)
crab.df<-read.table("http://students.washington.edu/zhh/qerm514/mcgraw.data.txt", header=F)
names(crab.df)<-c("density", "entrainment")

###############################################################

#Asking for help

#for a complete description of the function and all the arguments
help(read.table)
#OR 
?read.table 

#When something doesn't work, the best way to describe and ask for help looks like:

#I did X. Y happened. I thought Z would happen.
#with as much detail as possible about X. 

###############################################################
# we go through an example of ordinary linear model

attach(crab.df)
# summary your dataset first
summary(crab.df)

#Scatter Plot
plot(density, entrainment, main="dentrainment vs density")

#####################################3
# Create a simple linear model of the data
crab.lm<-lm(entrainment~density, data=crab.df)
# Look at a summary of the model.  Note that this produces the summary
# of this model including the regression coefficients and their signif levels.

summary(crab.lm)  
# extract what you need directly
crab.lm$coefficients

#add linear fitted Line to existed scatter plot
abline(crab.lm, col=2) 


# To force R to perform regression through the origin, modify
#  the formula in lm slighly
crab.lm2<-lm(entrainment~density-1, data=crab.df)
summary(crab.lm2)
  
# Add this regression line to our previous plot, compare two lines
abline(crab.lm2, col=3,lty=2)
legend(600,600,lty=1:2,col=2:3, legend=c("linear model","linear model through the origin"))


######################################################################
# Now plot the fitted vs. residuals for this model, what do we expect?
plot(crab.lm$fit, crab.lm$res, xlab="Fitted Values", ylab="Residuals")
# and add a horizontal line
abline(h=0)

##############################################################
######################################################################

# Now create the X matrix
crab.x<-model.matrix(crab.lm)

#((Xtranspose)*X)inverse can be calculated by:
crab.ssx<-solve(t(crab.x)%*%crab.x)

# shortcut to get the ((Xtranspose)*X)inverse 
crab.lmsum<-summary(crab.lm)
crab.lmsum$cov.unscaled

# get MSE
crab.mse<-crab.lmsum$sigma^2

# Then to estimate the variance - covariance matrix of the parameters:
crab.vc<-crab.mse*crab.ssx


######################################################################

# Let's play with the glm function a little bit...

# Now use the glm function.  We use the same formula as the call as above, 
#  but now we need to specify the family.  
# Note per Venables and Ripley (2002): "A call to glm with the default 
#  gaussian family achieves the same purpose as a call to lm but less 
#  efficiently".
crab.glm<-glm(entrainment~density, family = gaussian, data=crab.df)

# And look at the summary of the model.  How does it compare with above?
summary(crab.glm)

