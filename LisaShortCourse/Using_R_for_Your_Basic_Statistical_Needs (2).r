
####################################################
####################################################
###                                              ###
###   Using R for Your Basic Statistical Needs   ###
###               LISA Short-Course              ###
###       Nels Johnson, LISA Collaborator,       ###
###            Department of Statistics          ###
###            November 15 and 16, 2010          ###
###                                              ###
####################################################
####################################################

##########################
## 1. Bare Bones Basics ##
##########################

#######
# 1.1 #
#######

# R is a software language that runs one line at a time. No ; is neccessary to end a line,
# just make a new line. The # symbol creates comments. Everything after # is not run.
# That's why every line in which I type like this begins with #. 

# In R we create "objects" and use "functions" to evaluate those objects.

# Let's make some simple objects.

A <- 1
B <- pi
C <- 1i

# Now let's use some simple functions.

D <- exp(B*C) + A

# You've probably noticed when each of these lines is run the value is not returned.
# To return the value, simply run the object.

A
B
C
D

#######
# 1.2 #
#######

# All these objects we just made have a "class". Functions will only work on objects
# of the appropriate class or may work in a way you don't want because the object is
# the wrong class. A and B are "numeric" objects because they are real numbers. C and
# D are "complex" objects because they have real and non real parts. Let's check.

class(A)
class(D)

# Some objects can be coerced into other objects. Let's try.

E <- as.complex(A)
class(E)

#######
# 1.3 #
#######

# Numeric objects are very important. Another very important object class is "matrix".

A <- diag(1,3)
A

B <- matrix(0,nrow=3,ncol=3)
B

# Often R will refer to a "vector" which is not a class, but can be thought of as a one
# dimensional matrix. It can be numeric or matrix class.

A <- c(1,2,3,4,5,6,7,8,9)
B <- matrix(A,nrow=9,ncol=1)
class(A)
class(B)

# It can be a "character" class

A <- c("Dog","Cat","Pig")
class(A)

#######
# 1.4 #
#######

# Let's talk about some basic matrix manipulation. Unlike Matlab, operators in R are by
# default element-wise. To perform matrix algebra, we have to use special operators.

A <- matrix(c(1,2,3,4),nrow=2,ncol=2)
B <- matrix(c(1,3,2,4),nrow=2,ncol=2)

A
B

2*A+7
A+B
A*B
A^B

A%*%B # Matrix multiplication

solve(B) # Matrix inverse

t(A) # Matrix transpose

#######
# 1.5 #
#######

# Suppose you only want to look at only certain parts of a matrix

A <- matrix(1:2000,nrow=100,ncol=20)

A[,5] # All rows in the 5th column
A[33,] # All columns in the 33rd row
A[33,5] # The element in row 33 and column 5

A[,-5] # All columns except the 5th
A[-33,] # All rows except the 33rd

A[,c(1,3,5,7)] # Only columns 1, 3, 5, and 7.

# There are many more ways to partition matrices. For instance, using a "logical" class 
# vector instead of a numeric class. Or a numeric class vector made up entirely of 0's 
# and 1's.

A[A[,1]>90,]

# All rows in A such that the element in row 1 is greater than 90.

#######################
## 2. Importing Data ##
#######################

# As you can see, there is no option to import data in the top menus. We have to actually
# write the code that imports the data. To make this short-course easier to manage, we're going
# to first export one of the R datasets and then import it.

# Let's take a look at the built in R datasets
data()

# Let's look at the USArrests data
USArrests

?write.table #The question mark opens the help information about the function write.table.

write.table(USArrests,"C:/Users/Nels/Desktop/USArrests.txt",row.names=T)

?read.table

dat <- read.table("C:/Users/Nels/Desktop/USArrests.txt",sep="",header=T)
dat

class(dat)
head(dat)

# Notice that I'm on a Windows machine and I use / instead of \ for the file directory even
# though Windows machines use \ when specifying file pathways. This is a relic of R's creation
# on another operating system.

# Now that we have a dataset let's talk about managing it. We can actually look at a data object
# just like we would a matrix object. We can perform operations on it like a matrix too.

dat[,1]+1
dat[,-4]

# However because it is a data.frame object, its columns are "members". To call a member from
# an object we use $. First let's get the names of the members.

names(dat)

dat$Murder
dat[,1]

# It turns out for a data.frame the names are the same as the column names. We can get the row and
# column names of a matrix by the following:

rownames(dat)
colnames(dat)

#############################################
## 3. Some Basic Exploratory Data Analysis ##
#############################################

# Suppose we'd like some basic summary statistics on our dataset.

summary(dat)

var(dat) # Variance-covariance matrix
cor(dat) # Correlation matrix
mean(dat) # Means
sd(dat) # Standard deviations

# Notice that the var(), mean(), and sd() functions were applied to each column. This is because
# dat is a data.frame object. If dat were say a matrix

dat2 <- as.matrix(dat)

mean(dat2)

# Now mean(), etc. returns the mean of all observations in the matrix. If we want to have the mean
# applied to each column, we need to use a special function.

?apply

apply(dat2,2,mean)

# I lied, there is an easier way to do this, but apply is very useful.

colMeans(dat2)

#########################
## 4. Some Basic Plots ##
#########################

#######
# 4.1 #
#######

# Another very key part of EDA is graphing the data. The most general plotting function R has is.

plot(dat)

# Plot will return different plots depending on the class of the object (or objects) used as an 
# argument(s). dat is a data.frame object so plot returns a scatterplot matrix of the columns.

# Let's take some time now to try and explore the function plot(). I would like everyone to try to 
# figure out how to do the following:

# 1. Change the color of the plotted points
# 2. Change the symbol of the plotted points
# 3. Change the size of the plotted points
# 4. Put a title on the graph
# 5. Only plot Assault vs UrbanPop

# Hint 1: for 1-3: look up the par() function. Many of its arguments can be used in plot().
# Hint 2: I use all of these later in the presentation.

#######
# 4.2 #
#######

# Another popular summary plot is the histogram.

hist(dat$Assault)

# A similar plot to a histogram is the estimated density. 

plot(density(dat$Assault),main="Estimated Density")

# Some of you may have noticed by now that every time we make a new plot, the old one is lost.
# R overwrites new plots into the active graphics window. However there are functions to modify
# the plot in the active window too.

# Suppose we'd like to put that density plot on top of that histogram. Since the density is a line,
# we're going to use the lines() function. But we'll also need our histogram to be on the relative
# frequency scale (the same scale the density is on) instead of frequency (aka counts).

hist(dat$Assault,freq=F,col="tomato",main="Assault",xlab="Assaults per 100,000")
lines(density(dat$Assault),col="Blue",lwd=3)

#######
# 4.3 #
#######

# What about if we want to overlay two scatterplots? Scatter plots are made of points so use the
# function points().

plot(dat$UrbanPop,dat$Assault,pch=16,main="Murder and Assaults",ylab="Murder/Assaults per 100k",xlab="Urban Population")
points(dat$UrbanPop,dat$Murder,pch=16,col=2)

# Wait a second, why didn't that do anything? Because the size of the yaxis was set automatically
# when we plotted assault. It turns out the max number of murders is less than the number of assaults
# So let's fix that and add a legend to the plot

plot(dat$UrbanPop,dat$Assault,pch=16,ylim=c(0,350),xlab="Urban Population",ylab="Murders/Assaults per 100k")
points(dat$UrbanPop,dat$Murder,pch=16,col=2)
legend("topleft",c("Assaults/100k","Murders/100k"),pch=16,col=c(1,2))

# Well that isn't useful either since the scales are so different. Let's modify them to be on the same scale.

plot(dat$UrbanPop,dat$Assault,pch=16,col="Black",xlab="Urban Population",ylab="Assault")
points(dat$UrbanPop,scale(dat$Murder,scale=T,center=T)*sd(dat$Assault)+mean(dat$Assault),pch=16,col="Red")
axis(4,at=c(100,200,300),labels=c(5,10,15))
mtext("Murder",side=4,line=-2)
legend("topleft",c("Assaults/100k","Murders/100k"),pch=16,col=c("Black","Red"))

#######
# 4.4 #
#######

# What if we do want to make a new window?

win.graph()

# On a Mac do x11().

hist(dat$Assault,freq=F,col="tomato",main="Assault",xlab="Assaults per 100,000")
lines(density(dat$Assault),col="Blue",lwd=3)

# What if we want both graphs in the same window? Use par() (though I change the window size first).

win.graph(width=14,height=7)

par(mfrow=c(1,2))

plot(dat$UrbanPop,dat$Assault,pch=16,col="Black",xlab="Urban Population",ylab="Assault")
points(dat$UrbanPop,scale(dat$Murder,scale=T,center=T)*sd(dat$Assault)+mean(dat$Assault),pch=16,col="Red")
axis(4,at=c(100,200,300),labels=c(5,10,15))
mtext("Murder",side=4,line=-2)
legend("topleft",c("Assaults/100k","Murders/100k"),pch=16,col=c("Black","Red"))

hist(dat$Assault,freq=F,col="tomato",main="Assault",xlab="Assaults per 100,000")
lines(density(dat$Assault),col="Blue",lwd=3)

#######
# 4.5 #
#######

# OK, we have a nice plot and we want to save it as a pdf or a postscript file to put in a paper.

pdf("C:/Users/Nels/Desktop/Rplot.pdf")

plot(dat$UrbanPop,dat$Assault,pch=16,col="Black",xlab="Urban Population",ylab="Assault")
points(dat$UrbanPop,scale(dat$Murder,scale=T,center=T)*sd(dat$Assault)+mean(dat$Assault),pch=16,col="Red")
axis(4,at=c(100,200,300),labels=c(5,10,15))
mtext("Murder",side=4,line=-2)
legend("topleft",c("Assaults/100k","Murders/100k"),pch=16,col=c("Black","Red"))

hist(dat$Assault,freq=F,col="tomato",main="Assault",xlab="Assaults per 100,000")
lines(density(dat$Assault),col="Blue",lwd=3)

dev.off()

# Notice that every time a new plot object is created instead of overwriting the previous plot
# a new page is created in the pdf document. This makes it very easy to scan through many graphs
# at once if they are all placed after another in a pdf document.

# The dev.off() is important. It tells R when to end the creation of the pdf or ps, etc. file.

# As I said, we can save it as postscript as well.

postscript("C:/Users/Nels/Desktop/Rplot.ps")

plot(dat$UrbanPop,dat$Assault,pch=16,col="Black",xlab="Urban Population",ylab="Assault")
points(dat$UrbanPop,scale(dat$Murder,scale=T,center=T)*sd(dat$Assault)+mean(dat$Assault),pch=16,col="Red")
axis(4,at=c(100,200,300),labels=c(5,10,15))
mtext("Murder",side=4,line=-2)
legend("topleft",c("Assaults/100k","Murders/100k"),pch=16,col=c("Black","Red"))

hist(dat$Assault,freq=F,col="tomato",main="Assault",xlab="Assaults per 100,000")
lines(density(dat$Assault),col="Blue",lwd=3)

dev.off()

# Quick exercise: see if you can guess any other image types and search for them with the ?.
# Quick exercise 2: see if you can guess the function for boxplots and for dotplots.

##############################
## 5. Some Basic Statistics ##
##############################

#######
# 5.1 #
#######

# Let's start with a T-test.

help.search("t test")

# That's a lot of stuff there. Since we're looking for something basic lets restrict what
# ourselves to the functions with (stats) appended to the end of it. This means it is in
# the stats library, which is one of the basic R libraries.

# You'll see the function we want is t.test().

?t.test 

# If you scroll down to the bottom of the help for t.test you'll see a few examples. 
# Often the examples are very difficult to understand, but this one is nice and easy, 
# so I've copied it here. It uses the famous "sleep" data, which is a built in dataset 
# for R. To read more about it ?sleep.

# First let's plot the data to see if a T-test is appropriate. This is an alternative way
# to produce histograms using the plot() function. It uses the "formula" notation 'Y~X'.
# This is very common notation in many R functions.

plot(extra ~ group, data = sleep,ylab="Hours of Extra Sleep",xlab="Treatment Drug Received")

# Now let's do a 2 sample T-test to test if the mean of group 2 is significantly larger 
# than the mean of group 1. We use the formula notation.

t.test(extra ~ group, data = sleep, var.equal=TRUE, alternative="less")

# I've set it up so Ho: mean1 >= mean2 Ha: mean1 < mean2, and I'm assuming equal variances.

#######
# 5.2 #
#######

# Let's move to something only slightly more complicated, a one-way ANOVA. Now we fit a 
# linear model. Notice we use the same equation notation from before. We use the PlantGrowth
# built-in dataset.

plot(weight ~ group, data=PlantGrowth)
reg1 <- lm(weight ~ group, data=PlantGrowth)

class(reg1)

# Notice reg1 has an "lm" object class. Performing ANOVA is straightforward. 

anova(reg1)

# Using plot() on lm objects automatically creates nice diagnostic plots.

par(mfrow=c(2,2))
plot(reg1)

# These diagnostics look OK and our p-value is less than 0.05, so at least one treatment mean should
# be different from the others. We can investigate this with multiple comparisons, using Tukey's HSD
# adjustment.

?TukeyHSD

# Notice that this example uses a function called aov() instead of anova(). anova() is essentially a
# spruced up version of aov(). anova() should be preferred in most circumstances.

TukeyHSD(aov(reg1),"group")
plot(TukeyHSD(aov(reg1),"group"))

#################################
## 6. Some Regression Examples ##
#################################

#######
# 6.1 #
#######

# Suppose now we're actually interested in model selection for a multiple linear regression model.
# We'll use the "swiss" built-in data set concerning fertility in Switzerland. For more information
# remember to ?swiss.

# Here's another way to provide a scatterplot matrix, with some added content.

pairs(swiss, panel = panel.smooth, main = "swiss data", col = 3 + (swiss$Catholic > 50))

reg.swiss <- lm(Fertility ~ . ,data=swiss)

# Notice how in the X variables section I put a period. This tells R to use all the variables in
# the dataset specified by "data=" besides the dependent variable Fertility.

# When we use the function summary() on a lm object we get something different from what we would
# get on a data.frame object.

summary(reg.swiss)

par(mfrow=c(2,2))
plot(reg.swiss)

#######
# 6.2 #
#######

# Suppose we'd like to produce a reduced set of independent variables. We could use the function
# step() to perform stepwise model selection based on AIC which is -2log(Likelihood) + kp? Where k=2
# and p = number of model parameters (beta coefficients).

step(reg.swiss)

# What happens when we use BIC which is -2log(Likelihood) + kp? Where k=log(n) and n = sample size.

step(reg.swiss,k=log(dim(swiss)[1]))

# In this case using BIC gives the same solution as AIC.

reg.swiss2 <- lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality ,data=swiss)

summary(reg.swiss2)

par(mfrow=c(2,2))
plot(reg.swiss2)

# Notice the sign on the effect of agriculture on fertility is the opposite of what we'd expect
# based on the scatterplot matrix.

# When can also use anova() to compare hierarchically related models in a lack of fit test.

anova(reg.swiss,reg.swiss2)

#######
# 6.3 #
#######

# Some of you may have noticed in our scatterplot matrix that the covariates in the swiss dataset
# appear to be correlated. LASSO is a new and popular method for model selection. It is particularly
# good for p > n problems.

help.search("lasso")

library(lars)

?lars

reg.swiss3 <-lars(as.matrix(swiss[,2:6]), swiss[,1], type ="lasso")

summary(reg.swiss3)

# We use Mallow's Cp for model selection here. When Mallow's Cp is much larger than df for the model,
# that model is very biased. The goal is to pick a model with lower df but not a large increase in Cp.

plot(reg.swiss3)

# It looks like a model with only 4 regressors is a good idea. It turns out the model selected using
# lasso is different than using stepwise. All the coefficients have the sign we would expect too.

coef(reg.swiss3)[5,]
coef(reg.swiss2)[2:5]

# Let's plot them to compare fits. We can see in this case they actually provide very similar fits.

plot(swiss$Fertility,fitted(reg.swiss2),xlim=c(40,95),ylim=c(40,95),xlab="Fertility",ylab="Fitted Fertility",pch=16)
abline(a=0,b=1)
points(swiss$Fertility,predict(reg.swiss3,swiss[,2:6],s=5,type="fit")$fit,col=2,pch=16)
legend("topleft",c("Stepwise","LASSO"),col=c(1,2),pch=16)

#######
# 6.4 #
#######

# Another way to deal with collinearity is to regress the dependent variable onto principal components of the 
# independent variables. The PCs are independent so there is not multicollinearity issue. Model loses
# interpretability.

help.search("principal component")

?princomp

# We do PCA on the correlation matrix since variance of the dependent variables are not similar.

apply(swiss[,2:6],2,var)

swiss.pca <- princomp(swiss[,2:6],cor=T)

# We could decide to reduce the number of PCs we use based on this graphic.

plot(swiss.pca)

# We're going to use all of them.

names(swiss.pca)
Z <- swiss.pca$scores

reg.swiss4 <- lm(swiss$Fertility ~ Z)

summary(reg.swiss4)

par(mfrow=c(2,2))
plot(reg.swiss4)

#######
# 6.5 #
#######

# Let's look at all the fits together.

plot(swiss$Fertility,fitted(reg.swiss2),xlim=c(40,95),ylim=c(40,95),xlab="Fertility",ylab="Fitted Fertility",pch=16)
abline(a=0,b=1)
points(swiss$Fertility,predict(reg.swiss3,swiss[,2:6],s=5,type="fit")$fit,col=2,pch=16)
points(swiss$Fertility,fitted(reg.swiss4),col=4,pch=16)
legend("topleft",c("Stepwise","LASSO","PC Regression"),col=c(1,2,4),pch=16)

# An issue that still exists with this dataset is that it has a few outliers. PCA is sensitive to
# outliers so we still may not be getting the best answer we'd like.

#############
## Closing ##
#############

# I hope you've found this crash course in R and some regression tools useful. My last bit of advice
# I'll give you is concerning R resources.

# First if you want to do a library search for R, the official library subject is

# R (Computer Program Language)

# The Virginia Tech library has many good textbooks on R, many of which are available online as pdfs.
# To be clear, that means you can download entire books on R for free from the VT library website.

# Many R libraries have been organized by statistical subject at

# http://cran.r-project.org/web/views/

# You can download R from its website 

# http://cran.r-project.org














