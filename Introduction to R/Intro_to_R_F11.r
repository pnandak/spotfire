################################################################
##            LISA Short Course Series
##               Introduction to R:
##  with emphasis on data preparation and plotting 
##           Sai Wang, LISA Collaborator
##            Department of Statistics
##                 Sep 27/29, 2011
################################################################
##                 Outline of talk
##  Section I:   programming basics
##  Section II:  graphing
## *Section III: basic statistical analysis (* not in class)
################################################################


################################################################
##  Section I:   Programming Basics

################################
##  1. Introduction and Preliminaries

# Useful resources:
# Download & Install R: http://www.r-project.org/
# RStudio, a better coding environment: http://rstudio.org/
# Official R Introductory Document: http://cran.r-project.org/doc/manuals/R-intro.pdf

# R interface:
# 1.Enter commands directly in R console.
# 2.Use a script editor.

# Running a block of script from script editor:
# 1.Select a block of script and press 'ctrl+R' to run selection;
# 2.If no selection is made, 'ctrl+R' runs the line where the cursor is at.
# In RStudio, shortcut is 'ctrl+enter'

# Commenting in a script:
# prefix '#' to add comments, contents after '#' in a line will not be executed.

# Naming conventions:
# 1.R is case SENSITIVE!
# 2.The name of an object must begin with a letter or '.' then letter.
# 3.Only '.', '_' and alphanumeric symbols 'a-zA-Z0-9' allowed.
# 4.Avoid using the same names of R default constant and functions.
# 5.Use meaningful names.

# Getting help
# 1.Use ?ftnname for help on a specific function
?mean
# 2.Use ??'keyword' to search for help documents related to keyword
??'generalized linear model'
# 3.Google. 

# Download packages and load libraries
# In R: Menu:Packages -> install packages
# In RStudio: Menu:Tools -> install packages
library() # lists packages installed on your computer

# Assignment operation:
#'<-' and '=' both work!

# Three basic value types: numeric, logical, character 
2.13
TRUE; FALSE  # or T, F for short
"Hello"
'2.13'
is.numeric('2.13')
as.logical(1)

# List objects in workspace
ls()
# Remove all objects
rm(list=ls()) # or click 'clear all' in RStudio Workspace panel

# Working directory
getwd()
#setwd("C:/your_folder_location/") # Must use '/' or '\\' to separate folders

##  END of 1. Introduction and Preliminaries
################################


################################
##  2. Vectors
# In R, a vector is a 1-dimensional group of ordered elements.

######## 2.1. Vector assignment
# A constant is a vector of length 1.
v.1 = 2.13
v.2 = TRUE
v.3 = "Hello"
# Use 'c()' to combine multiple values into one vector
v.4 = c(1,3, 5,7)
v.5 = c(v.1, v.4)
# Elements in a vector can ONLY be of ONE value type: numeric, logical or character. 
c(v.1, v.2, v.3)

######## 2.2. Generating regular sequences
# Use 'seq()' or ':' to generate a regular sequence 
?seq # Get help document on 
# Default usage: 
#   seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#       length.out = NULL, along.with = NULL, ...)
seq(from=1.3, to=9.8, by=.4)  # from 1.3 to 9.8 with increment of 0.4
seq(1.3, 9.8, .4) 
seq(1,10, length.out=15)  # desired length of the sequence is 15

# ':' is a simplied version with increment of either +1 or -1
1:10
0.2:-5

# Use 'rep()' to replicate elements in a vector 
v.8 = 1:5
rep(v.8, times=3) # repeat whole sequence 3 times
rep(v.8, each=3) # repeat each element 3 times
rep(v.8, times=c(1,3,2,1,4)) 

######## 2.3. Referencing elements in a vector
v.10 = 1:10

# Use 'length()' to get number of elements in a vector .
length(v.10)

# Use '[ ]' and index vectors to select subset of elements in a vector.
# 1. Use index vector of positive integers to select elements.
v.10[3]
v.10[c(1,3,8)]
# 2. Use index vector of negtive integers to exclude elements.
v.10[-3]
v.10[-c(1,3,8)]  
# 3. Use logical index vector to select elements based their values.
v.10>=5
v.10[v.10>=5]
v.10[v.10>=5 & v.10<8]
v.10[v.10>=8 | v.10<4]

# You can modify values of selected elements using regular assignment operation.
v.10[c(3,5,7)] = -c(1:3)
v.10[v.10<0] = 0


######## 2.4. Vector Arithmetics and Commonly Used Functions

# The arithmetic operations on vectors are usually element-wise.
v.11 = 1:5; v.12 = 2:6;
v.11+5
v.11^2
v.11*v.12
v.11%*%v.12 # '%*%' inter-product or matrix multiplication

# Commonly Used arithmetic functions
floor(3.123)
ceiling(3.123)
round(3.512)
9%/%4 # integer divide
9%%4 # remainder of integer divide
4^2 # power function
log(4)
exp(2)

# Logical Operations  
pi>3.14
!(pi>3.14) # NOT 
10/2 == 5 # check whether equal
10/2 != 5 # check whether different
pi>3.14 & pi<3.15 # AND operation, TRUE iff both conditions are TRUE
pi>3.15 | pi<3.14 # OR operation, FALSE iff both conditions are FALSE

# Commonly Used descriptive functions
## sort(x) 
## min(x)
## max(x) 
## mean(x)
## sd(x)
## summary(x) # 5 number summary plus mean
## quantile(x,.05) # return sample quantiles

# Be careful with NA values! 
v.13=rnorm(10,mean=0,sd=1) # 'rnorm()' generates normal random numbers
v.13[1] = NA # now v.13 has an NA
mean(v.13)
# Functions may act differently from what you would expect if NA values present.
mean(v.13,na.rm=T)
# Many R built-in functions allow you to exclude NA values from calculation by specifying 
# the 'na.rm=TRUE' argument

##  END of 2. Vectors
################################


################################
##  3. Factors
# A factor is a 1-dim object used to specify a discrete classification (grouping) of your data. 

# Consider the following arbitrarily generated teacher income data
state = rep(c("GA","NC","NY","PA","VA"),each=10) 
# Use 'factor()' to create a factor object
statef <- factor(state)
set.seed(123)
incomes = rep(c(48,44,57,54,43),each=10) + rnorm(50,mean=0,sd=4)

# Use 'levels()' to display distinct classes/groups in a factor
levels(statef)

# Use 'tapply()' to apply functions to each factor level
tapply(incomes,statef,mean) # this returns mean income for each state
tapply(incomes,statef,min)
tapply(incomes,statef,max)
tapply(incomes,statef,sd)

##  END of 3. Factors
################################


################################
##  4. Matrix/Array
# A matrix is 2-dim, a special case of array. An array can be multi-dimesional.

# Use 'matrix()' to create a matrix
matrix(0,nrow=4,ncol=3)  # creates 4x3 matrix of 0's
m.1 = matrix(1:12,4,3)  # values filled by columns by default
matrix(1:12,4,3,byrow=T)  # you can specify values by rows

# Use 'dim()' to retrieve dimension of a matrix
dim(m.1) # retrieve dimension of m.1
dim(m.1)[1] # no. of rows in m.1
dim(m.1)[2] # no. of cols in m.1

# Use '[]' to reference elements in a matrix
m.2 = matrix(1:12,4,3)
# Inside '[ ]', indices of each dimension are separated by ','.
# To select one element
m.2[3,2] # 3rd row and 2nd col
# To select multiple elements:
m.2[2,] # all elements in 2nd row
m.2[,3] # all elements in 3rd col
m.2[2,c(1,3)] # 2nd row & 1st,3th col
# To exclude elements, use negative index
m.2[-2,-3] # all elements except the ones in 2nd row OR 3rd col
# Use logical matrix to select elements based on their values
m.2[m.2>=4] # this returns a vector
m.2[m.2>=4] = '4+' # elements in a matrix must be of the same value type too

# Use 'colnames()' to assign names to columns
colnames(m.2) = c('Var1','Var2','Var3')
  
# Commonly used matrix arithmetics
m.3 = matrix(c(5,6,5,6,4,2,3,4),4,2)
t(m.3) # transpose
m.3*m.3 # element-wise product
m.4 = t(m.3)%*%m.3 # matrix product
solve(m.4) # inverse
cbind(1:4,m.3) # combine vectors/matrices by columns
rbind(t(m.3),1:4) # combine vectors/matrices by rows
diag(m.4) # diagonal elements in m.4
diag(1:4,4,4) # create a diagonal matrix

##  END of 4. Matrix
################################


################################
##  5. List and Data.frame

######## 5.1. List
# A list is an object consisting of an ordered collection of objects known as its
# components. Each component is a separate object, which can be a vector, array, 
# or list itself. List is the most flexible data structure in R. The outputs of
# many statistical analysis functions are given as list objects.

# Use 'list()' to create list objects
lst.1 = list(name="Fred",wife="Mary",no.children=3,child.ages=c(4,7,9))

# Use 'length()' to retrieve the no. of top level components 
length(lst.1)

# Use 'names()' to retrieve/modify names of top level components
names(lst.1)

# Referencing components of list objects
# use '[[i]]' to access the ith component
lst.1[[4]]
# use '$component_name' to access specific component
lst.1$wife
lst.1$w # you can use abbreviations as long as it's unique
lst.1$n # NULL because names of both 1st and 3rd components begin with 'n'
lst.1$no
## do not use '[ ]' to reference a component
lst.1[[4]][2]


######## 5.2. Data Frame
# A data frame is used for storing data tables. It is a list of vectors of equal 
# length. We call each column a variable/ or a component.
v.n = c(2,3,5,7) 
v.c = c("aa","bb","cc","dd") 
v.l = c(TRUE,FALSE,TRUE,TRUE) 
df.1 = data.frame(v.n,v.c,v.l) #df.1 appears like a matrix, but they are different
m.5 = cbind(v.n,v.c,v.l)

# Use 'read.csv()' to import the data table from a .csv file into R, or use 
# 'read.table()' fro more general data structures
# Don't forget to set working directory to the folder where the file is stored
setwd('C:/Documents and Settings/sai/Desktop/R_intro_11Fall/') # use '/' or '\\'
# Now read in 'EAiris.csv' file, with proper handling of missing values
df.iris = read.csv('EAiris.csv',header=T,na.strings=c('.','NA','99999999'))
# Use 'write.csv' to output to an external file
#write.csv(df.iris,'iris2.csv')

# Referencing values in a data.frame
mtcars  # a built-in dataset about cars, '?mtcars' for description
head(mtcars) # display the first part of mtcars
#list style:
mtcars[[2]] # 2nd component/variable/column in mtcars
mtcars[['wt']] # component 'wt'
mtcars$hp # horse power
#matrix style: 
mtcars[1,] # 1st row (entry); 
mtcars[,-2] # except 2nd column
mtcars[3,2]

# Use 'attach()' to reference a column in a data.frame directly by its name  
mtcars$mpg  
attach(mtcars)
mpg
wt

# Example 1: EAiris data, Data Cleaning
head(df.iris)
lapply(df.iris,class)  # check variable classes, any problems?
df.iris[,2]
# Correct the following line and re-import to resovle this
df.iris = read.csv('EAiris.csv',header=T,na.strings=c('.','NA','N/A','99999999'))
lapply(df.iris,class) # apply function 'class' to each component of df.list
# Now checking character values
df.iris[,5]
which(df.iris[,5]=='Setosa' | df.iris[,5]=='setosia') # Which indices are TRUE
df.iris[which(df.iris[,5]=='Setosa' | df.iris[,5]=='setosia'), 5] = 'setosa'
levels(df.iris[,5]) # levels are not updated
df.iris[,5] = factor(df.iris[,5]) # recreate a factor to update unique levels
# Output the cleaned dataset to an external .csv file
write.csv(df.iris, 'EAiris_clean.csv')

# Example 2: mtcars, Creating Summary Table
head(mtcars)
attach(mtcars)
# Suppose we need for each cyl group: n, {mean,median,sd} of mpg and wt
# What is the dimension of the output table?
unique(cyl) # 3 rows
out = matrix(0,3,8)
colnames(out) = c('cyl','n',paste(rep(c('mpg','wt'),each=3),c('Mean','Median','SD'),sep='.'))
out[,1] = unique(cyl) # levels of cyl
out[,2] = table(cyl) # frequency n
out[,3] = tapply(mpg,cyl,mean)
out[,4] = tapply(mpg,cyl,median)
out[,5] = tapply(mpg,cyl,sd)
out[,6] = tapply(wt,cyl,mean)
out[,7] = tapply(wt,cyl,median)
out[,8] = tapply(wt,cyl,sd)
write.csv(out,'mtcars_summary.csv')

# Example 3: Rearrangement
df.ex3 = read.csv('repeated.csv',header=T)
# Suppose we want to rearrange data so that each row is one observation
attach(df.ex3)
subject = rep(subject,each=4)
dose = rep(dose,each=4)
week = rep(1:4,times=20)
y = as.vector(t(df.ex3[,3:6]))
df.new = data.frame(subject,dose,week,y)
write.csv(df.new,'repeated_rearrange.csv')

# Now you've seen different data structures in R, many R functions may operate
# differently depending on the structure of input objects.
df.2 = df.iris[,-5] # now df.3 is a data.frame with 4 numeric variable
m.5 = as.matrix(df.2) # now m.5 is a matrix with the same values
class(df.2); class(m.5)
mean(df.2,na.rm=T) # 'mean()' for a d.f. object returns means for each variable
mean(m.5,na.rm=T) # 'mean()' for a matrix returns mean of all elements
colMeans(m.5,na.rm=T) # for a matrix, 'colMeans()' works
## 'apply()' is useful if you want a function to operate along a specifc dimension.
apply(m.5,2,sum,na.rm=T) # sum for each row

##  END of 5. List and Data.Frame
################################


rm(list=ls()) # delete all objects


################################
##  6. Conditional Execution and Loops

######## 6.1. Conditional Execution with if...else... statement
# Format of if...else...:
##  if (condition) {
##		commands to be executed when condition is TRUE
##  } else {
## 		commands to be executed when condition is FALSE
##	} # the else statement is optional 
## Example:
set.seed(123)
v.1 = runif(1) # generates a random number in (0,1)
if (v.1>.5) print('Head') else print('Tail')
## do this again
v.1 = runif(1) # generates a random number in (0,1)
if (v.1>.5) print('Head') else print('Tail')

######## 6.2. 'for()' loops
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

######## 6.3. 'while()' loops
# A 'while()' loop will continue to iterate until the condition becomes FALSE.
v.3 = 0; niter = 0 # intial value of v.3 and iteration counter
while (v.3<0.9) {
  v.3 = runif(1)
  niter = niter+1
  cat('iter:',niter,'   v.3=',v.3,'\n')
}
# Be careful with your condition expression, there must be some command inside
# '{ }' to change the condition for the while loop to escape.

##  END of 6. Conditional Execution and Loops
################


##  END of Section I:   Programming Basics
################################################################



################################################################
##  Section II:   Graphing

# High-level plotting functions create a new plot on the graphics device, 
# possibly with axes, labels, titles and so on. 

# Low-level plotting functions add more information to an existing plot, 
# such as extra points, lines and labels. 

# The mtcars dataset will be used in examples
attach(mtcars) # now we can directly use component names

################
## 7. High-level Graphing Functions
# High-level plotting functions create a new plot on the graphics device, 
# possibly with axes, labels, titles and so on. 

#### 7.1. 'plot()'
plot(wt,mpg) # if inputs are 2 vectors, scatterplot of 1st on X, and 2nd on Y
plot(mpg~wt) # you can also use foumula expression
plot(mpg~factor(cyl)) # if 'y~factor', boxplot for each level
plot(factor(cyl)) # if input is a factor, bar chart of # of obs in each level
plot(cbind(wt,mpg,cyl)) # if input is a matrix, scatterplot of first 2 columns
plot(mtcars) # if input is a data.frame, scatterplot matrix
pairs(cbind(mpg,wt,hp,cyl)) # pairs()

#### 7.2. other high-level plotting functions
hist(mpg,freq=F) # histogram
boxplot(mpg) # boxplot
boxplot(mpg~cyl,col=rainbow(3)) # you can also use formula
cyl_freq = table(cyl);  pie(cyl_freq) # pie chart for freq. data
barplot(cyl_freq, horiz=T) # barplot for freq. data, horiz=T / F
qqnorm(mpg) # Normal Q-Q plot
curve(.5*(x-3)^2+log(x),from=0,to=4) # draw function curve

#### 7.3. Misc.
# High-level function creates a new plot on the current window. 
# Use 'x11()' to create a new empty window if you don't want your current 
# plot to be replaced.
x11() # win.graph() does the same for Windows OS 
plot(mpg~factor(cyl)) # if 'y~factor', boxplot for each level

# Some high-level plots can be overlayed with 'add=TRUE' argument.
x11()
curve(.5*x^2+log(x),from=0,to=1) # draw function curve
curve(.5*x^2+0.8*log(x),from=0,to=1,add=T,col='red')

# Optional arguments to high-level plotting functions:
plot(mpg~wt) # default plot
x11()
plot(mpg~wt,xlab='Weight',ylab='Miles per Gallon',main='add a titile',
sub='also a subtitile if you want',xlim=c(1,6),ylim=c(10,40))

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
##  END of 7. High-level Graphing Functions
################
par(mfrow=c(1,1))

################
##  8. Low-level Graphing Functions
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
x11()
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
text(x[50],y[50],'outlier',col='red',pos=1)
lines(0:4,1+.5*0:4,col='blue',lwd=2)
abline(fit$coefficients[1],fit$coefficients[2],col='red',lwd=2,lty=2)
title(main='Simple linear regression')
legend('bottomright',legend=c('True model','SLR fit'),col=c('blue','red'),lty=1:2,lwd=2)

# The following code defines a function that draws scatterplot of x and y, and 
## draws points for each level of the grouping variable z in different colors and
## and symbols.
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
# Apply this function to wt, mpg and cyl 
scatter.c(wt,mpg,cyl,obs=T)
title(xlab='weight',ylab='Miles per Gallon',main='Scatter plot of mpg*wt by cyl')
legend('topright',legend=c('cyl: 4','cyl: 6','cyl: 8'),col=rainbow(3),pch=1:3)

##  END of 8. Low-level Graphing Functions
################

##  END of Section II:   Graphing
################################################################



################################################################
##  Section III:   Basic Statistical Analysis
    
################
##  9. T-test
# Still use the 'mtcars' data set. Consider comparing mpg of cars between 
# automatic and manual transmission.

# First plot data
boxplot(mpg~am,xlab='Transmission',ylab='mpg',names=c('auto','manual'))

# Two sample t-test
t.test(mpg~am) # Default is Welch t-test with unequal variance assumption
t.test(mpg~am,var.equal=T) # T-test with equal variance assumption
# Suppose data in two vectors
mpg.a = mpg[am==0];    mpg.m = mpg[am==1]
t.test(mpg.a,mpg.m)
# t.test(x,y,paired=T) # paired t-test

# One sample t-test
t.test(mpg.a,mean=20) # test whether mean mpg for automatic is 20

# Power of two sample t-test
power.t.test(n=32, delta=2, sd=2) # power
power.t.test(delta=2,sd=2,power=.8) # min sample size to give power of 0.8

# Checking for normality
qqnorm(mpg.a)		# normal quantile-quantile plot
shapiro.test(mpg.a)	# Shapiro-Wilk Normality Test
##  END of 9. T-test
################


################
##  10. ANOVA
# ANOVA is used to compare means of more than two groups.

# One-way ANOVA: consider comparing mpg for 3 cyl levels
# First plot data
boxplot(mpg~cyl,xlab='cyl',ylab='mpg')
# One-way ANOVA:
a.1= aov(mpg~factor(cyl))
summary(a.1)
# pairwise comparisons
TukeyHSD(a.1)
plot(TukeyHSD(a.1))
# one-way anova without equal var assumption
oneway.test(mpg~cyl) 

# Two-way ANOVA: consier comparing mpg~cyl*am
# First plot data
par(mfrow=c(1,2))
boxplot(mpg~cyl,subset=am==0,ylim=c(10,35),xlab='cyl',main='automatic')
boxplot(mpg~cyl,subset=am==1,ylim=c(10,35),xlab='cyl',main='manual')
# Two-way ANOVA:
a.2 = aov(mpg~factor(cyl)*factor(am)) # main effects and intaction
# a.2 = aov(mpg~factor(cyl)+factor(am)) # main effects only
summary(a.2) # results based on type I SS
drop1(a.2, ~., test="F") 	# type III SS and F Tests
x11()
interaction.plot(cyl, am, mpg, type="b", col=c(1:3),leg.bty="o", 
  leg.bg="beige", lwd=2, pch=c(18,24,22), xlab="Number of Cylinders", 
  ylab="Mean Miles Per Gallon", main="Interaction Plot")
# pairwise comparisons
TukeyHSD(a.2)
plot(TukeyHSD(a.2,'factor(cyl):factor(am)'))
##  END of 10. ANOVA
################

  
################
##  11. Regression
# Consider the 'stackloss' dataset, which contains operational data of a plant 
# for the oxidation of ammonia to nitric acid. Read the descriptions about this 
# dataset by typing ?stackloss in R. 
?stackloss
# Plot data
plot(stackloss)
# Fit linear regression model
r.1 = lm(stack.loss~Air.Flow + Water.Temp + Acid.Conc., data=stackloss)
names(r.1)
r.1$fitted.values
summary(r.1) # Summary of analysis
anova(r.1) # Type I ANOVA table
drop1(r.1,~.,test='F') # Type III ANOVA table
# Using 'plot()' on fitted model from 'lm()' produces nice diagnostic plots.
par(mfrow=c(2,2))
plot(r.1)
# Use 'predict()' on fitted model to get prediction intervals
predict(r.1,interval='confidence',level=.9) # 90% confidence interval
predict(r.1, newdata=data.frame(Air.Flow=65,Water.Temp=22,Acid.Conc.=80),    
      interval='prediction') # 95% prediction interval for arbitrary point

##  END of 11. Regression
################

##  END of Section III:   Basic Statistical Analysis
################################################################