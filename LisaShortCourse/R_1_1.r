###############################################################
##        LISA Short Course Series
##        Introduction to R, Part I:
##        Jinhui Sun, LISA Collaborator
##        Department of Statistics
##        Feb 18, 2013
################################################################
##                 Outline of talk
##  Section I: programming basics
##    1: Introduction and Preliminaries
##    2: Vectors
##    3: Factors
##    4: Matrices/Arrays
##    5: Lists/ Data Frames
##    6: Examples
##  Section II: advanced programming and graghing
##    1: Conditional Execution and Loops
##    2: graphing
################################################################

################################################################
##  Section I:   Programming Basics
###############################################################
##  1. Introduction and Preliminaries

# Useful resources:
# Download & Install R: http://www.r-project.org/
# Official R Introductory Document: http://cran.r-project.org/doc/manuals/R-intro.pdf
# RStudio, a better coding environment: http://rstudio.org/
# Tinn-R, another coding enviroment: http://www.sciviews.org/Tinn-R/

# Running a block of script from RStudio:
# 1.Select a block of script and press 'ctrl+enter' to run selection;
# 2.If no selection is made, 'ctrl+enter' runs the line where the cursor is at.
# In R, shortcut is 'ctrl+r'

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
# 2.Use ??'keyword' or help.search("keyword") to search for help documents related to keyword
??'generalized linear model'
help.search("generalized linear model")
# 3.R reference
help.start()
# 4.Google

# Download packages and load libraries
# In RStudio: Tools -> install packages
library() # lists packages installed on your computer

# List objects in workspace
ls()
# Remove all objects
rm(list=ls()) # or click 'clear all' in RStudio Workspace panel

# Working directory
getwd()
#setwd("C:/your_folder_location/") # Must use '/' or '\\' to separate folders
setwd("C:/Users/vin/Desktop")

##  END of 1. Introduction and Preliminaries
################################


################################
##  2. Vectors
# In R, a vector is a 1-dimensional group of ordered elements.

######## 2.1. Data types 
# There are three basic value types in R: numeric, logical, character 
2.13 
TRUE; FALSE  # or T, F for short
"Hello" 
'2.13'
is.numeric('2.13')

######## 2.2. Vector Assignment
# A constant is a vector of length 1.
# Assignment operation:
#'<-' and '=' both work!
v.1 = 2.13; v.1
v.2 = TRUE
v.3 = "Happy Valentine's Day"
# Use 'c()' to combine multiple values into one vector
v.4 = c(1,3,5,7)
v.5 = c(v.1, v.4)
# Elements in a vector can ONLY be of ONE value type: numeric, logical or character. 
c(v.1, v.2, v.3)

######## 2.3. Generating  sequences
# Use 'seq()' or ':' to generate a sequence 
?seq 
seq(from=1.3, to=9.8, by=.4)  # from 1.3 to 9.8 with increment of 0.4
seq(1.3, 9.8, .4) 
seq(1,9, length.out=15)  # desired length of the sequence is 15

# ':'(colon) is a simplied version with increment of either +1 or -1
1:10
0.2:-5

# Use 'rep()' to replicate elements in a vector 
v.8 = 1:5
rep(v.8, times=3) # repeat whole sequence 3 times
rep(v.8, each=3) # repeat each element 3 times
rep(v.8, times=c(1,3,2,1,4)) 

######## 2.4. Referencing elements in a vector
v.10 = 11:20
# Use 'length()' to get number of elements in a vector .
length(v.10)

# Use '[ ]' and index vectors to select subset of elements in a vector.
# 1. Use index vector of positive integers to select elements.
v.10[3]
v.10[c(1, 3, 8)]
# 2. Use index vector of negative integers to exclude elements.
v.10[-3]
# 3. Use logical index vector (boolean statement) to select elements based their values.
v.10 >= 15
v.10[v.10 >= 15]
v.10[v.10 >= 15 & v.10 < 18] #Select elements of V.10 >= 15 AND < 18
v.10[v.10 >= 18 | v.10 < 14] #Select elements of v.10 >= 18 OR < 14


# You can modify values of selected elements using regular assignment operation.
v.10[c(3, 5, 7)] = -c(1:3); v.10
v.10[v.10 < 0] = 0; v.10


######## 2.5. Vector Arithmetic and Commonly Used Functions

# The arithmetic operations on vectors are usually element-wise.
v.11 = 1:5; v.12 = 2:6;
v.11+5
v.11^2 # Use ^ for exponents or equivalently v.11**2
v.11*v.12 # Elementwise multiplication
v.11%*%v.12 # '%*%' inter-product or matrix multiplication

# Commonly Used arithmetic functions
floor(3.123)
ceiling(3.123)
round(3.512)
round(3.512,1)
round(64.2,-1)
9%/%4 # integer divide
9%%4 # remainder of integer divide
log(4) # exponential log, use log10(x) for base 10 log
exp(1)

# Logical Operations  
pi > 3.14
! (pi > 3.14) # Tests whether pi is not greater(or less than) than 3.14 
10/2 == 5 # check whether equal Note: Double equals sign for testing equality
10/2 != 5 # check whether different

# Commonly Used descriptive functions
v.13 <- c(1, 5, 2, 6, 5, 12, 7, 8, 1, 4)
sort(v.13) 
min(v.13)
max(v.13) 
mean(v.13)
sd(v.13)
summary(v.13) # 5 number summary plus mean

# Be careful with NA values! 
v.14=v.13 

v.14[1] = NA; v.14 # now v.13 has an NA

# Functions may act differently from what you would expect if NA values present.
mean(v.14)

# Many R built-in functions allow you to exclude NA values from calculation by specifying 
# the 'na.rm=TRUE' argument
mean(v.14,na.rm=T)

##  END of 2. Vectors
################################


################################
##  3. Factors
# A factor is a 1-dim object used to specify a discrete classification (grouping) of your data. 

# Consider the following arbitrarily generated teacher income data
state = rep(c("IA","NC","NY","PA","VA"),each=10) 
# Use 'factor()' to create a factor object
?factor
statef <- factor(state)
# Use 'levels()' to display distinct classes/groups in a factor
levels(statef)

#Generate hypothetical teacher incomes
set.seed(7172012)
incomes = rep(c(38,44,57,54,43),each=10) + rnorm(50,mean=0,sd=4)

# Use 'tapply()' to apply functions to each factor level
?tapply
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
m.2[2,c(1,3)] # 2nd row & 1st,3th col
# To exclude elements, use negative index
m.2[-2,-3] # all elements except the ones in 2nd row OR 3rd col
# Use logical matrix to select elements based on their values
m.2[m.2>=4] # this returns a vector
m.2[m.2>=4] = '4+' # elements in a matrix must be of the same value type too

# Use 'colnames()' to assign names to columns
colnames(m.2) = c('Var1','Var2','Var3');m.2

# Commonly used matrix arithmetics
m.3 = matrix(c(5,6,5,6,4,2,3,4),4,2); m.3
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
lst.1 = list(name="Fred",spouse="Mary",no.children=3,child.ages=c(4,7,9))

# Use 'length()' to retrieve the no. of top level components 
length(lst.1)

# Use 'names()' to retrieve/modify names of top level components
names(lst.1)

# Referencing components of list objects
# use '[[i]]' to access the ith component
lst.1[[4]]
lst.1[4]
# use '$component_name' to access specific component
lst.1$spouse
lst.1$s # you can use abbreviations as long as it's unique
lst.1$n # NULL because names of both 1st and 3rd components begin with 'n'
lst.1$na

######## 5.2. Data Frame
# A data frame is used for storing data tables. It is a list of vectors of equal 
# length. We call each column a variable/ or a component.
# A data file importing into R is typically in this structure.
v.n = c(2,3,5,7) 
v.c = c("aa","bb","cc","dd") 
v.l = c(TRUE,FALSE,TRUE,TRUE) 
df.1 = data.frame(v.n,v.c,v.l); df.1 #df.1 appears like a matrix, but they are different

# Referencing values in a data.frame
mtcars  # a built-in dataset about cars, '?mtcars' for description
head(mtcars) # display the first part of mtcars
#list style:
mtcars[[2]] # 2nd component/variable/column in mtcars
mtcars[['wt']] # component 'wt'
mtcars$hp # horse power

# Use 'attach()' to reference a column in a data.frame directly by its name  
mtcars$mpg  
attach(mtcars)
mpg
wt
detach(mtcars)

# Use 'read.csv()' to import the data table from a .csv file into R, or use 
# 'read.table()' for more general data structures
# Don't forget to set working directory to the folder where the file is stored
setwd("C:/Users/vin/Desktop")
# use '/' or '\\'
# Now read in 'EAiris.txt' file, with proper handling of missing values
df.iris = read.table('EAiris_1.txt',header=T,sep=',',na.strings=c('.','NA','99999999'))
# Head funtion shows first few rows of data frame
head(df.iris)
# Use 'write.csv' to output to an external file
write.csv(df.iris,'iris2.csv')

##  END of 5. List and Data.Frame
################################

################################
##  6. Programming Examples

# Example 1: EAiris data, Data Cleaning
# Goal: import data set and clean data for analysis
df.iris = read.table('EAiris_1.txt',header=T,sep=',',na.strings=c('.','NA','99999999'))
head(df.iris) #head function displays first few rows of data
lapply(df.iris,class)  # check variable classes, any problems?
#SEPAL WIDTH IS A FACTOR AND SHOULD BE A NUMERIC VARIABLE
df.iris[,2] # "N/A" needs to be identified as an NA value
# re-import to resolve this, denote 'N/A' as an NA value
df.iris = read.table('EAiris_1.txt',header=T,sep=',',na.strings=c('.','NA','N/A','99999999'))
lapply(df.iris,class) # apply function 'class' to each component of df.list
# Now checking character values
df.iris[,5] #Notice multiple spellings of setosa
which(df.iris[,5]=='Setosa' | df.iris[,5]=='setosia') # Which indices are TRUE
df.iris[which(df.iris[,5]=='Setosa' | df.iris[,5]=='setosia'), 5] = 'setosa'
levels(df.iris[,5]) # levels are not updated
df.iris[,5] = factor(df.iris[,5]) # recreate a factor to update unique levels
# Output the cleaned dataset to an external .csv file
write.csv(df.iris, 'EAiris_clean.csv')
read.table('EAiris_1.txt',header=T,sep=','
# Example 2: Creating Summary Table
colnames(df.iris[1])='Sepal.Length'
attach(df.iris)
summary(df.iris)


# Suppose we need for each Species: n, {mean,sd} of sepal length & width, petal length & width
# What is the dimension of the output table?
unique(Species) # 3 rows
out = matrix(0,3,10)

colnames(out) = c('Species','n',paste(rep(c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'),each=2),c('Mean','SD'),sep='.'))
out[,1] = unique(Species) # levels of Species
out[,2] = table(Species) # frequency n
#out[,3] = tapply(Sepal.Length,Species,mean,na.rm=T)
#out[,4] = tapply(Sepal.Length,Species,sd,na.rm=T)
out[,5] = tapply(Sepal.Width,Species,mean,na.rm=T)
out[,6] = tapply(Sepal.Width,Species,sd,na.rm=T)
out[,7] = tapply(Petal.Length,Species,mean,na.rm=T)
out[,8] = tapply(Petal.Length,Species,sd,na.rm=T)
out[,9] = tapply(Petal.Width,Species,mean,na.rm=T)
out[,10] = tapply(Petal.Width,Species,sd,na.rm=T)
out
write.csv(out,'iris_summary.csv')


##  END of 6. Programming Examples
################################

##  END of Section I:   Programming Basics
################################################################

rm(list=ls()) # delete all objects


###########################################################################
## Section II: Advanced Programming and graphing
###########################################################################
####################################################################
##  1. Conditional Execution and Loops

######## 1.1. Conditional Execution with if...else... statement
# Format of if...else...:
##  if (condition) {
##    commands to be executed when condition is TRUE
##  } else {
##   	commands to be executed when condition is FALSE
##	} # the else statement is optional 
## Example:
set.seed(123)
v.1 = runif(1) # generates a random number in (0,1)
if (v.1 >= 0.5) print('Head') else print('Tail')
## do this again
v.2 = runif(1) # generates a random number in (0,1)
flip= if (v.2 >= 0.5) print('Head') else print('Tail')

######## 1.2. 'for()' loops
# 'for' loops are useful if you know exactly how many times to iterate  
for (i in 1:10) {
  # Put commands to run for each value of i inside the '{ }'
  print(i)
}

# Or similarly, conduct a simulation of 10 coin flips.
for (i in 1:10) {
  value=runif(1)
  flip= if (value >= 0.5) ("Head") else ("Tail")
  print(flip)
}

# You can specify a vector of values for i to loop over.    
v.2 = c('blue','red','yellow')
for (i in v.2) {
  print(i)
} 


######## 1.3. 'while()' loops
# A 'while()' loop will continue to iterate until the condition becomes FALSE.
set.seed(3)
v.3 = 0; niter = 0 # intial value of v.3 and iteration counter
while (v.3 <= 0.9) {
  v.3 = runif(1)
  niter = niter+1
  cat('iter:',niter,'   v.3=',v.3,'\n')
}
# Be careful with your condition expression, there must be some command inside
# '{ }' to change the condition for the while loop to escape.

##  END of 1. Conditional Execution and Loops
################

################################
##  2. graphing

# High-level plotting functions create a new plot on the graphics device, 
# possibly with axes, labels, titles and so on. 
           
# Low-level plotting functions add more information to an existing plot, 
# such as extra points, lines and labels. 
           
# The mtcars dataset will be used in examples
attach(mtcars) # now we can directly use component names
summary(mtcars)
help(matcars)
## 2.1. High-level Graphing Functions
# High-level plotting functions create a new plot on the graphics device, 
# possibly with axes, labels, titles and so on. 
           
#### 2.1.1. 'plot()'
plot(wt,mpg) # if inputs are 2 vectors, scatterplot of 1st on X, and 2nd on Y
plot(mpg~wt) # you can also use foumula expression
plot(mpg~factor(cyl)) # if 'y~factor', boxplot for each level
plot(factor(cyl)) # if input is a factor, bar chart of # of obs in each level
plot(cbind(wt,mpg,cyl)) # if input is a matrix, scatterplot of first 2 columns
plot(mtcars) # if input is a data.frame, scatterplot matrix
pairs(cbind(mpg,wt,hp,cyl)) # pairs()
           
#### 2.1.2. other high-level plotting functions
hist(mpg,freq=F) # histogram
boxplot(mpg) # boxplot
boxplot(mpg~cyl,col=rainbow(3)) # you can also use formula
cyl_freq = table(cyl);  pie(cyl_freq) # pie chart for freq. data
barplot(cyl_freq, horiz=T) # barplot for freq. data, horiz=T / F
qqnorm(mpg) # Normal Q-Q plot
curve(.5*(x-3)^2+log(x),from=0,to=4) # draw function curve
           
#### 2,1.3. Modifying Default Plots
# Optional arguments to high-level plotting functions:
plot(mpg~wt) # default plot
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
##  END of 2.1. High-level Graphing Functions
#########
          
           
################
##  2.2. Low-level Graphing Functions
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
text(x[50],y[50],'outlier',col='red',pos=1)
lines(0:4,1+.5*0:4,col='blue',lwd=2)
abline(fit$coefficients[1],fit$coefficients[2],col='red',lwd=2,lty=2)
title(main='Simple linear regression')
legend('bottomright',legend=c('True model','SLR fit'),col=c('blue','red'),lty=1:2,lwd=2)
           
## the lattice package. This package allows for more fine-tuning
## with your graphs.
library(lattice)
?lattice
  
##  END of 2.2. Low-level Graphing Functions
################
           
##  END of Section II:   Advanced Programming and graphing
################################################################




















