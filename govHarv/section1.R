############################################

#   Goverment 1001
#   Section 1 - Getting Started with R
#   February 15, 2007
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################


#   Goals for today:

#   1) Installing R on your own computer

#   2) Utility functions

#   3) Creating and working with objects

#   4) Descriptive statistics

#   5) Graphs and plots



#   1) Installing R

#   R is an open-source environment for statistical computing.  It has 
#   some characteristics of a "normal" statistical package and some 
#   characteristics of a programming language.  The learning curve for 
#   R may be a bit steeper than for some more traditional software, 
#   but it is much more powerful (and it is free!).

#   You can download and install R on you own computer.  The main R website is 

#   www.r-project.org

#   This has links to documentation, mirrors for downloading R, etc.  You
#   will need to pick a mirror in order download the program.  I usually
#   use 

#   cran.wustl.edu

#   because it is easy to remember.  You want to download the precompiled 
#   binary that is appropriate for your system.  Download the file and
#   proceed as usual when installing software.

#  When you have finished installation, start up R.  You will get a
#  welcome message, and then a command prompt:

#  >

#  You can enter commands at the propmt in order to make R do things
#  for you.


#  2) Utility functions

#  The R language is based around objects and functions that operate on
#  those objects.  You will be writing your own functions later on, but 
#  for now we will be using functions that are built into the system.

#  If you know the name of the function that you want to use, you
#  can get the documentation on it using the help() function:

help(mean)
?median

#  If you don't know the name of the function that you want, you can try 
#  to search for it using help.search(), which will bring up a list of possible
#  functions:

help.search("mean")

#  Note that you have to put the word you are searching for in quotes
#  when you use help.search, but not when you use help.

#  The functionality of R can be extended by adding packages that 
#  contain additional functions.  You load packages into your R session
#  by using the library() command as follows:

library(eiPack)

#  If you have never downloaded that particular package, you will get an 
#  error (as we just did).  You can download packages in two ways, either
#  by using the Packages>>Install package(s) menu at the top, or by using the 
#  install.packages() function at the command prompt:

install.packages("eiPack")

#  After you install the package, you still have to load it if you want
#  to use the functions in that package.

library(eiPack)

#  You can see what functions/datasets are in a package by using the 
#  following syntax:

library(help=eiPack)

#  Some other useful utility functions:

ls()        # lists the objects in your workspace
getwd()     # lists the path to your working directory
q()		# quits your R session
ls

#  3) Creating and working with objects

#  Everything in R is based on objects that contain data.  The basic
#  way to create an object is to assign some value (or values) to 
#  an object using the assignment operator:

aa <- 1
aa

#  You can also assign the output of a function to an object.  The 
#  most important function for the purpose is c(), which creates a 
#  of numbers or strings:

aa <- c(1,2,3,4,5,6,7,8,9,10)
bb <- c("Dem", "Dem", "Dem", "Dem", "Rep", "Rep", "Dem", "Rep", "Dem", "Rep")
aa
bb

#  Vectors are saved as different types, including numeric, character, logical, etc.
#  For a given object, you can check to see what type it is using class():

class(aa)
class(bb)

ee <- factor(bb)
class(ee)
ee

#  If you want to pull out an individual element of a vector, you use the 
#  indexing operator, which is a number (or numbers) surrounded by brackets:

aa[2]
bb[3:5]
bb[c(1,3,5)]

#  You can also assign the output of other functions to objects.  

rep(15, 0)
dd <-rep(0, times=15)
dd

sample(1:100, 10)
cc <- sample(1:100, 10)
cc

#  You can put objects of the same length (but different classes) together
#  into a single object known as a data frame.  This is the standard way to 
#  represent datasets, since you can include qualitative and quantitative 
#  variables.

mydata <- data.frame(aa, bb, cc)
mydata

#  You can change the row and column names of a data frame using
#  rownames() and colnames()

rownames(mydata) <- c("Obs1","Obs2","Obs3","Obs4","Obs5","Obs6",
                       "Obs7","Obs8","Obs9","Obs10")
colnames(mydata) <- c("Precinct", "Party", "Voters")
mydata

#  These functions also work in reverse, pulling out the row and column
#  names from a data frame:

rownames(mydata)
colnames(mydata)

#  If the data that you want to work with already exists in a data frame
#  in R or in one of the libraries that you have loaded, you can access it 
#  using the data() function:

data(senc)

#  In order to access the data in a data frame, you can either identify it
#  by row or column number:

senc[,4]
senc[3,]

#  Or you can pull out a particular variable (a column in the data frame) by 
#  using the $ operator:

colnames(senc)
senc$white

#  Or, you can attach the data frame to your workspace and operate on the 
#  variables directly:

attach(senc)
white

#    4) Descriptive statistics

#    Not surprisingly, there are functions in R that calculate most of the 
#    descriptive statistics that we have talked about in class.

mean(white)
mean(white, trim=.1)

median(white)

sd(white)

quantile(white)
quantile(white, prob = c(.025, .975))

#    There is also a function that will summarize all of the variables in a
#    data frame:

summary(senc)


#   5) Graphs and plots

#   One of the strengths of the R environment is the ability to 
#   make high quality graphs and plots.  We have seen several of them
#   in class already.

#    Histograms:

hist(whdem) 

#    Boxplots:

boxplot(whdem)
boxplot(senc[,4:6])

#    Barplots:

barplot(total)


#  You can control a lot of the features of the plot by adding arguments
#  to the main plot function; not all arguments apply to each
#  plotting function. These are some of the arguments:

#  main: adds a title to the plot
#  xlab: adds a label for the x-axis
#  ylab: adds a label for the y-axis
#  col: color of the plotted object
#  xlim, ylim: limits of the plot

#  Some examples:

hist(whdem, main="Number of White Democrats by Precinct")
hist(whdem, 
     main="Number of White Democrats by Precinct",
     xlab="No. of Registered Voters",
     col="skyblue",
     xlim=c(0, 3000))

#  You can also add more data to the chart using functions like
#  lines(), segments(), points(), legend(), etc.

points(cbind(whdem, 0), pch=3)

