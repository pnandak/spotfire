########################################################
## Gov 2001, Spring 2010
## R Orientation Session
## (i.e., Section 0)
## msen (adapted from gov2k notes)
########################################################

## To download R, please go to http://www.r-project.org/
## and follow the directions. You can also access R through
## any of the computers in the HMDC lab and also some of the FAS
## computers (for example, in the science center).

## R works by 
## 1. typing code in the console (next to the red 'greater than'
## sign) 
## 2. OR by typing on a script (like this one) and sending the lines of
## code to the console.

## You can use the R Editor (thge R GUI, pronounced "gooey")
## or you can use from a variety of free R Editors, like Tinn R. 
## Unless you are working on very simple calculations,
## you should always save your code as a script using a ".R" file extension.

## Every line that begins with '#' is ignored by the console.
## So you can add comments to your code without generating syntax errors.
## In fact, your code should be heavily commented so that you can understand
## what you've done if you had to come back to it later.

## Note: You should make your R code neat tidy by taking
## advantage of spacing, hitting return, and the "#" sign.

########################################################
## R help!
########################################################

## get used to using the "help" command; it's your friend.

?mean

## or 

help(mean)

## Remember also the Gov 2001 list! There's a steep learning
## curve but it will pay off! :)

########################################################
## R as a calculator
########################################################

## R can be used to run a wide variety of basic arithmetic commands

2 + 18

50821/6

21^4

34*.01

(900+1)/(45*.09)


## You can store an object for later retrieval


a <- 2 + 18
a


b <- 50821/6
b

a + b


## use the ls() command to see what objects are currently stored 
## in the R environment        


ls()

## You can use the following command to clear your workspace

rm(list = ls())


########################################################
## Making R interact with your computer and the WWW
########################################################

## Your working directory is the "folder" where R loads and saves data

getwd()

## To change the working directory, use "setwd"

setwd("C:/Documents and Settings/My Documents") 

## You can also do this manuually by clicking on the 
## R console, going to "File," and then clicking "Change dir"

## R can load all kinds of data easily

## 1. From computer, saved in a previous R sessions as "*.RData"

load("astrology.RData")
	# why is most of this information redundant?
ls()
astrology


## 2. From my computer, saved as a text document "*.txt"

lalonde <- read.table("lalonde.txt")
lalonde

## 3. From someone else's website, saved as a text document

nes <- read.table("http://www.people.fas.harvard.edu/~blackwel/nes.dat")
nes

########################################################
## Getting a feel for the data
########################################################

## There are a number of really useful commands that will
## quickly give you a feel for any data:

summary(nes) ## summary statistics

names(nes) ## column (generally variable) names

dim(nes)   ## the dimensions of the dataset

head(nes)  ## first few observations

########################################################
## R packages
########################################################

## People write software for R all the time.
## These are called "packages" and the one we'll use most often is 
## Zelig (Imai et al). To install a packge:

install.packages("Zelig")
	## this must be done only once 

library(Zelig)
     ## this must be done every time you use the foreign library

## Other useful packages are
## foreign -- permits you to load data formatted for other software
## xtables -- helps you write up tables in LaTeX code
## car -- has many datasets and linear regresison functions
## more packages are at http://cran.r-project.org/web/packages/

## As you get more familiar with R, you will use more packages!
## A great website to learn about cutting-edge stuff that
## people are writing is http://www.r-bloggers.com/

########################################################
## Working with objects
########################################################

## R can store objects, and you can call these up later.
## As noted above objects are defined using "<-" 

scalar1 <- 2
scalar1

R <- "fun"
R

truth.vec <- TRUE
truth.vec

## Note: Don't name your objects things like "mean"
## or "sum" or "7" since those are things that R already
## has pre-packaged.

# You can make longer things, like vectors,
# which we create with c(), for concatenate 

vec1 <- c(2,2,7,-1,4)

R <- c("Gov2001","is","fun")

## R performs math on numeric objects

vec2 <- c(2,5,1,3,2)

vec1 + vec2

vec1 - vec2

3*vec2

vec2*3

## Tricks for creating vectors

vec3 <- 1:5
vec3

vec3 <- c(1:5, 7, 11)
vec3

vec4 <- c(vec1, vec2)
vec4

## Subsetting (use [] to pick out elements of an object)
## recall: vec1 is c(2,2,7,-1,4); vec4 is c(2,2,7,-1,4, 2,5,1,3,2)

vec1[1]

vec1[6]

vec1[-1]
	
vec4[c(5,7)]

vec4[c(5:7)]

## You can also replace a particular element of a vector

vec1[3] <- 6

########################################################
## Basic R functions
########################################################

# R has many preprogrammed functions that manipulate objects.
# To use a function, you type the function name followed by the
# arguments in parentheses


a <- c(1,3,6,5,9,22)


b <- c(4,5,6,5,2,1)


sum(a) ## sum of all elements of a

sum(b)

sum(a,b)

max(a) ## maximum element in a

min(a) ## minimum element in a

mean(a) ## mean of a

length(a) ## number of elements in a,
	    ## useful for when you need to calculate the sample size, n

sort(a) ## sorts a from lowest to highest

## you can store the output of a function, as a new object.

output <- length(a)

## These functions are useful for creating vectors

seq(from = 0, to = 5, by = .5) 
	## creates a sequence of numbers

rep(10, 27) 
	## repeats the number "10" 27 times.

## to learn the arguments for a particular 
## function, use the help commands:

?sort

sort(a)
sort(a, decreasing = TRUE)

########################################################
## Matrixes in R
########################################################

## the matrix() function in R is one way to create a matrix

matrix(data = 1:12, nrow = 3, ncol = 4)

matrix(data = 1:12, nrow = 2, ncol = 6)

matrix(data = 1:12, nrow = 2, ncol = 6, byrow = TRUE)

## You can also create a matrix from vectors
## using the cbind and rbind commands

my.vec <- c(4:8)
my.vec2 <- c(5:9)
my.vec3 <- c(1:5)


cbind(my.vec, my.vec2, my.vec3)
rbind(my.vec, my.vec2, my.vec3)

## Let's store the last matrix

mat <- rbind(my.vec, my.vec2, my.vec3)

## You can give your matrix colums and rows names

rownames(mat)

colnames(mat) <- c("col1","col2","col3","col4","col5")

## We can extract particular elements of a matrix just like
## we could from a vector, though this time we have to specify
## two dimensions, the row and the column

mat[1,1]

mat[,1]

mat[1,]

#################################################################
## Writing Functions
#################################################################

## You can write your own functions in R using the "function" command
## Functions can be really complicated or really simple!

## here's the general idea:

## my.function <- function(x,y,z){
##					## tells R that this is a function
##   	out <- crazy function stuff
##					## the meat of the function (you usually "tab" this)
##  return(out)
##					## returns the output of the function
## }
						## closes the function up


## This function will take three numbers as arguments;  it will add
## the first two and divide the sum by the third

my.function <- function(x,y,z){
  out <- (x + y)/z
  return(out)
}


## Now we call our function with 

my.function(x = 5, y = 10, z = 3)

my.function(5, 10, 3)

## By the time you are done with this course, you will be writing
## more complicated functions. But the structure is the same.

#################################################################
## Writing For-Loops
#################################################################

## A for loop is a way of repeating a process a vast 
## number of times.  For each iteration of the loop
## R keeps an index number stored which can be handy.

## here is a simple example where we first define a 
## storage matrix to hold the output of our for loop

holder <- matrix(data = NA,  nrow = 100, ncol = 1)

for(i in 1:100){
  draw <- rnorm(1, mean = 0, sd = 1) 
		## here we are drawing one observation from a normal distribution
		## with mean zero and standard dev 1
  holder[i] <- draw
}

## and then to get a visual representation of this, we can
## use something like the hist command:

hist(holder)

## let's write a more flexible for loop that will let you 
## change the number if simulations quickly

sims <- 10000

holder <- matrix(data = NA,  nrow = sims, ncol = 1)

for(i in 1:sims){
  draw <- rnorm(1, mean = 0, sd = 1) 
  holder[i] <- draw
}

hist(holder)

## you can also use the plot command (using the density function)

plot(density(holder))

#################################################################
## More resources
#################################################################

## There are loads more resources for R:
## http://cran.r-project.org/doc/contrib/usingR.pdf (the Maindoland intro, very good)
## http://www.r-bloggers.com/ (the R bloggers website -- awesome applications!)
## the gov 2001 list!!!
