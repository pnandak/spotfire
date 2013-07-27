##########################################################################
## Maya Sen
## Gov 2000 
## Section 1
## Sept 10, 2009

## Credit to Jenn Larson, Matt Blackwell and other Gov 2000 TFs for this code.

##########################################################################
## about homeworks.
##########################################################################

## 1. please make it so that we can run your entire R file from beginning
## to end without errors. 

## 2. please make it so that R prints out your
## answers so we don't have to go around fishing for them in your code.

	## example
	
	x <- 14+7

	x

## 3. if your R file crashes, does not print out answers, or gives answers
## that contradict your write up, that's bad!

## 4. assume that we have the data saved in our working directory.
## don't change the working directory in the R file you submit 
## (because we don't ahve the same folders you do!)

##########################################################################
## Review of last time.
##########################################################################

## Do you have R loaded? if not, go to
## http://www.r-project.org/

## Now...
## R is a basic calculator:

2+2

8^2

(100+5)/5

log(45)

exp(.69)

## R can store objects and you can recall them later

p <- .05

p

t <- c(1,0,0,0,1,1,0) ## a vector (c = "concatinate")

t

z <- (100+5)/5

z

## R has many useful preprogrammed functions that manipulate objects

## Suppose you have a complicated vector:
a <- c(1,1,1,3,4,5,6,6,6,6,6,6,10,11,12,12,12,
	5,6,2,5,7,3,5,9,0,1,3,5,7,0,12,12,23,34,21,234)

summary(a)

sum(a)

mean(a)

median(a)

length(a) ## length is the number of elements in the vector

sort(a)

?sort
	## confused? just ask for help!

sort(a, decreasing = TRUE) 

var(a)

sd(a)

max(a)

min(a)

## You can also subset vectors pretty easily

a[5]

a[length(a)]

a[4:6]

## ...onto new material

########################################################################
## Matrices
########################################################################

## A matrix is the simplest kind of two-dimensional structure.
## We'll use the "matrix" function

## to see the arguments matrix() takes, use

args(matrix)
## or
?matrix

## Let's make some matrices:

matrix(data = 1:12, nrow = 3, ncol = 4)

matrix(data = 1:12, nrow = 2, ncol = 6)

matrix(data = 1:12, nrow = 2, ncol = 6, byrow = TRUE)

## You can also make an empty matrix to fill in later;
## this is useful for writing functions later on in the term

holder <- matrix(data = NA, nrow = 2, ncol = 3)

holder

## and give the colums and rows names:
colnames(holder) <- c("left", "middle", "right")
rownames(holder) <- c("up", "down")

holder

## You can also create a matrix by combining vectors together

my.vec <- c(4:8)
my.vec2 <- c(5:9)
my.vec3 <- c(1:5)

cbind(my.vec, my.vec2, my.vec3)
	## column binding

rbind(my.vec, my.vec2, my.vec3)
	## row binding

## Let's store the last matrix

example <- rbind(my.vec, my.vec2, my.vec3)

## We can extract particular elements of a matrix just like
## we could from a vector, though this time we have to specify
## two dimensions, the row and the column

example[1,1]

example[c(1,2), 1]

example[,1]
	## just the column

example[1,]
	## just the row

##########################################################################
## logical statements (or logical operators)
##########################################################################

x <- 5

x == 5  ## here we're asking R, "does x equal 5?"

## a few more to get some intuition:

TRUE == TRUE
TRUE == FALSE
5 == 5.0
"hello" == "hello!"

## Let's move on to vectors. 

x <- c(1,2,3,3,3,4,5,6,7,8,9)

## using the same "==" operator as before

x == 1
x == 2
x == 3

## now, try "!="

x != 1 ## here, we are asking R, "does x NOT equal 1"
x != 2
x != 3

## and "<=" (less than or equal to)
## and ">=" (less than or equal to)

x <= 4 ## is x less than 4?
x >= 4 ## is x greater than 4?

## to make it slightly clearer, we'll print them together, using
## the drata.frame command, which pulls collected vectors
## into one data.frame

## note: a data frame, a matrix-like structure whose columns may be 
## of differing types (characters, numbers, etc.)

data.frame(x, x == 1)
data.frame(x, x == 3)
data.frame(x, x == 2)

## We can also use these logical statement to pull out
## parts of the vector that we want.

x[x==1]

## These become hugely important in what are called "if" statements

x <- 5

if (x == 5) {
  print("gov2000 rocks!")
} else {
  print("this isn't giving me the right answer!")
}

x <- 6

if (x == 5) {
  print("gov2000 rocks!")
} else {
  print("this isn't giving me the right answer!")
}

## and this leads us into...

#################################################################
## Writing Functions
#################################################################

## You can write your own functions in R using the "function" command
## Functions can be really complicated or really simple!

## This function will take three numbers as arguments;  it will add
## the first two and divide the sum by the third

my.function <- function(x,y,z){
  out <- (x + y)/z
  return(out)
}

## Now we call our function with 

my.function(x = 5, y = 10, z = 3)

my.function(5, 10, 3)

## Now let's see a function that returns the smallest element in a vector
## using the R commands we've seen so far

small <- function(vec){
  sorted <- sort(vec)
  out <- sorted[1]
  return(out)
}

## Let's test this

small(a)

## how do we write a function to return a vector's maximum?

large <- function(vec){
  sorted <- sort(vec, decreasing = TRUE)
  out <- sorted[1]
  return(out)
}

large(a)

#################################################################
## A real data example
#################################################################

ls()
rm(list = ls())
 ## this command cleans everything in your R workspace

load("cambridge.RData") ## from section 1 on the course webpage
ls()

class(loans)   ## what kind of data this is
##(Note: a dataframe is slightly different than a matrix, since
## it allows you to also include character strings.)

head(loans)    ## show the top of the data 
nrow(loans)    ## get the number of rows of nes
ncol(loans)    ## get the number of columns of nes              
names(loans)   ## get the variable names of nes
summary(loans) ## summarize the variables                               

## nes is a dataframe, which is how R stores datasets.
## we can access the individual variables by using the dollar 
## sign:

loans$income
loans$amount

## but we can still use our old tricks--
## Remember how to grab certain rows:

loans[c(105,216,307,415,430),]

## or a certain individual observation

loans[452,]

## And certain columns:

loans[, c(1,2,3)]
loans[,c("hisp","income")]

## we can also use many functions to analyze the data:

summary(loans$amount)
max(loans$amount)
min(loans$amount)

## Now, we probably will want to subset the dataset by values of
## certain variables. Perhaps we would like two datasets: one
## for males and one for females.

men <- loans[loans$sex == "Male",]
women <- loans[loans$sex == "Female",]  ## or nes$gender != "Male"

mean(men$income)
mean(women$income)

mean(men$rate)
mean(women$rate)

## or one for blacks (race = 3) and non-blacks (race !=3)

blacks <- loans[loans$race == 3,]
nonblacks <- loans[loans$race != 3,]

mean(blacks$income)
mean(nonblacks$income)

mean(blacks$rate)
mean(nonblacks$rate)

## we can also construct complex logical statements with 
## "and" (&) and "or" (|)

## rich men
loans[(loans$income >= 150) & (loans$sex == "Male"),]

## rich men who are black
loans[(loans$income >= 150) & (loans$sex == "Male") & loans$race == 3,]

## QUESTION: Can we write code to compare the interest
## rates for blacks versus non black in the
## rich men demographic?

## yes!

mean(loans$rate[(loans$income >= 150) & (loans$sex == "Male") & loans$race == 3])
	## blacks
mean(loans$rate[(loans$income >= 150) & (loans$sex == "Male") & loans$race != 3])
	## non-blacks 

## =======> what conclusions can you draw?

#################################################################
## creating and saving figures
#################################################################

## R makes creating figures pretty easy;
## the standard subsetting rules apply

hist(loans$rate)
	## basic

## dressing it up:

hist(loans$rate, col = "gold", xlim = c(3,10),
	xlab = "interest rate", ylab = "# applicants", 
	main = "histogram, cambridge mortgage rates 2006")

## (see http://research.stowers-institute.org/efg/R/Color/Chart/ for more colors!)

## and then to add a vertical line at the mean:

abline(v = mean(loans$rate), col = "blue")

## To save the file:

pdf(file= "Histogram1.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
hist(loans$rate, col = "gold", xlim = c(3,10),
	xlab = "interest rate", ylab = "# applicants", 
	main = "histogram, cambridge mortgage rates 2006")
dev.off()

## We can also do different kinds of figures:

plot(density(loans$rate), xlab = "interest rate", ylab = "density",
	main = "density plot, cambridge mortgage rates 2006")

## we'll use these more extensively later on in the course!

#################################################################
## creating tables -- if we have time!
#################################################################

## The two main functions for creating Latex tables in R are xtable() 
## and latex().  Neither function is in the base package, so first you need to 
## install one of the relevant libraries.

## To use xtable(),

install.packages("xtable")
library(xtable)

## One of the biggest sources of error comes from installing a package
## but then forgetting to load the library.

## After you've installed a package, it will be on your computer
## and next time you can skip the install step and simply load the library

## The easiest way to use a function like xtable() is to collect
## everything you want in your table into a matrix.  
## matrix(), as.matrix(), cbind() and rbind() are useful here.

## Let's make an example matrix:

example <- matrix(data = 1:12, nrow = 4)
example

## I can add row and column names to my matrix with

rownames(example) <- c("row 1", "the second row", "numero tres", "Mr. 4")

colnames(example) <- c("a variable", "some means", "Something Else")

## Now we can generate the code to write the matrix as a latex table

xtable(example)

## Voila

## As you may have expected, we can play with the formatting

xtable(example, digits = c(2,2,2,2))


xtable(example, digits = c(2,2,2,2), caption = "Conclusive Evidence")


xtable(example, digits = c(2,2,2,2), caption = "Conclusive Evidence",
  align = c("l","c","c","c"))


xtable(example, digits = c(2,2,2,2), caption = "Conclusive Evidence",
  align = c("l||","c","c","c"))

## xtable() produces straightforward Latex tables.  If you are interested
## in a more complicated table that has more formatting options, 
## check out latex() in the Design library

install.packages("Design")
library(Design)

## latex() produces output in a separate file unless you 
## specify file = "" (which I recommend)

latex(example, file = "")
