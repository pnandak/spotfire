###################################################
#Loren Collingwood                                #
#2/11/2011, week 6                                #
#POLS 501 Grad Stats                              #
#Intro to R, session 1                            #
###################################################


##########################################
#               PACKAGES                 #
##########################################
#R is based on packages
#Some examples of all the exciting things one can do.
#Note -- these are just examples. The point is the capabilities are large
#You must load each package and then you can access that package's functions.
library(Zelig) # GLM package written by Gary King et al 
library(survival)#survival analysis functions 
library(RCurl)# used for web-scraping 
library(Rtexttools)# automated Text coding. install via Zip File
library(xtable) #Table interface with LaTex

#R is a pProgramming Language as well as a statistics language.

##########################################
#            Basic Operations            #
##########################################

# arithmetic, interacting with the interpreter

2+3
2-3
2*3
2/3
2^3  #exponentiation
2**3 #exponentiation

# precedence of operators

4^2-3*2
(4^2) - (3*2) # use parentheses group, spaces to clarify

##########################################
#    functions, arguments to functions   #
##########################################

log(100) #log is the function, 100 is the argument
log(100, base=10) # now 100 and base are the arguments
log(100, b=10) # can abbreviate

"+"(2, 3) # even operators are functions
`+`(2, 3) # "backtick" preferred in this context

##########################################
#              OBTAINING HELP            #
##########################################
#Various ways to access help files. Help files are extremely crucial and less consistent in R.

help(log)
?log
log(100, 10)

apropos("log")
help.search("log")
#Good for finding vignettes (what the package does and how to use it)
RSiteSearch("loglinear", "functions")

##########################################
#   Vectors, Matrices, and Data frames   #
#                                        #
#and in the process                      #
#objects and assignments                 #
##########################################

#LC Emphasis: VECTORS ARE FUNDAMENTAL TO R. 

#vector creating via concatenation

c(1,2,3,4)  # combine

1:4     # sequence operator
4:1
-1:2    # note precedence
seq(1,4)
seq(2, 8, by=2)
seq(0, 1, by=.1)
seq(0, 1, length=11)

seq_along(seq(2, 8, by=2)) # to get indices for vector of unknown length

#Assignment and object creation. Everything is an object.
a <- c(1,2,3,4)
dog <- c(1,2,3,4) #numeric vector
Ilovestatisticssomuch <- c("a","b","c","d") #character vector

# vectorized arithmetic
c(1,2,3,4)/2
c(1,2,3,4)/c(4,3,2,1)

c(1,2,3,4) + c(4,3)  # "recycling" rule
c(1,2,3,4) + c(4,3,2)


#Matrix making. Some functions do not take data frames as arguments so you'll need to convert to a matrix
#Two ways to make a matrix

mymat <- 1:12
dim(mymat) <- c(3,4)  # counts by rows columns 
mymat

#The prefered way to make a matrix
mymat2 <- matrix(1:12, nrow=3, byrow=T)
mymat2

#COLUMN AND ROW BINDING
#LC NOTE: THIS IS ESSENTIAL

#Making Matrices and Dataframesout of Vectors
vec1 <- c(1,2,3,4,5,6)
vec2 <- c(2,3,4,5,6,7)
vec3 <- c(8,8,9,3,4,1)

newmat <- cbind(vec1,vec2,vec3) #cbind function combines vectors by columns
class(newmat) #matrix class, contains certain properties
newmat2 <-as.data.frame(newmat) #We can change a matrix into a dataframe. Some functions, 
#like "lm" (equivalent to reg in Stata) only take a data frame, not matrix.
class(newmat2) # data frame class, contains certain properties

#Sometimes we want to export our data out to a type-setting program. Here we can do that with Latex.
library(xtable) 
xtable(newmat2)# or
mydat <- xtable(newmat2)
mydat

################################################
#             Insheeting Datasets              #
################################################

#So that's some of the basics, now let's move into 
#opening a real-life dataset. There are several
#ways to do this

#Load Package. This brings in the Stata .dta dataset. Can also use to bring in SPSS .sav files
library(foreign)

#Change to working directory. use "setwd" function to change to working directory
setwd("C:\\Users\\Loren\\Documents\\UW courses\\POLS501\\week6")
#"Open" up a dataset
wapoll <- read.dta("wapoll_oct2010_0211.dta", convert.factors=FALSE)
#Do some summaries of the data
summary(wapoll) #lists min, median, max, missing data for each variable
head(wapoll) #first 5 rows
tail(wapoll) #last 5 rows

#Do some Tabs, not as "nice" or straight forward as in Stata
require(gmodels) # may need to install this
#install.packages("gmodels") #installs from the "Comprehensive R Archive Network (CRAN)" repository
CrossTable(wapoll$white)
CrossTable(wapoll$ideo_r,wapoll$white)

#Always, the first thing you do when you open up a dataset is to run a regression.
#We save the output of the regression in an object called first_reg, we can access various
#outpus from this new object.
#We use summary function on this object to examine the results. Compare to STata.

#Run the Regression using lm for linear model, command.
summary(first_reg <- lm(ideo_r ~ income_r + female + college + black + married, data=wapoll))

#What can we extract from this object.
names(first_reg) #exam things you can extract for table making and post-estimation statistics

#############################################
#Example Regression, user written functions #
#############################################

#Let's examine, for example, R's flexibility. What is our expected ideology of someone who is
#middle income (4), female (1), college edumacated (1), not black (0), and happily married (1).

#create a vector of the values we are going to use to represent these "assignments" from above.
values <- c(income_r=4,female=1, college=1, black=0,married=1)

#Here we create a function called reg_values which allows us to determine how someone with the above
#characteristics will be scored on our ideology scale, based on the regression object "first_reg" above.

reg_values <- function(reg_values) { #vector
    ideo_r <- 3.9 + (-.04*reg_values[1]) + (-.415*reg_values[2]) + (-.35*reg_values[3]) + 
    (-.35*reg_values[4]) + (.98*reg_values[5])
    return(ideo_r)
}
#Now we call this function by passing it the vector of our values
reg_values(reg_values=values)

#But wait, we can improve this function by taking the coefficient values extracted from first_reg
#Below 1 is intercept, 2 is income, etc.
reg_values_improve <- function(reg_values,coefs) { #vector of values, coefficient vector
    ideo_r <- coefs[1] + (coefs[2]*reg_values[1]) + (coefs[3]*reg_values[2]) + (coefs[4]*reg_values[3]) + 
    (coefs[5]*reg_values[4]) + (coefs[6]*reg_values[5])
    return(ideo_r)
}
#pass the values vector and the coefficient vector
reg_values_improve(reg_values=values,coefs=first_reg$coef)
