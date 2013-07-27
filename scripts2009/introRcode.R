##### assignments
#a few cautions: don't name objects the names of R commands, as the object will then replace the command for your current 
#R-WinEdt helps to prevent you from doing so by italicizing identifiable R commands. 
#R is case sensitive


x <- 12
y <- 8
z <- 33

x*z
sqrt(x+y+5)
z/y

#shows objects stored in the workspace
ls()
#remove individual objects
rm(x, y, z)
#or clear the entire workspace
rm(list=ls())



##### some mathematical operations 
#log computes natural logarithms. log10 computes base 10 logarithms. can specify base in log(x, base)
log(10)
log(10, base=10) 
log10(10)
#exp computes the exponential function - "e to the power of"
exp(2.302585)
#R has functions for mean, median, mode, max, min, etc.
e <- rnorm(50, mean=32, sd=4) #randomly generating a normally distributed variable
mean(e)
sd(e)
max(e)
min(e)
#trigonometric functions: sin, cos, tan, etc.
#set operations
x<-1:8
y<-5:12
union(x, y)
intersect(x, y)
setdiff(x, y) #assymetric difference
setdiff(y, x)
setequal(x, y)
is.element(x,y)



##### vectors, lists, and matrices 
#c() concatenates, which is simply a fancy term for linking or combining the numbers inside the () into a vector or list
#creating a vector named g consisting of six numbers
g <- c(11.8, 4, 7, 1.3, 13, 8.5)  
c(11.8, 4, 7, 1.3, 13, 8.5) -> g #assignments can be made in either direction, as long as the arrow points from the expression to the name
h <- 5:11 # creates a vector from 5 to 11
length(h) #vectors have length but no dimension

#seq() generates sequences
seq(5, 11) #creates a sequence from 5 to 11, so same as 5:11 - seq(from, to)
seq(1, 4, by=.25) #argument "by" specifies increments
seq(1, 4, length=13) #generates same sequence as above, by be specifying length

#rep () replicates elements of vectors and lists
rep(3:6, times=3) #creates a vector from 3 to 6 and replicates 3 times
rep(3:6, len=12) #creates the same vector by specifying length1
rep(3:6, len=10) #two and a half replications
rep(3:6, each=3) #creates a vector where each element from 3 to 6 is replicated 3 times

o <- sample(1:100, 8) #sample 8 items from the sequence from 1 to 100. default is without replacement. need argument replace=TRUE if want with replacement.
p <- sample(1:100, 8)

#can sort items in a list or vector
sort(o)

#can use usual arithmetic and mathematical functions with vectors
o + p
o * p 

#logical vectors later
#character vectors - use the same c() with components separated by quotation marks
new.england <- c("CT", "ME", "MA", "NH", "RI", "VT")

#indexing a vector
new.england[2]
new.england[2:5]

##### matrices
#can create a matrix by combining vectors
op <- rbind(o,p) #binds as rows
op2 <- cbind(o,p) #binds as columns
dim(op) #matrices have both length and dimension
length(op)
dim(op2)
length(op2)
t(op) #transpose of a matrix

m <- matrix(1:12, nrow=3, ncol=4, byrow=TRUE) #generates a 3 by 4 matrix, filled by rows (default is filling by columns)
n <- matrix(1:12, ncol=4) 
s <- t(n)
m[2,3] #gets value in 2nd row, 3rd column in matrix m
m[,2:4] #refers to all rows and 2nd through 4th column
m[1:2,] #refers to 1st and 2nd row and all columns

m + n #adding matrices
m %*% s #multiplying matrices requires percent signs on either side of the multiplication sign, thus - %*%
diag(m) #get diagonal of matrix



##### control-flow and simple user functions in R
#an incredibly simple example to illustrate conditionals using 'if' and 'else' in R
abs.1 <- function(x) if (x<0) -x else x #creates a function that produces the absolute value of a number (borrowed from Fox's book)
#so if x is less than 0, then negate x, otherwise leave x
#if expression 1 (which is the condition), then expression 2 (consequence), else expression 3 (alternative)
abs.1(4)
abs.1(-4)

#another simple example to illustrate loops using 'for' and a bit more on functions (also borrowed from John Fox)
#the following function calculates the factorial of x
#note the use of braces {}. expressions or commands can be grouped together in {}, as they often are when writing functions. 
#the value of the group is the result of the last command.
fact.1 <- function(x){
    if (x <= 1) return(1)
    f <- 1 
    for (i in 1:x) f <- f * i
    f
    }
fact.1(4)

#the generic form for writing a user function is as follows:
#function( argument list ) expressions
#so above, the argument list was simply x. but the argument list for a more complex function can have several components.
#the expressions are what actually defines the function, and are the commands between the braces {}
#we'll do some more complex function writing as the quarter progresses



##### basic probability functions
#four prefixes: (1) d for density, (2) p for distribution or cumulative probability density, (3) q for quantile, and (4) r for simulating from a distribution (random generation)
##Normal Distribution - norm (can specify mean and sd, default is standardized normal)
source("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/intro R stuff/normplots.R") #source() loads and runs program. 
#can specify directory, or automatically loads from current R directory.
#also File --> Source R code . . . 
# this is the program in the above file
#if you want to try sourcing this file, copy the code below until the next '##' and paste it into a blank winedt document. save it as an .R file and 
#change the above file path in the source() command accordingly.
par(mfcol=c(3,1))
x<-seq(-5,5,.01)
Px<-dnorm(x) #density function 
plot(Px ~x,type="l",lty=1,col=1,lwd=3,
main="PDF for Normal Models") #can specify a line plot. point plot is the default.
x2<-seq(-5,5,.01)
Px2<-pnorm(x2) #probability function 
plot(Px2 ~x2,type="l",lty=1,col=1,lwd=3,
main="CDF for Normal Models")
x3<-seq(0,1,.001)
Px3<-qnorm(x3,mean=0, sd=1) #quantile function
plot(Px3 ~x3,type="l",lty=1,col=1,lwd=3,
main="quantile for Normal Models")
##
par(mfcol=c(2,1))
x4<-rnorm(1000, mean=0, sd=1) #random generation
plot(x4,type="l",lty=1,col=1,lwd=3)
hist(x4)

pnorm(2, mean=0, sd=1)
dnorm(2, mean=0, sd=1)
qnorm(.9, mean=0, sd=1)

#Poisson Distribution - pois, specify lambda
par(mfcol=c(2,1))
x2<-seq(0, 10, 1)
Px2<-dpois(x2,3) #density
plot(x2,Px2,type="l",lty=1,col='skyblue',lwd=3,
main="PDF for Poisson Model")
Px3<-ppois(x2,3) #probability
plot(x2,Px3,type="l",lty=1,col='tomato1',lwd=3,
main="CDF for Poisson Model")

par(mfcol=c(2,2))
p<-1
po <- rpois(1000, p)
hist(po)

#Binomial Distribution - binom, specify arguments size (number of trials) and prob (probability of success on each trial)
n<-20
p<-.8
bi1<-rbinom(1000, size=n, prob=p)
hist(bi1, xlim=c(0,n))
 
#other possibilities: beta, cauchy, chisq, exp, f, gamma, geom, hyper, lnorm, logis, nbinom, t, unif, weibull, wilcox
#each distribution has its own additional arguments, check out R documentation for them
#can use the same 4 prefixes on all probability distributions


##### reading in data

#reading in as a comma separated value or .csv file
#note options: must know if dataset has commas or tabs separating each element and whether or not variable names are in the first line of data
#gaydata <- read.csv("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/intro R stuff/gaydata2-1.csv", sep=",", header=TRUE)
#note direction of the slash marks. if you type a file name with the usual direction, namely "\", R will be unhappy. 

#as a text file
#gaydata <- read.table("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/intro R stuff/gaydata2.txt", sep="", header=TRUE)

#reading in data from other statistical programs (SPSS, Stata, SAS) requires using the foreign library
library(foreign) #can also be accomplished by Packages --> load package --> foreign
#gaydata <- read.dta("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/intro R stuff/gaydata2-1.dta")

#if you want to try the above ways of reading in data, you'll first have to download the dataset to some folder on your computer from 
#http://students.washington.edu/fishes/CSSS536/. then change the file path (remembering to watch the direction of "/"!) 

#finally, can read data straight from the web
gaydata <- read.table(file="http://students.washington.edu/fishes/CSSS536/gaydata2-1.csv", header=TRUE, sep=",")

gaydata #will print out all of the data

#see a summary of the data
summary(gaydata) 

#editing data (though i'd personally recommend doing so in other programs)
gaydata.edit <- fix(gaydata)
gaydata.edit <- edit(gaydata)

names(gaydata) #will give you a list of the variables
attributes(gaydata) #more information
                    #note class of gaydata as data.frame. all objects in R have a designated class - numeric, logical, character, etc.

ls()
objects()

##note: unfortunately, depending upon the version of the dataset you're working with, variable names may be slightly different. this is just a result of stat transferring 
#the dataset from one program to another - each has different rules for variable names. those from the .csv file are in CAPS and contain underscores. 
#those from the .dta file are in lower case and contain periods. so, the variable names in the below examples will need to be changed accordingly. 

#foo$bar (see http://en.wikipedia.org/wiki/Foo for more information as to whatever that means)
LEGISLCT #can't access individual variables within the dataframe yet
gaydata$STATE #need to use $ in order to reference a specific variable in a dataframe
gaydata$LEGISLCT

attach(gaydata) #attaches database to the R search path and makes objects within dataframe more accessible. once attach, no longer need the list name.
state #see? no longer need silly $. much easier.
legislct

detach(gaydata) #detaches dataset from R search path
attach(gaydata)

#if you're dealing with the .csv file and want to avoid having to continually type in all CAPS, or simply want more intuitive names for variables in a dataframe
#you can assign another object name to the variables.
#note that this does not get rid of the original CAPS variable name. it simply produces a second object with the same components. unless the objects are saved, 
#they'll need to be recreated the next time you open R.
legislct<-LEGISLCT
state<-STATE 
dcitideo<-DCITIDEO
change.gaypop<-DELTAGAY
pct.dem.house<-PDEMH_1

#however, you can attach new objects to a dataframe by using the $ and assignment command. 
gaydata$legislct<-legislct
gaydata$state<-STATE #can avoid previous renaming step
gaydata$dcitideo<-dcitideo
gaydata$change.gaypop<-change.gaypop
gaydata$pct.dem.house<-PDEMH_1

#the dataframe needs to be detached and attached again before these new components will be visible.
detach(gaydata)
attach(gaydata)
names(gaydata) #should result in a list with three new names included



##### the Three Ns
##missing data/values - two kinds
#in original dataset, blank fields were left where data was missing. R reads missing values as NA or "Not Available". applies to many modes - character, numeric, etc.
dcitideo 
is.na(gaydata$dcitideo) #produces a logical vector with value TRUE if and only if element is NA and FALSE otherwise

#missing values produced from numerical computations - NaN or "Not a Number". applies only to numeric modes.
0/0

#for lists with zero length - NULL
k<-c()
k
is.null(k) 

state<-as.character(state)
rownames(gaydata)<-state


##### lm, glm, optim, nlm
##fitting a linear model using lm
#results <- lm(y ~ x1 + x2 + x3) #generally, the first argument when model fitting is the formula of interest. 
#provides arguments for dealing with missing values (omit cases, do nothing), weighting cases, etc.
#lm and glm have similar formats. the latter has more optional arguments. since these variables aren't normally distributed, we'll skip to the glm.
#keep in mind that you can summarize the results and reference coefficients, residuals, etc. for an lm just like for a glm.

##fitting a generalized linear model using glm
#variables we might use: DV - legislct, IV - currct.1, pdemh.1, sodomy.1, ddompct, deltagay
bills.results2 <- glm(legislct ~ currct.1 + pdemh.1 + sodomy.1 + ddompct + deltagay, family=poisson) #see help(family) for family options 

summary(bills.results2)

attributes(bills.results2)

#if you want to use any of these - the coefficients, residuals, fitted.values, etc. - you'll need $ again
bills.coeff <- bills.results2$coef #outputs a vector of coefficients from the model
bills.results2$coef[4] #outputs the coefficient for the sodomy variable only. remember: including an intercept is the default.
bills.fitted <- bills.results2$fitted.values #produces a vector of fitted values from the model

#at the end of each session, R asks if you want to save all currently available objects. if you decide that you do, 
#all objects are written to an .RData file and command lines are saved to an .Rhistory file
save(gaydata, file="gaydata.Rdata") #saves specified objects
load("gaydata.Rdata")
save.image(file="gaydata3.Rdata") #saves whole environment

##minimize or maximize
nlm(f,p) #non-linear minimization. minimizes function with starting values.
optim(par, fn) #general purpose optimization. optim(initial values for paramters, function).
#for now, know they exist. we'll need them later once we get further into MLE. 
#here's an example of the optim function in action, borrowed from Kevin Quinn.

myfun <- function(theta){
  f <- -1 * (theta - 3.4)^2
  return(f)
}

myfund1 <- function(theta){
  d1 <- -2*theta + 2*3.4
  return(d1)
}

myfund2 <- function(theta){
  d2 <- -2
  return(d2)
}

theta.start <- -30

optim.out <- optim(theta.start, fn=myfun, gr=myfund1, method="BFGS",
                   control=list(fnscale=-1, trace=1, REPORT=1))



##### saving graphics
#as a pdf file - first you have to start the graphics device driver. file gets saved to the current directory. located by File -> Change dir . . .
#you're able to control things like font, width/height of graphics image, orientation of image, point size, background color, etc.
pdf(file="gayhist.pdf")
hist(legislct, main="histogram of count of bills introduced in state legislatures, 1990-2000", col="blue") #i recommend checking R documentation for other arguments

dev.off()#turns off the .pdf device
 
#as a postscript file, which is EPS (Encapsulated PostScript) compatible for inncluding in other documents (like LaTeX)
postscript(file="gaybox.ps")
boxplot(sodomy.1, main="boxplot of number of years a sodomy law is present in each state, 1990-2000", col="green", horizontal=TRUE)
dev.off()

#if don't open a graphics device, a graphics window automatically opens
#there are a ton of argument options for plots, these are just a few
#can then right click and save as either a Metafile or Bitmap for pasting in Word, for example
#the format for scatterplots is plot(x, y)
plot(pdemh.1, legislct, main="scatterplot", ylab="count of bills", xlab="percentage democrats in state house", col="purple", pch=8, cex=.6)
identify(pdemh.1, legislct, row.names(gaydata)) #interact with R to identify specific points. right click to stop.
abline(lm(legislct ~ pdemh.1)) #fits a regression line to the plot.

#a plotting example borrowed from Kevin Quinn:
# let's put 4 graphs on 1 page
par(mfcol=c(2,2)) #par() is used to set graphical parameters. see help(par) for all of the options available.
                    #mfrow or mfcol=c(nr, nc) produces an nr by nc array for graphics. if mfrow, it produces them by row. if mfcol, by columns. 

# plot the cosine function on [0,10]
x <- 0:100/10
plot(x, cos(x), type="l") #type = "l" plots a line, as opposed to a point, graph. there are other options for type of plot as well.

# plot exp(x) on [0,10]
plot(x,exp(x), type="l")

# plot the beta(3,4) density function on [0,1]
x <- 0:100/100
plot(x, dbeta(x,3,4), type="l") 

# plot a normal(0,4) density on [-10,10]
x <- -100:100/10
plot(x, dnorm(x,0,2), type="l")



##### the end, for now.
