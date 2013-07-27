# Gov 2000 Fall 2007
# Jens Hainmueller
# some stuff from a R-handout by
# Alison Post & Ryan T. Moore

#####
## brief review on distributions
## The t Distribution

## The t distribution is defined as the ratio of a unit normal random
## variable and the square root of a chi-squared random variable with n
## degrees of freedom.

## The t is similar to a normal distribution with fatter tails.  The
## greater the degrees of freedom for the chi-squared in the
## denominator, the more closely the t approximates the normal.

## Illustration

par(mfrow=c(1,1))
x<- seq(from=-3, to=3, by=.1)
plot(x,dnorm(x), col="blue", type="l", ylim=c(-.01,.45))
lines(x,dt(x,1), col="green")
lines(x,dt(x,2), col="red")
lines(x,dt(x,3), col="red")
lines(x,dt(x,4), col="red")
lines(x,dt(x,5), col="red")
lines(x,dt(x,6), col="red")
lines(x,dt(x,7), col="red")

## - why does t approach the normal as n, the degrees of freedom,
## increase?  Think about what happens to the chi-squared as the
## degrees of freedom increase.
## Recall: The Chi-squared distribution is defined as the sum of n
## unit-normally distributed random variables, squared. (Fox97 555)

## df = 1
x <- seq(from=0, to=5, by=.1)
plot(x, dchisq(x,1), type="l", main="PDF for the Chi Square, df=1")

## df = 4
x<- seq(from=0, to=20, by=.1)
plot(x, dchisq(x,4), type="l", main="PDF for the Chi Square, df=4")

## df = 100
x<- seq(from=0, to=200, by=.1)
plot(x, dchisq(x,100), type="l", main="PDF for the Chi Square, df=100")

# again with color
x <- seq(from=0, to=20, by=.1)
plot(x,dchisq(x,1), type="l", main="PDFs for the Chi Square, dfs=1-8")
lines(x,dchisq(x,2), col="red")
lines(x,dchisq(x,3), col="orange")
lines(x,dchisq(x,4), col="yellow")
lines(x,dchisq(x,5), col="green")
lines(x,dchisq(x,6), col="blue")
lines(x,dchisq(x,7), col="purple")
lines(x,dchisq(x,8), col="brown")


################################################
## new stuff: 30 minutes on selected R commands

# Matrix operations

# cbind and rbind are R's functions that allow you to connect vectors
X1 <- rep(1,10); X1
X2 <- rep(0,10); X2

# rbind appends the vectors below each other
rbind(X1,X2)

# cbind puts them next to each other
cbind(X1,X2)

# notice vectors have to be the same length

# The same works for two matrices
mat1 <- rbind(X1,X2)
mat2 <- rbind(X2,X1)

# append matrices below each other
rbind(mat1,mat2)

# and next to each other
cbind(mat1,mat2)

# Important cbind and rbind only work for numeric data types
# if you want to compbine a text vector and a numeric vector or a factor
# always use data.frame()
# Here is an example

X1 <- rep("help",10) ; X1
X2 <- rep(1,10) ; X2

# if you try this cbind will convert X1 into a string
cbind(X1,X2)

# data.frame will keep X1 numeric
data.frame(X1,X2)

# you can also do give names
dat <- data.frame(Var1 = X1, Var2 = X2)
dat
dat$Var1

# colnames and rownames can be useful
# any matrix allows for both
mat <- matrix(NA,5,3)
rownames(mat) <- c("row1","row2","row3","row4","row5")
colnames(mat) <- c("col1","col2","col3")
mat

# you can also index rownames
rownames(mat)[1] <- c"replaced"
mat

# again names have to match the dimensions of the matrix


## now we jump to a different topic
## Functions with multiple outputs:
## thus far the functions we have seen returned
## single elements as outputs (scalars, vectors)
## often we want to use functions that return multiple things
## here is how to do it

take.four <- function(x){

out.1 <- mean(x)
out.2 <- sd(x)
out.3 <- rep("help",length(x))
out.4 <- matrix(mean(x),4,4)

out <- list(
            mean.x=out.1,
            sd.x=out.2,
            help.x=out.3,
            mat.x=out.4
            )
return(out)
}

# the multiple outputs are returned via a list object here
# we will cover lists in more detail later
# the important thing here is that lists allow you to return
# elements that can be of different types and dimensions
# each elements of a list can be accessed with the "$" sign
# as in listname$element
# this can be very powerful

# here is how it works
x <- rnorm(10); x
res <- take.four(x)

# these are the outputs we have in the list res
names(res)

# we access them by ther names
res$mean.x
res$sd.x
res$help.x
res$mat.x

## now we jump to a different topic: transformations of varibales
## often you will encounter models that assume (approximatly)
## normally distributed variables
## outliers will skew distributions way from normality
## in these cases transformations can help to transform the varaibles
## so that they approximate a normal distribution

## let consider the non-normal vector x
x <- 10 + rchisq(1000, 3)
library(MASS)
truehist(x)

## possible transformations that look more normal
par(mfrow=c(2,4))
truehist(x,xlab="x")
truehist(log(x),xlab="log(x)") # why x + 1?
truehist(sqrt(x),xlab="sqrt(x)")
truehist(x^2,xlab="x^2")
truehist(x^3,xlab="x^3")
truehist(1/sqrt(x),xlab="1/sqrt(x)")
truehist(1/x^3,xlab="1/x^3")
truehist(1/x^2,xlab="1/x^2")

# which one works best?
dev.off()

# Let's repeat this with data:
library(car)
data(Freedman)
Freedman.clean <- na.omit(Freedman)

# The Freedman data frame has 110 rows and 4 columns.
# The observations are U. S. metropolitan areas with 1968 populations of 250,000 or more.
# population: Total 1968 population, 1000s.
# nonwhite: Percent nonwhite population, 1960.
# density: Population per square mile, 1968.
# crime: Crime rate per 100,000, 1969.

x <- Freedman.clean$population
## possible transformations that look more normal
par(mfrow=c(2,4))
truehist(x,xlab="x")
truehist(log(x),xlab="log(x)") # why x + 1?
truehist(sqrt(x),xlab="sqrt(x)")
truehist(x^2,xlab="x^2")
truehist(x^3,xlab="x^3")
truehist(1/sqrt(x),xlab="1/sqrt(x)")
truehist(1/x^3,xlab="1/x^3")
truehist(1/x^2,xlab="1/x^2")

# which one works best?
dev.off()

## The pairs() command creates a matrix of scatterplots displaying the
## relationship between all combinations of variables in the
## dataset. (No conditioning.) Notice that, in the first scatterplot,
## extreme values make it difficult to view relationships between the
## variables. A log tranformation makes it easier to view our entire
## data set. Notice that we are transforming both of our variables of
## interest. If we were to take the ln of just one, we would need to
## think in terms of rates of change.

# here with no transformations
pairs(Freedman.clean) ## package = graphics

# here with log transformations
Freedman.log <- Freedman.clean
Freedman.log$density <- log(Freedman.log$density)
Freedman.log$population <- log(Freedman.log$population)
Freedman.log$crime <- log(Freedman.log$crime)
pairs(Freedman.log)

# finally: let's take a quick look at the trellis library and alternative to our usual histogram

## There are various ways to visualize
## visualizing multivariate databases. On powerful teqchnique is called the "trellis display"
## On each panel of the display, a
## subset of the data is graphed by a scatterplot, box plot, normal
## quantile plot, or other method.  Each panel shows the relationship
## of certain variables conditional on the values of other variables.
## Conditioning is a key component of trellis graphics.

## Within R, the "lattice" package implements the trellis display
## techniques invented by William Cleveland at Bell labs.  Before using lattice graphics
## commands, it is necessary to load the lattice library.  Good
## documentation for this package can be found in Venables and Ripley,
## "Modern Applied Statistics with S."  Documentation on regular R
## graphics commands can be found in the Dalgaard and Fox books on R.

## Additionally, see Paul Murrell's book "R Graphics".  The preface,
## TOC, and Ch1 are all available at
## http://www.stat.auckland.ac.nz/~paul/RGraphics/rgraphics.html.
## Also available on the web are >120 (often multiplot) examples and
## the R code to create them.

## To read more about the "Trellis Display" project, visit:
## http://cm.bell-labs.com/cm/ms/departments/sia/project/trellis/ See
## especially the section "an interviewer asks tough questions".
## Among other responses, Cleveland acknowledge's Huff's "How to Lie
## with Statistics" (1952) a short, easy book with several examples of
## misleading graphs.

## Other citations in graphical data analysis and display include
## Tufte's series on information display and Gelman's [2002] "Let's
## practice what we preach: turning tables into graphs." {\em American
## Statistician} {\bf 56}, 121--130. (Andrew Gelman, Cristian
## Pasarica, and Rahul Dodhia)

## UNIVARIATE GRAPHICS

## HISTOGRAMS Versus BOXPLOTS

## Histograms
x <- rnorm(1000)
library(MASS)
truehist(x)    ## package = MASS
               ## different defaults, including "breaks" function

## Boxplot: Note that the box contains the "interquartile range," or
## the middle half of the data. The line through this box represents
## the median. How are the "whiskers" on the boxplots set?  The "inner
## fences" are the located at an additional +/- 1.5*(interquartile
## range).  Our "whiskers" extend to the upper and lower adjacent
## values, defined as the last observation falling within these inner
## fences. Observations beyond the "inner fences" are represented by
## dots. As Fox mentions, there is a rationale behind this layout; if
## the represented distribution were normal, we would only only expect
## roughly 1% of our observations to fall beyond the inner fences
## (which represent \bar{x} +/- 2.7s).

boxplot(x)     ## package = graphics
boxplot(x, col="wheat", notch=T)

## UNIVARIATE LATTICE GRAPHICS:  HISTOGRAMS, BOXPLOTS, STRIP PLOTS
library(lattice)
data(singer)
histogram(~height|voice.part, data=singer, layout=c(2,4),
          xlab = "Height (inches)")  ## package = lattice

## This gives us the distribution of heights conditional on voice
## part. (height on the x axis).
bwplot(~ height | voice.part, data=singer, xlab="Height (inches)")
  ## package = lattice
  ## this alternative doesn't facilitate easy comparison of dist'ns
bwplot(voice.part~height, data=singer, xlab="Height (inches)")
  ## boxplots lined up vertically for easy comparison of distributions
  ## of height given voice part.

stripplot(voice.part~jitter(height), data=singer, aspect=1, jitter=TRUE,
                xlab="Height (inches)")

## This gives us the distribution of height by voice part, this time
## in a jittered strip plot.  This lets us view the dispersion of
## individual observations.







