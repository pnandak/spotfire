##------------------------------------##
## Script for Part 1: Orientation     ##
##    John Fox                        ##
## An Introduction to R               ##
##    UCLA Feb. 2005                  ##
##------------------------------------##

    # Basics

# arithmetic, interacting with the interpreter

    # basic arithmetic operations

2+3
2-3
2*3
2/3
2^3


    # precedence of operators
        
4^2-3*2
(4^2) - (3*2) # use parentheses and spaces to group, clarify

-2--3
-2 - -3

1 - 6 + 4

2^-3


# functions, arguments to functions, obtaining help

log(100)
log(100, base=10)
log(100, b=10)

help(log)
?log
apropos("log")
help.search("log")

log(100,10)

"+"(2,3)

# vectors

c(1,2,3,4)  # combine

1:4     # sequence operator
4:1
-1:2    # note precedence
seq(1,4)
seq(2, 8, by=2)
seq(0, 1, by=.1)
seq(0, 1, length=11)

    # vectorized arithmetic
    
c(1,2,3,4)/2    
c(1,2,3,4)/c(4,3,2,1)
log(c(0.1,1,10,100), 10)

c(1,2,3,4) + c(4,3)
c(1,2,3,4) + c(4,3,2)

# variables

x <- c(1,2,3,4)
x
x/2
y <- sqrt(x)
y
x <- rnorm(100)
x
summary(x)  # a "generic" function

    # basic indexing

x[21]  
x[11:20]
x[-(11:100)]    # careful here!

z <- x[1:10]
z
z < -0.5
z > 0.5
z < -0.5 | z > 0.5   # | is vectored "or", & is "and"
abs(z) > 0.5
z[abs(z) > 0.5] # indexing by a logical vector


# user-defined functions

mean(x)
sum(x)/length(x)

my.mean <- function(x) sum(x)/length(x)
my.mean(x)
my.mean(y)
my.mean(1:100)
x

    # Duncan example
    
# creating a data frame from data stored in a file

Duncan <- read.table('http://socserv.socsci.mcmaster.ca/jfox/Courses/UCLA/Duncan.txt', header=TRUE)

Duncan
summary(Duncan)

# attaching a data frame

prestige

attach(Duncan)
prestige

# distributions and bivariate relationships

hist(prestige)

plot(income, education)
noteworthy <- identify(income, education, row.names(Duncan))
noteworthy
row.names(Duncan)[noteworthy]

pairs(cbind(prestige,income,education),
    panel=function(x,y){
        points(x,y)
        abline(lm(y~x), lty=2)
        lines(lowess(x,y))
        },
    diag.panel=function(x){
        par(new=TRUE)
        hist(x, main="", axes=FALSE)
        }
    )

# fitting a regression

duncan.model <- lm(prestige ~ income + education)
duncan.model
summary(duncan.model)

# regression diagnostics

library(car)

hist(rstudent(duncan.model))
qq.plot(duncan.model, labels=row.names(Duncan), simulate=TRUE)
    
plot(hatvalues(duncan.model))
abline(h = c(2,3)*3/45)
identify(1:45, hatvalues(duncan.model), row.names(Duncan))

plot(cookd(duncan.model))
abline(h = 4/(45-2-1))
identify(1:45, cookd(duncan.model), row.names(Duncan))

av.plots(duncan.model, labels=row.names(Duncan))
cr.plots(duncan.model)

# refitting the model

which.names(c('minister', 'conductor'), Duncan)
duncan.model.2 <- update(duncan.model, subset=-c(6, 16))
summary(duncan.model.2)
