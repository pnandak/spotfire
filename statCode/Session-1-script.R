##---------------------------------##
## Script for Session 1: Basics    ##
##    John Fox                     ##
## Statistical Computing in R/S    ##
##    ICPSR Summer Program 2008    ##
##---------------------------------##

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
(4^2)-(3*2) # use parentheses to group, clarify

1-6+4

2^-3
-2--3
-2 - -3 # use spaces to clarify


# functions, arguments to functions, obtaining help

log(100)
log(100, base=10)
log(100, b=10)
help(log)
?log
log(100, 10)

apropos("log")
help.search("log")

RSiteSearch("loglinear", "functions")

"+"(2,3) # even operators are functions

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

#comparison and logical operators

1 == 2
1 < 2

TRUE && FALSE  # AND
TRUE & c(TRUE, FALSE) # vectorized AND

TRUE || FALSE  # OR
FALSE | c(TRUE, FALSE) # vectorized OR

! c(T, F)   # NOT (abbreviations of TRUE and FALSE, best avoided!)

z <- x[1:10]
z

abs(z) > 0.5
z[abs(z) > 0.5] # indexing by a logical vector


# user-defined functions

mean(x)
sum(x)/length(x)

Mean <- function(x) sum(x)/length(x)
Mean(x)
Mean(y)
Mean(1:100)
x

# cleaning up

objects()

remove(y, z)
objects()

# using traceback()

letters
Mean(letters)

Var <- function(x) sum((x - Mean(x))^2)/(length(x) - 1)
Var(x)
var(x)

Var(letters)
traceback()


    # Duncan example
    

# creating a data frame from data stored in a file

Duncan <- read.table(file.choose(), header=TRUE)
Duncan
summary(Duncan)

# attaching a data frame

prestige

attach(Duncan)
prestige

# the search path

search()

# distributions and bivariate relationships

hist(prestige)

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


plot(income, education)
identify(income, education, row.names(Duncan))
row.names(Duncan)[c(6,16,27)]

# fitting a regression

duncan.model <- lm(prestige ~ income + education)
duncan.model
summary(duncan.model)

# regression diagnostics

library(car)
search()

plot(cookd(duncan.model))
abline(h = 4/(45-2-1))
identify(1:45, cookd(duncan.model), row.names(Duncan))

av.plots(duncan.model, labels=row.names(Duncan))

# refitting the model

remove <- which.names(c("minister", "conductor"), Duncan)
remove

duncan.model.2 <- update(duncan.model, subset=-remove)
summary(duncan.model.2)
