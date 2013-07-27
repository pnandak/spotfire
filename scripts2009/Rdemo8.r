############################
## R programming session 8
############################

##attributes

library(MASS)
lh ### a time series object

attributes(lh)

attr(lh,"tsp")

attr(lh,"ownership") <- "mine"

attributes(lh)


## generic functions

methods(summary)

methods(class="lm")

mylm <- lm(weight ~ feed, data=chickwts)

summary(mylm)

summary(chickwts)

##summary.aov, summary.lm:

npk.aov <- aov(yield ~ block + N*P*K, npk)
summary(npk.aov)
summary.lm(npk.aov)

myFn <- function(x, ...) UseMethod("myFn")
myFn.default <- function(x, ...)
              print("in myFn.default")
myFn("test")

myFn.character <- function(x, ...)
{
    print(x)
    NextMethod(x)
    print(c(x,2))
}
myFn("test")

## search paths, conflicts
search()
conflicts()
conflicts(detail=TRUE)
## finding things

find("lh")
exists("lh")

## local copies

lm <- lm
conflicts()

##evaluation environments

f1 <- function()
{
    print(x)
}
f2 <- function()
{
   x <- 2
   f1()
}

f2()  ## gives error

## functions with environments
s <- 5; x <- 6
f <- function()
{
    g <- function()
    {
        cat("s=")
        print(s)
        cat("x=")
        print(x)
    }
    j <- function()
    {
        k <- function()
        {
            function()
            {
                cat("s=")
                print(s)
                cat("x=")
                print(x)
            }
        }
        s <- 1
        k()
    }
    s <- 3
    x <- 4
    h <- j()
    cat("\n h\n")
    print(h)
    cat("\n h\n")
    h()
    cat("g\n")
    g()
}

f()
##returning values from functions

a <- NULL
a$start <- 1
myfn <- function(x)
{
    x$start <- 2
    return(x)
}
(a <- myfn(a))
