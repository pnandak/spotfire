##########################
##R programming session 7#
##########################
##options

options("digits"=4)
getOption("digits")
options("digits") ## note the difference

##traceback
##

library(MASS)
lda(Sex ~ FL + CL + CW + RW + BD, crabs)
Error in eval(expr, envir, enclos) : object "Sex" not found
traceback()

## browser

fn1<- function(mat)
{
    mults <- mat[mat[,1]==mat[,2]]
    nmul<- nrow(mults)
    if (nmul > 1)
        nmul^3
    else
        nmul^2
}

mymat<- cbind(1:4,c(1,4:6),4:7) ## somewhat artificial
fn1(mymat) ##oops

fn1<- function(mat)
{
    mults <- mat[mat[,1]==mat[,2],,drop=FALSE]
    nmul<- nrow(mults)
    browser()
    if (nmul > 1)
        nmul^3
    else
        nmul^2
}

##corrected version. Try with no duplicates too

mymat<- cbind(1:4,3:6,4:7) ## somewhat artificial

fn1<- function(mat)
{
    mults <- mat[mat[,1]==mat[,2],,drop=FALSE]
    nmul<- nrow(mults)
    if (nmul > 1)
        nmul^3
    else
        nmul^2
}

## debug
fn1<- function(mat)
{
    mults <- mat[mat[,1]==mat[,2]]
    nmul<- nrow(mults)
    if (nmul > 1)
        nmul^3
    else
        nmul^2
}

debug(fn1)
fn1(mymat)
n
n
mults
dim(mults)
Q

##recover
options(error=recover)
fn1<- function(mat)
{
    mults <- mat[mat[,1]==mat[,2]]
    nmul<- nrow(mults)
    if (nmul > 1)
        nmul^3
    else
        nmul^2
}
fn1(mymat)
1
ls()
dim(mults)
mults
c
0
## try

tmp<-try(solve(mymat[-3,]))
inherits(tmp,'try-error')
tmp<-try(solve(mymat[-1,]))
inherits(tmp,'try-error')
tmp<-try(solve(mymat[-1,]))
inherits(tmp,'try-error')

for (i in 1:4)
{
    if(!inherits(try(tmp<-solve(mymat[-i,]),silent=TRUE),"try-error"))
        print(tmp)
    else
        cat('inversion failed for matrix omitting row',i,'\n')
}

##tryCatch

tmp <- tryCatch(solve(mymat[-3,]),error=function(e)
                cat('Matrix inversion failed\n'))
tmp <- tryCatch(solve(mymat[-1,]),error=function(e)
                cat('Matrix inversion failed\n'))
##profile



inefficientFn<- function(x)
{
    tmp<-0
    for (i in 1:nrow(x))
    {
      b <- exp(1/1:ncol(x))
      tmp <- tmp + x[i,]*b
    }
   sum(tmp)
}
mymat<- matrix(runif(1000000),nrow=1000)
Rprof()
system.time(inefficientFn(mymat))
Rprof(NULL)
summaryRprof()
moreEfficientFn<- function(x)
{
    tmp<-0
    b <- exp(1/1:ncol(x))
   for (i in 1:nrow(x))
    {
      tmp <- tmp + x[i,]*b
    }
   sum(tmp)
}

Rprof()
system.time(moreEfficientFn(mymat))
Rprof(NULL)
summaryRprof()
