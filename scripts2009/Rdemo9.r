###############################
##R programming session 9
###############################
##
ourdist <- function(x)
{
    n <- nrow(x)
    ans <- .C("ourCdist", as.double(x),
                          as.integer(n),
                          as.integer(ncol(x)),
                          res=double(n*(n-1)/2))$res
    ans
}

##R CMD SHLIB ourCdist.c
dyn.load("ourCdist.dll")
##dyn.load("ourCdist.so") #on Linux

x <- matrix(runif(9),nrow=3)

ourdist(x)

dyn.unload("ourCdist.dll")
##dyn.unload("ourCdist.so") #on Linux


##[1] 0.6207019 0.7226275 0.5782042

"%+%" <- function(a, b)
    .C("convolve1",
       as.double(a), as.integer(length(a)),
       as.double(b), as.integer(length(b)),
       ab=double(length(a) + length(b) -1))$ab


##R CMD SHLIB convolve1.c
dyn.load("convolve1.dll")
#dyn.load("convolve1.so") #on linux

a<- rnorm(10)
b<- rnorm(10)

a %+% b

"%+%" <- function(a, b)
    .Call("convolve2",
          as.double(a),
          as.double(b))
##R CMD SHLIB convolve2.c

dyn.load("convolve2.dll")
#dyn.load("convolve2.so") #on linux
a %+% b

 [1] 1.5209188 0.7434175 1.6197601 1.5878224 2.2177662 1.8913225 2.5246791
 [8] 1.7338454 2.9634044 3.9734956 2.4525768 3.2300782 2.3537355 2.3856732
[15] 1.7557294 2.0821731 1.4488165 2.2396502 1.0100912

dyn.unload("convolve2.dll")
#dyn.unload("convolve2.so") # on linux

#R CMD SHLIB Rdemo9.cc
dyn.load("Rdemo9.dll")
#dyn.load("Rdemo9.so") # on linux

a%+%b

ans <- .Call("fun1",'g')
ans2 <- .Call("fun2",ans)

