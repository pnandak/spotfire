##
## homework 9 answer key
## mblackwell - 07 Dec 08
##
##

###############
## Problem 3 ##
###############

## Note that I have included some checks to make
## sure that the matrices are conformable
## and to give a sensible error message if they
## are not. This is good coding practice as cryptic
## error messages are one of the most frustrating
## parts of your coding life.

my.F.test <- function(lm.object, L, c) {
  if (length(c) != nrow(L))
    stop("L and c not conformable")
  if (ncol(L) != ncol(model.matrix(lm.object)))
    stop("L/c not conformable with the lm() object")
  beta <- coef(lm.object)
  vcv <- vcov(lm.object)
  q <- nrow(L)
  kplus1 <- ncol(model.matrix(lm.object))
  n <- nrow(model.matrix(lm.object))
  ## note that the model matrix has all k+1 columns
  
  devs <- L%*%beta - c

  F <- (t(devs) %*% solve(L %*% vcv %*% t(L)) %*% devs)/q

  pval <- pf(q=F, df1=q, df2=n-kplus1, lower.tail=FALSE)

  return(list(Fstat=F, pvalue=pval))
}

set.seed(123)
n <- 50
x1 <- runif(n)
x2 <- runif(n)
y <- 1 + 2*x1 + 3*x2 + rnorm(n)
lm.object <- lm(y ~ x1 + x2)
L <- rbind(c(0,1,0),c(0,0,1))
c <- c(0,0)
my.F.test(lm.object=lm.object, L=L, c=c)
summary(lm.object)

###############
## Problem 4 ##
###############

library(car)
dr <- read.table("donnoRussett.dat")

#####
# A #
#####

mod1 <- lm(lgdp ~ polity + neighborPolity, data=dr)

confidence.ellipse(mod1, which=c("polity","neighborPolity"), xlim=c(-.05,.1),
                   ylim=c(-.05,.1))
abline(h=confint(mod1)["neighborPolity",], col="red")
abline(v=confint(mod1)["polity",], col="red")

## note that i used the names instead of the column numbers
## here so that if I were to change the order later,
## I would still get the correct coefficients.

abline(h=0, col="blue", lty=2)
abline(v=0, col="blue", lty=2)
points(x=0,y=0, col="blue", cex=1.5, pch=19)

#####
# B #
#####

L <- matrix(c(0,1,-1), nrow=1,ncol=3)
c <- 0

my.F.test(mod1, L, c)
