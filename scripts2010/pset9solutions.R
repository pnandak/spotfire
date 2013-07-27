## gov2000 homework 9 solution key
## fall 2009
## prepared by msen

############################################ problem 1

1-pf(0.25,1,3) 
2*(1-pt(0.5,3))

1-pf(1,1,3) 
2*(1-pt(1,3))

1-pf(4,1,3)
2*(1-pt(2,3))

############################################ problem 2

## see write-up

############################################ problem 3

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

############################################ problem 4

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

#####
# D #
#####

mod2 <- lm(lgdp ~ polity + neighborPolity + polity*neighborPolity, data=dr)
variance <- vcov(mod2)[2,2] + 2 * mean(dr$neighborPolity) * vcov(mod2)[2,4] + mean(dr$neighborPolity)^2*vcov(mod2)[4,4]
se <- sqrt(variance)
se

gamma <- mod2$coef[2] + mod2$coef[4]*mean(dr$neighborPolity)
stat <- qt(.975, df = nrow(dr)-4)
gamma + stat*se
gamma - stat*se
