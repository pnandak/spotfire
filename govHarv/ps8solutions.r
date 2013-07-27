# ps 8 solutions JH
rm(list=ls())
load("C:\\Documents and Settings\\Jank\\My Documents\\Uniraps\\Gov 2000 TF\\My_PS\\PS_8\\ps6.rdata")
library(car)


# Question 3
my.F.test <- function(lm.object, L, c = rep(0,nrow(L))) {
  # logical tests
  if(length(c) != 1) stopifnot(nrow(L) == length(c))
  stopifnot(length(coef(lm.object)) == ncol(L))
  if(!is.matrix(L)) L <- matrix(L, nrow = 1)
  c <- as.matrix(c)
  VCV <- vcov(lm.object)
  beta.hat <- coef(lm.object)
  Lb.c <- L%*%beta.hat - c
  brackets <- solve(L %*% VCV %*% t(L))
  q <- nrow(L)
  Fval <- (t(Lb.c)%*%brackets%*%Lb.c)/q
  pval <-  1-pf(Fval,q,lm.object$df.residual)
  return(list(Fval = Fval, pval=pval))
}
# I tested the function on this synthetic data:
n <- 50
x1 <- runif(n)
x2 <- runif(n)
y <- 1 + 2*x1 + 3*x2 + rnorm(n)
lm.object <- lm(y ~ x1 + x2)
L <- rbind(c(0, 1, 0), c(0, 0, 1))
c <- c(0,0)
my.F.test(lm.object = lm.object, L = L, c = c)

# Q4

# part a, binary tests
nvec <- names(ps6)[c(5,6,11,12)]
store <- matrix(NA,4,4)
rownames(store) <- nvec
colnames(store) <- c("Ass","Not Ass","Diff","Pval")
for(i in 1:length(nvec)){
q1 <- lm(ps6[,nvec[i]]~ps6$assignmt)
store[i,1] <-sum(q1$coef)
store[i,2:3] <- q1$coef
store[i,4] <-summary(q1)$coef[2,4]
}

# part c, f test
lm.object <- lm(assignmt~sex +age +black +hispanic,data=ps6)
L <- matrix(0,4,5)
L[1,2] <- L[2,3] <- L[3,4] <- L[4,5] <- 1
c <- c(0,0,0,0)
my.F.test(lm.object = lm.object, L = L, c = c)

# part  d1
ps6$sexage <- ps6$age * ps6$sex
summary(lm(assignmt~sexage,data=ps6))

# part d2
lm.object <- lm(assignmt~sex +age +black +hispanic+sexage,data=ps6)
L <- matrix(0,4,5)
L[1,2] <- L[2,3] <- L[3,4] <- L[4,5] <- 1
c <- c(0,0,0,0)
my.F.test(lm.object = lm.object, L = L, c = c)

# part e, f test for joint test against all zero slopes
summary(lm(age~site,data=ps6))

# part f, f-test for mean age = 35 in site 13 and 14.
lm.object <- lm(age~site-1,data=ps6)
L <- matrix(0,2,16)
L[2,14] <- L[1,13] <- 1
L[1,14] <- -1
c <- c(0,35)
my.F.test(lm.object = lm.object, L = L, c = c)







  



