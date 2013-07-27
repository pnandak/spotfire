#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-20-01                                                   #
# UPDATE: 12-7-03, 12-3-06                                          #
# PURPOSE: Find C.I.s for a proportion                              #
#                                                                   #
# NOTES:                                                            #
#####################################################################

sum.y<-4
n<-10
alpha<-0.05

p<-sum.y/n

#Wald C.I.
lower<-p-qnorm(p = 1-alpha/2)*sqrt(p*(1-p)/n)
upper<-p+qnorm(p = 1-alpha/2)*sqrt(p*(1-p)/n)
cat("The Wald C.I. is:", round(lower,4) , "<= pi <=", round(upper,4), "\n \n")

#Agresti and Coull C.I.
p.tilde<-(sum.y+qnorm(p = 1-alpha/2)^2 /2)/(n+qnorm(1-alpha/2)^2)
lower<-p.tilde-qnorm(p = 1-alpha/2)*sqrt(p.tilde*(1-p.tilde)/(n+qnorm(1-alpha/2)^2))
upper<-p.tilde+qnorm(p = 1-alpha/2)*sqrt(p.tilde*(1-p.tilde)/(n+qnorm(1-alpha/2)^2))
cat("The Agresti and Coull C.I. is:", round(lower,4) , "<= pi <=", round(upper,4), "\n \n")


#Wilson C.I.
lower<-p.tilde-qnorm(1-alpha/2)*sqrt(n)/(n+qnorm(1-alpha/2)^2) * sqrt(p*(1-p)+qnorm(1-alpha/2)^2/(4*n))
upper<-p.tilde+qnorm(1-alpha/2)*sqrt(n)/(n+qnorm(1-alpha/2)^2) * sqrt(p*(1-p)+qnorm(1-alpha/2)^2/(4*n))
cat("The Wilson C.I. is:", round(lower,4) , "<= pi <=", round(upper,4), "\n \n")


#Jeffrey's C.I.
a<-sum.y+1/2
b<-n-sum.y+1/2
lower<-qbeta(p = alpha/2, shape1 = a, shape2 = b) 
upper<-qbeta(p = 1-alpha/2, shape1 = a, shape2 = b) 
cat("The Jeffrey's C.I. is:", round(lower,4) , "<= pi <=", round(upper,4), "\n \n")


#Clopper-Pearson
lower<-qbeta(p = alpha/2, shape1 = sum.y, shape2 = n-sum.y+1) 
upper<-qbeta(p = 1-alpha/2, shape1 = sum.y+1, shape2 = n-sum.y) 
cat("The C-P C.I. is:", round(lower,4) , "<= pi <=", round(upper,4), "\n \n")


#Binomial package
library(binom)
binom.confint(x = sum.y, n = n, conf.level = 1-alpha, methods = "all")

#Of course, you could just save these results to an object 
save.ci<-binom.confint(x = sum.y, n = n, conf.level = 1-alpha, methods = "ac")
save.ci

#Clopper-Pearson
binom.confint(x = sum.y, n = n, conf.level = 1-alpha, methods = "exact")



#Blaker - function code from Blaker (2000, 2001)

acceptbin<-function(x, n, p){
  #Computes the acceptability of p when x is observed and X is Bin(n,p)
  p1<-1 - pbinom(x - 1, n, p)
  p2<-pbinom(x, n, p)
  a1<-p1 + pbinom(qbinom(p1, n, p) - 1, n, p)
  a2<-p2 + 1 - pbinom(qbinom(1 - p2, n, p), n, p)
  return(min(a1,a2))
}

acceptinterval<-function(x, n, level, tolerance=1e-04){
  #Computes acceptability interval for p at 1 - alpha equal to level
  #(in (0,1)) when x is an observed value of X which is Bin(n,p).
  lower<-0
  upper<-1
  if (x!=0){lower<-qbeta((1-level)/2, x, n - x + 1)
    while (acceptbin(x, n, lower+tolerance) < (1 - level))
    lower<-lower+tolerance
  }
  if (x!=n){upper<-qbeta(1 - (1 - level)/2, x + 1, n - x)
    while (acceptbin(x, n, upper-tolerance) < (1 - level))
    upper<-upper-tolerance
  }
  c(lower, upper)
}
 
save<-acceptinterval(sum.y,n,1-alpha)
cat("The Blaker interval is:", round(save[1],4) , "<= pi <=", round(save[2],4), "\n \n")

 
