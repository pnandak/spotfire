# Code to run a four-category ordered probit

# Chris Adolph/Shauna Fisher

# www.chrisadolph.com



# Likelihood for 4 category ordered probit

llk.oprobit4 <- function(param, x, y) {

  # preliminaries

  os <- rep(1, nrow(x))

  x <- cbind(os, x)  

  b <- param[1:ncol(x)]

  t2 <- param[(ncol(x)+1)]

  t3 <- param[(ncol(x)+2)]

  

  # probabilities and penalty function

  xb <- x%*%b

  p1 <- log(pnorm(-xb))

  if (t2<=0)  p2 <- -(abs(t2)*10000)    # penalty function to keep t2>0

  else p2 <- log(pnorm(t2-xb)-pnorm(-xb))

  if (t3<=t2) p3 <- -((t2-t3)*10000)    # penalty to keep t3>t2

  else p3 <- log(pnorm(t3-xb)-pnorm(t2-xb))     

  p4 <- log(1-pnorm(t3-xb)) 



  # -1 * log likelihood (optim is a minimizer)

  -sum(cbind(y==1,y==2,y==3,y==4) * cbind(p1,p2,p3,p4))

}



library(MASS)

## using the function on the GSS data on feelings of working mothers
workmom <- read.csv("http://students.washington.edu/fishes/CSSS536/data/ordwarm2.csv", header=TRUE, sep=",")

attach(workmom)



y <- warm

x <- cbind(yr89, male, white, age, ed, prst)



# Use optim directly

ls.result <- lm(y~x)                    # use ls estimates as starting values

stval <- c(ls.result$coefficients,1,2)  # initial guesses

oprobit.result <- optim(stval, llk.oprobit4, method="BFGS", x=x, y=y, hess=T)

pe <- oprobit.result$par                # point estimates

vc <- solve(oprobit.result$hessian)     # var-cov matrix

se <- sqrt(diag(vc))                    # standard errors

ll <- -oprobit.result$value             # likelihood at maximum



# Use MASS polr to do ordered probit

warmf <- factor(warm, labels=c("Strongly Disagree",

                               "Disagree",

                               "Agree",

                               "Strongly Agree"))

glm.result <- polr(warmf ~ yr89 + male + white + age + ed + prst,

                   method="probit", na.action=na.omit)
