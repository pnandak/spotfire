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

# Load libraries
library(MASS)
library(simcf)
library(tile)

# Load data
workmom <- read.csv("ordwarm2.csv", header=TRUE, sep=",")
attach(workmom)

             # Data from 1979, 1989 GSS:  Attitudes towards working mothers
y <- warm    # Mother can have warm feelings towards child?  
             
x <- cbind(yr89, male, white, age, ed, prst)
             # 1989 dummy; male respondent; white resp; age of resp;
             # years of education of respondent;
             # prestige of respondent's occupation (% considering prestigious
model <- (warm ~ yr89 + male + white + age + ed + prst)

# Use optim directly
ls.result <- lm(y~x)                    # use ls estimates as starting values
stval <- c(ls.result$coefficients,1,2)  # initial guesses
oprobit.result <- optim(stval, llk.oprobit4, method="BFGS", x=x, y=y, hessian=T)
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

# Simulate parameters from predictive distributions
sims <- 10000
simbetas <- mvrnorm(sims,pe,vc)       # draw parameters, using MASS::mvrnorm

# Create example counterfactuals
xhyp <- cfMake(model,workmom,nscen=2)

xhyp <- cfName(xhyp,"High prestige female",scen=1)
xhyp <- cfChange(xhyp,"male",x=0,xpre=0,scen=1)
xhyp <- cfChange(xhyp,"prst",x=mean(prst)+sd(prst), xpre=mean(prst)-sd(prst), scen=1)

xhyp <- cfName(xhyp,"High prestige male",scen=2)
xhyp <- cfChange(xhyp,"male",x=1,xpre=1,scen=2)
xhyp <- cfChange(xhyp,"prst",x=mean(prst)+sd(prst), xpre=mean(prst)-sd(prst), scen=2)

# Simulate expected probabilities
oprobit.ev1 <- oprobitsimev(xhyp,simbetas,ci=c(0.67,0.95),cat=4)

oprobit.fd1 <- oprobitsimfd(xhyp,simbetas,ci=c(0.67,0.95),cat=4)

oprobit.rr1 <- oprobitsimrr(xhyp,simbetas,ci=c(0.67,0.95),cat=4)

