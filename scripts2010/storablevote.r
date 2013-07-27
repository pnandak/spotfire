library(R2WinBUGS)
library(arm)

dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec6") 
setwd(dd)


### ordered probit in Bugs: the storable vote examle here:
vote2<-read.csv("6playergames.csv", header=T, colClasses=rep("numeric", 9))
polr(factor(vote2$vote)~vote2$value)

y<-vote2$vote
x<-vote2$value
n<-length(y)
n.cut<-max(y)-1
n.player<-length(unique(vote2$person))
player<-as.numeric(as.factor(vote2$person))
data <- list ("y", "x", "n", "n.cut", "n.player", "player")

inits <- function () {list(C=matrix(c(rep(20, 30),rep(60, 30)), n.player, n.cut),  mu.c=rnorm(n.cut), sigma.c=runif(n.cut), mu.log.s=rnorm(1), sigma.log.s=runif(1), s=runif(30, 1, 100))}
params <- c ("C",  "s", "mu.c", "sigma.c", "mu.log.s", "sigma.log.s")
M2.bugs <- bugs (data, inits, params, "storablevote.bug", n.chains=3, n.iter=500, debug=TRUE) 
# this is very interesting, the Bugs code will blow if we increase the number of iterations. 
# 500 seems to be a cutoff here ????                                                                                     
plot(M2.bugs)

inits <- function () {list(C=matrix(c(runif(30, 1, 50),runif(30, 60, 100)), n.player, n.cut),  mu.c=rnorm(n.cut), sigma.c=runif(n.cut), mu.log.s=rnorm(1), sigma.log.s=runif(1), s=runif(30, 1, 100))}
params <- c ("C",  "s", "mu.c", "sigma.c", "mu.log.s", "sigma.log.s")
M2.bugs <- bugs (data, inits, params, "storablevote.bug", n.chains=1, n.iter=1500, debug=TRUE) 
plot(M2.bugs)
