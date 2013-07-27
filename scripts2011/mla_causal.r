library(R2WinBUGS)
library(arm)

dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec10") 
setwd(dd)

## analysis of "electric company" data
electric <- read.table ("electric.dat", header=T)
attach.all (electric)

post.test <- c (treated.Posttest, control.Posttest)
pre.test <- c (treated.Pretest, control.Pretest)
grade <- rep (Grade, 2)
treatment <- rep (c(1,0), rep(length(treated.Posttest),2))
n <- length (post.test)

## now we get into hierarichical model using the same data of Electric Company.
# hierarchical model including pair indicators
pair <- rep (1:nrow(electric), 2)
grade.pair <- grade[1:nrow(electric)]
y <- post.test
n <- length(y)
n.grade <- max(grade)
n.pair <- max(pair)


data <- c("n", "y", "n.grade", "grade", "grade.pair", "treatment", "n.pair", "pair")
inits <- function(){
  list (mu.a=runif(n.grade), theta=runif(n.grade), a=runif(n.pair),
        sigma.y=runif(n.grade), sigma.a=runif(n.grade))}
params <- c("theta","a","mu.a","sigma.y","sigma.a")
paired.0 <- bugs (data, inits, params, "paired.0.new.bug", n.iter=5000, debug=TRUE)
plot (paired.0)


data <- c("n", "y", "pre.test", "n.grade", "grade", "grade.pair", "treatment", "n.pair", "pair")
inits <- function(){
  list (mu.a=runif(n.grade), theta=runif(n.grade), a=runif(n.pair),
        sigma.y=runif(n.grade), sigma.a=runif(n.grade),
        b.pre.test=runif(n.grade))}
params <- c("mu.a","theta","a","sigma.y","sigma.a","b.pre.test")
paired.1 <- bugs (data, inits, params, "paired.1.new.bug", n.iter=5000, debug=TRUE)
plot (paired.1)
