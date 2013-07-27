##
## hw1key.R - answer key for ps1
## gov2000 - 15 Oct 08
## m.blackwell
##


###############
## Problem 1 ##
###############

#####
# b #
#####

set.seed(12345)
mattsdrinking <- sample(x = 1:4, size = 10000, replace = TRUE,
                        prob = c(.1,.2,.3,.4))
mean(mattsdrinking)
var(mattsdrinking)

#####
# d #
#####

px <- function(cups) {
  prob <- (cups %in% 1:4) * (cups/10)
  return(prob)
}

px(cups=mattsdrinking[1:5])
px(cups=-1:5)


###############
## Problem 2 ##
###############

#######
# a-d #
#######

## Z > 1.96
pnorm(q = 1.96, lower.tail = FALSE)

##  Z < -1.96 (or) Z > 1.96  
pnorm(q = 1.96, lower.tail = FALSE) + pnorm(q = -1.96, lower.tail = TRUE)

## -1.2 < Z < 1.96
pnorm(q = 1.96, lower.tail = TRUE) - pnorm(q = -1.2, lower.tail = TRUE)

## -1.2 <= Z <= 1.96
pnorm(q = 1.96, lower.tail = TRUE) - pnorm(q = -1.2, lower.tail = TRUE)

#####
# e #
#####

simulation <- rnorm(10000, mean = 0, sd = 1)

mean(simulation > 1.96)
mean((simulation > 1.96) | (simulation < -1.96))
mean((simulation > -1.2) & (simulation < 1.96))
mean((simulation >= -1.2) & (simulation <= 1.96))


###############
## Problem 3 ##
###############

## R is a calculator! See answer key for derivations

250/400
250/550
175/550
(300/1000)*(299/999)*(298/998)*(297/997)*(296/996)


###############
## Problem 4 ##
###############

africa <- read.csv(url("http://www.people.fas.harvard.edu/~blackwel/afgdp.csv"))

head(africa)

#####
# a #
#####

mean(africa$gdp)

var(africa$gdp)

#####
# b #
#####

plot(density(africa$gdp), col = "orange", xlab = "GDP per capita",
     main = "Normal Approximation of African GDP")

curve(dnorm(x, mean = mean(africa$gdp), sd = sd(africa$gdp)),
      from = min(africa$gdp)-2000, to = max(africa$gdp), col = "chocolate",
      add = TRUE, lty=2)

legend(x="topright", legend=c("African Income", "Normal Approximation"),
       col=c("orange","chocolate"), lty=c(1,2), bty="n")

#####
# c #
#####

plot(density(log(africa$gdp)), col = "orange", xlab = "Log GDP per capita",
     main = "Normal Approximation of Log African GDP")

curve(dnorm(x, mean = mean(log(africa$gdp)), sd = sd(log(africa$gdp))),
      from = min(log(africa$gdp))-2, to = max(log(africa$gdp)), col = "chocolate",
      add = TRUE, lty=2)

legend(x="topright", legend=c("Log African Income", "Normal Approximation"),
       col=c("orange","chocolate"), lty=c(1,2), bty="n")

#####
# d #
#####

mean(africa$gdp[africa$british == 1])
mean(africa$gdp[africa$british == 0])
