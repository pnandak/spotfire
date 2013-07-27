filename <- "c:\\cygwin\\home\\mwtros\\Data\\Sleuth\\ex2011.csv"
shuttle <- read.csv(file=filename)

attach(shuttle)

out <- glm(FAILURE~TEMP,family=binomial)

drop <- 28.975-23.030
sig <- 1-pchisq(drop,df=1)

ratio <- exp(-0.17132)

prob <- function(x) {
  b <- exp(10.87535-0.17132*x)
  return(b/(1+b))
  }
pfail31 <- prob(31)

x <- seq(from=30,to=100,length=141)
y <- prob(x)
plot(x,y,type="l",xlab="Temperature",ylab="Prob(Failure)")
title("Probability of Shuttle O-Ring Failure")
points(TEMP,as.numeric(FAILURE)-1)

detach(shuttle)
