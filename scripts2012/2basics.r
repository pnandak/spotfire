#### "Basics"
#### Harvard Gov2001 Code Library
#### Instructor: Gary King
#### Maintainers: Margaret Roberts and Brandon Stewart
#### May 1, 2011
#### Version 0.5
#### Contact: bstewart@fas.harvard.edu

# Summary: This file contains basic elements of probability and simulation
#
# Table of Contents:
#    Estimating PDFs and CDFs
#    3rd axiom of probability
#    Asymptotic Estimations of Probability
#    Simulating from the Beta-Binomial Distribution





#### Contributors: Julie Faller, Noah Nathan
#### April 19 2011
#### Estimating PDFs and CDFs
#### Basics: pg 23

#########################################################################################
#"Show how to estimate the pdf and cdf of each of these stochastic components using R." #
#########################################################################################

## Normal Distribution: continuous, unimodal, symmetric, unbounded
## PDF: In this section, we plot the PDFs of normal distributions while varying their parameters.  
## Within the dnorm function the second term is the mean and the third term is the standard deviation.

rm(list=ls(all=TRUE))
pdf(file = "normal_pdf.pdf", height=6, width=6, title="Normal PDF")
par(mfrow=c(2,2))
plot(function(x) dnorm(x, log=FALSE), -8, 8, 
    main = "Normal PDF, mu = 0, sd = 1", 
	col="dodgerblue4", ylab="")

plot(function(x) dnorm(x, 2, 1, log=FALSE), -8, 8, 
     main = "Normal PDF, mu = 2, sd = 1", 
	 col="firebrick1", ylab="")

plot(function(x) dnorm(x, 0, 0.5, log=FALSE), -8, 8, 
     main = "Normal PDF, mu = 0, sd = 0.5", 
	 col="chartreuse3", ylab="")

plot(function(x) dnorm(x, 0, 3, log=FALSE), -8, 8, 
     main = "Normal PDF, mu = 0, sd = 3", 
	 col="gold2", ylab="")
dev.off()

## Normal Distribution CDF
## In this section, we plot the CDFs of the normal distributions 
## with the same parameters as above.

pdf(file = "normal_cdf.pdf", height=6, width=6, title="Normal CDF")
par(mfrow=c(2,2))

plot(function(x) pnorm(x, log=FALSE), 0, 5, 
     main = "Normal CDF, mu = 0, sd = 1", 
	 col="dodgerblue4", ylab="")

plot(function(x) pnorm(x, 2, 1, log=FALSE), 0, 5, 
     main = "Normal CDF, mu = 2, sd = 1", 
	 col="firebrick1", ylab="")

plot(function(x) pnorm(x, 0, 0.5, log=FALSE), 0, 5, 
     main = "Normal CDF, mu = 0, sd = 0.5", 
	 col="chartreuse3", ylab="")

plot(function(x) pnorm(x, 0, 3, log=FALSE), 0, 5, 
     main = "Normal CDF, mu = 0, sd = 3", 
	 col="gold2", ylab="")

dev.off()


## Log Normal Distribution:continuous, unimodal, skewed, bounded from below by zero 
## PDF: In this section we vary the parameters of the log normal distribution as in the 
## above section to produce their PDFs.
pdf(file = "lognormal_PDF.pdf", height=6, width=6, title="Log Normal PDF")
par(mfrow=c(2,2))

plot(function(x) dlnorm(x, mean=0, sd=1), 0, 8, 
     main = "Log Normal PDF, mu = 0, sd = 1", 
	 col="dodgerblue4", ylab="")

plot(function(x) dlnorm(x, 0, .25), 0, 8, 
     main = "Log Normal PDF, mu = 0, sd = .25", 
	 col="firebrick1", ylab="")

plot(function(x) dlnorm(x, 3, 1), 0, 12, 
     main = "Log Normal PDF, mu = 3, sd = 1", 
	 col="chartreuse3", ylab="")

plot(function(x) dlnorm(x, 2, .75), 0, 12, 
     main = "Log Normal PDF, mu = 2, sd = .75", 
	 col="gold2", ylab="")
dev.off()


## Log Normal Distribution CDF
## In this section we vary the parameters of the log normal distributions to produce their CDFS.
pdf(file = "Figs/lognormal_CDF.pdf", height=6, width=6, title="Log Normal CDF")
par(mfrow=c(2,2))
plot(function(x) plnorm(x, mean=0, sd=1), 0, 4, 
     main = "Log Normal CDF, mu = 0, sd = 1", 
	 col="dodgerblue4", ylab="", ylim=c(0,1))

plot(function(x) plnorm(x, 0, .25), 0, 4, 
     main = "Log Normal CDF, mu = 0, sd = .25", 
	 col="firebrick1", ylab="", ylim=c(0,1))

plot(function(x) plnorm(x, 1, 1), 0, 16, 
     main = "Log Normal CDF, mu = 1, sd = 1", 
	 col="chartreuse3", ylab="", ylim=c(0,1))

plot(function(x) plnorm(x, 2, 0.75), 0, 16, 
     main = "Log Normal CDF, mu = 2, sd = .75", 
	 col="gold2", ylab="", ylim=c(0,1))
dev.off()

## Bernoulli Distribution: discrete, binary outcomes 
## To show how the bernoulli distribution varies with its one parameter, pi, we draw 
## 1000 samples from the uniform distribution. We then vary the pi by setting the cut-off 
## to different threshholds. For example, in y1, the bernoulli has a pi=.2. The bernoulli's 
## PDF is a histogram of the two possible outcomes (0 and 1). Its CDF is the sum of the two 
## columns of the histogram. 

## Generating the distributions
set.seed(02138)
sims <- 1000
u <- runif(sims)

y1 <- as.integer(u < .2)
y2 <- as.integer(u < .4)
y3 <- as.integer(u < .6)
y4 <- as.integer(u < .8)

## Bernoulli PDFs
pdf(file = "bernoulli_PDF.pdf", height=6, width=6, title="Bernoulli PDF")
par(mfrow=c(2,2))
hist(y1, col="dodgerblue4", main="Bernoulli PDF, Pi = 0.2", 
     breaks = 5, ylim=c(0,1000), xlab="% No   /   % Yes")
hist(y2, col="firebrick1", main="Bernoulli PDF, Pi = 0.4", 
     breaks = 5, ylim=c(0,1000), xlab="% No   /   % Yes")
hist(y3, col="chartreuse3", main="Bernoulli PDF, Pi = 0.6", 
     breaks = 5, ylim=c(0,1000), xlab="% No   /   % Yes")
hist(y4, col="gold2", main="Bernoulli PDF, Pi = 0.8", 
     breaks = 5, ylim=c(0,1000), xlab="% No   /   % Yes")
dev.off()


## Bernoulli Distribution CDF
## We use the ecdf function here. We add the polygons to fill in the histogram. 
## You can try plot(ecdf(object)) without any additional specifications 
## to see the function on its own. 

pdf(file = "bernoulli_CDF.pdf", height=6, width=6, title="Bernoulli CDF")
par(mfrow=c(2,2))

plot(ecdf(y1), xlim=c(0,2), col="white", lwd=.001, 
     verticals=FALSE, xlab="", do.points=FALSE, 
	 main="Bernoulli CDF, Pi=0.2", ylab="Cumulative Prob")
polygon(x = c(0,0,1,1), y = c(0,.8,.8,0), col="dodgerblue4")
polygon(x = c(1,1,2,2), y = c(.8,1,1,.8), col="springgreen")
polygon(x = c(1,1,2,2), y = c(0,.8,.8,0), col="springgreen")

plot(ecdf(y2), xlim=c(0,2), col="white", lwd=.001, 
     verticals=FALSE, xlab="", do.points=FALSE, 
	 main="Bernoulli CDF, Pi=0.4", ylab="Cumulative Prob")
polygon(x = c(0,0,1,1), y = c(0,.6,.6,0), col="dodgerblue4")
polygon(x = c(1,1,2,2), y = c(.6,1,1,.6), col="springgreen")
polygon(x = c(1,1,2,2), y = c(0,.6,.6,0), col="springgreen")

plot(ecdf(y3), xlim=c(0,2), col="white", lwd=.001, 
     verticals=FALSE, xlab="", do.points=FALSE, 
	 main="Bernoulli CDF, Pi=0.6", ylab="Cumulative Prob")
polygon(x = c(0,0,1,1), y = c(0,.4,.4,0), col="dodgerblue4")
polygon(x = c(1,1,2,2), y = c(.4,1,1,.4), col="springgreen")
polygon(x = c(1,1,2,2), y = c(0,.4,.4,0), col="springgreen")

plot(ecdf(y4), xlim=c(0,2), col="white", lwd=.001, 
     verticals=FALSE, xlab="", do.points=FALSE, 
	 main="Bernoulli CDF, Pi=0.8", ylab="Cumulative Prob")
polygon(x = c(0,0,1,1), y = c(0,.2,.2,0), col="dodgerblue4")
polygon(x = c(1,1,2,2), y = c(.2,1,1,.2), col="springgreen")
polygon(x = c(1,1,2,2), y = c(0,.2,.2,0), col="springgreen")

dev.off()

## Poisson Distribution: discrete, countably infinite on the nonnegative integers 
## The poisson distribution has one variable, lambda, is both the expected value 
## and the variance of the distribution 

## First, we created a vector of non-negative integers (which the poisson requires)
x<-c(1:100)

## PDF
pdf(file = "Figs/poisson_pdf.pdf", height=6, width=6, title="Poisson PDF")
par(mfrow=c(2,2))

plot(x, dpois(x, lambda= 1, log=FALSE), main="Poisson PDF, Lambda=1", col="darkblue")

plot(x, dpois(x, lambda= 25, log=FALSE), main="Poisson PDF, Lambda=25", col="seagreen")

plot(x, dpois(x, lambda= 50, log=FALSE), main="Poisson PDF, Lambda=50", col="purple")

plot(x, dpois(x, lambda= 75, log=FALSE), main="Poisson PDF, Lambda=75", col="sienna")

dev.off()

## CDF
pdf(file = "Figs/poisson_cdf.pdf", height=6, width=6, title="Poisson CDF")
par(mfrow=c(2,2))

plot(x, ppois(x, lambda= 1, log=FALSE), main="Poisson CDF, Lambda=1", col="darkblue")

plot(x, ppois(x, lambda= 25, log=FALSE), main="Poisson CDF, Lambda=25", col="seagreen")

plot(x, ppois(x, lambda= 50, log=FALSE), main="Poisson CDF, Lambda=50", col="purple")

plot(x, ppois(x, lambda= 75, log=FALSE), main="Poisson CDF, Lambda=75", col="sienna")

dev.off()







#### Contributors: Maxwell Palmer, Andrew Hall
#### April 20 2011
#### Prove the 3rd axiom of probability using simulation
#### Basics: pg 25

#########################################################################################
# Basics - pg 25 prove the 3rd axiom using simulation.                                  #
# provide a counterexample where the events are not mutually                            #
# exclusive                                                                             #
#                                                                                       #
# The third axiom states that if z1...zk are a series of mutually                       #
# exclusive events, the Probability of z1 or z2 or ... or zk is                         #
# the sum of the probabilities of each individual event.                                #
#                                                                                       #
# We can use simulation to test this.  Returning to the poker                           #
# example we used in the first problem set, let's generate a large                      #
# set of poker hands to evaluate.                                                       #
#                                                                                       #
# Some sets of poker hands are mutually exclusive, while others                         #
# are not.  Let's consider the set of hands:                                            #
#                                                                                       #
# Four of a King - Four cards of the same rank, and any other                           #
#	card.                                                                               #
# Full House - Three matching cards of one rank, and two matching                       #
# 	cards of another rank.                                                              #
#                                                                                       #
# These two hands are clearly mutually exclusive of each other.                         #
# It is impossible get a hand that is both a Full House and Four                        #
# or a Kind.                                                                            #
#                                                                                       #
# Generate a deck of cards.  Since none of the hands we care about                      #
# rely on matching suit across the hand, we only need four cards                        #
# of each value, and suit does not matter.                                              #
#########################################################################################
rm(list=ls(all=TRUE))
deck <- rep(seq(1,13),4)
sims = 10000
hands = matrix(NA,sims,5)
set.seed(12345)
## Draw a set of hands, and sort them.
for(i in 1:sims){
	hands[i,] <- sample(deck,5,replace=FALSE)
}

## Classify each hand
full.house <- rep(0,nrow(hands))
four.of.a.kind <- rep(0,nrow(hands))

## Cycle through hands
for(i in 1:nrow(hands)){
	# use table() to get frequencies of each card
	a <- table(hands[i,])
	if(sum(a==4)==1)
		four.of.a.kind[i] <- 1
	if(sum(a==3)==1 && sum(a==2)==1)
		full.house[i] <- 1
}

## The individual probabilities of each hand type
p.four <- sum(four.of.a.kind)/sims
p.full <- sum(full.house)/sims
p.four
p.full

## According to the third axiom, since a Four of a Kind and a Full
## House are mutually exclusive, the probability of getting a Four
## of a Kind or a Full House should be the sum of the individual
## probabilities:
p.sum <- p.four + p.full
p.sum

## We can also find the joint probability by looking at our
## simulated hands, and counting all the hands that are either
## a Four of a Kind or a Full House
joint <- rep(0,sims)
for (i in 1:nrow(hands)){
	if(four.of.a.kind[i]==1)
		joint[i]<-1
	if(full.house[i]==1)
		joint[i]<-1
}
p.joint <- sum(joint)/sims
p.joint

## The sum of the probabilities and the joint probabilities are
## the same!

## Now, let's consider two hands that are not mutually exclusive:
## One Pair and Three of a Kind. (note, we assume that Three of a Kind
## is not considered a pair as well).  When we have a hand that is
## both a Three of a Kind and has a Pair, then we have a Full House.

## Using the same set of hands already simulated, let's calculate the
## number of each type of hand.

one.pair <- rep(0,sims)
three.of.a.kind <- rep(0,sims)
pair.or.three <- rep(0,sims)
for (i in 1:nrow(hands)){
	a <- table(hands[i,])
	if(sum(a==2)==1){
		one.pair[i] <- 1
		pair.or.three[i] <- 1
	}
	if(sum(a==3)==1){
		three.of.a.kind[i] <- 1
		pair.or.three[i] <- 1
	}
}
p.pair <- sum(one.pair)/sims
p.three <- sum(three.of.a.kind)/sims
p.joint2 <- sum(pair.or.three)/sims
p.pair
p.three
p.pair + p.three
p.joint2

## The sum of these probabilities is not the joint probability,
## because we are double counting the hands with both a pair and
## a three of a kind - the full house hands.  If we subtract the
## probability of a full house, we get the right answer.

p.pair + p.three - p.full
p.joint2





#### Contributors: Ana Catalano, Yanfang Su, Erru Yang 
#### April 15, 2011
#### Estimated Probability Becomes more exact when N gets larger.
#### Basics: pg 30

#################################################################################################
#   We are trying to show that, as the simulation size N gets larger, the estimated             #
#   probability of two people having the same birthday gets closer to theoretical value         #
#                                                                                               #
#   Please note we don't consider the case that one year might have Feb. 29 and 366 days        #
#   The code first calcuate theoretical probability value                                       #
#       Total # of case in which 24 people don't share same birthday is 24-Permutations of 365  #
#       Total # of all possible of birthday case is 365^24                                      #
#       So probability is P(365,24)/(365^24)                                                    #
#   Second, the code will repeat following procedure to get estimated probability               #
#   1) randomly assign birthday to every people                                                 #
#   2) check if any people has same birthday by compare unique number of birthday to number of  #
#      people if unique number of birthday is smaller than number of people, we know someone    #
#      must have same birthday.                                                                 #
#   The frequency we found people have same birthday is the estimated probability               #
#   During our loop, we record current frequency and current simulation size                    #
#   After the loop finish, we plot the curve of frequency and simulation size                   #
#   as well as the theoretical probability value                                                #
#                                                                                               #
#   Note we get the code for simulate the probability from Gary King's lecture notes.           #
#################################################################################################
rm(list=ls(all=TRUE))
## Set the total number of people
people <- 24

## Calculate theoretical probability
## (Note we could use the Rmpfr package to 
## calculate the probability value with high precision
## But result will be similar)

require(Rmpfr)

people <- 24
mp.365 <- mpfr(365, 4096)
## calculate # of total case
mp.totalCase <- mp.365^24
## calcuate # of case where people don't share same birthday
mp.allDifferentBirthDayCase <- mp.365

for(i in 1:(people-1)) {
	mp.allDifferentBirthDayCase <- mp.allDifferentBirthDayCase * mpfr(365 - i, 4096)
}

## calculate theoretical probability value
mp.allDifferentBirthDayCase / mp.totalCase


## calculate number of total case
totalCase <- 365^24
## calcuate number of case where people don't share same birthday
allDifferentBirthDayCase <- 365;
for(i in 1:(people-1)) {
	allDifferentBirthDayCase <- allDifferentBirthDayCase * (365 - i)
}
## calculate theoretical probability value
prob.theoretical <- 1 - allDifferentBirthDayCase / totalCase

## Now begin our simulation case

## total simulation size
totalSimNum <- 10000
## days from 1 to 365, each represent one different day in the year
alldays <- seq(1, 365, 1)
## counter of how many times we see some people share same birthday
sameBirthDayCount <- 0
## vector to record frequence in progress
frequenceVector <- numeric(totalSimNum)
## index to track current locatin in Vector above to store result
for (i in 1:totalSimNum) {
	#assign birthday to people in the rooms
	room <- sample(alldays, people, replace = TRUE)
	#check if some people share same birth day
	if (length(unique(room)) < people)
	{
		#if so, increase the counter
		sameBirthDayCount <- sameBirthDayCount + 1
	}
    frequenceVector[i] <- sameBirthDayCount / i
}

## Finally we plot the curve
plot(1:totalSimNum,frequenceVector,type="l", 
    main="Estimated probability becomes more exact as N gets larger", 
	ylim=c(0,1), xlab="Simuation times (N) ", ylab="Probability value", col="green")
## draw the line of theoretical probability 
abline(prob.theoretical,0,col="blue")
grid()
legend(x=totalSimNum * 0.2, y=0.8, 
       legend=c("Estimated Probability", "Theoretical Probability"), 
	   col=c("green", "blue"), lwd=c(1,1))
savePlot(filename = "Figs/EstProb.pdf", type="pdf")






#### Contributors: Adi Dasgupta and Sean Ingham
#### April 25, 2011
#### Simulating from the Beta-Binomial Distribution
#### Basics: pg 50

#################################################################################################
#  The beta-binomial distribution is a hierarchical, or compound,                               #
#  distribution consisting of a binomial distribution in which the                              #
#  parameter p is treated as a random variable distributed by the beta                          #
#  distribution. That is, instead of assuming that the n independent                            #
#  trials all result in successes with the same probability p, it allows                        #
#  the success probabilities to be realizations of independent variables                        #
#  identically distributed according to a beta distribution.                                    #
#                                                                                               #
#  The function that simulates the distribution follows these steps:                            #
#  step 1: set parameters n (# of trials), and alpha and beta (parameters of beta dist)         #
#  step 2: generate realizations for the n trials, using alpha and beta                         #
#  step 3: store the number of successes                                                        #
#  step 4: repeat steps 2 and 3 M (say, 1000) times                                             #
#  The distribution of the M results is a simulated beta-binomial                               #
#################################################################################################
rm(list=ls(all=TRUE))
## steps 2 and 3, as a function
beta.binom <- function(n,a,b){
  prob <- rbeta(n,a,b)
  outcomes <- apply(as.matrix(prob),1,function(x) sample(c(0,1),
                    size=1,replace=TRUE,prob=c(1-x,x)))
  return(sum(outcomes))
}
beta.binom(100,4,6) 
#this is one draw from a beta-binomial dist. w. parameters n=100, a=4,b=6

## step 4
results <- replicate(1000,beta.binom(n=100,a=4,b=6))
hist(results,col='gold') #a simulation of the beta binomial dist, w/ paramters (100,4,6)
savePlot(filename = "Figs/BetaBinomial.pdf", type="pdf")

##as a single funciton,
sim.beta.binom <- function(M,trials,alpha,beta){
  beta.binom <- function(n,a,b){
    prob <- rbeta(n,a,b)
    outcomes <- apply(as.matrix(prob),1,
                      function(x) sample(c(0,1),size=1,
					  replace=TRUE,prob=c(1-x,x)))
    return(sum(outcomes))
  }
  return(replicate(M,beta.binom(n=trials,a=alpha,b=beta)))
}

results <- sim.beta.binom(1000,1000,4,6)

mean(results) 
table(results <=380)
#simulated probability that number of successes k is less than/equal to 380
