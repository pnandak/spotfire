#
# 1. Random Variables and Discrete Distributions
#

# the World Values Survey has a question, a poll of public attitudes on 
# politics, economics, and culture conducted in dozens of countries around 
# the world, which asks the following, with answers provided: 

# "How proud are you to be [French, e.g.]? " 
	# 1. Not at all proud
	# 2. Not very proud
	# 3. Quite proud
	# 4. Very proud

# (You can assume all respondents are citizens of the nation in question.  
#  The survey we are using was administered between 2004-06)  

# r permits us to randomly sample from events if we specify 
# some probabilities using the sample() function.  E.g.

answers <- c("not at all proud", "not very proud", "quite proud", "very proud")
probs <- c(.10,.20,.30,.40) # some possible probabilities for seeing each outcome
samp <- sample(x = answers, size = 5, replace = TRUE, prob = probs) 
samp

# a random variable is a function which maps an event into numbers; or 
# alternatively it takes a verbal story and renders it numeric which
# will be useful for making quantitative arguments

# For example, we could define X to be the answer to the above question
  #     1  if  "not at all proud"
  # X = 2  if  "not very proud"
  #     3  if  "quite proud"
  #     4  if  "very proud"

answers <- c(1,2,3,4)
samp <- sample(x = answers, size = 5, replace = TRUE, prob = probs)
samp
mean(samp)

# three relevant questions for any distribution: 
# 1. is the distribution of X that we have specified a valid pmf?  
sum(probs)

# 2. what is its mean?
sum(.1*1 + .2*2 + .3*3 + .4*4)
mean.X <- sum(probs*answers)

# 3. what is its variance?
sum(.1*(1-mean.X)^2  + .2*(2-mean.X)^2  + .3*(3-mean.X)^2  + .4*(4-mean.X)^2)
var.X <- sum(probs * (answers - mean.X)^2)

#(what is its standard deviation?)
sd.X <- sqrt(var.X)
sd.X

# questions 2. and 3. can also be answered using a *simulation* approach, 
# a method which is overkill for this particular problem but becomes
# very useful for only slightly more complex problems.

# basic logic: generate a lot of realizations of a random variable
# and then use R's functionality to examine the distribution

samp <- sample(x = answers, size = 1000, replace = TRUE, prob = probs)

hist(samp, breaks = c(0,1,2,3,4))
hist(samp, breaks = c(0,1,2,3,4), probability = TRUE)

# What about samples that were taken out in the field, rather than within R?  

# let's load the data
wvs <- read.csv("http://isites.harvard.edu/fs/docs/icb.topic636824.files/Section%202/wvs2005.csv")

# first lets subset some data from the Ukraine, USA and Canada
ukraine <- wvs$pride[wvs$country == "ukraine"]
usa <- wvs$pride[wvs$country == "usa"]
canada <- wvs$pride[wvs$country == "canada"]

par(mfrow=c(1,3))
hist(ukraine, probability = TRUE, breaks = c(0,1,2,3,4), 
     col = c("deepskyblue2","yellow"), xlab = "Expressed Level of Pride", 
     main = "Respondent Patriotism: Ukraine", ylim = c(0,.75))
legend("topright", legend = c("1: Not at all proud","2:Not very proud",
       "3:Quite proud","4:Very proud"), cex = 1, text.col = "gray", box.col = "gray")
hist(usa, probability = TRUE, breaks = c(0,1,2,3,4),
     col = c("white","red","white","blue"), xlab = "Expressed Level of Pride",
     main = "Respondent Patriotism: USA", ylim = c(0,.75))
hist(canada, probability = TRUE, breaks = c(0,1,2,3,4),
     col = c("red","white"), xlab = "Expressed Level of Pride", main = "Respondent 
     Patriotism: Canada", ylim = c(0,.75))

# again we might like to ask some questions about the distribution of these data
mean(ukraine)
mean(canada)

sd(ukraine)  
sd(canada)

# using the sample function, we could also put ourselves in the shoes
# of a researcher with a tiny research budget, who can only intervew 15
# Ukrainians, and 15 Canadians
set.seed(12345)
samp <- sample(x = ukraine, size = 15)
mean(samp)

samp <- sample(x = canada, size = 15)
mean(samp)

#
# 2. Continuous distributions
#

# continuous random variable, which can take on an uncountably infinite 
# number of values, are in general harder to program.  For this reason,
# R has a suite of distributions already preprogrammed.  

# Here's the general scheme for three important kinds of functions:
# d"dist"()  =  the pdf
# p"dist"()  =  the cdf
# r"dist"()  =  a random number generator from that function

# example: the uniform distribution
# what is the height of the density for the uniform distribution on (-1,1)
# at the point x = .5? 
dunif(x= .5, min = 0, max = 1)
	# question: is this the probability that x = .5?  

# what is the probability that some x < .5?
punif(q =.5, min = 0, max = 1)

# let's sample five values from a uniform distribution
runif(n = 5, min = 0, max = 1)

# now simulate the mean and variance of X when X ~ U(0,1)
draws <- runif(n = 10000, min = 0, max = 1)
hist(draws)
mean(draws)
var(draws)

# if we want to plot the actual distribution itself, here 
# is one way to do so...
my.seq <- seq(from = 0 , to =1, by = .01)
plot(x = my.seq, y = dunif(x = my.seq, min = 0, max = 1), type = "l")
	# note: the type = "l" argument makes this a connected line, 
	# rather than a sequence of dots

# what about the cdf?
plot(x = my.seq, y = punif(q = my.seq, min = 0, max = 1), type = "l")

# ex. 2: the Normal distribution
# what does the normal density look like?  
my.seq <- seq(from = -3.5 , to =3.5, by = .01)
plot(x = my.seq, y = dnorm(x = my.seq, mean = 0, sd = 1), type = "l")

# what is the height of the density at x=1?
dnorm(x = 1, mean = 0, sd = 1)

# what is the probability that x > 1?
1 - pnorm(q = 1, mean = 0, sd = 1)

# what is the probability that -2 < x < 1?
pnorm(q = 1, mean = 0, sd = 1) - pnorm(q = -2, mean = 0, sd = 1)

# let's draw a lot of samples from a slightly different normal 
# and then check out the distribution of our samples
draws <- rnorm(1000, mean = 2, sd = 2)
hist(draws, breaks = 50, col = "wheat")

draws <- rnorm(10000, mean = 2, sd = 2)
hist(draws, breaks = 50, col = "darkorange1")

#
# 3: for loops
#

# a for loop is a way of repeating a process a vast 
# number of times.  For each iteration of the loop
# R keeps an index number stored which can be handy.

# here is a simple example where we first define a 
# storage compartment

storage <- vector(length = 10) 

for(i in 1:10){
  draw <- sample(x = answers, size = 1, replace = TRUE, prob = probs)
  storage[i] <- draw
}
# what just happened here?  what's in storage now?  
storage

# now suppose I want to program the same loop, but I'm
# not sure if I'll need 10 or 20 draws...
sims <- 10

storage <- vector(length = sims) 
for(i in 1:sims){
  draw <- sample(x = answers, size = 1, replace = TRUE, prob = probs)
  storage[i] <- draw
}
storage

# now let's cut down on one line of code...
storage <- vector(length = sims) 
for(i in 1:sims){
  storage[i] <- sample(x = answers, size = 1, replace = TRUE, prob = probs)
}
storage

# so far this has been pretty trivial, because we already know how to 
# draw a sample of size 10 using the sample() function.  But what if we 
# want to draw 10 samples of size 10?  

# first we need to contemplate our storage.  
storage <- matrix(data = NA, nrow = 10, ncol = 10)
	# each sample will be a row

# two things have changed in the loop below.  What are they?  
for(i in 1:sims){
  storage[i,] <- sample(x = answers, size = 10, replace = TRUE, prob = probs)
}
storage

# now suppose we are going to conduct the same sampling scheme, 
# but we don't care about the sample's themselves, only their means
# and variances.  

# first some storage...
sims <- 10
storage <- matrix(data= NA, nrow = sims, ncol = 2)
colnames(storage) <- c("samp.mean", "samp.var")

# ...then our loop
for(i in 1:sims){
  draws <- sample(x = answers, size = 10, replace = TRUE, prob = probs)
  storage[i,"samp.mean"] <- mean(draws)
  storage[i,"samp.var"] <- var(draws)
}

# ...or even more succinctly...
for(i in 1:sims){
  draws <- sample(x = answers, size = 10, replace = TRUE, prob = probs)
  storage[i,] <- c(mean(draws), sd(draws))
}

#
# 4: simulating sampling distributions with for loops
#

# Here's our plan of attack:
# 1. find some data that represents a complete population (the population!)
# 2. define a quantity of interest (the parameter)
# 3. come up with a couple plausible estimators (the estimators)
# 4. draw a sample and plug into estimator (the estimate)
# 5. repeat this step a lot using a for loop (to build the simulated sampling distribution)

# Once we have done this we can examine the simulated sampling distributions 
# to see how our estimators perform.  

# 1. our data: total funds raised by every single D house member for 
#    2006 campaign, in thousands of dollars.   
load(url("http://isites.harvard.edu/fs/docs/icb.topic636824.files/Section%202/housedems06.RData"))

# let's subset the data to make it a neat numeric vector
receipts <- house$receipts

# what does it look like?
plot(density(receipts))

# 2: define a quantity of interest: we might be interested in the 
#    average amount raised by each democratic member.  

# because we have the whole population at our hands, we actually 
# know the population parameter:
mean(receipts)

# let's store this for later
true.mean <- mean(receipts)

# 3: develop a couple plausible estimators: now the thought 
#    experiment begins.  What if I didn't know the true 
#    parameter value, but I had a research budget which 
#    permitted me to audit the fundraising of 10 D-House 
#    members?  How would I estimate the population average?

# estimator 1: the obviuos estimator is the sample average,
#  which we showed in lecture has nice average properties.    

# estimator 2: a non-obvious choice would be to use the 
# sample median as an estimator for the population average.

# both of these estimators take the sample and run it through 
# a function and then spit out an estimate.  

# Q: when is the estimate a random variable?    

# 4: draw a sample and plug into the estimators
set.seed(111)
draw <- sample(receipts, size = 10)
mean(draw)
median(draw)

# 5: repeat this a lot using a for loop
# let's draw 2000 samples of size n = 10, and calculate each statistic 

sims <- 2000
n <- 10
storage <- matrix(data = NA, nrow = sims, ncol = 2)
colnames(storage) <- c("mean","median")
head(storage)

for(i in 1:sims){
  draws <- sample(receipts, size = n)
  storage[i,] <- c(mean(draws), median(draws))
}


# now we can examine our sampling distributions graphically...
summary(storage)

par(mfrow=c(1,2))
hist(storage[,"mean"], col =  "cornsilk2", main = "estimator 1: samp. mean")
abline(v = true.mean, lwd = 3) # this plots the true parameter value

hist(storage[,"median"], col = "darkolivegreen1", main = "estimator 2: samp. median")
abline(v = true.mean, lwd = 3) # this plots the true parameter value

# let's try this again, but this time using 5000 simulations,
# and taking a sample of size n=100 each round...
sims <- 5000
n <- 100
storage <- matrix(data = NA, nrow = sims, ncol = 2)
colnames(storage) <- c("mean","median")
head(storage)

for(i in 1:sims){
  draws <- sample(receipts, size = n)
  storage[i,] <- c(mean(draws), median(draws))
}

par(mfrow=c(1,2))
hist(storage[,"mean"], breaks = 50, probability = TRUE, col =  "cornsilk2", 
     main = "estimator 1: samp. mean", xlab = "Estimate", xlim = c(750, 1500))
abline(v = true.mean, lwd = 3)
my.seq <- seq(750, 1500, 1)  
	# this creates a sequence covering the approx domain of estimates
lines(my.seq, dnorm(my.seq, mean = mean(storage[,"mean"]), sd = sd(storage[,"mean"]))) 
	# this plots a series of points 

hist(storage[,"median"], breaks = 30, probability = TRUE, col = "darkolivegreen1", 
     main = "estimator 2: samp. median", xlim = c(750, 1500), xlab = "Estimate" )
abline(v = true.mean, lwd = 3)
lines(my.seq, dnorm(my.seq, mean = mean(storage[,"median"]), sd = sd(storage[,"median"]))) 

############
############

# here's a quick primer on the commands I used in the hist() function
	# 1. the first argument is always the data you wish to plot
	# 2. breaks tells hist() either where to create bars to approximately how many 
	# bars to create
	# 3. probability = TRUE plots a density histogram rather than a frequency 
	# histogram
	# 4. col = "" filss in the histogram bars
	# 5. main = "" gives an overall title
	# 6. xlim/ylim = c(# ,#) gives the domain of the plotting
	# 7. xlab/ylab = "" gives labels for the axes.  


###############
