# ---------------------------------------------------------------------
# 6 Jan 2011
#	IBS R workshop
#	Instructor: Frank Witmer
#	Course website: http://www.colorado.edu/ibs/crs/training.html
#
# This is an introductory script for learning some of the looping
#	and simulation capabilities of R.  There are short exercises 
#	embedded and longer exercises at the end.
#
# As we go through this file, feel free to add additional comments
#	and test additional R commands.
#
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# simple loop examples
# ---------------------------------------------------------------------
# for loop
for (i in 1:10) {
	# print statements are a good way to debug your code (i.e. see what's going on)
	print(paste("iteration number:", i))
	#browser()	# pause execution to check variable values
}	
print(paste("iteration number:", 1:10))	# can also include sequences within a print statement

# while loop version 
i<-1
while (i <= 10) {
	print(paste("iteration number:", i))
	#flush.console()	# forces the print statement to appear in the console during execution
}
# what's wrong with this loop? run it to check your suspicion
#	answer at the end of file


# Fibonacci numbers
numFib <- 30
fib <- numeric(numFib)	# declare a numeric vector of length numFib
#fib <- NULL		# can also just declare an empty object, and add to it dynamically, but this is slower
fib[1:2] <- 1	# initialize the sequence
for (i in 3:numFib) {
	fib[i] <- fib[i-2] + fib[i-1]
}
fib

# ---------------------------------------------------------------------
# lapply & sapply examples
# ---------------------------------------------------------------------
numList<-list(x=1:10,y=rnorm(100, 5, 5), z=rpois(100, 45))
lapply(numList, mean)
sapply(numList, mean)

txt<-c("IBS","stat","Rcourse")
lapply(txt, nchar)
sapply(txt, nchar)


# ---------------------------------------------------------------------
# another loop & tapply example with real data
# ---------------------------------------------------------------------
# set working directory for reading files
workingDir <- "C://Users/witmer/Documents/SoftwareReference/R/IBS_R_ShortCourse/"
events<-read.csv(paste(workingDir, "caucasusEvents.csv", sep=""))
str(events)
names(events)
plot(events$x_utm, events$y_utm)	# crude visualization of the spatial distribution of violence
table(events$category)		# count number of events in each category
length(events$event_id[events$category == "Arrest"])	# could also find for each category by subsetting

# can use a naive for loop to count the total number killed/injured by category...
arrCnt<-0
milCnt<-0
polCnt<-0
rebCnt<-0
for(i in 1:length(events$event_id)) {
	if (events$category[i] == "Arrest") {
		arrCnt <- arrCnt + events$tot_kll_in[i]
	} else if (events$category[i] == "Military") {
		milCnt <- milCnt + events$tot_kll_in[i]
	} else if (events$category[i] == "Police") {
		polCnt <- polCnt + events$tot_kll_in[i]
	} else if (events$category[i] == "Rebels") {
		rebCnt <- rebCnt + events$tot_kll_in[i]
	} else {	# it's good practice to put error-catching statements here
		print(paste("for i = ",i,", invalid category: ", events$category, sep=""))
	}
}
arrCnt
milCnt
polCnt
rebCnt

# ...but easier & faster to use tapply
tapply(events$tot_kll_in, events$category, sum)
tapply(events$tot_kll_in, events$category, range)

# find the means of selected columns
lapply(events[,c(6,9,10)], FUN=mean)
sapply(events[,c(6,9,10)], FUN=mean)

# ---------------------------------------------------------------------
# Exercise: re-write the last sapply statement using the column names instead of numbers
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Exercise: Find the mean latitude and longitude for each boston TOWN 
#			using the census tract coordinates
boston<-read.csv(paste(workingDir, "boston.csv", sep=""))
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# simulate tossing a coin
# ---------------------------------------------------------------------
coin <- c(1, 0)	# 1 is for heads; 0 is for tails
# function that returns the number of heads
coinToss <- function(numTosses){
	results <- numeric(numTosses)
	for (i in 1:numTosses){
		results[i] <- sample(coin, 1, replace = ?)	# should replace be T or F?
	}
	sum(results)	# the last value of a function is returned
}
coinToss(6)

# ---------------------------------------------------------------------
# simulate a political poll
# ---------------------------------------------------------------------
voters <- rep(0, 1000000)	# create a million voters
voters[1:570000] <- 1		# 57% support a given party, designate with a 1
# randomly select 100 people to participate in the poll
poll <- sample(voters, 100, replace = ?)	# replacement?
sum(poll)/length(poll)*100	# tally the results of the poll

# Exercise: run a new poll and this time sample 2500 voters


# ---------------------------------------------------------------------
# bootstrapping
# ---------------------------------------------------------------------
boston<-read.csv(paste(workingDir, "boston.csv", sep=""))
tmp<-fix(boston)	# check to make sure the data were read in properly
names(boston)

# ---------------------------------------------------------------------
# bootstrap estimate for the mean
# ---------------------------------------------------------------------
mean(boston$MEDV)
numBoot <- 1000
bootMeans<-numeric(numBoot)
for(i in 1:numBoot) {
	medvSample <- sample(boston$MEDV, replace=T)	# default sample size = length of input
	bootMeans[i] <- mean(medvSample)
}
quantile(bootMeans, c(0.025, 0.975))
hist(bootMeans)
abline(v=mean(boston$MEDV))

# ---------------------------------------------------------------------
# compare this result to using the 'boot' library [can skip this if time is short]
# ---------------------------------------------------------------------
library(boot)
# to use the boot library, you must write a function like this where x is the data, and d are the bootstrap indices
sampleMean <- function(dat, inds) {
  return(mean(dat[inds]))
}
bootMns <- boot(boston$MEDV, statistic=sampleMean, R=100)
bootMns$t[,1]
quantile(bootMns$t[,1], c(0.025, 0.975))
plot(bootMns)	# one advantage of using the boot library is that you get a nice default plot

# ---------------------------------------------------------------------
# residual resampling bootstrapping
# ---------------------------------------------------------------------
# fit the original linear model
mylm<-lm(log(MEDV) ~ RM + LSTAT + CRIM + ZN + CHAS + DIS, data=boston)
summary(mylm)
mylm$coef		# look at just the coefficients
residuals(mylm)	# look at the model residuals
mylm$resid		# same as residuals()

numBoot <- 1000
numCoefs <- 7	# num indep vars + 1 for the constant
# setup the xVars for use in the bootstrap loop
xVars <- model.matrix( ~ RM + LSTAT + CRIM + ZN + CHAS + DIS, data=boston)[,-1]	# -1 removes the first column

#tmp<-fix(xVars)
bCoefResid <- matrix(0, numBoot, numCoefs)
colnames(bCoefResid)<-names(mylm$coef)		# set the colnames to reduce errors in referencing variables
for(i in 1:numBoot) {
	sampleIndices <- sample(length(boston$OBS), replace=T)
	newY <- predict(mylm) + residuals(mylm)[sampleIndices]	# create new dep. var using sampled residuals
	# could also sample the residuals directly:
	#newY <- predict(mylm) + sample(residuals(mylm), replace=T)
	newlm <- lm(newY ~ xVars)
	bCoefResid[i,] <- newlm$coef
}
#tmp<-fix(bCoefResid)
quantile(bCoefResid[,4], c(0.025, 0.975))
quantile(bCoefResid[,"CRIM"], c(0.025, 0.975))	# better subsetting style
plot(density(bCoefResid[,"CRIM"]), xlab="Coefficient of CRIM", main="Bootstrap distribution of CRIM with 95% CI")
abline(v=quantile(bCoefResid[,"CRIM"], c(0.025, 0.975)))



# ---------------------------------------------------------------------
# Exercise 1: re-write the Fibonacci sequence generator such that it stops 
#		when the value exceeds 10 million
#
# ---------------------------------------------------------------------
#
# Exercise 2: write a loop to simulate 100 sets of 6 coin tosses. Store
#	the results and make a histogram of them
#
# ---------------------------------------------------------------------
#
# Exercise 3a: Re-estimate the beta coefficients in the boston regression model
#			using data resampling bootstraps
#	Here are the steps for regression data resampling:
#		i) Randomly select samples of size n from the original data (complete 
#			cases) with replacement
#		ii) For each bootstrap sample, estimate the regression coefficients
#		iii) Repeat steps i) & ii) 1000 or more times
#
# Exercise 3b: Compare your bootstrap results to the original regression 
#		coefficients using abline()
#
# Exercise 3c: Use a for loop to compare all coefficients from your two 
#		regression bootstrap results. Use quantile() within the loop
#		and print your results to the console
#
# Bonus Exercise: Write a function that can be used with the boot package
#		to estimate regression parameters using data resampling
# ---------------------------------------------------------------------



# ---------------------------------------------------------------------
# Answers
# ---------------------------------------------------------------------

#### while loop fix (hit ESC to cancel the infinite loop above)
i<-1
while (i<=10) {
	print(paste("iteration number:", i))
	i <- i + 1			# must increment i!
}

#### sapply re-write
sapply(events[,c(6,9,10)], FUN=mean)
sapply(events[,c("tot_kll_in","y_utm","x_utm")], FUN=mean)


#### boston mean lat/lon by town exercise
tapply(boston$LAT, boston$TOWN, mean)
tapply(boston$LON, boston$TOWN, mean)
# bonus: save the results in a new data structure...
meanCoords<-cbind(tapply(boston$LAT, boston$TOWN, mean), tapply(boston$LON, boston$TOWN, mean))
# (note that if the rows don't match up, use merge() instead of cbind()
colnames(meanCoords) <- c("LAT","LON")
tmp<-fix(meanCoords)

#### Exercise 1. Fibonacci numbers 
fib <- NULL		# declare an empty object and add to it dynamically
fib[1:2] <- 1	# initialize the sequence
i <- 2
while (fib[i] < 10000000) {
	i <- i + 1
	fib[i] <- fib[i-2] + fib[i-1]
}
fib

#### Exercise 2. Hint: use a for loop and the coinToss function from above
numSims <- 100
results <- numeric(numSims)
for(i in 1:numSims) {
	results[i] <- coinToss(6)	
}
hist(results)
hist(results, breaks=0:6)	# a little cleaner looking

#### Exercise 3a. Data resampling bootstrap
numBoot <- 1000
numCoefs <- 7	# num indep vars + 1 for the constant
bCoefData <- matrix(0, numBoot, numCoefs)
colnames(bCoefData)<-names(mylm$coef)		# set the colnames to reduce errors in referencing variables
for(i in 1:numBoot) {
	sampleIndices <- sample(length(boston$OBS), replace=T)
	sampledData <- boston[sampleIndices,]
	bootlm<-lm(log(MEDV) ~ RM + LSTAT + CRIM + ZN + CHAS + DIS, data=sampledData)
	bCoefData[i,] <- bootlm$coef
}
quantile(bCoefData[,"CRIM"], c(0.025, 0.975))
plot(density(bCoefData[,"CRIM"]), xlab="Coefficient of CRIM", main="Data resampling bootstrap of CRIM with 95% CI")
abline(v=quantile(bCoefData[,"CRIM"], c(0.025, 0.975)))

#### Exercise 3b. Comparison to original results
plot(density(bCoefData[,"CRIM"]), xlab="Coefficient of CRIM", main="Data resampling bootstrap of CRIM with 95% CI")
abline(v=mylm$coef["CRIM"])

#### Exercise 3c. Compare regression bootstrap results
quantile(bCoefResid[,"CRIM"], c(0.025, 0.975))
quantile(bCoefData[,"CRIM"], c(0.025, 0.975))
# can write a for loop to compare all
for(i in 1:length(bCoefData[1,])) {
	ci95 <- quantile(bCoefResid[,i], c(0.025, 0.975))
	print(paste("95% CI for residual bootstrap ", colnames(bCoefResid)[i], ", (", format(ci95[1], digits=3), ", ",format(ci95[2], digits=3), ")", sep=""))
	ci95 <- quantile(bCoefData[,i], c(0.025, 0.975))
	print(paste("95% CI for data bootstrap ", colnames(bCoefData)[i], ", (", format(ci95[1], digits=3), ", ",format(ci95[2], digits=3), ")", sep=""))
}

#### Bonus exercise
sampleCoeff <- function(dat, inds) {
	sampledData <- dat[inds,]
	bootlm<-lm(log(MEDV) ~ RM + LSTAT + CRIM + ZN + CHAS + DIS, data=sampledData)
	bootlm$coef
}
bootCoeffs <- boot(boston, statistic=sampleCoeff, R=100)
bootCoeffs$t[,4]	# column names are lost, so have to use numeric indexing
# compare CRIM to above results
quantile(bootCoeffs$t[,4], c(0.025, 0.975))
quantile(bCoefData[,"CRIM"], c(0.025, 0.975))

