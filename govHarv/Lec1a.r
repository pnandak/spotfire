### R code for Lecture 1 - Jan 31 ###


### create a vector

votes <- c("Clinton", "Clinton", "Obama", "Obama", "Other")

### the sample function ###

?sample

sample(votes,size=5,replace=FALSE)

sample(votes,size=5,replace=TRUE)

sample(votes,size=2,replace=FALSE)

### Another function for creating vectors ###

votes <- rep("Clinton",6)
votes

### putting both vector creation functions together ###

votes <- c(rep("Clinton",3),rep("Obama",4))


### logical vectors ###

votes == "Clinton"
sum(votes == "Clinton")
mean(votes == "Clinton")

### "New Hampshire" Simulation (Suppose we know the truth) ###

votes <- c(rep("Clinton",112251),rep("Obama",104772), rep("Other",67081) )
mean(votes == "Clinton")

poll <- sample(votes,size=600,replace=FALSE)

mean(poll == "Clinton")

## What if we took a bunch of polls? (Repeated Sampling) ##
set.seed(18203481)
par(mfrow=c(1,2))

numberOfPolls <- 1500
pollResultsClinton <- numeric()
pollResultsObama <- numeric()
for(i in 1:numberOfPolls){
	poll <- sample(votes,size=600,replace=FALSE)
	pollResultsClinton[i] <- mean(poll == "Clinton")
	pollResultsObama[i] <- mean(poll == "Obama")


if(i%in%(1:30*50)){
	hist(pollResultsClinton,col="blue",prob=TRUE, xlim=c(.25, .5))
	hist(pollResultsObama,col="blue",prob=TRUE, xlim=c(.25, .5))
	}

}

## What do you notice about this histogram? ##


hist(pollResultsClinton,col="blue",prob=TRUE, xlim=c(.25, .5))
abline(v = mean(votes == "Clinton"),col="red",lwd=2)

hist(pollResultsObama,col="blue",prob=TRUE, xlim=c(.25, .5))
abline(v = mean(votes == "Obama"),col="red",lwd=2)



## code for graphs

pdf("ClintonObama1.pdf", width=12)
par(mfrow=c(1,2))
hist(pollResultsClinton,prob=TRUE, xlim=c(.25, .5))
hist(pollResultsObama,prob=TRUE, xlim=c(.25, .5))
dev.off()


pdf("ClintonObama2.pdf", width=12)
par(mfrow=c(1,2))
hist(pollResultsClinton,prob=TRUE, xlim=c(.25, .5))
abline(v = .31,col="red",lwd=2)

hist(pollResultsObama,prob=TRUE, xlim=c(.25, .5))
abline(v = .40 ,col="red",lwd=2)
dev.off()

pdf("ClintonObama3.pdf", width=12)
par(mfrow=c(1,2))
hist(pollResultsClinton,prob=TRUE, xlim=c(.25, .5))
abline(v = .31,col="red",lwd=2)
abline(v = .30,col="blue",lwd=2)

hist(pollResultsObama,prob=TRUE, xlim=c(.25, .5))
abline(v = .40 ,col="red",lwd=2)
abline(v = .39,col="blue",lwd=2)
dev.off()

pdf("ClintonObama4.pdf", width=12)
par(mfrow=c(1,2))
hist(pollResultsClinton,prob=TRUE, xlim=c(.25, .5))
abline(v = .31,col="red",lwd=2)
abline(v = .30,col="blue",lwd=2)
abline(v=.28, col="green", lwd=2)


hist(pollResultsObama,prob=TRUE, xlim=c(.25, .5))
abline(v = .40 ,col="red",lwd=2)
abline(v = .39,col="blue",lwd=2)
abline(v=.36, col="green", lwd=2)
dev.off()


