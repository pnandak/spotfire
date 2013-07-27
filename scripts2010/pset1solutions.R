
#####################################################
## Gov2000
## Problem Set 1 Solutions
## Fall 2000
## prepared by Maya Sen (msen@fas)
#####################################################

## these solutions apply to pset 1, problem 4,
## which walked you through manipulating the lalonde dataset.

data <- read.table(file = "lalonde.txt", header = TRUE)
	## loads the data

########
## A  ##
########

data.subset <- data[,c(1,7,8,9,10)]
	## subsets the data to just those cols we are interested in

probA <- matrix(data = NA, nrow = 5, ncol = ncol(data.subset))
	## creates a holder matrix

colnames(probA) <- names(data.subset)
rownames(probA) <- c("Mean", "Maximum", 
	"Minimum", "Variance", "Standard Deviation")
		## gives names to rows and cols in the holder matrix

## now, a simple for-loop gets us our desired values
for(i in 1:ncol(data.subset)){
	probA[1,i] <- mean(data.subset[,i])
	probA[2,i] <- max(data.subset[,i])
	probA[3,i] <- min(data.subset[,i])
	probA[4,i] <- var(data.subset[,i])
	probA[5,i] <- sd(data.subset[,i])
}

probA
	## and we can run xtables on this if we wish

library(xtable)
xtable(probA, caption = "Problem A", digits = 2)
	## note that the digits command rounds the 
	## decimals to a neat number

########
## B  ##
########

black.subset <- data[data$black == 1, c(1,7,8,9,10)]
	## we subset the data to just blacks

probB <- matrix(data = NA, nrow = 5, ncol = ncol(data.subset))
	## and create a holder matrix
colnames(probB) <- names(black.subset)
rownames(probB) <- c("Mean", "Maximum", 
	"Minimum", "Variance", "Standard Deviation")

## now, we just run the same simple for loops to get our values
for(i in 1:ncol(black.subset)){
	probB[1,i] <- mean(black.subset[,i])
	probB[2,i] <- max(black.subset[,i])
	probB[3,i] <- min(black.subset[,i])
	probB[4,i] <- var(black.subset[,i])
	probB[5,i] <- sd(black.subset[,i])
}

probB

xtable(probB, caption = "Problem B", digits = 2)
	## and then use xtable to finish the problem

########
## C  ##
########


	## use this command to out two tables one below
	## the other for useful comparison.


pdf(file= "problemC.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
par(mfrow = c(2,1))
hist(data$educ[data$black != 1], col = "coral", xlim = c(min(data$educ),max(data$educ)),
	main = "Education, Non-blacks", xlab = "Years of Education")
hist(data$educ[data$black == 1], col = "skyblue", xlim = c(min(data$educ),max(data$educ)),
	main = "Education, Blacks", xlab = "Years of Education")
dev.off()

## looks like the non-blacks in the program are much more educated
## than the blacks. The disparity is VERY striking, and should probably
## be investigated further.

########
## D  ##
########

data[392,]
	## pulls up obs 392
	## seeing a high school dropout who earns $60k per
	## year (in 1978!!) even though he earned next to nothing in 1974
	## and 1975 should set some warning bells off.

	## in addition, this person's 1978 earning
	## makes him the maximum earner that year by A LOT.
xtable(data[392,])

sort(data$re78, decreasing = TRUE)

	## this would be a good candidate to examine more
	## closely; perhaps there's measurement error going on.

########
## E  ##
########

# pt A

sq.fn <- function(x){
	out <- x*x
	return(out)
}

## to check

sq.fn(x = 4) == 4^2
sq.fn(x=8) == 8^2

# pt B

mean.fn <- function(x){
	denominator <- sum(x)
	numerator <- length(x)
	out <- denominator/numerator
	return(out)
}

## to check

a <- c(1,2,3,4,5,6,7,8,9,10)

mean.fn(x = a) == mean(a)

## now to run on the lalonde data

mean.fn(data$educ) == mean(data$educ)

########
## D  ##
########

mean(data$re78[data$t == 1])
mean(data$re78[data$t == 0])

mean(data$re78[data$t == 1])-mean(data$re78[data$t == 0])

## the mean for the 1978 earnings looks to be quite a bit
## higher for those who completed the treatment.

## But let's explore the other variables a bit.
## (Note -- there are a lot ways of doing this,
## for example by using histograms and plots!)

mean(data$educ[data$t == 1])
mean(data$educ[data$t == 0])
	## education is higher for treated units

mean(data$re75[data$t == 1])
mean(data$re75[data$t == 0])
	## so were their 1975 wages

mean(data$black[data$t == 1])
mean(data$black[data$t == 0])
	## there way fewer blacks, a big
	## problem if you think there might have been
	## wage discrimination in the 1970s.

mean(data$hispanic[data$t == 1])
mean(data$hispanic[data$t == 0])
	## and fewer hispanics as well, ditto.

mean(data$age[data$t == 1])
mean(data$age[data$t == 0])
	## they are also a little bit older
	## and so maybe have more experience

## in sum, the differences in the 1978 earning
## could be due to the training that the treated units
## received. But it could also be due to the differences in 
## the other demographic variables. We don't yet know!
## it would be VERY premature to say that there's a causal effect here.


