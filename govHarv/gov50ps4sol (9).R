
####################################

##  Gov 50 - Problem Set 4

##  Sample Code

####################################



##   Problem 1

satvec <- c(730, 550, 510, 570, 620, 500, 630, 480, 480, 620, 750, 720)
mean(satvec)
sd(satvec)

mean(satvec) - qt(.95, 11)*sd(satvec)/sqrt(12)
mean(satvec) + qt(.95, 11)*sd(satvec)/sqrt(12)

##   Problem 3
gni <- read.csv("H:/gni.csv")
#  Calculate the mean for each year and then take the difference in means:

mean2004 <- mean(gni$Y2004)
mean2003 <- mean(gni$Y2003)
diffmeans <- mean2004 - mean2003
diffmeans

#  Look at the standard deviations within each group; they are close this time, but
#  to be safe we will calculate the standard error assuming unequal variances.

sd(gni$Y2004)
sd(gni$Y2003)
se.diffmeans <- sqrt(var(gni$Y2004)/length(gni$Y2004) + var(gni$Y2003)/length(gni$Y2003))
se.diffmeans

#  Construct a 95\% confidence interval; we will use the standard normal reference
#  distribution because the sample size is relatively large.

ci.lower <- diffmeans - qnorm(.975)*se.diffmeans
ci.upper <- diffmeans + qnorm(.975)*se.diffmeans
c(ci.lower, ci.upper)
t.test(gni$Y2004, gni$Y2003, var.equal=FALSE)


#  Now, calculate the differences within each country and take the mean difference:

meandiff <- mean(gni$Y2004 - gni$Y2003)
meandiff

#  Find the standard error of the mean difference:

se.meandiff <- sqrt(var(gni$Y2004 - gni$Y2003)/length(gni$Y2004))
se.meandiff

#  Construct a 95\% confidence interval; again, we will use the standard normal reference
#  distribution because the sample size is relatively large.

ci.lower <- meandiff - qnorm(.975)*se.meandiff
ci.upper <- meandiff + qnorm(.975)*se.meandiff
c(ci.lower, ci.upper)
t.test(gni$Y2004 - gni$Y2003)

###  Problem 4

flcount <- read.csv("C:/datasets/Buchanan.csv")
attach(flcount)

###  Calculate proportions in PBC

pbcvoteprop <- TotalPresBuch[50]/TotalPresVotes[50]
pbcabsprop <- TotalAbsBuch[50]/TotalAbsVotes[50]

pbcvoteprop - pbcabsprop
sqrt(pbcvoteprop*(1 - pbcvoteprop)/TotalPresVotes[50] +
pbcabsprop*(1 - pbcabsprop)/TotalAbsVotes[50])

###   Calculate proportions in Broward

browvoteprop <- TotalPresBuch[6]/TotalPresVotes[6]
browabsprop <- TotalAbsBuch[6]/TotalAbsVotes[6]

browvoteprop - browabsprop
sqrt(browvoteprop*(1 - browvoteprop)/TotalPresVotes[6]+
browabsprop*(1 - browabsprop)/TotalAbsVotes[6])

###  Calculate difference in differences

diffindiff <- (pbcvoteprop - pbcabsprop) - (browvoteprop - browabsprop)

se.diffindiff <- sqrt(pbcvoteprop*(1 - pbcvoteprop)/TotalPresVotes[50] +
pbcabsprop*(1 - pbcabsprop)/TotalAbsVotes[50] + 
browvoteprop*(1 - browvoteprop)/TotalPresVotes[6]+
browabsprop*(1 - browabsprop)/TotalAbsVotes[6])

##   Multiply by total in-person vote in PBC

(diffindiff - 2*se.diffindiff)*TotalPresVotes[50]
(diffindiff + 2*se.diffindiff)*TotalPresVotes[50]


