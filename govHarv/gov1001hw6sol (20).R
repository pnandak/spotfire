
##  Problem 1

#  Calculate the proportions in January and February, and their difference:

time1 <- 880 
time2 <- 944
sample <- 1600

pi1 <- time1/sample
pi2 <- time2/sample
deltapi <- pi2 - pi1

pi1
pi2
deltapi

#  Calculate the standard error of the difference:

se.deltapi <- sqrt(pi1*(1-pi1)/sample + pi2*(1 - pi2)/sample)
se.deltapi

#  Construct a 99% confidence interval

ci.lower <- deltapi - qnorm(.995)*se.deltapi
ci.upper <- deltapi + qnorm(.995)*se.deltapi
c(ci.lower, ci.upper)

##  Problem 3

#  Load in the dataset and take the specified random samples:

salary <- read.csv("H:/200607.csv")
flint <- subset(salary, CAMPUS=="UM_FLINT") 
annarbor <- subset(salary, CAMPUS=="UM_ANN-ARBOR") 
annarbor.idx <- sample(1:nrow(annarbor), 1000, replace=FALSE) 
flint.idx <- sample(1:nrow(flint), 200, replace=FALSE) 
annarbor.subset <- annarbor[annarbor.idx,] 
flint.subset <- flint[flint.idx,]
#    Find the critical value for the test; we use the standard normal reference distribution because the sample
#    size is large:
qnorm(.99) 
#    It is pretty clear that the variances are not equal in the two groups, so we will not assume equal variances:
sd(annarbor.subset$FTR)
sd(flint.subset$FTR)

#  We can use the t.test function to obtain the test statistic.
t.test(annarbor.subset$FTR, flint.subset$FTR, var.equal=FALSE) 

##   Problem 4
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