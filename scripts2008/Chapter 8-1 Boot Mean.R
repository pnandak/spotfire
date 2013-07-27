#Set Seed
set.seed(43123)

#Our Fake Data
data <- as.matrix(c(13,31,37,18, -3))
n <-length(data)

#The number of bootstrap resamples
B <- 1000
#A Storage Matrix
avg.bs <- matrix(NA,B,1)

#Example of sample command
sample(data,5,replace=T)
sample(data,5,replace=T)
sample(data,5,replace=T)

#Now Let's do the Resampling
 for(b in 1:B){ 
 #First take a resample
 resample <- sample(data,5,replace=T)
 #Now apply function. Here the mean and store it.
 avg.bs[b,] <- mean(resample)
 drop(resample)
 }
 
#Now Calc Overall Bootstrap Mean
mean.boot <- mean(avg.bs)
#Calc Unadjusted Bootstrap Standard Error
sqd <- (avg.bs - mean.boot)^2
se.boot <- sqrt(sum(sqd)/(B-1))
#Now Sample Size Adjusted Standard Error
se.adj <- sqrt(n/(n-1)) * se.boot


cat("The Classical Estimate of the Mean is:", mean(data),"\n")
cat("The Classical Estimate of the S.E is:", sd(data),"\n")

cat("The Bootstrap Estimate of the Mean is:", mean.boot,"\n")
cat("The Bootstrap Estimate of the S.E is:", se.boot,"\n")
cat("The Sample Size Adj. Bootstrap Estimate of the S.E is:", se.adj,"\n")


#Normal Theory Intervals
ci.normal <- c(mean.boot + 1.96*se.adj, mean.boot - 1.96*se.adj)

cat("Upper and Lower Normal Theory CI:", ci.normal, "\n")

#Check Normality Assumption
hist(avg.bs,probability=T); lines(density(avg.bs))
#These plots need work check hlm notes
qqnorm(avg.bs)
qqline(avg.bs)

#Figure 8.1
qqnorm(avg.bs)
qqline(avg.bs)

#Bootstrap Percentile Confidence Intervals
quantile(avg.bs,c(.025,.975))

#Bias Corrected CIs
library(boot)

durat <- as.data.frame(data)
mean.w <- function(x, w){
           sum(x*w)
           }
           
boot.out <- boot(data=durat, statistic=mean.w, R=1000, stype="w")
boot.ci(boot.out, conf = 0.95, type = "bca")
