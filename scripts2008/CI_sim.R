#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-23-02                                                    #
# UPDATE: 12-7-03, 12-3-06, 12-13-07                                #
# PURPOSE: C.I. pi simulation                                       #
#                                                                   #
# NOTES: See plot on p. 107 of Brown, et al. (2001)                 #
#####################################################################

############################################################
# Do one time

n<-50
pi<-0.184
sum.y<-rbinom(n = 1, size = n, prob = pi)
sum.y

alpha<-0.05
p<-sum.y/n

#Wald C.I.
lower<-p-qnorm(p = 1-alpha/2)*sqrt(p*(1-p)/n)
upper<-p+qnorm(p = 1-alpha/2)*sqrt(p*(1-p)/n)
cat("The Wald C.I. is:", round(lower,4) , "<= pi <=", round(upper,4), "\n \n")


###########################################################
# Repeate 10,000 times
numb<-10000

n<-50
pi<-0.184
set.seed(4516)
sum.y<-rbinom(n = numb, size = n, prob = pi)

alpha<-0.05
p<-sum.y/n

#Wald C.I.
lower<-p-qnorm(p = 1-alpha/2)*sqrt(p*(1-p)/n)
upper<-p+qnorm(p = 1-alpha/2)*sqrt(p*(1-p)/n)
#data.frame(lower, upper)

save<-ifelse(test = pi>lower, yes = ifelse(test = pi<upper, yes = 1, no = 0), no = 0)
coverage<-mean(save)
cat("An estimate of the true confidence level is:", round(coverage,4), "\n")

#Quicker way than to use the ifelse() functions above
sum((pi>lower) & (pi<upper))/numb


##############################################################
# Using the binom package

library(binom)
save.ci<-binom.confint(x = sum.y, n = n, conf.level = 1-alpha, methods = "asymptotic") #Wald
head(save.ci) #print first few intervals
save<-ifelse(pi>save.ci$lower, ifelse(pi<save.ci$upper, 1, 0), 0)
coverage<-sum(save)/numb
cat("The true confidence level is:", round(coverage,4), "\n")

#Direct use of a function in the package
binom.coverage(p = pi, n = 50, conf.level = 0.95, method = "asymptotic")
#Expected length
binom.length(p = pi, n = 50, conf.level = 0.95, method = "asymptotic")

#This finds a plot of the true confidence level
#  Method corresponds to the type of C.I. used.  Note that this corresponds
#  to the actual function name, not the option in binom.confint(), that calculates
#  the interval.  For example, use binom.asymp, not asymptotic, for the Wald interval.
#  See all of the binom.____ function names in the binom package help for a list of the names.
win.graph(width = 6, height = 6, pointsize = 10)
binom.plot(n = c(10,20, 30, 50), method = binom.asymp, np = 500, conf.level = 0.95, type = "xyplot",
           ylim = c(0.86, 1), ylab = "True confidence level", xlab = expression(pi))
#NOTE: I am not sure why the dotted and dashed lines appear in the plot


#Alternative way to find a plot of the true confidence level
win.graph(width = 6, height = 6, pointsize = 10)
plot(y = binom.coverage(p = seq(from = 0.005, to = 0.995, by = 0.005), n = 50, conf.level = 0.95, method = "asymptotic")[,4],
     x = seq(from = 0.005, to = 0.995, by = 0.005), ylim = c(0.86, 1), ylab = "True confidence level", xlab = expression(pi),
     type = "l", col = "red", main = "True confidence level plot for Wald C.I. and n = 50", 
     panel.first = grid(col = "gray", lty = "dotted"))
abline(h = 0.95, lwd = 2)
     

#
