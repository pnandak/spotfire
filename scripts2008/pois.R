#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-19-01                                                   #
# UPDATE: 1-3-03, 12-7-03, 1-6-05, 12-2-06, 12-13-07                #
# PURPOSE: Adventures with the Poisson distribution                 #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#P(Y=1) when mu=5
5^1*exp(-5)/factorial(1)

#P(Y=1) when mu=5
dpois(x = 1, lambda = 5)

#P(Y=1) when mu=5
dpois(1, 5)

#P(Y<=1) when mu=5
ppois(q = 1, lambda = 5)
saveit<-ppois(q = 1, lambda =5)
saveit


#Create a plot of the PDF
save<-dpois(x = 0:20, lambda = 5)
Y<-0:20
plot(x = Y, y = save, main = "Poor plot of a P(Y=y)= mu^y * (e^-mu) / y!")


#Another version of the plot
plot(x = Y, y = save, type = "h", main = "Better plot of a P(Y=y)= mu^y * (e^-mu) / y!",
     xlab = "y", ylab = "f(y)", panel.first=grid(col="gray", lty="dotted") )
abline(h = 0)

#Another version of the plot with use of expression() to make the title look nicer
plot(x = Y, y = save, type = "h", main = expression(paste("Better plot of a P(Y=y) = ", frac(mu^y*e^-mu, "y!"))),
     xlab = "y", ylab = "f(y)", panel.first=grid(col="gray", lty="dotted") )
abline(h = 0)


#print probability distribution
save

#Make the printing look a little nicer
data.frame(Y = 0:20, prob = round(save,4))





####################################################################
# ANOTHER way to do the plot
#Make the printing look a little nicer
save<-data.frame(Y = 0:20, prob = round(dpois(x = 0:20, lambda = 5),4))
save

plot(x = save$Y, y = save$prob, type = "h", main = "Better plot of a P(Y=y)= mu^y * (e^-mu) / y!",
     xlab = "Y", ylab = "f(y)", panel.first=grid(col="gray", lty="dotted") )
abline(h = 0)




###################################################################

#Simulate observations from a Poisson distribution
set.seed(9954)
pois5<-rpois(n = 1000, lambda = 5) 
pois5[1:20]

mean(pois5)
var(pois5)

hist(x = pois5, main = "Poisson with mu=5, n=1000", xlab = "Y")

#Also works for the plot here, but be careful because "0" categories will not show up
barplot(height = table(pois5))

#Way to overlap the true distribution and the histogram
x<-c((min(pois5)-1):(max(pois5)+1))
x<-(min(pois5)-1):(max(pois5)+1)
plot(x = x, y = dpois(x, lambda = 5), main = "Histogram with Poisson PDF overlay", ylab = "Probability", 
     xlab="Y", type = "h", col = "red", ylim = c(0, 0.2), lwd = 3) 
hist(x = pois5, freq = FALSE, add = TRUE, breaks = (x+0.5)) 


x<-0:(max(pois5)+1)
plot(x = x, y = dpois(x, lambda = 5), main = "Histogram with Poisson PDF overlay", ylab = "Probability", 
     xlab="Y", type = "h", col = "red", ylim = c(0, 0.2), lwd = 3) 
hist(x = pois5, freq = FALSE, add = TRUE, breaks = (x-0.5)) 



#Check autocorrelation
#library(ts)
par(mfrow = c(2,1))
ts.plot(pois5)
temp<-acf(pois5, type = "correlation")
names(temp)
temp$acf
