# A VERY SIMPLE LIKELIHOOD FUNCTION
#
# Define the likelihood function for binomial sample with N=2 and Y=1:
lklhd <- function(p) 2*p*(1-p)
# 
# Plot the likelihood function:
plot(lklhd,0,1,xlab="pi",ylab="l(p)",main="Binomial likelihood, N=2, Y=1")
# 
# Find the MLE (for multiparameter problems, we would use the nlm() function):
optimize(lklhd,c(0,1),maximum=TRUE)
# 
# What if N=10 and Y=7:
# 
lklhd <- function(p) dbinom(7,10,p)
plot(lklhd,0,1,xlab="pi",ylab="l(p)",main="Binomial likelihood, N=10, Y=7")
optimize(lklhd,c(0,1),maximum=TRUE)

