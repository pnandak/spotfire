#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  9-21-03                                                    #
# Purpose: Johnson p.398 example                                    #
#                                                                   #
# NOTES:                                                            #
#####################################################################

set1<-c(1, 3, 5, 2, 3, 5, 1, 4, 6, 1, 4, 4, 3, 4, 7, 2, 5, 6)
set2<-matrix(set1, 6,3, byrow=T)
set2

p<-ncol(set2)
N<-nrow(set2)

sigma.0<-matrix(c(2,1,0,1,2,0,0,0,1),3,3)
sigma.0

#There is no built in R function to calculate a trace so I wrote a function
tr<-function(x) {
    sum(diag(x))
}


sigma<-var(set2)

delta<-exp(p*(N-1)/2) * (det(sigma)^((N-1)/2)) * (det(sigma.0)^(-(N-1)/2)) * exp(-0.5*(N-1)*tr(solve(sigma.0)%*%sigma))

#Calculate Johnson's value - note he uses lambda (I will call it delta.johnson)
Q<-solve(sigma.0)%*%sigma*(N-1)
delta.johnson<-(exp(1)/(N-1))^(p*(N-1)/2) * det(Q)^((N-1)/2) * exp(-0.5*tr(Q))

stat<--2*log(delta)
crit.val<-qchisq(0.95, 0.5*p*(p+1))
p.value<-1-pchisq(stat, 0.5*p*(p+1))


stat 
crit.val
p.value
 
delta.johnson
    
#############################################################################
#Example 10.2

delta <- det(sigma)/(1/p * tr(sigma))^p
stat <-  - (N - 1 - ((2 * p^2 + p + 2)/(6 * p))) * log(delta)
crit.val <- qchisq(0.95, 0.5 * p * (p + 1) - 1)
p.value <- 1 - pchisq(stat, 0.5 * p * (p + 1) - 1)
stat
crit.val
p.value
