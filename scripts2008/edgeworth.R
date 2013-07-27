#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  11-15-07                                                   #
# UPDATE:                                                           #
# PURPOSE: Edgeworth example from p. 32-33 of Ferguson (1996)       #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################

t<-seq(from = -2, to = 2, by = 0.2) # Number of  stand. from mean
beta1<-2
beta2<-6
n<-5

#CLT approximation
clt.app<-pnorm(q = t, mean = 0, sd = 1)

#Edgeworth with 2 terms
E1<-pnorm(q = t, mean = 0, sd = 1) - beta1*(t^2-1)/(6*sqrt(n))*dnorm(x = t, mean = 0, sd = 1)

#Edgeworth with 3 terms
E2<-E1 - ( beta2*(t^3-3*t)/(24*n) + beta1^2*(t^5 - 10*t^3 + 15*t)/(72*n) )*dnorm(x = t, mean = 0, sd = 1)

#X_bar ~ Gamma(n,1) in BMA's notation.  Therefore, use mu +- (# stand. dev. from mean)*(stand. dev.) = mu +- t*sqrt(1/n) 
exact<-pgamma(q = 1+t/sqrt(5), shape = n, scale = 1/n)  

#Table 1 of Ferguson on p. 33 - compare the approximations
round(data.frame(t, clt.app, E1, E2, exact),3)

#Max differences:
round(data.frame(max.clt = max(abs(clt.app - exact)), max.E1 = max(abs(E1 - exact)), max.E2 = max(abs(E2 - exact))),3)
#Ferguson (1996) says max difference for E2 is 0.005 because they replace negative E2 values with 0.  


#CDF plot - remember that the expr part of curve() needs to have an "x"
curve(expr = pgamma(q = 1+x/sqrt(5), shape = n, scale = 1/n), xlim = c(-2, 2), col = "black", ylab = "G(t)", lwd = 1, ylim = c(0,1), xlab = "t",
      panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), main = "Normal and Edgeworth approximations")
curve(expr = pnorm(q = x, mean = 0, sd = 1), xlim = c(-2, 2), col = "red", add = TRUE)
curve(expr = pnorm(q = x, mean = 0, sd = 1) - beta1*(x^2-1)/(6*sqrt(n))*dnorm(x = x, mean = 0, sd = 1), 
      xlim = c(-2, 2), col = "blue", add = TRUE)
curve(expr = pnorm(q = x, mean = 0, sd = 1) - beta1*(x^2-1)/(6*sqrt(n))*dnorm(x = x, mean = 0, sd = 1)
      - ( beta2*(x^3-3*x)/(24*n) + beta1^2*(x^5 - 10*x^3 + 15*x)/(72*n) )*dnorm(x = x, mean = 0, sd = 1), 
      xlim = c(-2, 2), col = "darkgreen", add = TRUE)
legend(locator(1), legend = c("Exact", "CLT", "Edgeworth 1", "Edgeworth 2"), col=c("black", "red","blue", "darkgreen"),  
       bty="n", lwd=c(1,1,1,1), cex=0.75)

           







#
