#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  9-18-03                                                    #
# UPDATE: 12-11-03                                                  #
# Purpose: Graph pi_i vs X_1 for beta=0.5 and beta=-0.5             #
#                                                                   #
# NOTES:                                                            #
#####################################################################


alpha<-1
beta1<-0.5

par(pty="s")
curve(expr = exp(alpha+beta1*x)/(1+exp(alpha+beta1*x)), from = -15, to = 15, col = "red",
      main = expression(pi(x) == frac(e^{alpha+beta*x}, 1+e^{alpha+beta*x})), xlab =  "x",
      ylab = expression(pi(x)), panel.first = grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted"))
#See help(plotmath) for more on the expression function and see demo(plotmath)
#Also, plogis(alpha+beta*x) could be used instead of exp(alpha+beta1*x)/(1+exp(alpha+beta1*x))


#####################################################################
# Old way to do the plot
  
x<-seq(from = -15, to = 15, by = 0.5)
pi<-exp(alpha+beta1*x)/(1+exp(alpha+beta1*x))

par(pty="s")
plot(x = x, y = pi, xlab = "x", ylab = "pi(x)", ylim = c(0,1), 
     type = "l", col = 2, main = "pi(x) = exp(1+beta*x)/(1+exp(1+beta*x))", panel.first = 
     grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted"))



######################################################################
# All distributions

alpha<-1
beta<-0.5

par(pty="s")
curve(expr = plogis(alpha+beta*x), from = -15, to = 15, col = "red", lwd = 2, lty = "solid",
      main = expression(paste(pi(x), " vs. x for ", alpha, " = 1 and ", beta," = 0.5")), xlab =  "x",
      ylab = expression(pi(x)), panel.first = grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted"))
curve(expr = pnorm(alpha+beta*x, mean=0, sd=1), from = -15, to = 15, col = "blue", add = TRUE, lty = 2, lwd = 2)
curve(expr = 1-exp(-exp(alpha+beta*x)), from = -15, to = 15, col = "green", add = TRUE, lty = 4, lwd = 2)
#grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
legend(locator(1), legend = c("Logit", "Probit", "Cloglog"), lty = c(1,2,4), lwd = c(2,2,2), 
       col = c("red", "blue", "green"), bty = "n")

#There is a pgumbel(q, loc=0, scale=1, lower.tail = TRUE) function in the evd and VGAM packages


#
