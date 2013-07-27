#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  9-18-03                                                    #
# Purpose: Graph pi_i vs X_1 for beta=0.5 and beta=-0.5             #
#                                                                   #
# NOTES:                                                            #
#####################################################################



beta0<-1
beta1<--0.5
x1<-seq(from = -15, to = 15, by = 0.5)
pi<-exp(beta0+beta1*x1)/(1+exp(beta0+beta1*x1))


par(pty="s")
plot(x = x1, y = pi, xlab = expression(X[1]), ylab = expression(pi), ylim = c(0,1), 
     type = "l", col = 2, main = "pi = exp(1+0.5X_1)/(1+exp(1+0.5X_1))")
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
