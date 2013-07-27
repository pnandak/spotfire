############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-21-03                                                           #
# PURPOSE: Chapter 10 material                                             #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################



mu<-c(15,20)
sigma<-matrix(data = c(1, 0.5, 0.5, 1.25), nrow = 2, ncol = 2)
N<-20
p<-2




#####################################################################################
# Hotelling's T^2
library(mvtnorm) #Need for dmvnorm() function


#Use the same seed number
set.seed(508)
x<-rmvnorm(n = N, mean = mu, sigma = sigma)
#Note: mvrnorm() from the MASS package also can do this

meanx1<-mean(x[,1])
meanx2<-mean(x[,2])
mu.hat<-rbind(meanx1, meanx2)
mu.hat

sigma.hat<-var(x)
sigma.hat

#construct the Hotelling's T^2 for the hypothesis Ho:mu=[15,20], Ha:mu<>[15,20]
mu.Ho<-rbind(15,20)

T.sq<-N*t(mu.hat-mu.Ho)%*%solve(sigma.hat)%*%(mu.hat-mu.Ho)
(N-p)/(p*(N-1))*T.sq
qf(0.95, p, N-p)
p.val<-1-pf((N-p)/(p*(N-1))*T.sq, p, N-p)


cat("T^2 = ", round(T.sq,2), "and the p-value is", round(p.val,4), "\n")


#######################################################################################
# Plots - most of the code is from chapter4_plots.R

  #sequence of numbers from 10-25 by 0.1
  x1<-seq(10, 25, 0.1)
  x2<-seq(10, 25, 0.1)
  #Finds all possible combinations of x1 and x2
  all<-expand.grid(x1, x2)

  #Finds f(x) - dmvnorm() is in the mvtnorm package
  f.x<-dmvnorm(all, mean = mu, sigma = sigma)

  #Puts f(x) values in a matrix - need for contour(), but not for contourplot()
  f.x2<-matrix(f.x, nrow=length(x1), ncol=length(x2))



  #Reset graphics parameters
  par(pty = "s")
    
  #The f.x2 values need to be in a matrix.  Think of x1 as denoting the rows and x2 as denoting the columns
  #   corresponding to the values of x1 and x2 used to produce a f(x) in a cell of the matrix
  contour(x = x1, y = x2, z = f.x2, xlim = c(-10,25), ylim = c(-10,25), levels = c(0.01, 0.001), 
          xlab = expression(X[1]), ylab = expression(X[2]), 
          main = "Contour plot for bivariate normal distribution \n with eigenvectors plotted", 
          sub = "mu = [15, 20]', sigma = [(1, 0.5)', (0.5, 1.25)']", 
          panel.first=grid(col="gray", lty="dotted") )
  abline(h = 0, lty = 1, lwd = 2)  
  abline(v = 0, lty = 1, lwd = 2) 
  #Vector lengths are 10 in order to help seem better on the plot
  arrows(x0 = 0, y0 = 0, x1 = 10*0.6154, y1 = 10*0.7882, col = 2, lty = 1)
  arrows(x0 = 0, y0 = 0, x1 = 10*0.7882, y1 = 10*(-0.6154), col = 2, lty = 1)

  points(x = x[,1], y = x[,2], pch = 1, lwd = 2, col = "blue")

  points(x = mu.hat[1], mu.hat[2], pch = 3, col = 1, lwd = 5)
  points(x = mu[1], mu[2], pch = 4, col = 2, lwd = 2)
  legend(locator(1), legend=c("mu.hat", "mu"), pch = c(3,4), col = c(1,2)) 


#ANOTHER plot with different x and y axis
  contour(x = x1, y = x2, z = f.x2, xlim = c(10,25), ylim = c(10,25), levels = c(0.01, 0.001), 
          xlab = expression(X[1]), ylab = expression(X[2]), 
          main = "Contour plot for bivariate normal distribution", 
          sub = "mu = [15, 20]', sigma = [(1, 0.5)', (0.5, 1.25)']", 
          panel.first=grid(col="gray", lty="dotted") )
 
  points(x = x[,1], y = x[,2], pch = 1, lwd = 2, col = "blue")

  points(x = mu.hat[1], mu.hat[2], pch = 3, col = 1, lwd = 5, cex = 2)
  points(x = mu[1], mu[2], pch = 4, col = 2, lwd = 2, cex=2)
  legend(locator(1), legend=c("observations", "mu.hat", "mu"), pch = c(1,3,4), col = c("blue",1,2)) 

 
 
 
 
################################################################################ 
#Repeat 20 times with 20 plots

  set.seed(508)
  mu.Ho<-rbind(15,20)
  save<-matrix(data = 0, nrow = 20, ncol = 1)
  
  #sequence of numbers from 10-25 by 0.1
  x1<-seq(10, 25, 0.1)
  x2<-seq(10, 25, 0.1)
  
  #Finds all possible combinations of x1 and x2
  all<-expand.grid(x1, x2)

   #Repeat 20 times
   numb<-20
  
   par(mfrow=c(4,5), mai=c(0.2,0.2,0.1,0.1))
  
  
   for(i in 1:numb) {
     x<-rmvnorm(n = N, mean = mu, sigma = sigma)

     meanx1<-mean(x[,1])
     meanx2<-mean(x[,2])
     mu.hat<-rbind(meanx1, meanx2)
     sigma.hat<-var(x)
     sigma.hat

     T.sq<-N*t(mu.hat-mu.Ho)%*%solve(sigma.hat)%*%(mu.hat-mu.Ho)
     p.val<-1-pf((N-p)/(p*(N-1))*T.sq, p, N-p)
     save[i,1]<-p.val
     
     #Finds f(x) - dmvnorm() is in the mvtnorm package
     f.x<-dmvnorm(all, mean = mu, sigma = sigma)

     #Puts f(x) values in a matrix - need for contour(), but not for contourplot()
     f.x2<-matrix(f.x, nrow=length(x1), ncol=length(x2))

     contour(x = x1, y = x2, z = f.x2, xlim = c(10,20), ylim = c(15,25), levels = c(0.01, 0.001), 
             xlab = expression(X[1]), ylab = expression(X[2]), cex = 0.25, 
             panel.first=grid(col="gray", lty="dotted"), drawlabels = FALSE)

     points(x = x[,1], y = x[,2], pch = 1, lwd = 0.5, col = "blue")

     points(x = mu.hat[1], mu.hat[2], pch = 3, col = 1, lwd = 2, cex = 1)
     points(x = mu[1], mu[2], pch = 4, col = 2, lwd = 2, cex = 1)
     #legend(x = 20, y = 10, legend=c("observations", "mu.hat", "mu"), pch = c(1,3,4), col = c("blue",1,2)) 
  }   
 
 
  #
