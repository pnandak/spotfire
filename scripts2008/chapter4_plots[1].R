############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-10-03                                                           #
# PURPOSE: Chapter 4 material                                              #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################




#Input mu, sigma
mu<-c(15,20)
sigma<-matrix(data = c(1, 0.5, 0.5, 1.25), nrow=2, ncol=2)
mu
sigma


#Find the eigenvalues with eigenvectors
eig<-eigen(sigma)
eig

#Note how individual components of eig can be accessed
eig$values
eig$vectors
eig$vectors[1,1]



############################################################################
# Eigenvector plot

  #Square plot (default is "m" which means maximal region)
  par(pty = "s")

  #Set up some dummy values for plot
  a1<-c(-1,1)
  a2<-c(-1,1)

  plot(x = a1, y = a2, type = "n", main = expression(paste("Eigenvectors of ", Sigma)), 
       xlab = expression(a[1]), , ylab = expression(a[2]), panel.first=grid(col="gray", lty="dotted")) 
  #Run demo(plotmath) for help on greek letters and other characters
     
  abline(h = 0, lty = 1, lwd = 2)  #h specifies a horizontal line, lwd is line width
  abline(v = 0, lty = 1, lwd = 2)  #v specifies a vertical line

  arrows(x0 = 0, y0 = 0, x1 = 0.6154, y1 = 0.7882, col = 2, lty = 1)
  arrows(x0 = 0, y0 = 0, x1 = 0.7882, y1 = -0.6154, col = 2, lty = 1)

  #Note: the first arrows statement could also have been done by: 
  #  arrows(x0 = 0, y0 = 0, x1 = eig$vectors[1,1], y1 = eig$vectors[2,1], col = 2, lty = 1)



##############################################################################
#Plot contours of Bivariate normal distribution and put the eigenvectors on it


  library(mvtnorm) #Need for dmvnorm() function

  #sequence of numbers from 10-25 by 0.1
  x1<-seq(from = 10, to = 25, by = 0.1)
  x2<-seq(from = 10, to = 25, by = 0.1)
  #Finds all possible combinations of x1 and x2
  all<-expand.grid(x1, x2)

  #Finds f(x) - dmvnorm() is in the mvtnorm package
  f.x<-dmvnorm(all, mean = mu, sigma = sigma)

  #Puts f(x) values in a matrix - need for contour(), but not for contourplot()
  f.x2<-matrix(f.x, nrow=length(x1), ncol=length(x2))

dmvnorm(c(10,10.1), mean = mu, sigma = sigma)



  #Reset graphics parameters
  par(pty = "s")

  #The f.x2 values need to be in a matrix.  Think of x1 as denoting the rows and x2 as denoting the columns
  #   corresponding to the values of x1 and x2 used to produce a f(x) in a cell of the matrix
  contour(x = x1, y = x2, z = f.x2, xlim = c(-10,25), ylim = c(-10,25), levels = c(0.01, 0.001), 
          xlab = expression(X[1]), ylab = expression(X[2]), panel.first=grid(col="gray", lty="dotted"), 
          main = "Contour plot for bivariate normal distribution \n with eigenvectors plotted",
          sub = "mu = [15, 20]', sigma = [(1, 0.5)', (0.5, 1.25)']")
  abline(h = 0, lty = 1, lwd = 2)  
  abline(v = 0, lty = 1, lwd = 2) 
  #Vector lengths are 10 in order to help seem better on the plot
  arrows(x0 = 0, y0 = 0, x1 = 10*0.6154, y1 = 10*0.7882, col = 2, lty = 1)
  arrows(x0 = 0, y0 = 0, x1 = 10*0.7882, y1 = 10*(-0.6154), col = 2, lty = 1)




  #Another version of a contour plot with an easier way to get the contour plot.  
  library(lattice)
  trellis.par.set(col.whitebg())

  save<-data.frame(x1 = all[,1], x2 = all[,2], f.x)
  arrows(x0 = 0, y0 = 0, x1 = 10*0.6154, y1 = 10*0.7882, col = 2, lty = 1)
  contourplot(formula = f.x ~ x1 * x2, data = save, xlim = c(-10,25), ylim = c(-10,25))

  #Another interesting plot
  levelplot(formula = f.x ~ x1 * x2, data = save)
 
