############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-12-03, 7-7-05                                                   #
# PURPOSE: Chapter 5 goblet analysis                                       #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################


#Read in the data - already adjusted for serving size
goblet<-read.table("C:\\chris\\UNL\\STAT873\\Chapter 5\\goblet.txt", header=FALSE, 
        col.names = c("goblet", "x1", "x2", "x3", "x4", "x5", "x6"))

goblet2<-data.frame(goblet$goblet, w1 = goblet$x1/goblet$x3, w2 = goblet$x2/goblet$x3, 
                                   w4 = goblet$x4/goblet$x3, w5 = goblet$x5/goblet$x3, 
                                   w6 = goblet$x6/goblet$x3)

#A little bit easier way to do the goblet2 assignment is as follows:
attach(goblet) #Tells R that you want to work with the varibales in goblet
goblet2<-data.frame(goblet, w1 = x1/x3, w2 = x2/x3, 
                            w4 = x4/x3, w5 = x5/x3, 
                            w6 = x6/x3)
detach(goblet)                            
#Notice the original data set is still in goblet2 since goblet was specified here
#  R is confused since it does not know if you are referring to the goblet variable
#  or the goblet data set.  It probably would have been better to use a variable by a 
#  different name for the goblet variable.  I used the goblet variable name to be 
#  consistent with the variable names used in the SAS program


#Stars plot
goblet.w<-data.frame(w1 = goblet2$w1, w2 = goblet2$w2, w4 = goblet2$w4, 
                     w5 = goblet2$w5, w6 = goblet2$w6)
stars(x = goblet.w, nrow = 5, ncol = 5, key.loc = c(0,12), draw.segments = TRUE, cex=0.5, 
      main = "Goblet star plot")

         
#Parallel coordinate plot
 
library(MASS)
goblet.w2<-data.frame(obs = 1:25, goblet.w)
parcoord(x = goblet.w2, main = "Parallel coordinate plot for goblet data")


col.w5<-ifelse(goblet.w2$w5 <= median(goblet.w2$w5), "red", "blue")
parcoord(x = goblet.w2, col = col.w5, main = "Parallel coordinate plot for goblet data \n color split is for median of w5")

 
col.w2<-ifelse(goblet.w2$w2 <= median(goblet.w2$w2), "green", "orange")
parcoord(x = goblet.w2, col = col.w2, main = "Parallel coordinate plot for goblet data \n color split is for median of w2")
       
                            
#########################################################################################
# PCA

#Do the PCA and save results in an object called save - using the correlation matrix
#  To use the covariance matrix instead, change the cor = TRUE to FALSE
save<-princomp(formula = ~ w1 + w2 + w4 + w5 + w6, data = goblet2, cor = TRUE, scores = TRUE)

#This function works as well
#save<-prcomp(formula = ~ w1 + w2 + w4 + w5 + w6, data = goblet2, cor = TRUE, scores = TRUE)

#Summary of the eigenvalues and eigenvectors (loadings)
summary(save, loadings = TRUE, cutoff = 0.0)
#Note that the eigenvectors are of the opposite sign in SAS for this example 


#Plots for eigenvalues
  par(pty = "s")
  par(mfrow = c(1,2))
  screeplot(save, type="barplot", main="Bar plot of eigenvalues for goblet data")
  screeplot(save, type="lines", main="Scree plot for goblet data")

  par(mfrow = c(1,1))
  biplot(x = save, scale = 1, pc.biplot = FALSE, main = "Biplot for goblet data", panel.first=grid(col="gray", lty="dotted"))





##########################################################################################
#My version of a biplot

#Open new plotting sheet and plot PC scores
  win.graph(width = 7, height = 7, pointsize = 10)
  plot(x = save$scores[,1], y = save$scores[,2], type = "n", xlab = "PC1", ylab = "PC2")
  title("Biplot for goblet data")
  text(x = save$scores[,1], y = save$scores[,2], labels = 1:40)
  
  grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
  abline(h = 0, lty = 1, lwd = 2)  
  abline(v = 0, lty = 1, lwd = 2)  

  #Notice how the eigenvector values can be extracted
  arrows(x0 = 0, y0 = 0, x1 = 3*save$loadings[,1][[1]], y1 = 3*save$loadings[,2][[1]], col = 2, lty = 1)
  text(x = 3*save$loadings[,1][[1]]+0.1, y = 3*save$loadings[,2][[1]]+0.1, label = "w1", col = 2) 

  arrows(x0 = 0, y0 = 0, x1 = 3*save$loadings[,1][[2]], y1 = 3*save$loadings[,2][[2]], col = 3, lty = 1)
  text(x = 3*save$loadings[,1][[2]]+0.1, y = 3*save$loadings[,2][[2]]+0.1, label = "w2", col = 3)

  arrows(x0 = 0, y0 = 0, x1 = 3*save$loadings[,1][[3]], y1 = 3*save$loadings[,2][[3]], col = 4, lty = 1)
  text(x = 3*save$loadings[,1][[3]]+0.1, y = 3*save$loadings[,2][[3]]+0.1, label = "w4", col = 4)

  arrows(x0 = 0, y0 = 0, x1 = 3*save$loadings[,1][[4]], y1 = 3*save$loadings[,2][[4]], col = 5, lty = 1)
  text(x = 3*save$loadings[,1][[4]]+0.1, y = 3*save$loadings[,2][[4]]+0.1, label = "w5", col = 5)

  arrows(x0 = 0, y0 = 0, x1 = 2*save$loadings[,1][[5]], y1 = 2*save$loadings[,2][[5]], col = 6, lty = 1)
  text(x = 2*save$loadings[,1][[5]]+0.1, y = 2*save$loadings[,2][[5]]+0.1, label = "w6", col = 6)
#notice the biplots (my version and R's versoin) do not match up exactly




#######################################################################################################
# Try to reproduce the R biplot() function results - note that I can not get the 2nd x and y-axis the same

#See p. 60-62 of the S-Plus 6 Guide to Statistics V.2
S<-matrix(save$scores[,1:2], nrow = 25, ncol = 2)
D<-diag(save$sdev[1:2]*sqrt(25)) #25 is the sample size
G<-S%*%solve(D) #So the scores are just being rescaled by the corresponding eigenvalues

L<-cbind(save$loadings[,1], save$loadings[,2])
H<-L%*%D #So the eigenvectors are just being rescaled

win.graph(width = 7, height = 7, pointsize = 12)
par(pty = "s")
plot(x = G[,1], y = G[,2], xlab = "g1 and 0.1*h1", ylab = "g2 and 0.1*h1", type="n", xlim=c(-0.5, 0.5), ylim=c(-0.5, 0.5), tck=0.01)
text(x = G[,1], y = G[,2])
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

arrows(x0 = 0, y0 = 0, x1 = 0.1*H[1,1], y1 = 0.1*H[1,2], col = 1, lty = 1)
text(x = 0.1*H[1,1]+0.03, y = 0.1*H[1,2]+0.03, label = "w1", col = 1)

arrows(x0 = 0, y0 = 0, x1 = 0.1*H[2,1], y1 = 0.1*H[2,2], col = 2, lty = 1)
text(x = 0.1*H[2,1]+0.03, y = 0.1*H[2,2]+0.03, label = "w2", col = 2)

arrows(x0 = 0, y0 = 0, x1 = 0.1*H[3,1], y1 = 0.1*H[3,2], col = 3, lty = 1)
text(x = 0.1*H[3,1]+0.03, y = 0.1*H[3,2]+0.03, label = "w4", col = 3)

arrows(x0 = 0, y0 = 0, x1 = 0.1*H[4,1], y1 = 0.1*H[4,2], col = 4, lty = 1)
text(x = 0.1*H[4,1]+0.03, y = 0.1*H[4,2]+0.03, label = "w5", col = 4)

arrows(x0 = 0, y0 = 0, x1 = 0.1*H[5,1], y1 = 0.1*H[5,2], col = 5, lty = 1)
text(x = 0.1*H[5,1]+0.03, y = 0.1*H[5,2]+0.03, label = "w6", col = 5)


#axis(3, labels=c(-4, -2, 0, 2, 4)) #Puts scales on the top
#axis(4, labels=c(-4, -2, 0, 2, 4)) #Puts scales on the right side



#For more on biplots:
#  Gabriel, K. R. (1971). The biplot graphical display of matrices
#     with applications to principal component analysis. Biometrika, 58,
#     453-467.
#  Gabriel, K. R. and Odoroff, C. L. (1990). Biplots in biomedical
#     research. Statistics in Medicine, 9, 469-485.

  
  
  
  
