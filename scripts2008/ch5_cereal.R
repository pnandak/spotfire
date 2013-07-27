############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-12-03                                                           #
# PURPOSE: Chapter 5 cereal data analysis                                  #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################


library(mva)

#Read in the data - already adjusted for serving size
cereal<-read.table("C:\\chris\\UNL\\STAT873\\Chapter 3\\cereal.txt", header=TRUE)

#Do the PCA and save results in an object called save
save<-princomp(formula = ~ sugar + fat + sodium, data = cereal, cor = TRUE, scores = TRUE)


#Components stored in save - access each component with $
names(save)
save$scores
save$loadings #Note that small loadings are excluded (cutoff) from the printing


#Summary of the eigenvalues and eigenvectors (loadings)
summary(save, loadings = TRUE, cutoff = 0.0)


#Plots for eigenvalues
  par(pty = "s")
  par(mfrow = c(1,2))
  screeplot(save, type="barplot", main="Bar plot of eigenvalues for cereal data")
  screeplot(save, type="lines", main="Scree plot for cereal data")

  par(mfrow = c(1,1))
  biplot(x = save, scale = 1, pc.biplot = FALSE) #full funciton name: biplot.princomp



####################################################################################################################
#My version of a biplot - note that I do not rescale the PC scores by the corresponding eigenvalues (biplot() does)

#Open new plotting sheet and plot PC scores
  win.graph(width = 7, height = 7, pointsize = 10)
  plot(x = save$scores[,1], y = save$scores[,2], type = "n", xlab = "PC1", ylab = "PC2")
  title("Biplot for cereal data")
  text(x = save$scores[,1], y = save$scores[,2], labels = 1:40)
  
  grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
  abline(h = 0, lty = 1, lwd = 2)  
  abline(v = 0, lty = 1, lwd = 2)  

  arrows(x0 = 0, y0 = 0, x1 = 0.667, y1 = 0.091, col = 2, lty = 1)
  text(x = 0.667+0.1, y = 0.091+0.1, label = "Sugar", col = 2) #Add a small constant to move out of way of arrow

  arrows(x0 = 0, y0 = 0, x1 = 0.588, y1 = 0.545, col = 3, lty = 1)
  text(x = 0.588+0.1, y = 0.545+0.1, label = "Fat", col = 3)

  arrows(x0 = 0, y0 = 0, x1 = -0.458, y1 = 0.833, col = 4, lty = 1)
  text(x = -0.458+0.1, y = 0.833+0.1, label = "Sodium", col = 4)
  
  

  
  
  
################################################################################
#The prcomp function will also do PCA - note the different input
cereal2<-data.frame(cereal$sugar, cereal$fat, cereal$sodium)
save2<-prcomp(x = cereal2)
summary(save2)
names(save2)
  

  
  
  
  
