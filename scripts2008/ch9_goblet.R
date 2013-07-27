############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-12-03                                                           #
# PURPOSE: Chapter 9 goblet analysis                                       #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################


goblet<-read.table(file = "C:\\chris\\UNL\\STAT873\\Chapter 5\\goblet.txt", header = FALSE, 
        col.names = c("goblet", "x1", "x2", "x3", "x4", "x5", "x6"))

#Make sure that ONLY the variables of interest are in the data set!!!
goblet2<-data.frame(w1 = goblet$x1/goblet$x3, w2 = goblet$x2/goblet$x3, 
                    w4 = goblet$x4/goblet$x3, w5 = goblet$x5/goblet$x3, 
                    w6 = goblet$x6/goblet$x3)

###########################################################################
# NOTE: THE DATA WILL NOT BE STANDARDIZED FOR THIS ANALYSIS.  TYPICALLY
#       ONE DOES WANT TO STANDARDIZE.  I DECIDED NOT TO SINCE THE VARIABLES
#       ARE MEASURED ON THE SAME SCALE (SEE CHAPTER 5 NOTES).  
###########################################################################


                            
#########################################################################################
# Cluster Analysis
  
  #Note that other distance measures exist - Manhattan and 
  #   Canberra
  save<-hclust(d = dist(goblet2, method = "euclidean"), method = "single")
  summary(save) #Not helpful
  save          #Not much help either
  names(save)

  #Order of clusters merging
  save$merge

  plclust(tree = save, main = "Goblet data tree")

  clusters<-cutree(tree = save, k = 5) #k specifies number of clusters
  clusters
  cutree(tree = save, h = 0.24) #h specifies height

  #interact with tree to identify clusters
  identify(x = save)

  #Also can use this function to non-interactively find clusters
  save.clust<-rect.hclust(tree = save, k = 5, border="blue")
  save.clust


################################################################
# PCA with clusters identified
  win.graph(width = 7, height = 7, pointsize = 10)
  par(pty = "s")
  
  #Notice that I am using the covariance matrix here
  #  Also, to reproduce the PC scores plots in the Chapter 5 notes, I would need
  #  to put a negative in front of save.pc$scores[,2] in the y = option of the plot() function
  #  Remember the non-uniqueness of the eigenvectors. This can produce different score values
  #  between different STAT software packages.  The interpretations of the PCs will remain the same
  save.pc<-princomp(formula = ~ w1 + w2 + w4 + w5 + w6, data = goblet2, cor = FALSE, scores = TRUE)  
  plot(x = save.pc$scores[,1], y = save.pc$scores[,2], type = "n", xlab = "PC1", ylab = "PC2", 
       main = "PC scores for goblet data", panel.first=grid(col="gray", lty="dotted"))
  numb<-1:40
  text(x = save.pc$scores[,1], y = save.pc$scores[,2], labels = 1:25, col=1)
  abline(h = 0, lty = 1, lwd = 2)  
  abline(v = 0, lty = 1, lwd = 2)  

  #Add color to the above plot to help see the clusters
  text(x = save.pc$scores[clusters==1,1], y = save.pc$scores[clusters==1,2], labels = numb[clusters==1], col=1)
  text(x = save.pc$scores[clusters==2,1], y = save.pc$scores[clusters==2,2], labels = numb[clusters==2],col=2)
  text(x = save.pc$scores[clusters==3,1], y = save.pc$scores[clusters==3,2], labels = numb[clusters==3],col=3)
  text(x = save.pc$scores[clusters==4,1], y = save.pc$scores[clusters==4,2], labels = numb[clusters==4],col=4)
  text(x = save.pc$scores[clusters==5,1], y = save.pc$scores[clusters==5,2], labels = numb[clusters==5],col=5)
 
  legend(x=-0.35, y=0.4, legend = 1:5, col = 1:5, pch = 20) #Specify a location


  

################################################################
# cluster package
  library(cluster)

#Again, I am not going to standardize these variables!

  save.agnes<-agnes(x = goblet2, metric = "euclidean", stand = FALSE, method = "single")
  summary(save.agnes)


  win.graph(width = 7, height = 7, pointsize = 10)
  par(pty = "s")
  plot.agnes(save.agnes)




#####################################################################
# multiv package

library(multiv)  
goblet.matrix <- as.matrix(goblet2)
#Shows step-by-step the cluster process
save.heirclust<-hierclust(a = goblet.matrix, method = 2, option = "prompt", movie = TRUE, diagnostics=TRUE, show="all")


###################################################################
# Hmisc - also contains clustering functions

library(Hmisc)



  
