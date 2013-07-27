############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-12-03, 7-7-05                                                   #
# PURPOSE: Chapter 6 goblet data analysis                                  #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################



#Read in the data - already adjusted for serving size
goblet<-read.table(file = "C:\\chris\\UNL\\STAT873\\Chapter 5\\goblet.txt", header=FALSE, 
        col.names = c("goblet", "x1", "x2", "x3", "x4", "x5", "x6"))

attach(goblet) 
goblet2<-data.frame(goblet, w1 = x1/x3, w2 = x2/x3, 
                            w4 = x4/x3, w5 = x5/x3, 
                            w6 = x6/x3)
detach(goblet)  


save<-factanal(x = ~ w1 + w2 + w4 + w5 + w6, factors = 2, data = goblet2, scores = "regression", 
               rotation = "varimax")
names(save)
#summary(save) #Does not produce meaningful results here
save #Does not print all of the loadings (cuts off)



#print.loadings() function can print all of the loadings - according to the help
#  however, R says that it does not exist!  

par(pty = "s")
plot(x = save$scores[,1], y = save$scores[,2], type = "n", xlab = "Common Factor 1", ylab = "Common Factor 2")
title("Factor 2 vs. Factor 1 for goblet data")
text(x = save$scores[,1], y = save$scores[,2], labels = 1:25)
  
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
abline(h = 0, lty = 1, lwd = 2)  
abline(v = 0, lty = 1, lwd = 2)  
