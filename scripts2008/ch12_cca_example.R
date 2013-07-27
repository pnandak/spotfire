############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-22-03                                                           #
# PURPOSE: Chapter 12 material                                             #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################



firo<-read.table("C:\\chris\\UNL\\STAT873\\Chapter 12\\firo.txt", header=FALSE, 
        col.names = c(paste("faces", 1:30, sep = ""), "kms1", "kms2", "kms3")) 

library(mva)        
save<-cancor(x = firo[,1:30], y = firo[,31:33])
names(save)

save$cor
