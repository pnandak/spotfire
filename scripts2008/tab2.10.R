#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-29-01                                                   #
# UPDATE: 12-9-03                                                   #
# PURPOSE: Analyze table 2.10 of Agresti (1996)                     #
#                                                                   #
# NOTES:                                                            #
#####################################################################

n.table<-array(data = c(0, 1, 0, 
                  7, 1, 8, 
                  0, 1, 0, 
                  0, 1, 0, 
                  0, 1, 0, 
                  0, 1, 0, 
                  0, 1, 0, 
                  1, 0, 0, 
                  1, 0, 0), dim=c(3,9))

n.table

fisher.test(n.table)

x.sq<-chisq.test(n.table, correct=F)
x.sq
