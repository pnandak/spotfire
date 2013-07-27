############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  11-16-03                                                          #
# PURPOSE: Chapter 11 MANOVA for CPT data                                  #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################

cpt<-read.table("C:\\chris\\UNL\\STAT873\\Chapter 10\\cpt.txt", header=FALSE, 
        col.names = c("dose", "patient", "time1", "time2", "time3", "time4"))

#cbind combines the columns into one matrix
#factor needs to be used to make sure R treats does non-numerically
save<-manova(formula = cbind(time1, time2, time3, time4) ~ factor(dose), data = cpt)

summary(save, test = "Pillai")
summary(save, test = "Wilks")
summary(save, test = "Hotelling-Lawley")
summary(save, test = "Roy")
