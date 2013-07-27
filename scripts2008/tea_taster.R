#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-28-01                                                   #
# UPDATE: 12-8-03                                                   #
# PURPOSE: Tea taster                                               #
#                                                                   #
# NOTES:                                                            #
#####################################################################

n.table<-array(data = c(3, 1, 1, 3), dim = c(2,2), dimnames=list(Actual = c("Pour Milk", "Pour Tea"), 
                    Guess = c("Pour Milk", "Pour Tea")))
n.table


#P(3)
dhyper(3, 4, 4, 4) 

#P(0),...,P(4)
dhyper(0:4, 4, 4, 4) 


fisher.test(x = n.table)
fisher.test(x = n.table, alternative = "greater")
