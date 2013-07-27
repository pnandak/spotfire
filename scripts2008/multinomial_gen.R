#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-19-01                                                   #
# UPDATE: 12-7-03, 12-10-05                                         #
# PURPOSE: Generate data from a multinomial distribution            #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#Original way coded 
codes<-1:5  
probs<-c(0.25, 0.35, 0.2, 0.1, 0.1)

set.seed(2195)
save<-sample(x = codes, size = 1000, replace = T, prob = probs) 
save[1:20]

hist(x = save)
save.count<-table(save)
save.count
save.count/1000

barplot(height = save.count, names = c("1", "2", "3", "4", "5"), 
        main = "Sample from multinomial distribution with \n pi=(0.25, 0.35, 0.2, 0.1, 0.1)")


#New way to code - only available in R (not S-Plus?)
  set.seed(2195)
  save<-rmultinom(n = 1, size = 1000, prob = c(0.25, 0.35, 0.2, 0.1, 0.1))
  save
  save/1000

  #Height of bars correspond to the columns so need to transpose save
  barplot(height = t(save), names = c("1", "2", "3", "4", "5"), col = "red", 
          main = "Sample from multinomial distribution with \n pi=(0.25, 0.35, 0.2, 0.1, 0.1)")

#Example with dmultinom, default for size option is the sum(x)
  
   #Simple example with obvious result of 0.25
   dmultinom(x = c(1,0,0,0,0), size = NULL, prob = c(0.25, 0.35, 0.2, 0.1, 0.1))
   
   #Example showing probability of getting n1 = 1 and n5 = 1 when N = 2
   dmultinom(x = c(1,0,0,0,1), size = NULL, prob = c(0.25, 0.35, 0.2, 0.1, 0.1))
  
   
   #
