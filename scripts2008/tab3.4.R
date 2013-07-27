#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-31-01                                                   #
# UPDATE: 12-9-03, 1-3-08                                           #
# PURPOSE: Table 3.4 example in Agresti (1996)                      #
#          Table 5.8 in Agresti (2007)                              #
#                                                                   #
# NOTES: SAS PROC FREQ will not do this for all three strata        #
#        Instead, it will do it within strata (2003 last checked)   #
#####################################################################

#Create contingency table - notice the data is entered by columns
n.table<-array(data = c(0, 4, 7, 16, 0, 4, 7, 13, 0, 2, 8, 13), dim = c(2,2,3),  
              dimnames = list(Race = c("Black", "White"), 
                              Promotion = c("Yes", "No"), Month = c("July", "August", "September")))
n.table
ftable(n.table, row.vars = "Race", col.vars = c("Month","Promotion"))


mantelhaen.test(n.table, correct=F)

#Exact test using mantelhaen.test()
mantelhaen.test(n.table, correct=F, exact = TRUE)


######################################################################
#Permutation test

all.data<-matrix(data = NA, nrow = 0, ncol = 3)

#Put data in "raw" form
for (i in 1:2) {
    for (j in 1:2) {
        for (k in 1:3) {
            all.data <- rbind(all.data, matrix(c(i, j, k), n.table[i, j, k], 3, byrow = T))

        }
    }
}

#Need in a data.frame format for the ftable() function call coming up
all.data2<-data.frame(all.data)

#One way to assign column names
names(all.data2)<-c("Race", "Promotion", "Month")
all.data2[1:5,]

#Check data
ftable(Month+Promotion~Race, data = all.data2)




#############################################################################################
#Do actual permutation test with 1000 permutations the bootstrap function
#   Note this will take a little time
#   Sampling is done within strata!
library(boot)

m.sq<-function(data,i) {
  perm.data<-data[i]
  mantelhaen.test(all.data[,1], perm.data, all.data[,3], correct=F)$statistic
}


                    
summarize.boot<-function(results, df) {

  par(mfrow = c(1,2))
  
  #Histogram
  hist(results$t, main = "Histogram of perm. dist.", col = "blue", xlab="statistic^*")
  segments(x0 = results$t0, y0 = -10, x1 = results$t0, y1 = 10)

  #QQ-Plot 
  chi.quant<-qchisq(seq(from = 0.001, to = 0.999, by = 0.001), df)
  qqplot(x = results$t, y = chi.quant, main = "QQ-Plot of perm. dist.", xlab="statistic^*")
  abline(a = 0, b = 1)

  par(mfrow = c(1,1))
  
  cat("The observed statistic is:", results$t0, "\n")

  #p-value
  p.value<-sum(results$t>=results$t0)/results$R
  cat("The p-value is:", p.value,"\n")
  invisible()
} 



col<-all.data[,2]
set.seed(9517)
perm.test<-boot(data = col, statistic = m.sq, R = 1000, sim = "permutation", strata = all.data[,3]) 
summarize.boot(perm.test, 1)
