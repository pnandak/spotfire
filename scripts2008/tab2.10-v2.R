#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-29-01                                                   #
# UPDATE: 12-9-03, 12-18-05                                         #
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

#Note that the table can only be 10x10 at most for this function
fisher.test(n.table)

x.sq<-chisq.test(n.table, correct=F)
x.sq



######################################################################
#Put data into raw form

all.data<-matrix(data = NA, nrow = 0, ncol = 2)

#Put data in "raw" form
for (i in 1:nrow(n.table)) {
    for (j in 1:ncol(n.table)) {
        all.data<-rbind(all.data, matrix(data = c(i, j), nrow = n.table[i,j], ncol = 2, byrow=T))
    }
}
#Note that warning messages will be generated since n.table[i,j]=0 sometimes

all.data
save<-xtabs(~all.data[,1]+ all.data[,2])
save

rowSums(save)
colSums(save)


######################################################################
#Do one permutation to illustrate 
set.seed(4088)
all.data.star<-cbind(all.data[,1], sample(all.data[,2], replace=F))
all.data.star
calc.stat<-chisq.test(all.data.star[,1], all.data.star[,2],  correct=F)
calc.stat$statistic

save.star<-xtabs(~all.data.star[,1] + all.data.star[,2])
save.star
rowSums(save.star)
colSums(save.star)



###########################################################################
# A simple function and for loop to find the permutation distribution
#  Note that the do.it function is outside of the for loop because R
#  will work faster this way

do.it<-function(data.set){
    all.data.star<-cbind(data.set[,1], sample(data.set[,2], replace=F))
    chisq.test(all.data.star[,1], all.data.star[,2], correct=F)$statistic
}

summarize<-function(result.set, statistic, df, B) {

  par(mfrow = c(1,2))
  
  #Histogram
  hist(x = result.set, main = expression(paste("Histogram of ", X^2, " perm. dist.")), col = "blue", freq = FALSE)
  curve(expr = dchisq(x = x, df = df), col = "red", add = TRUE)
  segments(x0 = statistic, y0 = -10, x1 = statistic, y1 = 10)

  #QQ-Plot 
  chi.quant<-qchisq(p = seq(from = 1/(B+1), to = 1-1/(B+1), by = 1/(B+1)), df = df)
  plot(x = sort(result.set), y = chi.quant, main = expression(paste("QQ-Plot of ", X^2, " perm. dist.")))
  abline(a = 0, b = 1)

  par(mfrow = c(1,1))

  #p-value
  mean(result.set>=statistic)
} 

#Example use of do.it function
do.it(data.set = all.data)


B<-1000
results<-matrix(data = NA, nrow = B, ncol = 1)

set.seed(5333)
for(i in 1:B) {
    results[i,1]<-do.it(data.set = all.data)
    }

summarize(result.set = results, statistic = x.sq$statistic, df = (nrow(n.table)-1)*(ncol(n.table)-1), B = B)

#Shows the different X^2* values
table(round(results,2))

#chi-square app.
round(pchisq(q = as.numeric(names(table(round(results,2)))), df = (nrow(n.table)-1)*(ncol(n.table)-1)),4)







############################################################################
#Simpler way to get the p-value 

set.seed(7709)
chisq.test(n.table, correct = FALSE, simulate.p.value = TRUE, B = 1000)




############################################################################
#Do actual permutation test with 1000 permutations using the boot function in the boot package
#   Note this will take a little time
library(boot)


#Function for the statistic
#  First element is the original or permuted data.
#  Second element represents the indices of the data.  For example, the indices will be 1:24 for the observed
#    data.   
x.sq.temp<-function(data, i) {
  perm.data<-data[i]
  cat("data:", perm.data, "\n i:", i, "\n")
  stat<-chisq.test(all.data[,1], perm.data, correct=F)$statistic
  cat("stat:", stat, "\n")
  chisq.test(all.data[,1], data, correct=F)$statistic
}

col<-all.data[,2]
set.seed(2729)
#Do once to see what the function is doing
perm.test<-boot(data = col, statistic = x.sq.temp, R = 2, sim = "permutation") 
                  
      
                     
#Perform the test
x.sq<-function(data, i) {
  perm.data<-data[i]
  chisq.test(all.data[,1], perm.data, correct=F)$statistic
}

col<-all.data[,2]
set.seed(2729)
perm.test<-boot(data = col, statistic = x.sq, R = 1000, sim = "permutation") 
names(perm.test)
              
 
                     
summarize.boot<-function(results, df) {

  par(mfrow = c(1,2))
  
  #Histogram
  hist(results$t, main = "Histogram of perm. dist.", col = "blue", xlab="statistic^*")
  segments(x0 = results$t0, y0 = -10, x1 = results$t0, y1 = 10)

  #QQ-Plot 
  chi.quant<-qchisq(p = seq(from = 0.001, to = 0.999, by = 0.001), df = df)
  qqplot(x = results$t, y = chi.quant, main = "QQ-Plot of perm. dist.", xlab="statistic^*")
  abline(a = 0, b = 1)

  par(mfrow = c(1,1))
  
  cat("The observed statistic is:", results$t0, "\n")

  #p-value
  p.value<-sum(results$t>=results$t0)/results$R
  cat("The p-value is:", p.value,"\n")
  invisible()
} 

summarize.boot(perm.test, (nrow(n.table)-1)*(ncol(n.table)-1))
