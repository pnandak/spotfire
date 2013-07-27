#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-30-01                                                   #
# UPDATE: 12-9-03, 12-18-05                                         #
# PURPOSE: Try permutation test on Bird table                       #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#Create contingency table - notice the data is entered by columns
n.table<-array(data = c(251, 48, 34, 5), dim=c(2,2), dimnames=list(First = c("made", "missed"), 
             Second = c("made", "missed")))
n.table

x.sq<-chisq.test(n.table, correct=F)
x.sq


######################################################################
#Find raw data

all.data<-matrix(data = NA, nrow = 0, ncol = 2)

#Put data in "raw" form
for (i in 1:nrow(n.table)) {
    for (j in 1:ncol(n.table)) {
        all.data<-rbind(all.data, matrix(c(i, j), n.table[i,j], 2, byrow=T))
    }
}

#Check
xtabs(~all.data[,1]+ all.data[,2])


##############################################################################
# Permutation test - illustrated 3 ways, but really only needed to do one way

#Method #1
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


#Create contingency table - notice the data is entered by columns
n.table<-array(data = c(251, 48, 34, 5), dim=c(2,2), dimnames=list(First = c("made", "missed"), 
             Second = c("made", "missed")))
n.table

x.sq<-chisq.test(n.table, correct=F)
x.sq


######################################################################
#Find raw data

all.data<-matrix(data = NA, nrow = 0, ncol = 2)

#Put data in "raw" form
for (i in 1:nrow(n.table)) {
    for (j in 1:ncol(n.table)) {
        all.data<-rbind(all.data, matrix(data = c(i, j), nrow = n.table[i,j], ncol = 2, byrow=T))
    }
}

#Check
xtabs(~all.data[,1]+ all.data[,2])


##############################################################################
# Permutation test - illustrated 3 ways, but really only needed to do one way

#Method #1
do.it<-function(data.set){
    all.data.star<-cbind(data.set[,1], sample(data.set[,2], replace=F))
    chisq.test(all.data.star[,1], all.data.star[,2], correct=F)$statistic
}


B<-1000
results<-matrix(data = NA, nrow = B, ncol = 1)

set.seed(1938)
for(i in 1:B) {
    results[i]<-do.it(data.set = all.data)
    }

summarize(result.set = results, statistic = x.sq$statistic, df = (nrow(n.table)-1)*(ncol(n.table)-1), B = B)

#Shows the different X^2* values
table(round(results,2))

#chi-square app.
round(pchisq(q = as.numeric(names(table(round(results,2)))), df = (nrow(n.table)-1)*(ncol(n.table)-1)),4)








#Method #2
set.seed(8912)
chisq.test(n.table, correct = FALSE, simulate.p.value = TRUE, B = 1000)




# Method #3
library(boot)    
                     
#Perform the test
x.sq<-function(data, i) {
  perm.data<-data[i]
  chisq.test(all.data[,1], perm.data, correct=F)$statistic
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
set.seed(6488)
perm.test<-boot(data = col, statistic = x.sq, R = 1000, sim = "permutation") 
summarize.boot(perm.test, (2-1)*(2-1))
