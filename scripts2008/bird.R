#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  2-6-06                                                     #
# UPDATE: 10-29-07                                                  #
# PURPOSE: Based upon bird_perm.R in UNL STAT 875                   #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
#  Data in a contigency table format

  #Create contingency table - notice the data is entered by columns
  n.table<-array(data = c(251, 48, 34, 5), dim=c(2,2), dimnames=list(First = c("made", "missed"), 
                 Second = c("made", "missed")))
  n.table




######################################################################
# Row and column number format (raw data)

  all.data<-matrix(NA, 0, 2)

  #Put data in "raw" form
  for (i in 1:nrow(n.table)) {
     for (j in 1:ncol(n.table)) {
        all.data<-rbind(all.data, matrix(data = c(i, j), nrow = n.table[i,j], ncol = 2, byrow=T))
    }
   }

  xtabs(~all.data[,1]+ all.data[,2])
  
  
  
###################################################################
# Pearson chi-square test for independence
  
  x.sq<-chisq.test(n.table, correct=F)
  x.sq
  
  chisq.test(x = all.data[,1], y = all.data[,2], correct=F)
  



######################################################################
# Permutation test - based upon "method #3" in bird_perm.R in Chapter 2 of UNL STAT 875
#   See the Chapter 2 "additional notes" in UNL STAT 875 for this example with the boot package
  
  library(boot)    
                     
  #Perform the test
  calc.t<-function(data, i, row.numb) {
    perm.data<-data[i]
    chisq.test(row.numb, perm.data, correct=F)$statistic
  }

  
  calc.t(data = all.data[,2], i = 1:length(all.data[,2]), row.numb = all.data[,1])

  set.seed(6488)
  R<-999
  perm.res<-boot(data = all.data[,2], statistic = calc.t, R = R, sim = "permutation", row.numb = all.data[,1]) 
  perm.res
  plot(perm.res, qdist="chisq", df = 1)
  
  #P-value 
  (1 + sum(perm.res$t>=perm.res$t0))/(R + 1)
  
  #P-value calculated in UNL STAT 875 - notice the effect of the statistic's discreteness
  #  There are many t* values that are the same
  sum(perm.res$t>=perm.res$t0)/R
  #Show the number of unique values of the test statistic
  xtabs(~perm.res$t)
  
  #Show the margins are the same as observed
  save.index<-boot.array(perm.res, indices = TRUE)
  i<-save.index[1,]
  d<-all.data[i,2]
  xtabs(~all.data[,1]+d)  
 
  
  par(mfrow = c(1,2), pty = "s", xaxs = "i")
  
  #Histogram^{2*}
  hist(perm.res$t, main = "Histogram of perm. dist.", xlab=expression(X^{"2*"}), freq = FALSE)
  abline(v = perm.res$t0[1], col = "darkgreen", lwd = 5)
  curve(dchisq(x, df = (nrow(n.table)-1)*(ncol(n.table)-1)), col = "red", add = TRUE)

  plot.ecdf(perm.res$t, verticals = TRUE, do.p = FALSE, main = expression(paste("EDF for ", X^{"2*"})), lwd = 2,
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylab = expression(hat(G)), xlab = expression(X^{"2*"}))
  curve(expr = pchisq(q = x, df = (nrow(n.table)-1)*(ncol(n.table)-1)), col = "red", add = TRUE)
  legend(x = 6, y = 0.4, legend = c(expression(hat(G)), "chi-square"), lwd = c(2,1), col = c("black", "red"), cex = 0.75)  

  chisq.quant<-qchisq(p = seq(from = 1/(R+1), to = 1-1/(R+1), by = 1/(R+1)), df = (nrow(n.table)-1)*(ncol(n.table)-1))
  plot(y = sort(perm.res$t), x = chisq.quant, main = expression(paste("QQ-Plot for ", X^{"2*"})), ylab = 
       expression(X^{"2*"}), xlab = expression(paste(chi[1]^{2}, " quantiles")), panel.first = grid(nx = NULL, ny = NULL, 
       col="gray", lty="dotted"))
  abline(a = 0, b = 1, col = "red")
 

 

#########################################################################
# Simpler implementation
#   In the chisq.test() code, it uses the following for the p-value: PVAL <- (1 + sum(ss >= STATISTIC))/(B + 1)
#   Notice >= is used instead of just >

  set.seed(8912)
  chisq.test(n.table, correct = FALSE, simulate.p.value = TRUE, B = 999)

  

#########################################################################
# Bootstrap approach 
 

  #Construct form of data set needed
  set1<-rbind(data.frame(index = all.data[,1], table.part = "X"), 
              data.frame(index = all.data[,2], table.part = "Y"))
  head(set1)
  tail(set1)
  
         
  #NOTE forcing a particular structure to data - "X" and "Y"
  #Check table size
  check.table<-function(data) {
    x<-data[data$table.part == "X",]  #Notice use of ==
    y<-data[data$table.part == "Y",]
    #save.table<-table(x$index, y$index)
    save.table<-xtabs(~x$index + y$index)
    numb.row<-nrow(save.table)
    numb.col<-ncol(save.table)
    c(numb.row, numb.col)
  }

                     
  #Perform the test
  calc.t<-function(data, i) {
    d<-data[i,]
    x<-d[d$table.part == "X",]  #Notice use of ==
    y<-d[d$table.part == "Y",]
    
    #Note: chisq.test() will return an error message if I<2 or J<2
    x.sq<-chisq.test(x$index, y$index, correct=F)$statistic  
    
    c(x.sq, check.table(d))
  }


  #Test calc.t with observed data
  calc.t(data = set1, i = 1:nrow(set1))

 
  set.seed(9180)
  R<-999
  boot.res<-boot(data = set1, statistic = calc.t, R = R, sim = "ordinary", strata = set1$table.part) 
  boot.res
  plot(boot.res, qdist="chisq", df = (nrow(n.table)-1)*(ncol(n.table)-1))
  
  #p-value
  (1 + sum(boot.res$t[,1]>=boot.res$t0[1]))/(R + 1)
   
  
  #How many contingency table* do not have original size?
  sum(boot.res$t[,2] != check.table(set1)[1])  #Not same number of rows
  sum(boot.res$t[,3] != check.table(set1)[2])  #Not same number of columns
  
  
  #Reform a contingency table*
  save.index<-boot.array(boot.res, indices = TRUE)
  i<-save.index[3,]
  d<-set1[i,]
  x<-d[d$table.part == "X",]  
  y<-d[d$table.part == "Y",]
  table(x$index, y$index)  
  check.table(d) != check.table(set1)   
  
  
  #Similar plots as for the permutation test
  par(mfrow = c(1,2), pty = "s", xaxs = "i")
  
  #Histogram
  hist(boot.res$t[,1], main = "Histogram of resampling dist.", xlab=expression(X^{"2*"}), freq = FALSE)
  abline(v = boot.res$t0[1], col = "darkgreen", lwd = 5)
  curve(dchisq(x, df = (nrow(n.table)-1)*(ncol(n.table)-1)), col = "red", add = TRUE)

  plot.ecdf(boot.res$t[,1], verticals = TRUE, do.p = FALSE, main = expression(paste("EDF for ", X^{"2*"}, "boot")), lwd = 2,
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylab = expression(hat(G)), xlab = 
            expression(X^{"2*"}))
  curve(expr = pchisq(q = x, df = (nrow(n.table)-1)*(ncol(n.table)-1)), col = "red", add = TRUE)
  legend(x = 6, y = 0.4, legend = c(expression(hat(G)), "chi-square"), lwd = c(2,1), col = c("black", "red"), cex = 0.75)  

  #Not necessary since the plot function with an object of class boot can also produce one of these plots
  chisq.quant<-qchisq(p = seq(from = 1/(R+1), to = 1-1/(R+1), by = 1/(R+1)), df = (nrow(n.table)-1)*(ncol(n.table)-1))
  plot(y = sort(boot.res$t[,1]), x = chisq.quant, main = expression(paste("QQ-Plot for ", X^{"2*"}, " boot"), ylab = 
       expression(X^{"2*"}), xlab = "chi-square(1) quantiles", panel.first = grid(nx = NULL, ny = NULL, 
       col="gray", lty="dotted"))
  abline(a = 0, b = 1, col = "red")
 

 
  
#
