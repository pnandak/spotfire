#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  2-7-06                                                     #
# UPDATE: 10-29-07                                                  #
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


chisq.test(n.table, correct=F)


######################################################################
# Row and column number format (raw data)
  
  all.data<-matrix(NA, 0, 2)

  #Put data in "raw" form
  for (i in 1:nrow(n.table)) {
     for (j in 1:ncol(n.table)) {
        all.data<-rbind(all.data, matrix(data = c(i, j), nrow = n.table[i,j], ncol = 2, byrow=T))
    }
   }
  #Note that warning messages will be generated since n.table[i,j]=0 sometimes


  xtabs(~all.data[,1]+ all.data[,2])
  

######################################################################
# Permutation test - see tab2.10-v2.R in Chapter 2 of STAT 875
  
  library(boot)    
                     
  #Perform the test
  calc.t<-function(data, i, row.numb) {
    perm.data<-data[i]
    chisq.test(row.numb, perm.data, correct=F)$statistic
  }


  set.seed(6488)  #NOTE: I should have used a different seed number than in bird.R
  R<-999
  perm.res<-boot(data = all.data[,2], statistic = calc.t, R = R, sim = "permutation", row.numb = all.data[,1]) 
  #Warnings mentioned in output are the "Chi-squared approximation may be incorrect" warnings from having 
  #  low cell counts in the contingency tables*
  perm.res
  plot(perm.res, qdist="chisq", df = (nrow(n.table)-1)*(ncol(n.table)-1))
  
  
  #P-value 
  (1 + sum(perm.res$t>=perm.res$t0))/(R + 1)

  
  par(mfrow = c(1,2), pty = "s", xaxs = "i")
  
  #Histogram
  hist(perm.res$t, main = "Histogram of perm. dist.", xlab=expression(X^{"2*"}), freq = FALSE)
  abline(v = perm.res$t0[1], col = "darkgreen", lwd = 5)
  curve(dchisq(x, df = (nrow(n.table)-1)*(ncol(n.table)-1)), col = "red", add = TRUE)

  plot.ecdf(perm.res$t, verticals = TRUE, do.p = FALSE, main = expression(paste("EDF for ", X^{"2*"})), lwd = 2,
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylab = expression(hat(G)), xlab = expression(X^{"2*"}))
  curve(expr = pchisq(q = x, df = (nrow(n.table)-1)*(ncol(n.table)-1)), col = "red", add = TRUE)
  #legend(x = 6, y = 0.4, legend = c("G^", "chi-square"), lwd = c(2,1), col = c("black", "red"), cex = 0.75)  

  #Not necessary since the plot function with an object of class boot can also produce one of these plots
  chisq.quant<-qchisq(p = seq(from = 1/(R+1), to = 1-1/(R+1), by = 1/(R+1)), df = (nrow(n.table)-1)*(ncol(n.table)-1))
  plot(y = sort(perm.res$t), x = chisq.quant, main = "QQ-Plot for X^2*", ylab = 
       "X^2*", xlab = "chi-square(1) quantiles", panel.first = grid(nx = NULL, ny = NULL, 
       col="gray", lty="dotted"))
  abline(a = 0, b = 1, col = "red")
 


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
    save.table<-table(x$index, y$index)
    numb.row<-nrow(save.table)
    numb.col<-ncol(save.table)
    c(numb.row, numb.col)
  }

                     
  #Perform the test
  calc.t<-function(data, i) {
    d<-data[i,]
    x<-d[d$table.part == "X",]  #Notice use of ==
    y<-d[d$table.part == "Y",]
    x.sq<-chisq.test(x$index, y$index, correct=F)$statistic  
    c(x.sq, check.table(d))
  }


  #Test calc.t with observed data
  calc.t(data = set1, i = 1:nrow(set1))

 
  set.seed(8372)
  R<-999
  boot.res<-boot(data = set1, statistic = calc.t, R = R, sim = "ordinary", strata = set1$table.part) 
  boot.res
  plot(boot.res)
  
  
  #How many contingency table* do not have original size?
  sum(boot.res$t[,2] != check.table(set1)[1])  #Not same number of rows
  sum(boot.res$t[,3] != check.table(set1)[2])  #Not same number of columns
  
  
  #Reform a contingency table*
  save.index<-boot.array(boot.res, indices = TRUE)
  i<-save.index[1,]
  d<-set1[i,]
  x<-d[d$table.part == "X",]  
  y<-d[d$table.part == "Y",]
  table(x$index, y$index)  
  check.table(d) != check.table(set1) 
   
  #FINAL NOTE: This is a good example of a place not to use the bootstrap!
 
 
 
 
 
# 
