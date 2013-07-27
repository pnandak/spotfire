#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  3-9-05                                                     #
# UPDATE: 3-28-05, 12-26-07 for new edition of book                 #
# Purpose: Show how to create Table 3.3 of Agresti (2007) and assess#
#  goodness of fit                                                  #
#                                                                   #
#####################################################################

#Read in data
crab<-read.table(file = "c:\\Chris\\UNL\\STAT875\\chapter3\\horseshoe.txt", header=FALSE, col.names = c("satellite", "width"))

#Fit model to original data format
mod.fit<-glm(formula = satellite ~ width, data = crab, family = poisson(link = log), 
             na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)

#Put predictions from the model for the full data set into the same data.frame as the data
crab2<-data.frame(crab, predicted = mod.fit$fitted.values)

###########################################################################################
# Reproduce part of Table 3.3

  #Agresti chose these classes - Why?  I do not know; however, it makes some sense to go by 1 cm per class
  #  and to start low and end high
  groups<-ifelse(crab$width<23.25, 1,
          ifelse(crab$width<24.25, 2,
          ifelse(crab$width<25.25, 3,
          ifelse(crab$width<26.25, 4,
          ifelse(crab$width<27.25, 5,
          ifelse(crab$width<28.25, 6,
          ifelse(crab$width<29.25, 7, 8)))))))
  crab.group<-data.frame(crab2,groups)    
    
  library(nlme) #Need package for the gsummary() function

  width.mean<-gsummary(object = crab.group, FUN = mean, groups = groups) 
  sat.count<-gsummary(object = crab.group, FUN = length, groups = groups)
  sat.sum<-gsummary(object = crab.group, FUN = sum, groups = groups)
  sat.var<-gsummary(object = crab.group, FUN = var, groups = groups)
  
  
  table3.3<-data.frame(width.group = width.mean$width, number.cases = sat.count$satellite, 
                       number.sat = sat.sum$satellite, mean.per.group = round(width.mean$satellite,2), 
                       var.per.group =  round(sat.var$satellite,2), fitted.count =  round(sat.sum$predicted,2), 
                       Pearson.residual =  round((sat.sum$satellite - sat.sum$predicted)/sqrt(sat.sum$predicted),2))
  table3.3
 
    
  #This produces the Pearson statistic given on p. 90 (Agresti, 1996).  
  cat("Ad-hoc Pearson statistic:", round(sum(table3.3$Pearson.residual^2),2), "with 6 DF results in a p-value of", 
      round(1-pchisq(sum(table3.3$Pearson.residual^2), 6),2), "using a chi-square distribution approximation \n")
 
  #This produces the G^2 statistic given on p. 90 (Agresti, 1996).  
  G.sq2<-2*sum(table3.3$number.sat*log(table3.3$number.sat/table3.3$fitted.count))
  cat("Ad-hoc G^2 statistic:", round(G.sq2,2), "with 6 DF results in a p-value of", 
      round(1-pchisq(G.sq2, 6),2), "using a chi-square distribution approximation \n")


  #Visual assessment
  plot(x = crab$width, y = crab$satellite, xlab = "Width (cm)", ylab = "Number of satellites", 
       main = "Horseshoe crab data set \n with poisson regression model fit", panel.first = 
       grid(col = "gray", lty = "dotted"))
  curve(expr = exp(mod.fit$coefficients[1]+mod.fit$coefficients[2]*x), lty = 1, col = "red", add = TRUE)
  points(x = table3.3$width.group, y = table3.3$mean.per.group, pch = 18, col = "darkgreen", cex = 2)
  points(x = table3.3$width.group, y = table3.3$fitted.count/table3.3$number.cases, pch = 17, col = "darkblue", cex = 2)
  

  #Put group breaks on plot
  width.agresti<-c(23.25, 24.25, 25.25, 26.25, 27.25, 28.25, 29.25)
  for (i in (1:7)) {
    abline(v = width.agresti[i], lty = 1, col = "lightgreen")
  }

  legend(locator(1), legend = c("Table 3.3 obs. means", "Table 3.3 predicted (using my interpret)"), pch = c(18,17), col = c("darkgreen","darkblue"),
         cex = 0.75, bg = "white")

 
###################################################################################################
# This is what Agresti (1996) does, but he does not in the 2007 edition of the book
#   Agresti directly refits the model to the data in Table 3.3 (I am not a proponent of this approach)
#   Agresti concludes there is no evidence of lack of fit.  

  #Fit model to the table 3.3 version of the data (like a rate data format); thus, there are 8 rows of values here
  mod.fit.table<-glm(formula = number.sat ~ width.group + offset(log(number.cases)), data = table3.3, family = poisson(link = log), 
               na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
  summary(mod.fit.table)
  
  #Find ad-hoc Pearson residuals for this version of the data
  Pearson<-resid(mod.fit.table, type="pearson")
  
  #Find ad-hoc adjusted residuals for this version of the data
  h<-lm.influence(mod.fit.table)$h 
  adj.pearson<-Pearson/sqrt(1-h)

  
  #This matches Agresti (1996) Table 4.3
  table4.3.actual<-data.frame(table3.3[,-(5:6)], fitted.count = round(mod.fit.table$fitted.values,1),
                              Pearson.residual = round(Pearson,2), Adjusted.residual = round(adj.pearson,2))
  table4.3.actual
  
  
  #Interesting that the Pearson statistic is 6.5 on p. 90 (Agresti, 1996), but I get 6.25 here.  
  #  The Pearson residuals are the same as given in Table 4.3 of Agresti (1996)!!!!
  cat("Ad-hoc Pearson statistic:", round(sum(Pearson^2),2), "with 8-2=6 DF results in a p-value of", 
      round(1-pchisq(sum(Pearson^2), 6),2), "using a chi-square distribution approximation \n")
  #Note that the LRT results in a G^2 = 6.51 here, but Agresti gives 6.9.  
 



#########################################################################################
# More general way to put observations into classes

  #Find 8 (9 quantiles) groups (why 8?  Since Agresti had chosen 8 - other choices could have been made)
  cutoff<-quantile(crab$width, probs = 0:8/8, na.rm = F) 
  cutoff

  #Use midpoint for the width group designation; note that I could have used the mean width among all crabs 
  #  within the group as well - there is not one correct way to do this. 
  groups<-ifelse(crab$width<cutoff[2], (cutoff[2]+cutoff[1])/2, 
          ifelse(crab$width<cutoff[3], (cutoff[3]+cutoff[2])/2,
          ifelse(crab$width<cutoff[4], (cutoff[4]+cutoff[3])/2,
          ifelse(crab$width<cutoff[5], (cutoff[5]+cutoff[4])/2,
          ifelse(crab$width<cutoff[6], (cutoff[6]+cutoff[5])/2, 
          ifelse(crab$width<cutoff[7], (cutoff[7]+cutoff[6])/2,
          ifelse(crab$width<cutoff[8], (cutoff[8]+cutoff[7])/2, 
                (cutoff[9]+cutoff[8])/2)))))))

  library(nlme) #Need package for the gsummary() function - don't need to rerun if already did above

  crab.group<-data.frame(crab2, groups)
  sat.count<-gsummary(object = crab.group, FUN = length, groups = groups)
  sat.sum<-gsummary(object = crab.group, FUN = sum, groups = groups) 
  new.table3.3<-data.frame(width.group = sat.count$groups, number.cases = sat.count$satellite, 
                           number.sat = sat.sum$satellite, mean.per.group = sat.sum$satellite/sat.count$satellite,
                           fitted.count = round(sat.sum$predicted,1), 
                           Pearson.residual = round((sat.sum$satellite - sat.sum$predicted)/sqrt(sat.sum$predicted),2))
  new.table3.3
  
  #Pearson statistic
  cat("Ad-hoc Pearson statistic:", round(sum(new.table3.3$Pearson.residual^2),2), "with 6 DF results in a p-value of", 
      round(1-pchisq(sum(new.table3.3$Pearson.residual^2), 6),2), "using a chi-square distribution approximation \n")
 
  #G^2
  G.sq2<-2*sum(new.table3.3$number.sat*log(new.table3.3$number.sat/new.table3.3$fitted.count))
  cat("Ad-hoc G^2 statistic:", round(G.sq2,2), "with 6 DF results in a p-value of", 
      round(1-pchisq(G.sq2, 6),2), "using a chi-square distribution approximation \n")
  
  #This is interesting that these two measures suggest the model does not fit well!  I would hope
  #  that goodness-of-fit conclusions would be invariant to the way one chooses to group the observations
  #  Possibly, this is example of why ad-hoc procedures can not always be trusted.  
 
 
 #Visual assessment
  win.graph(width = 6, height = 6, pointsize = 10)
  plot(x = crab$width, y = crab$satellite, xlab = "Width (cm)", ylab = "Number of satellites", 
       main = "Horseshoe crab data set \n with poisson regression model fit", panel.first = 
       grid(col = "gray", lty = "dotted"))

  width<-21:34
  lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*width
  mu.hat<-exp(lin.pred)
  lines(x = width, y = mu.hat, lty = 1, col = "red")

  points(x = new.table3.3$width.group, y = new.table3.3$mean.per.group, pch = 18, col = "darkgreen", cex = 2)
  #Notice these points are not on the estimated model line; probably due to using group average value for x-axis instead of 
  #  weighted mean like did for the previous plot
  points(x = new.table3.3$width.group, y = new.table3.3$fitted.count/new.table3.3$number.cases, pch = 17, col = "darkblue", cex = 2)
  

  #Put group breaks on plot
  for (i in (2:8)) {
    abline(v = cutoff[i], lty = 1, col = "lightgreen")
  }

  legend(locator(1), legend = c("Obs. group means", "Predicted group means"), pch = c(18,17), col = c("darkgreen","darkblue"),
         cex = 0.75, bg = "white")


###############################################################################################################
#Hosmer and Lemeshow type of test for the model - put into groups based upon predicted values instead of widths
#  This test produces the same result as the last one since there is only one explanatory variable. 
  






#
