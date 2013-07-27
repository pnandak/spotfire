#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-3-06                                                     #
# UPDATE: 12-8-07                                                   #
# PURPOSE: Simple in-class example                                  #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Read in data

  library(xlsReadWrite)
  gpa<-read.xls(file = "C:\\chris\\UNL\\STAT950\\chapter1\\gpa.xls", colNames = TRUE) 
  gpa

  #More general way to read in.  
  library(RODBC)
  z<-odbcConnectExcel("C:\\chris\\UNL\\STAT950\\chapter1\\gpa.xls")
  gpa2<-sqlFetch(z, "sheet1")
  close(z)
  
  head(gpa2)

#####################################################################
# Initial examination of sample

  y<-gpa$College
  t<-mean(y)
  cat("My sample is", sort(y), "\n which produces an observed statistic of", t, "\n")

  #EDF
  plot.ecdf(x = y, verticals = TRUE, do.p = FALSE, main = "EDF for College GPA", lwd = 2,
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylab = expression(hat(F)), xlab = "y")
 
  #If you want more information
  edf<-ecdf(x = y)
  summary(edf)
  plot(x = edf, verticals=TRUE, do.p = FALSE, main = "EDF for College GPA", ylab = "F^", xlab = "y")
  knots(edf)


#####################################################################
# Resample

  set.seed(4518)
  y.star<-sample(x = y, replace = TRUE)
  t.star<-mean(y.star)

  cat("My resample is", sort(y.star), "\n which produces an observed statistic of", t.star, "\n")
 
  table(y)
  table(y.star)

  R<-1000
  set.seed(4518)
  save.resample<-matrix(data = NA, nrow = R, ncol = length(y))
  #There are more efficient ways to do this than a for loop
  for (i in 1:R) {
    y.star<-sample(x = y, replace = TRUE)
    save.resample[i,]<-y.star
  }
  
  
  #Example resamples
  table(save.resample)/R

  
  #Compare to CLT approximation for T
  t.star<-apply(X = save.resample, MARGIN = 1, FUN = mean)
  summary(t.star)
  plot.ecdf(x = t.star, verticals = TRUE, do.p = FALSE, main = "Boot. estimate of G", lwd = 2, xlab = "t*",
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylab = "EDF or CLT est. CDF")
  #lines(x = sort(t.star), y = pnorm(sort(t.star), mean = t, sd = sd(y)/sqrt(length(y))), col = "red")   
  curve(expr = pnorm(x, mean = t, sd=sd(y)/sqrt(length(y))), col = "red", add = TRUE) 
  legend(locator(1), legend = c("T* EDF", "CLT app. for T"), col=c("black","red"), bty="n", lwd=c(2,1), cex=0.75)

  t.star.quantiles<-quantile(x = t.star, probs = seq(from = 0.05, to = 0.95, by = 0.05))
  CLT.quantiles<-round(p = qnorm(seq(from = 0.05, to = 0.95, by = 0.05), mean = t, sd = sd(y)/sqrt(length(y))),2)
  data.frame(t.star.quantiles, CLT.quantiles)
  
  hist(x = t.star, main = "Histogram for t*", freq=FALSE, ylim = c(0,3), xlab = "t*")
  curve(expr = dnorm(x, mean = t, sd=sd(y)/sqrt(length(y))), col = "red", add = TRUE)
  
#####################################################################
# Fit simple linear regression model.  

  mod.fit<-lm(formula = College ~ HS, data = gpa)
  summary(mod.fit)

  names(mod.fit)
  class(mod.fit)
  
  plot(mod.fit) 
  plot.lm(mod.fit)
  
  
  
#####################################################################
# Function for a statistic (not in notes)

  #  First element is the original or resampled data.
  #  Second element represents the indices of the data.  For example, the indices will be 1:20 for the observed
  #    data.   
  #    The reason for this second element will be discussed in Chapter 2
  #  Third element just shows how additional options can be passed into the function; 
  #   note that trim=0 is default for the mean() function and is not needed.  
  calc.t<-function(data, i, trim = 0) {
     data2<-data[i]
     mean(data2, trim = trim) 
  }

  #Try it
  calc.t(data = y, i = 1:20, trim = 0)



#####################################################################
# Example of how to use the boot package to do the resampling (not in notes)

  #Need to load it first even though it is automatically installed with R
  library(boot)
   
  #Do bootstrap
  set.seed(2729)
  boot.res<-boot(data = y, statistic = calc.t, R = 1000, sim="ordinary", trim = 0) 
  plot(boot.res)   
  
  names(boot.res)
  boot.res$t0
  head(boot.res$t)
  boot.res$statistic
  
  #List of indices from the resamples
  save.ind<-boot.array(boot.res, indices=TRUE)
  head(save.ind)
  
  #First resample
  y[save.ind[1,]]
  mean(y[save.ind[1,]])


  class(boot.res)

#
