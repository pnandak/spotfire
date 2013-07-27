############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-19-06                                                           #
# PURPOSE: Chapter 3 examples with the GPA data set                        #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#Read in the data
gpa<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter1\\gpa.txt", header=TRUE, sep = "")
head(gpa)

#Fit the simple linear regression model and save the results in mod.fit
mod.fit<-lm(formula = College.GPA ~ HS.GPA, data = gpa)
sum.fit<-summary(mod.fit)
sum.fit



##########################################################################
# Section 3.1 - Diagnostics for predictor variable

  #Examine range, mean, ...
  summary(gpa)
  
  #Box and dot plot
  par(mfrow = c(1,2)) #1 row and 2 columns of plots for a graphics window
  boxplot(x = gpa$HS.GPA, col = "lightblue", main = "Box plot", ylab = "HS GPA", xlab = " ")
  set.seed(1280) #Jitter option will randomly move in the x-axis direction the points to try to avoid
                 #  overlaying - setting a seed here forces the exact same jittering every time the code is run
  stripchart(x = gpa$HS.GPA, method = "jitter", vertical = TRUE, pch = 1, main = "Dot plot", 
             ylab = "HS GPA")
  
  
##############################################################################
# Examine response variable also

  #Box and dot plot
  par(mfrow = c(1,2)) #1 row and 2 columns of plots for a graphics window
  boxplot(x = gpa$College.GPA, col = "lightblue", main = "Box plot", ylab = "College GPA", xlab = " ")
  set.seed(1280) #Jitter option will randomly move in the x-axis direction the points to try to avoid
                 #  overlaying - setting a seed here forces the exact same jittering every time the code is run
  stripchart(x = gpa$College.GPA, method = "jitter", vertical = TRUE, pch = 1, main = "Dot plot", 
             ylab = "College GPA")
 

##############################################################################
# Modified Levene's test (Brown-Forsythe test)

  library(car)  #The Levene's Test function is in the package for Fox's book (although the function is not mentioned in the book!)
 
  group<-ifelse(gpa$HS.GPA < median(gpa$HS.GPA), 1, 2)
  levene.test(y = mod.fit$residuals, group = group)
  
  #Using the formulas instead for Levene's test
  e.tilde<-tapply(X = mod.fit$residuals, INDEX = group, FUN = median)
  data.frame(gpa, e = mod.fit$residuals, group, med.e = e.tilde[group])  #Show groups, e.tilde[group] matches up residuals with correct median
  d<-abs(mod.fit$residuals - e.tilde[group]) 
  t.test(formula = d ~ group, mu = 0, var.equal = TRUE, alternative = "two.sided")
 
  
##############################################################################
# Breusch Pagan test

  library(lmtest)  #Location of the function
  #bptest(formula = College.GPA ~ HS.GPA, data = gpa)
  bptest(formula = College.GPA ~ HS.GPA, data = gpa, studentize = FALSE)  #KNN Version of the test


 















#
