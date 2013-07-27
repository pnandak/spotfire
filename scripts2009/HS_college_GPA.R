############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  12-20-03, 12-10-05, 7-16-06                                       #
# PURPOSE: Chapter 1 example with the GPA data set                         #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#Read in the data
gpa<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter1\\gpa.txt", header=TRUE, sep = "")

#Print data set
gpa

#Print one variable
gpa$HS.GPA
gpa[,1]

#Summary statistics for variables
summary(gpa)

#Simple scatter plot
plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA", 
     xlim = c(0,4.5), ylim = c(0,4.5), col = "red", pch = 1, cex = 1.0, panel.first = grid(col = "gray", lty = "dotted"))



#Read in an Excel version of the file (12-10-05) - be careful with how it removes the "." in the variable names
library(RODBC)
z<-odbcConnectExcel("C:\\chris\\UNL\\STAT870\\R_intro\\gpa.xls")
gpa.excel<-sqlFetch(z, "sheet1")
close(z)
gpa.excel

#Alternative way to get Excel file into R (8-14-08)
library(xlsReadWrite)
gpa.excel<-read.xls(file = "C:\\chris\\UNL\\STAT870\\R_intro\\gpa.xls", colNames = TRUE) 
gpa.excel


########################################################################
# Find estimated simple linear regression model (sample model)
     
  #Fit the simple linear regression model and save the results in mod.fit
  mod.fit<-lm(formula = College.GPA ~ HS.GPA, data = gpa)

  #A very brief look of what is inside of mod.fit - see the summary function for a better way
  mod.fit

  #See the names of all of the object components
  names(mod.fit)
  mod.fit$coefficients 
  mod.fit$residuals

  #Put some of the components into a data.frame object
  save.fit<-data.frame(gpa, College.GPA.hat = round(mod.fit$fitted.values,2), residuals = round(mod.fit$residuals,2))

  #Print contents save.fit 
  save.fit

  #Summarize the information stored in mod.fit
  summary(mod.fit)
  
  
  #Prediction - note that the actual function used here is predict.lm()
  predict(object = mod.fit)
  predict.lm(object = mod.fit)
  new.data<-data.frame(HS.GPA = c(2,3))
  save.pred<-predict(object = mod.fit, newdata = new.data)
  round(save.pred,2)
   

  #Other ways to find MSE:
  
    #Method #1
    names(mod.fit)
    mod.fit$residuals
    sum(mod.fit$residuals^2)/mod.fit$df.residual

    #Method #2
    summary.fit<-summary(mod.fit)
    names(summary.fit)
    summary.fit$sigma
    summary.fit$sigma^2



########################################################################
#Put sample model on plot

  #Open a new graphics window
  win.graph(width = 6, height = 6, pointsize = 10)

  #Same scatter plot as before
  plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA", 
       xlim = c(0,4.5), ylim = c(0,4.5), col = "red", pch = 1, cex = 1.0, panel.first=grid(col = "gray", lty = "dotted"))

  #Puts the line y = a + bx on the plot
  abline(a = mod.fit$coefficients[1], b = mod.fit$coefficients[2], lty = 1, col = "blue", lwd = 2)


  #Notice the above line goes outside of the range of the x-values.  To prevent this, we can use the segments function
  #Open a new graphics window - do not need to
  win.graph(width = 6, height = 6, pointsize = 10)
 
  #Same scatter plot as before
  plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA", 
       xlim = c(0,4.5), ylim = c(0,4.5), col = "red", pch = 1, cex = 1.0, panel.first=grid(col = "gray", lty = "dotted"))

  #Draw a line from (x0, y0) to (x1, y1)
  segments(x0 = min(gpa$HS.GPA), y0 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*min(gpa$HS.GPA), 
           x1 = max(gpa$HS.GPA), y1 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*max(gpa$HS.GPA),
           lty = 1, col = "blue", lwd = 2)

  #Another way
  #curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, col = "blue", lty = 1, lwd = 2, add = TRUE, xlim = c(min(gpa$HS.GPA), max(gpa$HS.GPA)))



############################################################################################################
# Create a function to find the sample model and put the line on a scatter plot

  my.reg.func<-function(x, y, data) {
  
    #Fit the simple linear regression model and save the results in mod.fit
    mod.fit<-lm(y ~ x, data = data)
  
    #Open a new graphics window - do not need to
    win.graph(width = 6, height = 6, pointsize = 10)
 
    #Same scatter plot as before
    plot(x = x, y = y, xlab = "x", ylab = "y", main = "y vs. x", 
         col = "red", pch = 1, cex = 1.0, panel.first=grid(col = "gray", lty = "dotted"))

    #Draw a line from (x0, y0) to (x1, y1)
    segments(x0 = min(x), y0 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*min(x), 
             x1 = max(x), y1 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*max(x),
             lty = 1, col = "blue", lwd = 2)
    
    #This is the object returned
    mod.fit           
  }


  save.it<-my.reg.func(gpa$HS.GPA, gpa$College.GPA, gpa)  
  names(save.it)
  summary(save.it)
  
  








#
