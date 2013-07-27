############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  12-20-03                                                          #
# PURPOSE: Simple data analysis example in R using the gpa data set        #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#Read in the data
gpa<-read.table(file = "C:\\chris\\UNL\\STAT875\\R_intro\\gpa.txt", header=TRUE)

#Print data set
gpa

#Summary statistics for variables
summary(gpa)

#Simple scatter plot
plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA", 
     xlim = c(0,4.5), ylim = c(0,4.5), col = "red", pch = 1, cex = 1.0, panel.first=grid(col = "gray", lty = "dotted"))


########################################################################
# Find estimated simple linear regression model
     
  #Fit the simple linear regression model and save the results in mod.fit
  mod.fit<-lm(College.GPA ~ HS.GPA, data = gpa)

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


########################################################################
#Put regression line on plot

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



############################################################################################################
# Create a function to find the estimated simple linear regression model and put the line on a scatter plot

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
