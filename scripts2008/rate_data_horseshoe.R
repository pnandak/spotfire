#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  3-9-05                                                     #
# UPDATE:                                                           #
# Purpose: Horseshoe crab example - using rate data and investigate #
#          plots                                                    #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#Read in data
crab<-read.table(file = "c:\\Chris\\OSU\\STAT5073\\chapter3\\horseshoe.txt", header=FALSE, col.names = c("satellite", "width"))


#################################################################################################################
# Convert to rate data and fit model

  library(nlme) #gsummary function is located here

  sum.rate.data<-gsummary(object = crab, FUN = sum, groups = crab$width) 
  length.rate.data<-gsummary(object = crab, FUN = length, groups = crab$width) 

  rate.data<-data.frame(y = sum.rate.data$satellite, t = length.rate.data$satellite, width = length.rate.data$width)
  rate.data[1:5,]

  mod.fit<-glm(formula = y ~ width+offset(log(t)), data = rate.data, family = poisson(link = log), 
               na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
  summary(mod.fit)



################################################################################################################
# PLOT #1 - Total number of satellites vs. distinct width - BETTER version of last plot in Section 4.3
#           However, we can do better than this!

  plot(x = rate.data$width, y = rate.data$y, xlab = "Width (cm)", ylab = "Number of satellites", 
       panel.first = grid(col = "gray", lty = "dotted"), 
       main = "Horseshoe crab data set \n with poisson regression model fit (rate data)")

  #Predicted values at their corresponding distinct widths
  points(x = rate.data$width, y = mod.fit$fitted.values, pch = 19, col = "blue", cex = 1)



################################################################################################################
# PLOT #2 - Total number of satellites vs. distinct width with plotting point equal to t 
#           This helps to make the trends for each t easier to see
#           Notice how I get the estimated model on the plot using min(width):max(width) for each t

  #Open a new graphics window
  win.graph(width = 6, height = 6, pointsize = 10)

  #Find the number of unique values of t and put into a vector
  plot.char.numb<-names(table(rate.data$t))

  #Do this first to get gridlines BEHIND the plotting points :)
  plot(x = rate.data$width, y = rate.data$y, xlab = "Width (cm)", ylab = "Number of satellites", type = "n",
     panel.first = grid(col = "gray", lty = "dotted"), 
     main = "Horseshoe crab data set \n with poisson regression model fit (rate data)")
  
  #Put observed values and estimated model on plot by values of t
  for (t in as.numeric(plot.char.numb)) {
     
    #Notice that plot.char.numb contains characters, but R transforms them to numeric in the x, y axis plotting.
    #  May need to be careful with this.  Could try as.numeric() function with it
    points(x = rate.data$width[rate.data$t == plot.char.numb[t]], y = rate.data$y[rate.data$t == plot.char.numb[t]],
           pch = plot.char.numb[t], cex = 0.5, col = t)
    
    lines(x = rate.data$width[rate.data$t == plot.char.numb[t]], y = mod.fit$fitted.values[rate.data$t == plot.char.numb[t]],
         lty = 1, col = t)        
  }
  
  #NOTE: The line for 8 is right on top of 8.  There was only one observation with t = 8.




################################################################################################################
# PLOT #3 - Total number of satellites vs. distinct width with plotting point equal to t 
#           This helps to make the trends for each t easier to see
#           Notice how I get the estimated model on the plot using 21:34 as width range

  #Open a new graphics window
  win.graph(width = 6, height = 6, pointsize = 10)

  #Find the number of unique values of t and put into a vector
  plot.char.numb<-names(table(rate.data$t))

  #Do this first to get gridlines BEHIND the plotting points :)
  plot(x = rate.data$width, y = rate.data$y, xlab = "Width (cm)", ylab = "Number of satellites", type = "n",
     panel.first = grid(col = "gray", lty = "dotted"), 
     main = "Horseshoe crab data set \n with poisson regression model fit (rate data)")
     
  #Do part of prediction which does not change with for different t values
  width<-21:34
  lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*width

  #Put observed values and estimated model on plot by values of t
  for (t in as.numeric(plot.char.numb)) {
     
    #Notice that plot.char.numb contains characters, but R transforms them to numeric in the x, y axis plotting.
    #  May need to be careful with this.  Could try as.numeric() function with it
    points(x = rate.data$width[rate.data$t == plot.char.numb[t]], y = rate.data$y[rate.data$t == plot.char.numb[t]],
           pch = plot.char.numb[t], cex = 0.5, col = t)
    mu.hat<-t*exp(lin.pred)
    lines(x = width, y = mu.hat, lty = 1, col = t)        
  }



################################################################################################################
# PLOT #4 - AVERAGE number of satellites vs. distinct width 
 
  #Open a new graphics window
  win.graph(width = 6, height = 6, pointsize = 10)

  plot(x = rate.data$width, y = rate.data$y/rate.data$t, xlab = "Width (cm)", ylab = "Average number per distinct width", 
     panel.first = grid(col = "gray", lty = "dotted"), 
     main = "Horseshoe crab data set \n with poisson regression model fit (rate data)")

  width<-21:34
  lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*width
  mu.hat<-exp(lin.pred)
  lines(x = width, y = mu.hat, lty = 1, col = "red")        

  #Notice how you can see the upward trend in the points here better than the first plot in Section 4.3.  This is because
  #  we have multiple crabs for each distinct width allowing for means (when t>1) to be plotted.  Remember, we are modeling
  #  the expected mean response.
  #Also, notice that only one line for the model appears on the plot.  This is because we are examining E(Y)/t, not E(Y)
  #      as done with the previous plots here.
  
  
  
################################################################################################################
# PLOT #5 - AVERAGE number of satellites vs. distinct width 
#           Plotting point corresponds to t

  #Open a new graphics window
  win.graph(width = 6, height = 6, pointsize = 10)

  #Find the number of unique values of t and put into a vector
  plot.char.numb<-names(table(rate.data$t))

  #Do this first to get gridlines BEHIND the plotting points :)
  plot(x = rate.data$width, y = rate.data$y/rate.data$t, xlab = "Width (cm)", ylab = "Average number per distinct width", type = "n",
     panel.first = grid(col = "gray", lty = "dotted"), 
     main = "Horseshoe crab data set \n with poisson regression model fit (rate data)")
  width<-21:34
  lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*width
  mu.hat<-1*exp(lin.pred)
  lines(x = width, y = mu.hat, lty = 1, col = "red")        

  #Put observed values on plot by values of t
  for (t in as.numeric(plot.char.numb)) {
     
    #Notice that plot.char.numb contains characters, but R transforms them to numeric in the x, y axis plotting.
    #  May need to be careful with this.  Could try as.numeric() function with it
    points(x = rate.data$width[rate.data$t == plot.char.numb[t]], 
           y = rate.data$y[rate.data$t == plot.char.numb[t]]/t,
           pch = plot.char.numb[t], cex = 0.5, col = t)
  }

  #I don't see any trends here.  If there were trends, this may indicate problems with the model fit.  
  #  For example, think about what it would mean if all t = 1's were below the line and all t = 2's were above the line.  





#
