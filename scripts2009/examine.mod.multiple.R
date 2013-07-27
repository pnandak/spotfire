######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  7-26-06                                                     #
# UPDATE:                                                            #
# Purpose: Automate part of the simple linear regression diagnostic  #
#          procedures into one function.                             #
#                                                                    #
# NOTES: Based on examine.mod.simple.R used in Chapter 3.  I have    #
#        generalized it to allow for more than one predictor variable#
#        This function needs to be run just once before it is used.  #
#        One needs to call the function with model fit object only.  #
#        For example,                                                #
#                                                                    #
#  examine.mod.simple(mod.fit.obj = mod.fit)                         #
#                                                                    #
#        The seed for jittering can be changed with the              #
#        seed option.  The Levene and Breusch-Pagan tests can be     #
#        performed if the var.test option is change to TRUE.  Note   #
#        that the car and lmtest packages need to be loaded for these#
#        tests.  The Y optional values can be used if a              #
#        transformation of Y was used for the model.  For            #
#        example, if log(Y) was used, then fit the model with log(Y) #
#        and include the Y = log(original Y variable) when invoking  #
#        this function.  Unlike examine.mod.simple.R, there is not a #
#        similar X option.  The boxcox.find = TRUE option produces   #
#        an estimate for lambda.                                     #
#                                                                    #
######################################################################

examine.mod.multiple<-function(mod.fit.obj, seed = 9180, const.var.test = FALSE, Y = NULL, boxcox.find = FALSE) {

  n<-length(mod.fit.obj$residuals)
  pred.var.numb<-length(mod.fit.obj$coefficients)-1

  #X<-mod.fit.obj$model[,2:(2+pred.var.numb-1)]
  X<-model.matrix(mod.fit.obj)[,2:(2+pred.var.numb-1)]  #NOTE: This does not containing the first column of 1's in X!
  if (is.null(Y)) { Y<-mod.fit.obj$model[,1]}
  
   
  ##############################################################################
  #Graph window set #1
  
    #Open a new plotting window 1x2
    win.graph(width = 6, height = 6, pointsize = 10)
    par(mfrow = c(1,2)) 
    
    set.seed(seed) 
    boxplot(x = Y, col = "lightblue", main = "Box plot", ylab = "Response variable", xlab = " ")                   
    stripchart(x = Y, method = "jitter", vertical = TRUE, pch = 1, main = "Dot plot", ylab = "Response variable")  
      
      
    counter<-1 #Need for opening new plot windows below
    win.graph(width = 6, height = 6, pointsize = 10)
    par(mfrow = c(2,2)) 
 
    #X.ij box and dot plots
    for(j in 1:pred.var.numb) {
 
      if (counter/2 > 1) { 
         win.graph(width = 6, height = 6, pointsize = 10)
         par(mfrow = c(2,2))
         counter<-1  
      
      }
       
      boxplot(x = X[,j], col = "lightblue", main = "Box plot", ylab = paste("Predictor variable", j), xlab = " ")                 
      stripchart(x = X[,j], method = "jitter", vertical = TRUE, pch = 1, main = "Dot plot", ylab = paste("Predictor variable", j)) 
      
      counter<-counter + 1
    }
    
    #Initial summary statistics
    summary.data<-summary(data.frame(Y,X))
  
  
  ##############################################################################
  #Graph window set #2
  
    #Open a new plotting window 2x2
    win.graph(width = 6, height = 6, pointsize = 10)
    par(mfrow = c(1,2), pty = "s") 

    sum.fit<-summary(mod.fit.obj)
    semi.stud.resid<-mod.fit.obj$residuals/sum.fit$sigma  
  
    #e.i vs. Y.hat.i
    plot(x = mod.fit.obj$fitted.values, y = mod.fit.obj$residuals, xlab = "Estimated mean response", ylab = "Residuals", 
         main = "Residuals vs. estimated mean response", panel.first = grid(col = "gray", lty = "dotted"))
    abline(h = 0, col = "red")

    #e.i.star vs. Y.hat.i
    plot(x = mod.fit.obj$fitted.values, y = semi.stud.resid, xlab = "Estimated mean response", 
        ylab = "Semistud. residuals", main = expression(paste(e[i]^{"*"}, " vs. estimated mean response")), 
        panel.first = grid(col = "gray", lty = "dotted"), ylim = c(min(semi.stud.resid,-3), max(semi.stud.resid,3)))
    abline(h = 0, col = "red")
    abline(h = c(-3,3), col = "red", lwd = 2)
    identify(x = mod.fit.obj$fitted.values, y = semi.stud.resid)


  ##############################################################################
  #Graph window set #3

    win.graph(width = 6, height = 6, pointsize = 10)
    par(mfrow = c(2,2), pty = "m") 
    counter<-1 #Need for opening new plot windows below

    #e.i vs. X.ij
    for(j in 1:pred.var.numb) {
  
      if (counter/4 > 1) { 
         win.graph(width = 6, height = 6, pointsize = 10)
         par(mfrow = c(2,2))
         counter<-1  
      }
        
      plot(x = X[,j], y = mod.fit.obj$residuals, ylab = "Residuals", main = paste("Residuals vs. predictor",j), 
          xlab = paste("Predictor variable",j), panel.first = grid(col = "gray", lty = "dotted"))
      abline(h = 0, col = "red")
      
     counter<-counter + 1
    }



  ##############################################################################
  #Graph window set #4

    #Open a new plotting window 2x2
    win.graph(width = 6, height = 6, pointsize = 10)
    par(mfrow = c(2,2)) 

    #(1,1) - e.i vs. obs. number
    plot(x = 1:n,, y = mod.fit.obj$residuals, xlab = "Observation number", ylab = "Residuals", type = "o",
         main = "Residuals vs. observation number", panel.first = grid(col = "gray", lty = "dotted"))
    abline(h = 0, col = "red")
    
    #(1,2) - Histogram of the semi-studentized residuals with normal distribution overlay
    hist(x = semi.stud.resid, main = "Histogram of semistud. residuals", xlab = "Semistud. residuals",
         freq = FALSE)
    curve(expr = dnorm(x, mean = mean(semi.stud.resid), sd = sd(semi.stud.resid)), col = "red", add = TRUE)

    #(2,1) - QQ-plot done by R
    qqnorm(y = semi.stud.resid, ylab = "Semistud. residuals", panel.first = grid(col = "gray", lty = "dotted"))
    qqline(y = semi.stud.resid, col = "red")
 
 
  #Save all results so far
  save.res<-list(sum.data = summary.data, semi.stud.resid = round(semi.stud.resid,2))
 
  ##############################################################################
  #Levene and BP tests - Examine the residuals for normality
  
  if (const.var.test) {
  
    library(car)     #The Levene's Test function is in the package for Fox's book
    
    save.levene<-matrix(NA, nrow = pred.var.numb, ncol = 3)
    for(j in 1:pred.var.numb) {
    
      #This code will not work for indicator variables so this is a quick fix to skip over them.
      if(length(table(X[,j])) != 2) 
        {
         group<-ifelse(X[,j] < median(X[,j]), 1, 2)
         save.levene.test<-levene.test(y = mod.fit.obj$residuals, group = group)
         save.levene[j,]<-c(j, round(save.levene.test$"F value"[1],4), save.levene.test$"Pr(>F)"[1])
        }
    } 
 
    library(lmtest)  #Location of BP test function
    save.bp<-bptest(mod.fit.obj,  studentize = FALSE)  #KNN Version of the test
    
    save.res$levene<-save.levene
    save.res$bp<-save.bp
    
  }
  
  
  ##############################################################################
  #Box-cox transformation
  
  if (boxcox.find) {
  
    library(MASS)     #Function is in the MASS library
    save.bc<-boxcox(object = mod.fit.obj, lambda = seq(from = -2, to = 2, by = 0.01))
    title(main = "Box-Cox transformation plot")
    lambda.hat<-save.bc$x[save.bc$y == max(save.bc$y)] 
    save.res$lambda.hat<-lambda.hat
    
  }
  
  ##############################################################################
  #Return results
  
    save.res

}
