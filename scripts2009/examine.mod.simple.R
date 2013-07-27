######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  7-21-06                                                     #
# UPDATE:                                                            #
# Purpose: Automate part of the simple linear regression diagnostic  #
#          procedures into one function.                             #
#                                                                    #
# NOTES: This function needs to be run just once before it is used.  #
#        One needs to call the function with model fit object only.  #
#        For example,                                                #
#                                                                    #
#  examine.mod.simple(mod.fit.obj = mod.fit)                         #
#                                                                    #
#        The seed for jittering can be changed with the              #
#        seed option.  The Levene and Breusch-Pagan tests can be     #
#        performed if the var.test option is change to TRUE.  Note   #
#        that the car and lmtest packages need to be loaded for these#
#        tests.  The X and Y optional values can be used if a        #
#        transformation of X or Y was used for the model.  For       #
#        example, if log(Y) was used, then fit the model with log(Y) #
#        and include the Y = log(original Y variable) when invoking  #
#        this function.  The boxcox.find = TRUE option produces      #
#        an estimate for lambda.                                     #
#                                                                    #
######################################################################

examine.mod.simple<-function(mod.fit.obj, seed = 9180, const.var.test = FALSE, X = NULL, Y = NULL, boxcox.find = FALSE, boxcox.low = -2,
                             boxcox.up = 2) {

  n<-length(mod.fit.obj$residuals)
  
  if (is.null(X)) { X<-mod.fit.obj$model[,2]}
  if (is.null(Y)) { Y<-mod.fit.obj$model[,1]}
  
   
  ##############################################################################
  #Graph window #1
  
    #Open a new plotting window 2x2
    win.graph(width = 6, height = 6, pointsize = 10)
    par(mfrow = c(2,2)) 
    
    set.seed(seed) 
    boxplot(x = X, col = "lightblue", main = "Box plot", ylab = "Predictor variable", xlab = " ")                  #(1,1)
    stripchart(x = X, method = "jitter", vertical = TRUE, pch = 1, main = "Dot plot", ylab = "Predictor variable") #(1,2)
    boxplot(x = Y, col = "lightblue", main = "Box plot", ylab = "Response variable", xlab = " ")                   #(2,1)
    stripchart(x = Y, method = "jitter", vertical = TRUE, pch = 1, main = "Dot plot", ylab = "Response variable")  #(2,2)

    #Initial summary statistics
    summary.data<-summary(data.frame(Y,X))
  
  ##############################################################################
  #Graph window #2
  
    #Open a new plotting window 2x2
    win.graph(width = 6, height = 6, pointsize = 10)
    par(mfrow = c(2,2)) 
 
    sum.fit<-summary(mod.fit.obj)
    semi.stud.resid<-mod.fit.obj$residuals/sum.fit$sigma  

    #(1,1) - Scatter plot with sample model
    plot(x = X, y = Y, main = "Response vs. predictor", xlab = "Predictor variable", ylab = "Response variable",
        panel.first = grid(col = "gray", lty = "dotted"))
    curve(expr = mod.fit.obj$coefficients[1] + mod.fit.obj$coefficients[2]*x, col = "red", 
          add = TRUE, from = min(X), to = max(X))

    #(1,2) - e.i vs. X.i
    plot(x = X, y = mod.fit.obj$residuals, ylab = "Residuals", main = "Residuals vs. predictor", xlab = "Predictor variable",
         panel.first = grid(col = "gray", lty = "dotted"))
    abline(h = 0, col = "red")

    #(2,1) - e.i vs. Y.hat.i
    plot(x = mod.fit.obj$fitted.values, y = mod.fit.obj$residuals, xlab = "Estimated mean response", ylab = "Residuals", 
         main = "Residuals vs. estimated mean response", panel.first = grid(col = "gray", lty = "dotted"))
    abline(h = 0, col = "red")

    #(2,2) - e.i.star vs. Y.hat.i
    plot(x = mod.fit.obj$fitted.values, y = semi.stud.resid, xlab = "Estimated mean response", 
        ylab = "Semistud. residuals", main = expression(paste(e[i]^{"*"}, " vs. estimated mean response")), 
        panel.first = grid(col = "gray", lty = "dotted"), ylim = c(min(semi.stud.resid,-3), max(semi.stud.resid,3)))
    abline(h = 0, col = "red")
    abline(h = c(-3,3), col = "red", lwd = 2)
    identify(x = mod.fit.obj$fitted.values, y = semi.stud.resid)


  ##############################################################################
  #Graph window #3

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
    group<-ifelse(X < median(X), 1, 2)
    save.levene<-levene.test(y = mod.fit.obj$residuals, group = group)
 
    library(lmtest)  #Location of BP test function
    save.bp<-bptest(formula = Y ~ X,  studentize = FALSE)  #KNN Version of the test
    
    save.res$levene<-save.levene
    save.res$bp<-save.bp
    
  }
  
  
  ##############################################################################
  #Box-cox transformation
  
  if (boxcox.find) {
  
    library(MASS)     #Function is in the MASS library
    save.bc<-boxcox(object = mod.fit.obj, lambda = seq(from = boxcox.low, to = boxcox.up, by = 0.01))
    title(main = "Box-Cox transformation plot")
    lambda.hat<-save.bc$x[save.bc$y == max(save.bc$y)] 
    save.res$lambda.hat<-lambda.hat
    
  }
  
  par(mfrow = c(1,1)) 

  ##############################################################################
  #Return results
  
    save.res

}
