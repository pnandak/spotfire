######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  8-15-06                                                     #
# UPDATE:                                                            #
# Purpose: Automate part of the linear regression diagnostic         #
#          procedures into one function.                             #
#                                                                    #
# NOTES: Based on examine.mod.multiple.R used in Chapter 6.          #
#        This function needs to be run just once before it is used.  #
#        One needs to call the function with model fit object and    #
#        first.order (how many "first order" model terms).           #
#        For example,                                                #
#                                                                    #
#  examine.mod.simple(mod.fit.obj = mod.fit, first.order = 4)        #
#                                                                    #
#        The first.order requirement tells R the number of predictor #
#        variables there are before examining squared terms,         #
#        interactions, ... .  These predictor variables need to be   #
#        given FIRST in the formula statement when fitting the model.#
#                                                                    #
#        The seed for jittering can be changed with the              #
#        seed option.  The Levene and Breusch-Pagan tests can be     #
#        performed if the var.test option is changed to TRUE.  Note  #
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

examine.mod.multiple.final<-function(mod.fit.obj, first.order, seed = 9180, const.var.test = FALSE, Y = NULL, 
                                     boxcox.find = FALSE, labels = 1:n) {

  n<-length(mod.fit.obj$residuals)
  p<-length(mod.fit.obj$coefficients)
  
  #X<-mod.fit.obj$model[,2:(2+first.order-1)]
  X<-as.matrix(mod.fit.obj$model[,2:(2+first.order-1)]) 
  #Need as.matrix() since first.order may be 1, which would cause problems in boxplot(x = X[,j], ...) part of code
  #  If first.order = 1, R will not know how to interpret X[,j] since it will be of a vector, not matrix, class type
  
  if (is.null(Y)) { Y<-mod.fit.obj$model[,1]}
 
  r.i<-rstandard(model = mod.fit.obj)
  t.i<-rstudent(model = mod.fit.obj)
 
   
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
    for(j in 1:first.order) {
 
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
  
    #Open a new plotting window 1x1
    win.graph(width = 6, height = 6, pointsize = 10)
    par(mfrow = c(1,1), pty = "m") 
 
    #e.i vs. Y.hat.i
    plot(x = mod.fit.obj$fitted.values, y = mod.fit.obj$residuals, xlab = "Estimated mean response", ylab = "Residuals", 
         main = "Residuals vs. estimated mean response", panel.first = grid(col = "gray", lty = "dotted"))
    abline(h = 0, col = "darkgreen")


    #Open a new plotting window 1x2
    win.graph(width = 8, height = 6, pointsize = 10)
    par(mfrow = c(1,2), pty = "s") 

    #r.i vs. Y.hat.i
    plot(x = mod.fit.obj$fitted.values, y = r.i, xlab = "Estimated mean response", 
        ylab = "Studentized residuals", main = expression(paste(r[i], " vs. estimated mean response")), 
        panel.first = grid(col = "gray", lty = "dotted"),
        ylim = c(min(qt(p = 0.05/(2*n), df = mod.fit.obj$df.residual), min(r.i)), 
                 max(qt(p = 1-0.05/(2*n), df = mod.fit.obj$df.residual), max(r.i))))
    abline(h = 0, col = "darkgreen")
    abline(h = c(qt(p = 0.01/2, df = mod.fit.obj$df.residual),qt(p = 1-0.01/2, df = mod.fit.obj$df.residual)), 
           col = "red", lwd = 2)
    abline(h = c(qt(p = 0.05/(2*n), df = mod.fit.obj$df.residual),qt(p = 1-0.05/(2*n), df = mod.fit.obj$df.residual)), 
           col = "darkred", lwd = 2)
    identify(x = mod.fit.obj$fitted.values, y = r.i, labels = labels)
        
 
    #t.i vs. Y.hat.i
    plot(x = mod.fit.obj$fitted.values, y = t.i, xlab = "Estimated mean response", 
        ylab = "Studentized deleted residuals", main = expression(paste(t[i], " vs. estimated mean response")), 
        panel.first = grid(col = "gray", lty = "dotted"),
        ylim = c(min(qt(p = 0.05/(2*n), df = mod.fit.obj$df.residual-1), min(t.i)), 
                 max(qt(p = 1-0.05/(2*n), df = mod.fit.obj$df.residual-1), max(t.i))))
    abline(h = 0, col = "darkgreen")
    abline(h = c(qt(p = 0.01/2, df = mod.fit.obj$df.residual-1),qt(p = 1-0.01/2, df = mod.fit.obj$df.residual-1)), col = "red", lwd = 2)
    abline(h = c(qt(p = 0.05/(2*n), df = mod.fit.obj$df.residual-1),qt(p = 1-0.05/(2*n), df = mod.fit.obj$df.residual-1)), col = "darkred", lwd = 2)
    identify(x = mod.fit.obj$fitted.values, y = t.i, labels = labels)

    #Fox's outlier.test() function gives the Bonferroni adjusted p-value for the largest in absolute value 
    #  studentized deleted residual; see p. 194 of Fox
    library(car)
    outlier.res<-outlier.test(mod.fit.obj)
    

  ##############################################################################
  #Graph window set #3 - could also do partial regression plots: cr.plots(model = mod.fit.obj, ask=FALSE, span=span)  

    win.graph(width = 6, height = 6, pointsize = 10)
    par(mfrow = c(2,2), pty = "m") 
    counter<-1 #Need for opening new plot windows below

    #e.i vs. X.ij
    for(j in 1:first.order) {
  
      if (counter/4 > 1) { 
         win.graph(width = 6, height = 6, pointsize = 10)
         par(mfrow = c(2,2))
         counter<-0  #Set back to 0 (Will be 1 after counter<-counter+1
      }
        
      plot(x = X[,j], y = mod.fit.obj$residuals, ylab = "Residuals", main = paste("Residuals vs. predictor",j), 
          xlab = paste("Predictor variable",j), panel.first = grid(col = "gray", lty = "dotted"))
      abline(h = 0, col = "darkgreen")
      
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
    abline(h = 0, col = "darkgreen")
    
    #(1,2) - Histogram of the residuals with normal distribution overlay
    hist(x = mod.fit.obj$residuals, main = "Histogram of residuals", xlab = "Residuals",
         freq = FALSE)
    curve(expr = dnorm(x, mean = mean(mod.fit.obj$residuals), sd = sd(mod.fit.obj$residuals)), col = "red", add = TRUE)

    #(2,1) - QQ-plot done by R
    qqnorm(y = mod.fit.obj$residuals, ylab = "Residuals", panel.first = grid(col = "gray", lty = "dotted"))
    qqline(y = mod.fit.obj$residuals, col = "red")
 
 
  ##############################################################################
  #Graph window set #5

    #Open a new plotting window 2x2
    win.graph(width = 8, height = 6, pointsize = 10)
    par(mfrow = c(2,1))   
    
    #(1,1) - DFFITS vs. observation number
    dffits.i<-dffits(model = mod.fit.obj)
    plot(x = 1:n, y = dffits.i, xlab = "Observation number", ylab = "DFFITS", main = "DFFITS vs. observation number", 
         panel.first = grid(col = "gray", lty = "dotted"), 
         ylim = c(min(-1, -2*sqrt(p/n), min(dffits.i)), max(1, 2*sqrt(p/n), max(dffits.i))))
    abline(h = 0, col = "darkgreen")
    abline(h = c(-2*sqrt(p/n), 2*sqrt(p/n)), col = "red", lwd = 2)
    abline(h = c(-1,1), col = "darkred", lwd = 2)
    identify(x = 1:n, y = dffits.i, labels = labels)
    
    #(1,2) - Cook's distance vs. observation number
    cook.i<-cooks.distance(model = mod.fit.obj)
    plot(x = 1:n, y = cook.i, xlab = "Observation number", ylab = "Cook's D", main = "Cook's D vs. observation number", 
         panel.first = grid(col = "gray", lty = "dotted"), 
         ylim = c(0, qf(p=0.5,df1=p, df2=mod.fit.obj$df.residual)))
    abline(h = 0, col = "darkgreen")
    abline(h = qf(p=0.5,df1=p, df2=mod.fit.obj$df.residual), col = "red", lwd = 2)
    identify(x = 1:n, y = cook.i, labels = labels)


    #Open a new plotting window 2x2
    win.graph(width = 8, height = 6, pointsize = 10)
    par(mfrow = c(2,2))   
    counter<-1 #Need for opening new plot windows below

    #DFBETAS vs. observation number
    dfbeta.all<-dfbetas(model = mod.fit.obj) 
    for(j in 1:(p-1)) {
    
      if (counter/4 > 1) { 
         win.graph(width = 8, height = 6, pointsize = 10)
         par(mfrow = c(2,2))
         counter<-0  #Set back to 0 (Will be 1 after counter<-counter+1
      }
  
      plot(x = 1:n, y = dfbeta.all[,1+j], xlab = "Observation number", ylab = "DFBETAS", 
           main = paste("DFBETAS for term", j, "vs. observation number"), 
           panel.first = grid(col = "gray", lty = "dotted"), 
           ylim = c(min(-1, -2/sqrt(n), min(dfbeta.all[,1+j])), max(1, 2/sqrt(n), max(dfbeta.all[,1+j]))))
      abline(h = 0, col = "darkgreen")
      abline(h = c(-2/sqrt(n), 2/sqrt(n)), col = "red", lwd = 2)
      abline(h = c(-1,1), col = "darkred", lwd = 2)
      identify(x = 1:n, y = dfbeta.all[,1+j], labels = labels)
      
      counter<-counter + 1
    }
  
  #Only need to do the VIF if more than 1 predictor variable
  if (first.order > 1) { vif1<-vif(mod.fit.obj)}
  else { vif1<-NA }
  
 
  #Save all results so far
  save.res<-list(sum.data = summary.data, dffits = round(dffits.i,2), cook = round(cook.i,2), 
                 studentized = r.i, stud.del = t.i, dfbetas = dfbeta.all, outlier.test = outlier.res,
                 vif = vif1)
 

 
  ##############################################################################
  #Levene and BP tests - Examine the residuals for normality
  
  if (const.var.test) {
  
    library(car)     #The Levene's Test function is in the package for Fox's book
    
    save.levene<-matrix(NA, nrow = first.order, ncol = 3)
    for(j in 1:first.order) {
    
      group<-ifelse(X[,j] < median(X[,j]), 1, 2)
      save.levene.test<-levene.test(y = mod.fit.obj$residuals, group = group)
      save.levene[j,]<-c(j, round(save.levene.test$"F value"[1],4), save.levene.test$"Pr(>F)"[1])
 
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
