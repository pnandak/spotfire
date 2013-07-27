######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  1-26-02                                                     #
# UPDATE: 12-13-03, 1-17-05, 12-24-05                                #
# Purpose: Automate part of the logistic regression diagnostic       #
#          procedures into one function.                             #
#                                                                    #
# NOTES: This function needs to be run just once before it used.     #
#        One only needs to call the function with the model fit      #
#        object.  For example,                                       #
#                                                                    #
#  mod.fit<-glm(formula = y/n ~ distance, data = place.pattern,      #
#            weight=n, family = binomial(link = logit),              #
#            control = list(epsilon = 0.0001, maxit = 50, trace = T))#
#  examine.resid(mod.fit)                                            #
#                                                                    #
#  Note that the data needs to be in explanatory variable pattern    #
#  form.                                                             #
######################################################################

examine.resid<-function(mod.fit.obj=mod.fit){
    
  h<-influence(mod.fit.obj)$h 
  pearson<-resid(mod.fit.obj, type="pearson")
  stand.resid<-pearson/sqrt(1-h)
  sq.stand.resid<-stand.resid^2
  pred<-mod.fit.obj$fitted.values
  n<-mod.fit.obj$prior.weights
  df<-mod.fit.obj$df.residual
  delta.beta<-sq.stand.resid*h/(1-h)

  #Pearson statisic
  pear.stat<-sum(pearson^2)
  cat("The Pearson statistic is", round(pear.stat,4), "with p-value =", 
      round(1-pchisq(pear.stat, df),4))

  #Put in a blank line
  cat("\n")

  dev<-summary(mod.fit.obj)$deviance
  cat("The G^2 is", round(dev, 4), "with p-value =", round(1-pchisq(dev, df),4), "\n")

  #Open a new plotting window
  win.graph(width = 6, height = 6, pointsize = 10)
  
  #Split plot into four parts
  par(mfrow=c(2,2))

  #Pearson residual vs oj (explanatory variable pattern number) plot
  plot(x = 1:length(pearson), y = pearson, xlab="j (explanatory variable pattern number)", ylab="Pearson residuals", 
       main = "Pearson residuals vs. j")
  abline(h = c(qnorm(0.975), qnorm(0.995), qnorm(0.025), qnorm(0.005)), lty = 3, col = "blue")
 
  #Standardized residual vs j (explanatory variable pattern number) plot
  plot(x = 1:length(stand.resid), y = stand.resid, xlab="j (explanatory variable pattern number)", ylab="Standardized residuals",
       main = "Standardized residuals vs. j")
  abline(h = c(qnorm(0.975), qnorm(0.995), qnorm(0.025), qnorm(0.005)), lty = 3, col = "blue")

  #Standardized pearson residual vs. predicted prob.
  plot(x = pred, y = sq.stand.resid, xlab="Predicted probabilities", ylab="Sq. standardized residuals", 
       main = "Sq. standardized residuals vs. pred. prob.")
  abline(h = c(qchisq(0.95,1), qchisq(0.99,1)), lty = 3, col = "blue")
  identify(pred, sq.stand.resid)
  
  # Bubble plot version with bubble proportional to sample size
  symbols(x = pred, y = sq.stand.resid, circles=sqrt(n), xlab="Predicted probabilities", ylab="Sq. standardized residuals",
          main = "Sq. standardized residuals vs. pred. prob. \n with plot point proportional to n_j", inches = 0.1)
  abline(h = c(qchisq(0.95,1), qchisq(0.99,1)), lty = 3, col = "blue")

  ##########################
  #Second plotting sheet   #
  ##########################

  #Open a new plotting window
  win.graph(width = 6, height = 6, pointsize = 10)
  
  #Split plot into four parts
  par(mfrow=c(2,2))

  #delta.beta vs. j (explanatory variable pattern number)
  plot(x = 1:length(delta.beta), y = delta.beta, xlab="j (explanatory variable pattern number)", ylab="Delta.beta", main = "Delta.beta vs. j")

  #delta.beta vs. predicted prob.
  plot(x = pred, y = delta.beta, xlab="Predicted probabilities", ylab="Delta.beta", main = "Delta.beta vs. pred. prob.")
  identify(pred, delta.beta)
  

  # Bubble plot of adj.pearson.sq with bubble proportional to delta.beta
  symbols(x = pred, y = sq.stand.resid, circles=sqrt(delta.beta), xlab="Predicted probabilities", ylab="Sq. standardized residuals",
          main = "Sq. standardized residuals vs. pred. prob. \n with plot point proportion to delta.beta", inches = 0.1)
  abline(h = c(qchisq(0.95,1), qchisq(0.99,1)), lty = 3, col = "blue")

  #Dummy plot
  plot(x = pred, delta.beta, type="n", axes=F, xlab=" ", ylab=" ",
        main=paste("X^2 =", round(pear.stat,2), "with p-value =", 
                 round(1-pchisq(pear.stat, df),4), "\n", "G^2=", 
                 round(dev, 2), "with p-value =", round(1-pchisq(dev, df),4)))
  
  #Information is stored in the object, but not printed unless requested! 
  invisible(list(h=h, pearson=pearson, sq.stand.resid=sq.stand.resid, 
            delta.beta=delta.beta, pear.stat=pear.stat, dev=dev))

}
