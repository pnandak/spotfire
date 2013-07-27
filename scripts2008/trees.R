############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-22-06                                                           #
# PURPOSE: Trees data example in the MASS package                          #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

library(MASS)
head(trees)

mod.fit<-lm(formula = Volume ~ Height, data = trees) 
summary(mod.fit)

#Plot of Y vs. X with sample model
plot(x = trees$Height, y = trees$Volume, xlab = "Height", ylab = "Volume", main = "Volume vs. Height", 
       panel.first = grid(col = "gray", lty = "dotted"))
curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, 
        col = "red", lty = "solid", lwd = 1, add = TRUE, from = min(trees$Height), to = max(trees$Height))

#e.i vs. Yhat.i
plot(x = mod.fit$fitted.values, y = mod.fit$residuals, xlab = expression(hat(Y)), ylab = "Residual", 
     main = expression(paste("Residuals vs. ", hat(Y))), panel.first = grid(col = "gray", lty = "dotted"))
abline(h = 0, col = "red")

#Determine lambda.hat
save.bc<-boxcox(object = mod.fit, lambda = seq(from = -2, to = 2, by = 0.01))
title(main = "Box-Cox transformation plot")
lambda.hat<-save.bc$x[save.bc$y == max(save.bc$y)] 
lambda.hat
    
#####################################################################################
#Try transformation with lambda.hat

  mod.fit2<-lm(formula = Volume^lambda.hat ~ Height, data = trees) 
  summary(mod.fit2)

  plot(x = mod.fit2$fitted.values, y = mod.fit2$residuals, xlab = expression(hat(Y)^{-0.19}), ylab = "Residual", 
       main = expression(paste("Residuals vs. ", hat(Y)^{-0.19})), panel.first = grid(col = "gray", lty = "dotted"))
  abline(h = 0, col = "red")

#####################################################################################
#Try natural log transformation since 0 was in the interval.

  mod.fit3<-lm(formula = log(Volume) ~ Height, data = trees) 
  summary(mod.fit3)

  plot(x = mod.fit3$fitted.values, y = mod.fit3$residuals, xlab = expression(log(hat(Y))), ylab = "Residual", 
       main = expression(paste("Residuals vs.", log(hat(Y)))), panel.first = grid(col = "gray", lty = "dotted"))
  abline(h = 0, col = "red")


#####################################################################################
# Perform tests for variance

  #Levene's test
  library(car)  
  group<-ifelse(trees$Height < median(trees$Height), 1, 2)
  levene.test(y = mod.fit$residuals, group = group)

  # Breusch Pagan test
  library(lmtest)
  bptest(formula = Volume ~ Height, data = trees, studentize = FALSE)


  #Do same tests for transformed Y
  levene.test(y = mod.fit2$residuals, group = group)
  bptest(formula = Volume^lambda.hat ~ Height, data = trees, studentize = FALSE)
  levene.test(y = mod.fit3$residuals, group = group)
  bptest(formula = log(Volume) ~ Height, data = trees, studentize = FALSE)


#####################################################################################
# One function used to do many of the tests (located in examine.mod.simple2.R)

  examine.mod.simple(mod.fit.obj = mod.fit, const.var.test = TRUE, boxcox.find = TRUE)







#
