############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-19-06                                                           #
# PURPOSE: Show other examples of non-linear relationship                  #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

############################################################################
#Simulate data

  X<-seq(from = 1, to = 99, by = 2)
  set.seed(1719)  #Set seed so that I can reproduce the exact same results
  epsilon1<-rnorm(n = length(X), mean = 0, sd = 25)  #Simulate from N(0,25^2)
  epsilon2<-rnorm(n = length(X), mean = 0, sd = 5)   #Simulate from N(0,5^2)
  beta0<-2
  beta1<-10
  
  #Create response variable - notice how the mix of vectors and scalar quantities in creating the Y's
  Y1<-beta0 + beta1*X + epsilon1         #linear
  Y2<-beta0 + beta1*sqrt(X) + epsilon2      #non-linear

  head(data.frame(Y1, X))
  
###########################################################################
# Analyze Y1
    
  mod.fit<-lm(formula = Y1 ~ X)  #Do not need data = option since X and Y exist outside of one data set
  mod.fit$coefficients
  
  par(mfrow = c(1,2))
  plot(x = X, y = Y1, main = "Y1 vs. X", panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, col = "red", add = TRUE, from = min(X), to = max(X))
  #Population model
  #curve(expr = beta0 + beta1*x, col = "blue", add = TRUE, from = min(X), to = max(X))

  plot(x = X, y = mod.fit$residuals, ylab = "Residuals", main = "Residuals vs. X", panel.first = grid(col = "gray", lty = "dotted"))
  abline(h = 0, col = "red")



###########################################################################
# Analyze Y2

  mod.fit<-lm(formula = Y2 ~ X)  
  mod.fit$coefficients
  
  plot(x = X, y = Y2, main = "Y2 vs. X", panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, col = "red", add = TRUE, from = min(X), to = max(X))
  #Population model
  #curve(expr = beta0 + beta1*sqrt(x), col = "blue", add = TRUE, from = min(X), to = max(X))
  plot(x = X, y = mod.fit$residuals, ylab = "Residuals", main = "Residuals vs. X", panel.first = grid(col = "gray", lty = "dotted"))
  abline(h = 0, col = "red")

  #Try a transformation
  mod.fit<-lm(formula = Y2 ~ sqrt(X))  
  mod.fit$coefficients

  plot(x = X, y = Y2, main = "Y2 vs. X", panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*sqrt(x), col = "red", add = TRUE, from = min(X), to = max(X))
  plot(x = X, y = mod.fit$residuals, ylab = "Residuals", main = "Residuals vs. X", panel.first = grid(col = "gray", lty = "dotted"))
  abline(h = 0, col = "red")


###########################################################################
# Analyze Y3

  X<-seq(from = 0.01, to = 1, by = 0.01)
  set.seed(1278)  #Set seed so that I can reproduce the exact same results
  epsilon3<-rnorm(n = length(X), mean = 0, sd = 5)  #Simulate from N(0,5^2)
  beta0<-2
  beta1<-10
  
  #Create response variable - notice how the mix of vectors and scalar quantities in creating the Y's
  Y3<-beta0 + beta1*1/X + epsilon3      #non-linear

  mod.fit<-lm(formula = Y3 ~ X)  #Do not need data = option since X and Y exist outside of one data set
  mod.fit$coefficients
   
  plot(x = X, y = Y3, main = "Y3 vs. X", panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, col = "red", add = TRUE, from = min(X), to = max(X))
  #Population model
  #curve(expr = beta0 + beta1*1/x, col = "blue", add = TRUE, from = min(X), to = max(X))
  plot(x = X, y = mod.fit$residuals, ylab = "Residuals", main = "Residuals vs. X", panel.first = grid(col = "gray", lty = "dotted"))
  abline(h = 0, col = "red")

  #Try a transformation
  mod.fit<-lm(formula = Y3 ~ I(1/X))  
  mod.fit$coefficients

  plot(x = X, y = Y3, main = "Y3 vs. X", panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*1/x, col = "red", add = TRUE, from = min(X), to = max(X))
  plot(x = X, y = mod.fit$residuals, ylab = "Residuals", main = "Residuals vs. X", panel.first = grid(col = "gray", lty = "dotted"))
  abline(h = 0, col = "red")




#
