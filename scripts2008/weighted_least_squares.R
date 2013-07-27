############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  8-17-06                                                           #
# PURPOSE: WLS example for Chapter 11                                      #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#Simulate data with nonconstant variance
X<-seq(from = 1, to = 40, by = 0.25)
epsilon<-rnorm(n = length(X), mean = 0, sd = 1)
epsilon2<-X*epsilon  #Var(epsilon2) = X^2 * 1 = X^2 (non-constant variance)
Y<-2 + 3*X + epsilon2
set1<-data.frame(Y,X)

#Y vs. X with sample model
plot(x = X, y = Y, xlab = "X", ylab = "Y", main = "Y vs. X", panel.first = grid(col = "gray", lty = "dotted"))
mod.fit<-lm(formula = Y ~ X, data = set1)
curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, col = "red", lty = "solid", lwd = 2, add = TRUE,
      from = min(set1$X), to = max(set1$X)) 

#Residuals vs. Y^
plot(x = mod.fit$fitted.values, y = mod.fit$residuals, xlab = expression(hat(Y)), ylab = "Residuals",
     main = "Residuals vs. estimated mean response", panel.first = grid(col = "gray", lty = "dotted"))
abline(h = 0, col = "darkgreen")

#Try calculating a P.I. for X = 40
pred<-predict(object = mod.fit, newdata = data.frame(X = 40), interval = "prediction", level = 0.95)


#############################################################################
# Method #1

  #Find quantiles for Y^'s
  #  See R help for different ways (type) to calculate quantiles
  quant5<-quantile(x = mod.fit$fitted.values, probs = c(0.2, 0.4, 0.6, 0.8), type = 1)  
  quant5
  
  #Put Y^'s into groups based upon quantiles
  groups<-ifelse(mod.fit$fitted.values < quant5[1], 1,
          ifelse(mod.fit$fitted.values < quant5[2], 2,
          ifelse(mod.fit$fitted.values < quant5[3], 3,
          ifelse(mod.fit$fitted.values < quant5[4], 4, 5))))
  #Quick way to find the variance of residuals for each group 
  var.eps<-tapply(X = mod.fit$residuals, INDEX = groups, FUN = var)
  var.eps

  #Visualization of creating the groups
  plot(x = mod.fit$fitted.values, y = mod.fit$residuals, xlab = expression(hat(Y)), ylab = "Residuals",
     main = "Residuals vs. estimated mean response", panel.first = grid(col = "gray", lty = "dotted"))
  abline(h = 0, col = "darkgreen")
  abline(v = quant5, col = "red", lwd = 3)

  #Put the group variances into a vector corresponding to each observation
  group.var<-ifelse(groups == 1, var.eps[1],
             ifelse(groups == 2, var.eps[2],
             ifelse(groups == 3, var.eps[3],
             ifelse(groups == 4, var.eps[4], var.eps[5]))))

  mod.fit1<-lm(formula = Y ~ X, data = set1, weight = 1/group.var^2)
  summary(mod.fit1)

  #Try calculating a P.I. for X = 40
  pred1<-predict(object = mod.fit1, newdata = data.frame(X = 40), interval = "prediction", level = 0.95)


#############################################################################
# Method #2

  #Find quantiles for Y^'s
  #  See R help for different ways (type) to calculate quantiles
  quant3<-quantile(x = mod.fit$fitted.values, probs = c(1/3, 2/3), type = 1)  
  quant3
  
  #Put Y^'s into groups based upon quantiles
  groups<-ifelse(mod.fit$fitted.values < quant3[1], 1,
          ifelse(mod.fit$fitted.values < quant3[2], 2, 3))
  #Quick way to find the variance of residuals for each group 
  var.eps<-tapply(X = mod.fit$residuals, INDEX = groups, FUN = var)
  var.eps

  #Visualization of creating the groups
  plot(x = mod.fit$fitted.values, y = mod.fit$residuals, xlab = expression(hat(Y)), ylab = "Residuals",
     main = "Residuals vs. estimated mean response", panel.first = grid(col = "gray", lty = "dotted"))
  abline(h = 0, col = "darkgreen")
  abline(v = quant3, col = "red", lwd = 3)

  #Put the group variances into a vector corresponding to each observation
  group.var<-ifelse(groups == 1, var.eps[1],
             ifelse(groups == 2, var.eps[2], var.eps[3]))

  mod.fit2<-lm(formula = Y ~ X, data = set1, weight = 1/group.var^2)
  summary(mod.fit2)

  #Try calculating a P.I. for X = 40
  pred2<-predict(object = mod.fit2, newdata = data.frame(X = 40), interval = "prediction", level = 0.95)


#############################################################################
# Method #3

  mod.fit3<-lm(formula = Y ~ X, data = set1, weight = 1/X^2)
  summary(mod.fit3)
  names(mod.fit3)
  head(mod.fit3$weights)
  tail(mod.fit3$weights)

  #Show some calculations without lm()
  X.mat<-cbind(1,X)
  w<-1/X^2
  W<-diag(w)
  b.w<-solve(t(X.mat)%*%W%*%X.mat)%*%t(X.mat)%*%W%*%Y
  b.w
  mod.fit3$coefficients
  
  n<-length(Y)
  Y.hat<-X.mat%*%b.w
  MSE.w<-sum(w*(Y-Y.hat)^2)/(n-2)
  cov.b<-MSE.w*solve(t(X.mat)%*%W%*%X.mat)
  cov.b
  vcov(mod.fit3)

  X.h<-c(1,40)
  sqrt(MSE.w*t(X.h)%*%solve(t(X.mat)%*%W%*%X.mat)%*%X.h)
  predict(object = mod.fit3, newdata = data.frame(X = 40), interval = "confidence", se.fit = TRUE, level = 0.95)


  #WITHOUT WLS
  mod.fit3.temp<-lm(formula = Y ~ X, data = set1)
  predict(object = mod.fit3.temp, newdata = data.frame(X = 40), interval = "confidence", se.fit = TRUE, level = 0.95)
  sqrt(summary(mod.fit3.temp)$sigma^2*t(X.h)%*%solve(t(X.mat)%*%X.mat)%*%X.h)


  #Try calculating a P.I. for X = 40
  pred3<-predict(object = mod.fit3, newdata = data.frame(X = 40), interval = "prediction", level = 0.95)
  pred3
  se.pi<-sqrt(MSE.w*(1+t(X.h)%*%solve(t(X.mat)%*%W%*%X.mat)%*%X.h))
  alpha<-0.05
  X.h%*%b.w+c(qt(alpha/2, n-2),qt(1-alpha/2, n-2))*se.pi 
  

############################################################################
# Sumamrize results

  data.frame(name = c("Least Squares", "WLS 1", "WLS 2", "WLS 3"), 
    round(rbind(mod.fit$coefficients, mod.fit1$coefficients, mod.fit2$coefficients, mod.fit3$coefficients),2))

  data.frame(name = c("Least Squares", "WLS 1", "WLS 2", "WLS 3"), 
    round(rbind(pred, pred1, pred2, pred3),2))








#
