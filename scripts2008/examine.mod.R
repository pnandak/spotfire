######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  12-28-06                                                    #
# PURPOSE: Automate part of the ARIMA model diagnostic               #
#          procedures into one function.                             #
# NOTES:                                                             #
#   mod.fit.obj = Model fit object from arima()                      #
#   mod.name    = Descriptive name for the plots                     #
#   max.lag     = This is used in the gof.lag option of tsdiag() and #
#                 in the lag.max option of the pacf() function       #
######################################################################

examine.mod<-function(mod.fit.obj, mod.name, max.lag = 20) {

  z<-mod.fit.obj$coef/sqrt(diag(mod.fit.obj$var.coef))
  p.value<-2*(1-pnorm(q = abs(z), mean = 0, sd = 1))
  data.frame(z, p.value)
 
  win.graph(width = 8, height = 6, pointsize = 10)  
  tsdiag(object = mod.fit.obj, gof.lag = max.lag)
  mtext(text = paste("Model is", mod.name), side = 1, line = 3, at = 1, col = "red", adj = 0)
  #title(sub = "ARIMA(1,1,1)", col.sub = "red", cex.sub = 0.75)

  win.graph(width = 8, height = 6, pointsize = 10)  
  par(mfrow = c(2,2)) 
  pacf(x = mod.fit.obj$residuals, lag.max = max.lag, main = paste("Estimated PACF for residuals", mod.name), 
       xlim = c(1,max.lag), ylim = c(-1,1))  
  hist(x = mod.fit.obj$residuals, main = "Histogram of residuals", xlab = "Residuals", freq = FALSE)
  curve(expr = dnorm(x, mean = mean(mod.fit.obj$residuals), sd = sd(mod.fit.obj$residuals)), col = "red", add = TRUE)
  qqnorm(y = mod.fit.obj$residuals, ylab = "Residuals", panel.first = grid(col = "gray", lty = "dotted"))
  qqline(y = mod.fit.obj$residuals, col = "red")
  par(mfrow = c(1,1)) 
  
  list(z = z, p.value = p.value)
}
