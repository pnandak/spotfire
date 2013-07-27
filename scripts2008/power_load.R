############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-19-06                                                           #
# PURPOSE: Work with the power load data set                               #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#Read in the data
power.load<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter3\\power.txt", header=TRUE, sep = "")
head(power.load)
tail(power.load)

par(mfrow = c(1,1))
plot(x = power.load$temp, y = power.load$load, xlab = "Temperature", ylab = "Power load", main = "Power load vs. Temperature", 
     col = "black", pch = 1, panel.first = grid(col = "gray", lty = "dotted"))


#############################################################################
# First order model

  mod.fit<-lm(formula = load ~ temp, data = power.load)
  sum.fit<-summary(mod.fit)
  sum.fit


  #e.i vs. X.i
  plot(x = power.load$temp, y = mod.fit$residuals, xlab = "Temperature", ylab = "Residuals", main = "Residuals vs. Temperature", 
       col = "black", pch = 1, panel.first = grid(col = "gray", lty = "dotted"))
  abline(h = 0, col = "red")
  #This function can be useful when you want to identify particular observations on the plot
  #identify(x = power.load$temp, y = mod.fit$residuals) #labels option can be used as well

  #Scatter plot with sample model
  plot(x = power.load$temp, y = power.load$load, xlab = "Temperature", ylab = "Power load", main = "Power load vs. Temperature", 
      col = "black", pch = 1, panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, 
        col = "red", lty = "solid", lwd = 1, add = TRUE, from = min(power.load$temp), to = max(power.load$temp))

  #Additional plots automatically created - plot.lm() also
  par(mfrow=c(2,2))
  plot(mod.fit)


#############################################################################
# Second order model

  mod.fit2<-lm(formula = load ~ temp + I(temp^2), data = power.load)
  sum.fit2<-summary(mod.fit2)
  sum.fit2


  #e.i vs. X.i
  par(mfrow=c(1,1))
  plot(x = power.load$temp, y = mod.fit2$residuals, xlab = "Temperature", ylab = "Residuals", main = "Residuals vs. Temperature", 
       col = "black", pch = 1, panel.first = grid(col = "gray", lty = "dotted"))
  abline(h = 0, col = "red")

  #Scatter plot with sample model
  plot(x = power.load$temp, y = power.load$load, xlab = "Temperature", ylab = "Power load", main = "Power load vs. Temperature", 
      col = "black", pch = 1, panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit2$coefficients[1] + mod.fit2$coefficients[2]*x + mod.fit2$coefficients[3]*x^2, 
        col = "red", lty = "solid", lwd = 1, add = TRUE, from = min(power.load$temp), to = max(power.load$temp))

  #Another way to get the regression model on the plot
  curve(expr = predict(object = mod.fit2, newdata = data.frame(temp = x)), 
        col = "blue", lty = "solid", lwd = 1, add = TRUE, from = min(power.load$temp), to = max(power.load$temp))


#############################################################################
# Examine semi-studentized residuals  

  semi.stud.resid<-mod.fit$residuals/sum.fit$sigma

  #e.i.star vs. Yhat.i
  par(mfrow = c(1,1))
  plot(x = mod.fit$fitted.values, y = semi.stud.resid, xlab = expression(hat(Y)), ylab = "Semi-studentized residuals", 
       main = expression(paste("Semi-studentized residuals vs.", hat(Y))), panel.first = grid(col = "gray", lty = "dotted"),
       ylim = c(-3,3))
  abline(h = 0, col = "red")
  abline(h = c(-3,3), col = "red", lwd = 2)

  #e.i.star vs. Yhat.i with different y-axis label
  par(mfrow = c(1,1), pty = "s")  #pty = "s" makes the plot Square
  plot(x = mod.fit$fitted.values, y = semi.stud.resid, xlab = expression(hat(Y)), ylab = expression(paste(e[i]^{"*"})), 
       main = expression(paste("Semi-studentized residuals vs.", hat(Y))), panel.first = grid(col = "gray", lty = "dotted"),
       ylim = c(-3,3))
  abline(h = 0, col = "red")
  abline(h = c(-3,3), col = "red", lwd = 2)


#############################################################################
# Examine normality

  par(pty = "m")  #pty = "m" makes the plot Maximize the plot window (this is the default)
  
  ##############
  #HISTOGRAM
  
  #Regular histogram of the semi-studentized residuals
  hist(x = semi.stud.resid, main = "Histogram of semistudentized residuals", xlab = "Semistudentized residuals")
  
  #Histogram of the semi-studentized residuals with normal distribution overlay
  hist(x = semi.stud.resid, main = "Histogram of semistudentized residuals", xlab = "Semistudentized residuals",
       freq = FALSE)
  curve(expr = dnorm(x, mean = mean(semi.stud.resid), sd = sd(semi.stud.resid)), col = "red", add = TRUE)
  #Extra code to show the heights on the above histogram
  save.hist<-hist(x = semi.stud.resid, plot = FALSE)
  n<-length(semi.stud.resid)
  save.hist$counts/(n*(save.hist$breaks[2]-save.hist$breaks[1]))  

  #############
  #QQ-PLOT

  #QQ-plot done by R
  qqnorm(y = semi.stud.resid)
  qqline(y = semi.stud.resid, col = "red")
 
  #What are the qqnorm() and qqline() functions plotting exactly?
  save.qqnorm<-qqnorm(y = semi.stud.resid, plot = FALSE)
  n<-length(semi.stud.resid)
  save.it<-data.frame(y.axis = round(sort(save.qqnorm$y),2),
                      semi.stud.res = round(sort(semi.stud.resid),2), 
                      prob1 = ppoints(semi.stud.resid), prob2 = (1:n - 1/2)/n, 
                      x.axis = round(sort(save.qqnorm$x),2), 
                      quant.norm = round(qnorm(p = (1:n - 1/2)/n, mean = 0, sd = 1),2))
  save.it

  #Another QQ-plot
  qqnorm(y = semi.stud.resid)
  qqline(y = semi.stud.resid)
  points(x = qnorm(p = c(0.25, 0.75), mean = 0, sd = 1), y = quantile(x = semi.stud.resid, probs = c(0.25, 0.75)), 
         pch = 2, lwd = 2, col = "darkgreen") #Line is drawn through 25th an 75th percentiles
  abline(a = 0, b = sd(semi.stud.resid), col = "blue")  #Using Z = (X-mu)/sigma implies X = mu + sigma*Z line
  
 
  #My version of a QQ-plot
  win.graph(width = 6, height = 6, pointsize = 12)  #Open a new graphics window
  n<-length(semi.stud.resid)
  norm.quant<-qnorm(p = seq(from = 1/(n+1), to = 1-1/(n+1), by = 1/(n+1)), mean = mean(semi.stud.resid), sd = sd(semi.stud.resid))
  plot(y = sort(semi.stud.resid), x = norm.quant, main = "QQ-Plot for semi-studentized residuals", ylab = 
         "Semi-studentized residuals", xlab = "Normal quantiles", panel.first = grid(nx = NULL, ny = NULL, 
         col="gray", lty="dotted"))
  abline(a = 0, b = 1, col = "red")
  
  #What is being plotted above?
  probs<-round(seq(from = 1/(length(semi.stud.resid)+1), to = 1-1/(length(semi.stud.resid)+1), by = 
                    1/(length(semi.stud.resid)+1)),4)
  data.frame(sort.semi.stud.res = round(sort(semi.stud.resid),2), norm.quant = round(norm.quant,2), probs)
  
  
  #Empirical cumulative distribution function (ECDF) plot
  plot.ecdf(semi.stud.resid, verticals = TRUE, do.p = FALSE, main = "ECDF plot of semi-studentized residuals",
            lwd = 2, panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylab = "ECDF",  
            xlab = "Semi-studentized residuals")
  curve(expr = pnorm(x, mean = mean(semi.stud.resid), sd = sd(semi.stud.resid)), col = "red", add = TRUE)

  











#
