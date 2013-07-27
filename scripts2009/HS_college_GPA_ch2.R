############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-18-06                                                           #
# PURPOSE: Chapter 2 examples with the GPA data set                        #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#Read in the data
gpa<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter1\\gpa.txt", header=TRUE, sep = "")
head(gpa)

#Fit the simple linear regression model and save the results in mod.fit
mod.fit<-lm(formula = College.GPA ~ HS.GPA, data = gpa)
sum.fit<-summary(mod.fit)
sum.fit
names(sum.fit)
MSE<-sum.fit$sigma^2


########################################################################################
#"By hand" calculations - normally, you would not want to do it in R this way

  alpha<-0.05
  n<-nrow(gpa)
  quant.t<-qt(p = 1-alpha/2, df = n-2)
  X.h<-1.88
  Y.hat.h<-as.numeric(mod.fit$coefficients[1] + mod.fit$coefficients[2]*X.h) #as.numeric just removes an unneeded name (try without it to see what happens) 
  ssx<-var(gpa$HS.GPA)*(n-1)  #SUM( (X_i - X_bar)^2 )
  X.bar<-mean(gpa$HS.GPA)

  low.ci<-round(Y.hat.h - quant.t*sqrt(MSE * (1/n + (X.h - X.bar)^2 / ssx)),3)
  up.ci<-round(Y.hat.h + quant.t*sqrt(MSE * (1/n + (X.h - X.bar)^2 / ssx)),3)
  low.pi<-round(Y.hat.h - quant.t*sqrt(MSE * (1 + 1/n + (X.h - X.bar)^2 / ssx)),3)
  up.pi<-round(Y.hat.h + quant.t*sqrt(MSE * (1 + 1/n + (X.h - X.bar)^2 / ssx)),3)

  data.frame(alpha, n, quant.t, X.h, Y.hat.h, ssx, X.bar, MSE, low.ci, up.ci, low.pi, up.pi)



#Scatter plot with the C.I. and P.I. plotted upon it
  plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA", 
       xlim = c(0,4.5), ylim = c(0,4.5), col = "black", pch = 1, cex = 1.0, panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, 
        col = "red", lty = "solid", lwd = 1, add = TRUE, from = min(gpa$HS.GPA), to = max(gpa$HS.GPA))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x - quant.t*sqrt(MSE * (1/n + (x - X.bar)^2 / ssx)), 
        col = "darkgreen", lty = "dashed", lwd = 1, add = TRUE, from = min(gpa$HS.GPA), to = max(gpa$HS.GPA))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x + quant.t*sqrt(MSE * (1/n + (x - X.bar)^2 / ssx)), 
        col = "darkgreen", lty = "dashed", lwd = 1, add = TRUE, from = min(gpa$HS.GPA), to = max(gpa$HS.GPA))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x - quant.t*sqrt(MSE * (1 + 1/n + (x - X.bar)^2 / ssx)), 
        col = "blue", lty = "dashed", lwd = 1, add = TRUE, from = min(gpa$HS.GPA), to = max(gpa$HS.GPA))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x + quant.t*sqrt(MSE * (1 + 1/n + (x - X.bar)^2 / ssx)), 
        col = "blue", lty = "dashed", lwd = 1, add = TRUE, from = min(gpa$HS.GPA), to = max(gpa$HS.GPA))
  legend(locator(1), legend = c("Sample model", "95% C.I.", "95% P.I."), col = c("red", "darkgreen", "blue"), 
         lty = c("solid", "dashed", "dashed"), bty = "n", cex = 0.75)
       
       
       

#####################################################################################################
#Easier ways to do the C.I. and P.I. calculations and plots

  more.gpa<-data.frame(HS.GPA = c(1.88, 2, 3, 4))
  predict(object = mod.fit, newdata = more.gpa, se.fit = TRUE, interval = "confidence", level = 0.95)
  predict(object = mod.fit, newdata = more.gpa, se.fit = TRUE, interval = "prediction", level = 0.95)


  #C.I. for all X's in the data set, Note: R will use the original data set by default
  save.ci<-predict(object = mod.fit, se.fit = TRUE, interval = "confidence", level = 0.95)
  #P.I.s for all X's in the data set, Note: The code below will produce an error without the newdata specification
  save.pi<-predict(object = mod.fit, newdata = gpa, interval = "prediction", level = 0.95)
 
  save.ci
  save.pi
  #Print C.I.s and P.I.s together, note how I extracted the P.I.s (R does not allow the same syntax as when working with the C.I.s)
  data.frame(gpa, Y.hat = save.ci$fit[,1], ci.low = save.ci$fit[,2], ci.up = save.ci$fit[,3], 
                                       pi.low = save.pi[,2], pi.up = save.pi[,3])

  
  #Open a new graphics window (don't have to)
  win.graph(width = 6, height = 6, pointsize = 12)
  par(pty = "s") #Make sure plot is square
  par(xaxs = "i", yaxs = "i") #Use "r" to set it back

  #C.I. and P.I. bands plot
  plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA", 
       xlim = c(0,4.5), ylim = c(0,4.5), col = "black", pch = 1, cex = 1.0, panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = predict(object = mod.fit, newdata = data.frame(HS.GPA = x)), 
        col = "red", lty = "solid", lwd = 1, add = TRUE, from = min(gpa$HS.GPA), to = max(gpa$HS.GPA))
  curve(expr =  predict(object = mod.fit, newdata = data.frame(HS.GPA = x), interval = "confidence", level = 0.95)[,2], 
        col = "darkgreen", lty = "dashed", lwd = 1, add = TRUE, from = min(gpa$HS.GPA), to = max(gpa$HS.GPA))
  curve(expr =  predict(object = mod.fit, newdata = data.frame(HS.GPA = x), interval = "confidence", level = 0.95)[,3], 
        col = "darkgreen", lty = "dashed", lwd = 1, add = TRUE, from = min(gpa$HS.GPA), to = max(gpa$HS.GPA))
  curve(expr =  predict(object = mod.fit, newdata = data.frame(HS.GPA = x), interval = "prediction", level = 0.95)[,2], 
        col = "blue", lty = "dashed", lwd = 1, add = TRUE, from = min(gpa$HS.GPA), to = max(gpa$HS.GPA))
  curve(expr =  predict(object = mod.fit, newdata = data.frame(HS.GPA = x), interval = "prediction", level = 0.95)[,3], 
        col = "blue", lty = "dashed", lwd = 1, add = TRUE, from = min(gpa$HS.GPA), to = max(gpa$HS.GPA))
  legend(locator(1), legend = c("Sample model", "95% C.I.", "95% P.I."), col = c("red", "darkgreen", "blue"), 
         lty = c("solid", "dashed", "dashed"), bty = "n", cex = 0.75)
  
  #Graphical example (using the plot above) showing the lower bound of the P.I.
  save.pred<-predict(object = mod.fit, newdata = data.frame(HS.GPA = X.h), interval = "prediction", level = 0.95)
  save.pred
  segments(x0 = X.h, y0 = 0, x1 = X.h, y1 = save.pred[1,2], lty = 2, col = "black", lwd = 2) 
  segments(x0 = X.h, y0 = save.pred[1,2], x1 = 0, y1 =save.pred[1,2] , lty = 2, col = "black", lwd = 2) 
  mtext(text = round(save.pred[1,2],2), at = save.pred[1,2], side = 2) 
  


###########################################################################################
# ANOVA

anova(mod.fit)

qf(p = 0.99, df1 = 1, df2 = 18)

#
    
 
