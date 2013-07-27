############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  8-21-06                                                           #
# PURPOSE: Example on p. 450 of KNN                                        #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

table11.7<-read.table(file = "C:\\chris\\UNL\\STAT870\\Instructor_CD\\Data Sets\\Chapter 11 Data Sets\\CH11TA07.txt", 
                  header = FALSE, col.names = c("income", "risk.aversion", "insurance"), sep = "")
table11.7


#############################################################################
# Regular least squares

  mod.fit<-lm(formula = insurance ~ income + risk.aversion, data = table11.7)
  summary(mod.fit)

  win.graph(width = 6, height = 6, pointsize = 10) 
  par(mfrow = c(1,1))
  income1<-seq(from = min(table11.7$income), to = max(table11.7$income), by = 1)
  risk.aversion1<-seq(from = min(table11.7$risk.aversion), to = max(table11.7$risk.aversion), by = 0.5)
  pred.data<-expand.grid(income = income1, risk.aversion = risk.aversion1)
  save.pred<-predict(object = mod.fit, newdata = pred.data)  
  
  #The 3D graphing function persp() expects data to be in a format of 
  #                                    risk.aversion
  # income           risk.aversion= 1.0 risk.aversion= 1.5 risk.aversion= 2.0
  #  income=26.852        Pred. value     Pred. value         Pred. value
  #  income=27.852        Pred. value     Pred. value         Pred. value
  #  income=28.852        Pred. value     Pred. value         Pred. value
  save.pred.ls<-matrix(save.pred, nrow = length(income1), ncol = length(risk.aversion1), byrow = FALSE)
  save.pred.ls[1:3, 1:3]
  persp(x = income1, risk.aversion1, z = save.pred.ls, theta=200, phi=20, ticktype="detailed", xlab="Income", zlim = c(-200, 500), 
        ylab="Risk aversion", zlab="Insurance", expand = 1, shade=0.5, col = "green3", main = "1st order least squares")
  #Note that persp() is a little easier to use with the predict() output after fitting a lowess model
  


  library(Rcmdr)
  library(rgl) 
  rgl.clear("all") #Clears plot window
  rgl.light() #Gray background
  rgl.bbox()  #Puts numbers on plot and box around it
  scatter3d(x = table11.7$income, y = table11.7$insurance, z = table11.7$risk.aversion, fit="linear",
            bg.col="black", grid=TRUE, xlab="Income", ylab="Insurance", zlab="Risk aversion")
            
            
#############################################################################
# Loess model

  mod.fit.loess<-loess(formula = insurance ~ income + risk.aversion, data = table11.7, span = 0.5, degree = 1)
  mod.fit.loess
  summary(mod.fit.loess)
  names(mod.fit.loess)

  save.pred<-predict(object = mod.fit.loess, newdata = pred.data)  #BE CAREFUL - Different format for resulting predictions
  persp(x = income1, risk.aversion1, z = save.pred, theta=200, phi=20, ticktype="detailed", xlab="Income", zlim = c(-200, 500), 
        ylab="Risk aversion", zlab="Insurance", expand = 1, shade=0.5, col = "green3", main = "1st order lowess, span=0.5")

  #Try different span values
  mod.fit.loess2<-loess(formula = insurance ~ income + risk.aversion, data = table11.7, span = 0.2, degree = 1)
  save.pred2<-predict(object = mod.fit.loess2, newdata = pred.data)  #BE CAREFUL - Different format for resulting predictions
  win.graph(width = 6, height = 6, pointsize = 10)
  persp(x = income1, risk.aversion1, z = save.pred2, theta=200, phi=20, ticktype="detailed", xlab="Income", zlim = c(-200, 500), 
        ylab="Risk aversion", zlab="Insurance", expand = 1, shade=0.5, col = "green3", main = "1st order lowess, span=0.2")

  mod.fit.loess3<-loess(formula = insurance ~ income + risk.aversion, data = table11.7, span = 0.8, degree = 1)
  save.pred3<-predict(object = mod.fit.loess3, newdata = pred.data)  #BE CAREFUL - Different format for resulting predictions
  win.graph(width = 6, height = 6, pointsize = 10)
  persp(x = income1, risk.aversion1, z = save.pred3, theta=200, phi=20, ticktype="detailed", xlab="Income", zlim = c(-200, 500), 
        ylab="Risk aversion", zlab="Insurance", expand = 1, shade=0.5, col = "green3", main = "1st order lowess, span=0.8")

  
  #The 2x2 plot
  win.graph(width = 8, height = 6, pointsize = 8)
  par(mfrow=c(2,2))
  persp(x = income1, risk.aversion1, z = save.pred.ls, theta=200, phi=20, ticktype="detailed", xlab="Income", zlim = c(-200, 500), 
        ylab="Risk aversion", zlab="Insurance", expand = 1, shade=0.5, col = "green3", main = "1st order least squares")
  persp(x = income1, risk.aversion1, z = save.pred, theta=200, phi=20, ticktype="detailed", xlab="Income", zlim = c(-200, 500), 
        ylab="Risk aversion", zlab="Insurance", expand = 1, shade=0.5, col = "green3", main = "1st order lowess, span=0.5")
  persp(x = income1, risk.aversion1, z = save.pred2, theta=200, phi=20, ticktype="detailed", xlab="Income", zlim = c(-200, 500), 
        ylab="Risk aversion", zlab="Insurance", expand = 1, shade=0.5, col = "green3", main = "1st order lowess, span=0.2")
  persp(x = income1, risk.aversion1, z = save.pred3, theta=200, phi=20, ticktype="detailed", xlab="Income", zlim = c(-200, 500), 
        ylab="Risk aversion", zlab="Insurance", expand = 1, shade=0.5, col = "green3", main = "1st order lowess, span=0.8")


  #Compare SSE values
  yhat.loess<-predict(object = mod.fit.loess)
  yhat.loess2<-predict(object = mod.fit.loess2)
  yhat.loess3<-predict(object = mod.fit.loess3)
  yhat.ls<-predict(object = mod.fit)
  sse.loess<-sum((table11.7$insurance - yhat.loess)^2)
  sse.loess2<-sum((table11.7$insurance - yhat.loess2)^2)
  sse.loess3<-sum((table11.7$insurance - yhat.loess3)^2)
  sse.ls<-sum((table11.7$insurance - yhat.ls)^2)
  data.frame(span = c("LS", 0.5, 0.2, 0.8), sse =rbind(sse.ls, sse.loess, sse.loess2, sse.loess3))
  
  
  #Comparison of residuals
  plot(x = 1:length(mod.fit$residuals), y = abs(mod.fit$residuals), pch = 1, col = "blue", lwd = 2, 
       xlab = "Residual number", ylab = "|Residual|", ylim = c(-1, max(mod.fit$residuals)), 
       main = "Compare residuals")
  points(x = 1:length(mod.fit.loess$residuals), y = abs(mod.fit.loess$residuals), pch = 2, col = "red", lwd = 2)
  abline(v = 1:length(mod.fit.loess$residuals), lty = "dotted", col = "lightgray")
  legend(locator(1), legend = c("Least squares", "Loess, span = 0.5"), pch = c(1,2), col = c("blue", "red"), 
          pt.lwd = 2, bg = "white")
  
  
  
  #Predict for one data value
  save.loess<-predict(object = mod.fit.loess, newdata = data.frame(income = 30, risk.aversion = 3), se = TRUE)
  save.loess$fit
  save.loess$se
  predict(object = mod.fit, newdata = data.frame(income = 30, risk.aversion = 3), interval = "confidence")

  

  #Try second order model
  mod.fit.loess<-loess(formula = insurance ~ income + risk.aversion, data = table11.7, span = 0.8, degree = 2)
  save.pred<-predict(object = mod.fit.loess, newdata = pred.data)  
  win.graph(width = 6, height = 6, pointsize = 10)
  persp(x = income1, risk.aversion1, z = save.pred, theta=200, phi=20, main = "2nd order lowess",
        ticktype="detailed", xlab="Income", ylab="Risk aversion", zlab="Insurance", expand = 1, shade=0.8, col = "green3")

 


#
