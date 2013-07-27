############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  8-4-06                                                            #
# PURPOSE: NBA data example for Chapter 10                                 #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

nba<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter6\\nba_data.txt", header=TRUE, sep = "")
head(nba)

mod.fit<-lm(formula = PPM ~ MPG + height + FGP + age, data = nba)
sum.fit<-summary(mod.fit)
sum.fit
plot(mod.fit, which=1:6)


############################################################################
# Section 10.1

  #Partial regression plots
  library(car)
  cr.plots(model = mod.fit)  #Interactive
  cr.plots(model = mod.fit, ask=FALSE, span=0.5)  #Puts on one plot


############################################################################
# Section 10.2

  #Studentized and studentized deleted residuals
  r.i<-rstandard(model = mod.fit)
  t.i<-rstudent(model = mod.fit)
  r.i[1:5]
  t.i[1:5]
  r.i[abs(r.i)>qt(p = 1-0.01/2, df = mod.fit$df.residual)]
  t.i[abs(t.i)>qt(p = 1-0.01/2, df = mod.fit$df.residual-1)]
  
  #Critical values
  n<-length(mod.fit$fitted.values)
  qt(p = 1-0.01/2, df = mod.fit$df.residual)
  qt(p = 1-0.01/2, df = mod.fit$df.residual-1)
  qt(p = 1-0.05/(2*n), df = mod.fit$df.residual)
  qt(p = 1-0.05/(2*n), df = mod.fit$df.residual-1)

  #r.i vs. Y.hat.i
  plot(x = mod.fit$fitted.values, y = r.i, xlab = "Estimated mean response", 
       ylab = "Studentized residuals", main = expression(paste(r[i], " vs. estimated mean response")), 
       panel.first = grid(col = "gray", lty = "dotted"), 
       ylim = c(min(qt(p = 0.05/(2*n), df = mod.fit$df.residual), min(r.i)), max(qt(p = 1-0.05/(2*n), df = mod.fit$df.residual), max(r.i))))
  abline(h = 0, col = "darkgreen")
  abline(h = c(qt(p = 0.01/2, df = mod.fit$df.residual),qt(p = 1-0.01/2, df = mod.fit$df.residual)), col = "red", lwd = 2)
  abline(h = c(qt(p = 0.05/(2*n), df = mod.fit$df.residual),qt(p = 1-0.05/(2*n), df = mod.fit$df.residual)), col = "darkred", lwd = 2)
  identify(x = mod.fit$fitted.values, y = r.i)

  #t.i vs. Y.hat.i
  plot(x = mod.fit$fitted.values, y = t.i, xlab = "Estimated mean response", 
       ylab = "Studentized deleted residuals", main = expression(paste(t[i], " vs. estimated mean response")), 
       panel.first = grid(col = "gray", lty = "dotted"), 
       ylim = c(min(qt(p = 0.05/(2*n), df = mod.fit$df.residual-1), min(t.i)), max(qt(p = 1-0.05/(2*n), df = mod.fit$df.residual-1), max(t.i))))
  abline(h = 0, col = "darkgreen")
  abline(h = c(qt(p = 0.01/2, df = mod.fit$df.residual-1),qt(p = 1-0.01/2, df = mod.fit$df.residual-1)), col = "red", lwd = 2)
  abline(h = c(qt(p = 0.05/(2*n), df = mod.fit$df.residual-1),qt(p = 1-0.05/(2*n), df = mod.fit$df.residual-1)), col = "darkred", lwd = 2)
  identify(x = mod.fit$fitted.values, y = t.i)

  #Fox's outlier.test() function gives the Bonferroni adjusted p-value for the largest in absolute value 
  #  studentized deleted residual; see p. 194 of Fox
  outlier.test(mod.fit)
  t.i[37]
  2*(1-pt(abs(t.i[37]), df = mod.fit$df.residual - 1)) #Unadjusted
  105*2*(1-pt(abs(t.i[37]), df = mod.fit$df.residual - 1)) #Adjusted


############################################################################
# Section 10.3

  h.ii<-hatvalues(model = mod.fit)
  p<-length(mod.fit$coefficients)
  n<-length(mod.fit$residuals)
  round(h.ii[h.ii>2*p/n],2)
  
  plot(x = 1:n, y = h.ii, ylab = expression(h[ii]), xlab = "Observation number", main = expression(paste("Index plot of ", h[ii])))
  abline(h = 2*p/n, col = "red", lty = "dashed")
  abline(h = c(0.2,0.5), col = "blue", lty = "dotted")
  identify(x = 1:n, y =h.ii)


############################################################################
# Section 10.4

  win.graph(width = 6, height = 6, pointsize = 12)
  par(mfrow = c(1,1))

  dffits.i<-dffits(model = mod.fit)
  dffits.i[abs(dffits.i)>1]
  dffits.i[abs(dffits.i)>2*sqrt(p/n)]  
    
  #DFFITS vs. observation number
  plot(x = 1:n, y = dffits.i, xlab = "Observation number", ylab = "DFFITS", main = "DFFITS vs. observation number", 
       panel.first = grid(col = "gray", lty = "dotted"), 
       ylim = c(min(-1, -2*sqrt(p/n), min(dffits.i)), max(1, 2*sqrt(p/n), max(dffits.i))))
  abline(h = 0, col = "darkgreen")
  abline(h = c(-2*sqrt(p/n), 2*sqrt(p/n)), col = "red", lwd = 2)
  abline(h = c(-1,1), col = "darkred", lwd = 2)
  identify(x = 1:n, y = dffits.i)
    
  
  cook.i<-cooks.distance(model = mod.fit)
  cook.i[cook.i>qf(p=0.5,df1=p, df2=mod.fit$df.residual)]

  #Cook's distance vs. observation number
  plot(x = 1:n, y = cook.i, xlab = "Observation number", ylab = "Cook's D", main = "Cook's D vs. observation number", 
       panel.first = grid(col = "gray", lty = "dotted"), 
       ylim = c(0, qf(p=0.5,df1=p, df2=mod.fit$df.residual)))
  abline(h = 0, col = "darkgreen")
  abline(h = qf(p=0.5,df1=p, df2=mod.fit$df.residual), col = "red", lwd = 2)
  identify(x = 1:n, y = cook.i)



  dfbeta.all<-dfbetas(model = mod.fit) 
  pred.var.numb<-length(mod.fit$coefficients)-1
  
  win.graph(width = 8, height = 6, pointsize = 10)
  par(mfrow = c(2,2))
  
  for(j in 1:pred.var.numb) {
 
    plot(x = 1:n, y = dfbeta.all[,1+j], xlab = "Observation number", ylab = "DFBETAS", 
         main = paste("DFBETAS for variable", j, "vs. observation number"), 
         panel.first = grid(col = "gray", lty = "dotted"), 
         ylim = c(min(-1, -2/sqrt(n), min(dfbeta.all[,1+j])), max(1, 2/sqrt(n), max(dfbeta.all[,1+j]))))
    abline(h = 0, col = "darkgreen")
    abline(h = c(-2/sqrt(n), 2/sqrt(n)), col = "red", lwd = 2)
    abline(h = c(-1,1), col = "darkred", lwd = 2)
    identify(x = 1:n, y = dfbeta.all[,1+j])
  
  }
  
  
  
  #Bubble plot example - note that I need to use the absolute value function here (size of bubble can not be negative!)
  par(mfrow = c(1,1))
  symbols(x = mod.fit$fitted.values, y = r.i, circles = abs(dffits.i), xlab=expression(hat(Y)), ylab=expression(r[i]),
          main = "Studentized residual vs. predicted value \n Plotting point proportional to |DFFITS|", inches=0.25,
          panel.first=grid(col="gray", lty="dotted"), 
          ylim = c(min(qt(p = 0.05/(2*n), df = mod.fit$df.residual), min(r.i)), max(qt(p = 1-0.05/(2*n), df = mod.fit$df.residual), max(r.i))))
  abline(h = 0, col = "darkgreen")
  abline(h = c(qt(p = 0.01/2, df = mod.fit$df.residual),qt(p = 1-0.01/2, df = mod.fit$df.residual)), col = "red", lwd = 2)
  abline(h = c(qt(p = 0.05/(2*n), df = mod.fit$df.residual),qt(p = 1-0.05/(2*n), df = mod.fit$df.residual)), col = "darkred", lwd = 2)
  identify(x = mod.fit$fitted.values, y = r.i)




  #Plots automatically produced by R
  plot(mod.fit, which=1:6)



############################################################################
# Section 10.5

  vif(mod.fit)

  mod.fit.MPG<-lm(formula = MPG ~ height + FGP + age, data = nba)
  sum.fit.MPG<-summary(mod.fit.MPG)
  1/(1-sum.fit.MPG$r.squared)














#
