############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-24-06                                                           #
# PURPOSE: Ad responses example for Chapter 6                              #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

ad.responses<-c(100, 400, 100, 300, 200, 400)
size<-c(1, 8, 3, 5, 6, 10)
circulation<-c(20000, 80000, 10000, 70000, 40000, 60000)

set1<-data.frame(ad.responses, size, circulation)
set1


###########################################################################
# Use matrix algebra to find important items

  X<-cbind(1, set1$size, set1$circulation)
  Y<-set1$ad.responses
  b<-solve(t(X)%*%X) %*% t(X)%*%Y
  b
  Y.hat<-X%*%b
  e<-Y-Y.hat
  n<-length(Y)
  J<-matrix(data = 1, nrow = n, ncol = n)  
  SSTO<-t(Y)%*%Y-1/n*t(Y)%*%J%*%Y
  SSE<-t(e)%*%e
  MSE<-SSE/(n-nrow(b))
  SSR = SSTO - SSE
  data.frame(X, Y, Y.hat, e)
  data.frame(n, SSTO, SSE, MSE, SSR)

  cov.beta.hat<-as.numeric(MSE)*solve(t(X)%*%X)
  cov.beta.hat

##########################################################################
# Using lm() results
  
  mod.fit<-lm(formula = ad.responses ~ size + circulation, data = set1)
  sum.fit<-summary(mod.fit)
  sum.fit
  
  add.data<-data.frame(size = 5, circulation = 50000)
  predict(object = mod.fit, newdata = add.data)
  
  MSE<-sum.fit$sigma^2
  
  data.frame(set1, Y.hat = round(mod.fit$fitted.values,2), e = round(mod.fit$residuals,2), 
             e.star = round(mod.fit$residuals/sqrt(MSE),2))
             
  #Section 6.5
  anova(mod.fit)


########################################################################
# 3D Plots


#3D plot not interactive - no longer discuss
  library(scatterplot3d) #Need to install it if have not already
  win.graph(width = 6, height = 6, pointsize = 10)
  save.plot<-scatterplot3d(x = set1$size, y = set1$circulation, z = set1$ad.responses,
                main = "3D scatterplot for ad. responses data", xlab = "Size", ylab = "Circulation",
                zlab = "Ad. responses", type = "h", pch=16, highlight.3d=TRUE, angle=55, lwd = 2)
  #Instead of highlight.3d=TRUE, could color each point with col = "___"
  
  #Add plane
  save.plot$plane3d(mod.fit, lty.box = "solid", col = "blue")  
  #Would be useful to have a "tilt" option so that you can tilt the axes like in SAS PROC G3D
  
  
  
  #Another way to do 3D plot in R - can not get both points and plane on the same plot
  library(lattice) 
  save.xyz<-expand.grid(size = 1:10, circulation = seq(from = 10000, to = 70000, by = 10000))
  save.xyz$ad.resp.hat<-as.matrix(cbind(1, save.xyz))%*%mod.fit$coefficients
  wireframe(ad.resp.hat ~ size + circulation, data = save.xyz, 
            scales = list(arrows = FALSE),
            drape = TRUE, colorkey = TRUE,
            screen = list(z = 0, x = -60))
  #Can not get both points and plane on the same plot!
  cloud(ad.responses ~ size + circulation, data = set1,
      screen = list(x = -90, y = 70), distance = .4, zoom = .6, arrows=FALSE)
  wireframe(ad.resp.hat ~ size + circulation, data = save.xyz, add = TRUE)  #Add = TRUE does not work

  #Note that persp() could also be used


#3D interactive

  library(Rcmdr)
  scatter3d(x = set1$circulation, y = set1$ad.responses, z = set1$size, fit="linear", bg="white", grid=TRUE, xlab="circulation", ylab="ad.responses", zlab="size")
  identify3d(x = set1$circulation, y = set1$ad.responses, z = set1$size, labels=1:6) #Can not get identify3d() to work with my computer :(


  #Additional options to change plot - allows one to see the x, y, z-axis numbers
  library(rgl) #If Rcmdr is not actually open, will need to use this (Rcmdr actually uses this package for the 3D interactive plots)
  rgl.clear("all") #Clears plot window
  rgl.light() #Gray background
  rgl.bbox()  #Puts numbers on plot and box around it
  scatter3d(x = set1$circulation, y = set1$ad.responses, z = set1$size, 
          fit="linear", grid=TRUE, xlab="circulation", ylab="ad.responses", zlab="size", 
          bg.col="black")



########################################################################
# 2D Plots
  
  #Sample model holding circulation constant
  win.graph(width = 6, height = 6, pointsize = 10)
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x + mod.fit$coefficients[3]*10000, col = "red", lty = 
        "solid", lwd = 2, xlim = c(1,10), ylim = c(0, 500), xlab = "Size", ylab = "Estimated ad. responses", 
        main = "Estimated ad. responses vs. size of ad. \n holding circulation constant", panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x + mod.fit$coefficients[3]*45000, col = "blue", lty = 
        "solid", lwd = 2, add = TRUE, from = 1, to = 10)
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x + mod.fit$coefficients[3]*80000, col = "darkgreen", lty = 
        "solid", lwd = 2, add = TRUE, from = 1, to = 10)
  legend(locator(1), legend = c("Circulation = 10,000", "Circulation = 45,000", "Circulation = 80,000"), col = c("red", "blue", "darkgreen"), 
         lty = c("solid", "solid", "solid"), bty = "n", cex = 0.75)

  #Sample model holding size constant
  win.graph(width = 6, height = 6, pointsize = 10)
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*1 + mod.fit$coefficients[3]*x, col = "red", lty = 
        "solid", lwd = 2, xlim = c(10000, 80000), ylim = c(0, 500), xlab = "Circulation", ylab = "Estimated ad. responses", 
        main = "Estimated ad. responses vs. circulation \n holding size constant", panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*5.5 + mod.fit$coefficients[3]*x, col = "blue", lty = 
        "solid", lwd = 2, add = TRUE, from = 10000, to = 80000)
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*10 + mod.fit$coefficients[3]*x, col = "darkgreen", lty = 
        "solid", add = TRUE, from = 10000, to = 80000)
  legend(locator(1), legend = c("Size = 1", "Size = 5.5", "Size = 10"), col = c("red", "blue", "darkgreen"), 
         lty = c("solid", "solid", "solid"), bty = "n", cex = 0.75)
 
  
  
#
