############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-31-06                                                           #
# PURPOSE: NBA data example for Chapter 8                                  #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

nba<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter6\\nba_data.txt", header=TRUE, sep = "")
head(nba)


############################################################################
# Just age

  #Without adjusting age
  mod.fit1<-lm(formula = PPM ~ age + I(age^2), data = nba)
  summary(mod.fit1)
 
  predict(object = mod.fit1, newdata = data.frame(age = 20))
  c(1,20,20^2)%*%mod.fit1$coefficients


  #Mean adjusting age - lm() part works, but predict() part does not
  mod.fit2<-lm(formula = PPM ~ I(age-mean(age)) + I((age-mean(age))^2), data = nba)
  summary(mod.fit2)
  
  predict(object = mod.fit2, newdata = data.frame(age = 20))  #Incorrect answer
  c(1,20-mean(nba$age),(20-mean(nba$age))^2)%*%mod.fit2$coefficients  #Correct
  c(1,20,20^2)%*%mod.fit2$coefficients  #Incorrect answer
  c(1,0,0)%*%mod.fit2$coefficients  #Incorrect answer - this is what predict() does above
 
 
  #Mean adjusting age - lm() part works AND predict() part does as well
  mod.fit3<-lm(formula = PPM ~ I(age-mean(nba$age)) + I((age-mean(nba$age))^2), data = nba)
  summary(mod.fit3)
  
  predict(object = mod.fit3, newdata = data.frame(age = 20))
  c(1,20-mean(nba$age),(20-mean(nba$age))^2)%*%mod.fit3$coefficients  #Correct
 
  
  #Compare C.I.s - notice they are the same
  predict(object = mod.fit1, newdata = data.frame(age = 20), se.fit = TRUE, interval = "confidence")
  predict(object = mod.fit3, newdata = data.frame(age = 20), se.fit = TRUE, interval = "confidence")


  #Examine estimated covariance matrices for b
  sum.fit1<-summary(mod.fit1)
  cov.b.fit1<-sum.fit1$sigma^2 * sum.fit1$cov.unscaled #See Section 5.13
  sum.fit3<-summary(mod.fit3)
  cov.b.fit3<-sum.fit3$sigma^2 * sum.fit3$cov.unscaled
  x<-20
  x.bar<-mean(nba$age)
  var.Y.hat.fit1<-cov.b.fit1[1,1] + x^2*cov.b.fit1[2,2] + x^4*cov.b.fit1[3,3] + 2*x*cov.b.fit1[1,2] + 2*x^2*cov.b.fit1[1,3] + 2*x^3*cov.b.fit1[2,3]
  var.Y.hat.fit3<-cov.b.fit3[1,1] + (x-x.bar)^2*cov.b.fit3[2,2] + (x-x.bar)^4*cov.b.fit3[3,3] + 2*(x-x.bar)*cov.b.fit3[1,2] + 2*(x-x.bar)^2*cov.b.fit3[1,3] + 2*(x-x.bar)^3*cov.b.fit3[2,3]
  sqrt(var.Y.hat.fit1)
  sqrt(var.Y.hat.fit3)

  #Using matrices
  X.h<-c(1, 20, 20^2)
  X<-cbind(1, nba$age, nba$age^2)
  sqrt(as.numeric(sum.fit1$sigma^2)*X.h%*%solve(t(X)%*%X)%*%X.h)

  X.adj.h<-c(1, 20-x.bar, (20-x.bar)^2)
  X.adj<-cbind(1, nba$age-x.bar, (nba$age-x.bar)^2) #Notice how EVERY element in nba$age vector has x.bar subtracted from it
  sqrt(as.numeric(sum.fit3$sigma^2)*X.adj.h%*%solve(t(X.adj)%*%X.adj)%*%X.adj.h)



  #Examine correlations
  cor(x = nba$age, y = nba$age^2)
  cor(x = nba$age - mean(nba$age), y = (nba$age-mean(nba$age))^2)



  plot(x = nba$age, y = nba$PPM, xlab = "Age", ylab = "PPM", main = "PPM vs. Age", 
       panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = predict(object = mod.fit1, newdata = data.frame(age = x)), 
        col = "red", lty = "solid", lwd = 1, add = TRUE, from = min(nba$age), to = max(nba$age))


############################################################################
# Using age and MPG

  mod.fit.comp<-lm(formula = PPM ~ age + MPG + age*MPG + I(age^2) + I(MPG^2), data = nba)
  sum.fit.comp<-summary(mod.fit.comp)
  sum.fit.comp
  sum.fit.comp$sigma^2
  
  mod.fit.red<-lm(formula = PPM ~ age + MPG, data = nba)
  summary(mod.fit.red)

  anova(mod.fit.red, mod.fit.comp)

  p<-length(mod.fit.comp$coefficients)  #Number of betas in model is p
  g<-length(mod.fit.red$coefficients)-1 #Number of variables remaining in the reduced model is g
  qf(p = 0.95, df1 = p-1-g, df2 = mod.fit.comp$df.residual)  
   
   

############################################################################
# Using age and MPG transformed

  mod.fit.comp.tran<-lm(formula = PPM ~ I(age-mean(age)) + I(MPG-mean(MPG)) + I(age-mean(age))*I(MPG-mean(MPG)) + 
                                   I((age-mean(age))^2) + I((MPG-mean(MPG))^2), data = nba)
  summary(mod.fit.comp.tran)

  mod.fit.red.tran<-lm(formula = PPM ~ I(age-mean(age)) + I(MPG-mean(MPG)), data = nba)
  summary(mod.fit.red.tran)

  anova(mod.fit.red.tran, mod.fit.comp.tran)



############################################################################
# 3D plots

  library(Rcmdr)
  library(rgl) 
  rgl.clear("all") #Clears plot window
  rgl.light() #Gray background
  rgl.bbox()  #Puts numbers on plot and box around it
  scatter3d(x = nba$age, y = nba$PPM, z = nba$MPG, 
          fit="quadratic", grid=TRUE, xlab="age", ylab="PPM", zlab="MPG", 
          bg.col="black")
          
    
  #White background (lose x, y, and z-axis labels)
  library(Rcmdr)
  scatter3d(x = nba$age, y = nba$PPM, z = nba$MPG, fit="quadratic", bg="white", grid=TRUE, xlab="age", ylab="PPM", zlab="MPG")
  #identify3d(x = nba$age, y = nba$PPM, z =  nba$MPG)
      
          
  library(scatterplot3d) 
  win.graph(width = 8, height = 6, pointsize = 10)
  save.plot<-scatterplot3d(x = nba$age, y = nba$MPG, z = nba$PPM,
                main = "3D scatterplot for NBA data", xlab = "Age", ylab = "MPG",
                zlab = "PPM", type = "h", pch=16, highlight.3d=TRUE, angle=55, lwd = 2)
  
  #Add plane - Will not add it to plot.
  save.plot$plane3d(mod.fit.comp, lty.box = "solid", col = "blue")  


############################################################################
# Section 8.2 - interaction regression models

  mod.fit.inter<-lm(formula = PPM ~ age + MPG + age*MPG, data = nba)
  summary(mod.fit.inter)


  #Another way to do 3D plot in R - can not get both points and plane on the same plot
  library(lattice) 
  save.xyz<-expand.grid(age = min(nba$age):max(nba$age), MPG = floor(min(nba$MPG)):ceiling(max(nba$MPG)))
  save.xyz$PPM.hat<-as.matrix(cbind(1, save.xyz, save.xyz$age*save.xyz$MPG))%*%mod.fit.inter$coefficients
  win.graph(width = 8, height = 6, pointsize = 10)
  wireframe(PPM.hat ~ age + MPG, data = save.xyz, scales = list(arrows = FALSE), drape = TRUE, 
           colorkey = TRUE, aspect = c(0.7, 0.3))
  wireframe(PPM.hat ~ age + MPG, data = save.xyz, scales = list(arrows = FALSE), drape = TRUE, 
           colorkey = TRUE, aspect = c(1.3, 0.6))
           
   
  #Additional ways to represent the model
  mod.fit.inter1<-lm(formula = PPM ~ age + MPG + age:MPG, data = nba)
  mod.fit.inter1$coefficients

  mod.fit.inter2<-lm(formula = PPM ~ age*MPG, data = nba)
  mod.fit.inter2$coefficients

  mod.fit.inter3<-lm(formula = PPM ~ (age+MPG)^2, data = nba)
  mod.fit.inter3$coefficients


#
