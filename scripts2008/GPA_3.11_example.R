############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-21-06                                                           #
# PURPOSE: Do all parts of Chapter 3 with respect to the GPA data set      #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#Read in the data
gpa<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter1\\gpa.txt", header=TRUE, sep = "")
head(gpa)

#Fit the simple linear regression model and save the results in mod.fit
mod.fit<-lm(formula = College.GPA ~ HS.GPA, data = gpa)
summary(mod.fit)


############################################################################
#  Investigate E(Y) = beta0 + beta1*X model

  #Use function from examine.mod.simple.R - Remember that this program needs to be run once during the R
  #  session before the code below can be run!  
  save.it<-examine.mod.simple(mod.fit.obj = mod.fit, const.var.test = TRUE, boxcox.find = TRUE)
  save.it
  
  #Could also use to get the same results
  save.it<-examine.mod.simple(mod.fit.obj = mod.fit, const.var.test = TRUE, Y = gpa$College.GPA, X = gpa$HS.GPA)
  save.it


############################################################################
#  Investigate NEW model of trans(Y) = beta0 + beta1*X model

  mod.fit2<-lm(formula = log(College.GPA) ~ HS.GPA, data = gpa)
  save.it2<-examine.mod.simple(mod.fit.obj = mod.fit2, const.var.test = TRUE, Y = log(gpa$College.GPA))
  summary(mod.fit2)
  
  mod.fit3<-lm(formula = sqrt(College.GPA) ~ HS.GPA, data = gpa)
  save.it3<-examine.mod.simple(mod.fit.obj = mod.fit3, const.var.test = TRUE, Y = sqrt(gpa$College.GPA))

  mod.fit4<-lm(formula = 1/College.GPA ~ HS.GPA, data = gpa)
  save.it4<-examine.mod.simple(mod.fit.obj = mod.fit4, const.var.test = TRUE, Y = 1/gpa$College.GPA)
  
  
############################################################################
#  Investigate if pizza consumption should be added to the model.  

  gpa2<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter2\\data\\College_GPA_pizza.txt", header=TRUE, sep = "")
  head(gpa2)
  
  par(mfrow = c(1,1))  #Change back to 1 plot per graphics window
  plot(x = gpa2$pizza, y = mod.fit$residuals, xlab = "Times ate pizza", ylab = "Residual", 
      main = "Residual vs. Pizza consumption", panel.first = grid(col = "gray", lty = "dotted"))
  abline(h = 0, col = "red")

  
  
  
  
  
  
  
  
  
  
  
#
