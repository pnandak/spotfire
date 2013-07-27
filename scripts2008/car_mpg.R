############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-31-06                                                           #
# PURPOSE: Car data for Chapter 8                                          #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

library(RODBC)
z<-odbcConnectExcel("C:\\chris\\UNL\\STAT870\\Chapter8\\car_data98.xls")
car.data<-sqlFetch(z, "Sheet1")
close(z)

head(car.data)

###########################################################################
# Class variable

  mod.fit<-lm(formula = MPG ~ Class, data = car.data)
  summary(mod.fit)

  #Show how the qualitative variable was coded
  contrasts(car.data$Class)

  #This is the default way to code qualitative variables - "set first level to 0"
  #options(contrasts=c("contr.treatment", "contr.poly"))

  #This code changes the default to the "sum to 0" way of coding qualitative variables
  #options(contrasts=c("contr.sum", "contr.poly"))


###########################################################################
# Cylinder variable

  mod.fit1<-lm(formula = MPG ~ Cylinders, data = car.data)
  summary(mod.fit1)
  
  #Treat Cylinders as qualitative
  mod.fit2<-lm(formula = MPG ~ factor(Cylinders), data = car.data)
  summary(mod.fit2)
  
  factor(car.data$Cylinders)
  contrasts(factor(car.data$Cylinders))


###########################################################################
# Sections 8.5-8.6

  mod.fit3<-lm(formula = MPG ~ Engine + Class + Engine:Class, data = car.data)
  summary(mod.fit3)

  mod.fit.red<-lm(formula = MPG ~ Engine + Class, data = car.data)
  #summary(mod.fit.red)

  anova(mod.fit.red, mod.fit3)


  #Sample model plot - could use the predict() function in the expr statement?
  win.graph(width = 6, height = 6, pointsize = 10)
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x, col = "red", lty = 
        "solid", lwd = 2, xlim = c(1,6), ylim = c(0,55), xlab = "Engine size", ylab = "MPG", 
        main = "MPG vs. engine size", panel.first = grid(col = "gray", lty = "dotted"))  #Compact
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[3] + mod.fit3$coefficients[9]*x, 
        col = "blue", lty = "solid", lwd = 2, add = TRUE, from = 1, to = 6)        #Large
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[4] + mod.fit3$coefficients[10]*x, 
        col = "green", lty = "solid", lwd = 2, add = TRUE, from = 1, to = 6)       #Mid-size
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[5] + mod.fit3$coefficients[11]*x, 
        col = "orange", lty = "solid", lwd = 2, add = TRUE, from = 1, to = 6)      #Mini-compact
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[6] + mod.fit3$coefficients[12]*x, 
        col = "black", lty = "solid", lwd = 2, add = TRUE, from = 1, to = 6)       #Station wagon
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[7] + mod.fit3$coefficients[13]*x, 
        col = "lightblue", lty = "solid", lwd = 2, add = TRUE, from = 1, to = 6)   #Sub-compact
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[8] + mod.fit3$coefficients[14]*x, 
        col = "brown", lty = "solid", lwd = 2, add = TRUE, from = 1, to = 6)       #Two-seater
  legend(locator(1), legend = c("Compact", "Large", "Mid-size", "Mini-compact", "Station wagon", "Sub-compact", "Two-seater"),
         col = c("red", "blue", "green", "orange", "black", "lightblue", "brown"),
         lty = rep(x = "solid", times = 7), bty = "n", cex = 1, lwd = 2)


  #Plot the model with the observations
  library(car) #recode() function is in the car package
  class.colors<-recode(var = car.data$Class, recodes = "'Compact'='red'; 'Large'='blue'; 'Mid-Size'='green'; 
                                                       'Mini-Compact'='orange'; 'Station Wagon'='black'; 
                                                       'Sub-Compact'='lightblue'; 'Two Seater'='brown'")
  class.symbols<-recode(var = car.data$Class, recodes = "'Compact'=1; 'Large'=2; 'Mid-Size'=3; 
                                                       'Mini-Compact'=4; 'Station Wagon'=5; 
                                                       'Sub-Compact'=6; 'Two Seater'=7")
  win.graph(width = 6, height = 6, pointsize = 10)
  #This is a scatter plot of the data with the plotting symbol corresponding to the Class.  Note that the recode()
  #  function does not produce the correct class type needed for the col and pch options below so I used as.character()
  #  and as.numeric() to change it to the correct type.  
  plot(x = car.data$Engine, y = car.data$MPG, xlab = "Engine size", ylab = "MPG", main = "MPG vs. engine size", 
       col = as.character(class.colors), pch = as.numeric(class.symbols), cex = 1.0, lwd = 1, 
       xlim = c(1,6), ylim = c(0,55), panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x, col = "red", lty = "solid", lwd = 1, add = TRUE)  #Compact
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[3] + mod.fit3$coefficients[9]*x, 
        col = "blue", lty = "solid", lwd = 1, add = TRUE, from = 1, to = 6)        #Large
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[4] + mod.fit3$coefficients[10]*x, 
        col = "green", lty = "solid", lwd = 1, add = TRUE, from = 1, to = 6)       #Mid-size
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[5] + mod.fit3$coefficients[11]*x, 
        col = "orange", lty = "solid", lwd = 1, add = TRUE, from = 1, to = 6)      #Mini-compact
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[6] + mod.fit3$coefficients[12]*x, 
        col = "black", lty = "solid", lwd = 1, add = TRUE, from = 1, to = 6)       #Station wagon
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[7] + mod.fit3$coefficients[13]*x, 
        col = "lightblue", lty = "solid", lwd = 1, add = TRUE, from = 1, to = 6)   #Sub-compact
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[8] + mod.fit3$coefficients[14]*x, 
        col = "brown", lty = "solid", lwd = 1, add = TRUE, from = 1, to = 6)       #Two-seater
  legend(locator(1), legend = c("Compact", "Large", "Mid-size", "Mini-compact", "Station wagon", "Sub-compact", "Two-seater"),
         col = c("red", "blue", "green", "orange", "black", "lightblue", "brown"), pch = 1:7,
         lty = rep(x = "solid", times = 7), bty = "n", cex = 1, lwd = 1)


  #Plot for each individual class
  win.graph(width = 9, height = 6, pointsize = 10)
  par(mfrow = c(2,4))
  plot(x = car.data$Engine[car.data$Class == "Compact"], y = car.data$MPG[car.data$Class == "Compact"], 
       xlab = "Engine size", ylab = "MPG", main = "MPG vs. engine size", col = "red", pch = 1, cex = 1.0, lwd = 1,
       xlim = c(1,6), ylim = c(0,55), panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x, col = "red", lty = "solid", lwd = 1, add = TRUE)  #Compact

  plot(x = car.data$Engine[car.data$Class == "Large"], y = car.data$MPG[car.data$Class == "Large"], 
       xlab = "Engine size", ylab = "MPG", main = "MPG vs. engine size", col = "blue", pch = 2, cex = 1.0, lwd = 1,
       xlim = c(1,6), ylim = c(0,55), panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[3] + mod.fit3$coefficients[9]*x, 
        col = "blue", lty = "solid", lwd = 1, add = TRUE, from = 1, to = 6)        #Large

  plot(x = car.data$Engine[car.data$Class == "Mid-Size"], y = car.data$MPG[car.data$Class == "Mid-Size"], 
       xlab = "Engine size", ylab = "MPG", main = "MPG vs. engine size", col = "green", pch = 3, cex = 1.0, lwd = 1,
       xlim = c(1,6), ylim = c(0,55), panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[4] + mod.fit3$coefficients[10]*x, 
        col = "green", lty = "solid", lwd = 1, add = TRUE, from = 1, to = 6)       #Mid-size

  plot(x = car.data$Engine[car.data$Class == "Mini-Compact"], y = car.data$MPG[car.data$Class == "Mini-Compact"], 
       xlab = "Engine size", ylab = "MPG", main = "MPG vs. engine size", col = "orange", pch = 4, cex = 1.0, lwd = 1,
       xlim = c(1,6), ylim = c(0,55), panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[5] + mod.fit3$coefficients[11]*x, 
        col = "orange", lty = "solid", lwd = 1, add = TRUE, from = 1, to = 6)      #Mini-compact

  plot(x = car.data$Engine[car.data$Class == "Station Wagon"], y = car.data$MPG[car.data$Class == "Station Wagon"], 
       xlab = "Engine size", ylab = "MPG", main = "MPG vs. engine size", col = "black", pch = 5, cex = 1.0, lwd = 1,
       xlim = c(1,6), ylim = c(0,55), panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[6] + mod.fit3$coefficients[12]*x, 
        col = "black", lty = "solid", lwd = 1, add = TRUE, from = 1, to = 6)       #Station wagon
        
  plot(x = car.data$Engine[car.data$Class == "Sub-Compact"], y = car.data$MPG[car.data$Class == "Sub-Compact"], 
       xlab = "Engine size", ylab = "MPG", main = "MPG vs. engine size", col = "lightblue", pch = 6, cex = 1.0, lwd = 1,
       xlim = c(1,6), ylim = c(0,55), panel.first = grid(col = "gray", lty = "dotted"))  
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[7] + mod.fit3$coefficients[13]*x, 
        col = "lightblue", lty = "solid", lwd = 1, add = TRUE, from = 1, to = 6)   #Sub-compact

  plot(x = car.data$Engine[car.data$Class == "Two Seater"], y = car.data$MPG[car.data$Class == "Two Seater"], 
       xlab = "Engine size", ylab = "MPG", main = "MPG vs. engine size", col = "brown", pch = 7, cex = 1.0, lwd = 1,
       xlim = c(1,6), ylim = c(0,55), panel.first = grid(col = "gray", lty = "dotted"))  
  curve(expr = mod.fit3$coefficients[1] + mod.fit3$coefficients[2]*x + mod.fit3$coefficients[8] + mod.fit3$coefficients[14]*x, 
        col = "brown", lty = "solid", lwd = 1, add = TRUE, from = 1, to = 6)       #Two-seater

  #Dummy plot for legend - type = "n" tells R to not plot anything, xaxt and yaxt = "n" remove the x and y-axis labels, respectively
  plot(x = car.data$Engine, y = car.data$MPG, type = "n", xlab = " ", ylab = " ", main = "", xaxt = "n", yaxt = "n")  
  legend(x = 1.5, y = 45, legend = c("Compact", "Large", "Mid-size", "Mini-compact", "Station wagon", "Sub-compact", "Two-seater"),
         col = c("red", "blue", "green", "orange", "black", "lightblue", "brown"), pch = 1:7,
         lty = rep(x = "solid", times = 7), bty = "n", cex = 1.25)
 








#
