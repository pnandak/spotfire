#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-9-02                                                     #
# UPDATE: 1-9-03 (predict function), 12-11-03 R, 12-23-05, 12-24-07 #
# Purpose: Horseshoe crab example - find poisson regression model   #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#####################################################################
# Read in data and fit initial model

  #Read in data
  crab<-read.table(file = "c:\\Chris\\UNL\\STAT875\\chapter3\\horseshoe.txt", header=FALSE, col.names = c("satellite", "width"))

  #Fit model
  mod.fit<-glm(formula = satellite ~ width, data = crab, family = poisson(link = log), 
    na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
  summary(mod.fit)


  #Predict number of satellites
  lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*23
  exp(lin.pred)

  lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*30
  exp(lin.pred)


  #Easier way to predict
  predict.data<-data.frame(width = c(23, 30))
  alpha<-0.05
  save.mu.hat<-predict(object = mod.fit, newdata = predict.data, type = "response", se = TRUE)

  lower<-save.mu.hat$fit-qnorm(1-alpha/2)*save.mu.hat$se
  upper<-save.mu.hat$fit+qnorm(1-alpha/2)*save.mu.hat$se
  data.frame(predict.data, mu.hat = round(save.mu.hat$fit,4), lower = round(lower,4), upper = round(upper,4))



##################################################################
# Plot

  #Plot of data and estimated model
  plot(x = crab$width, y = crab$satellite, xlab = "Width (cm)", ylab = "Number of satellites", 
    main = "Horseshoe crab data set \n with poisson regression model fit", panel.first = 
    grid(col = "gray", lty = "dotted"))
  curve(expr = exp(mod.fit$coefficients[1]+mod.fit$coefficients[2]*x), col = "red", add = TRUE, lty = 1) 


  #The is part of Table 3.3 on p. 80 of Agresti (2007).  The last two "columns" are the number of cases
  #  and the number of satellites.  The first "column" is the group width mean corresponding to the 
  #  width categories given in Table 3.3.  These means are stated on p. 90 of Agresti (1996).  In the 2007
  #  edition, he did not state them.  However, these can be simply found as shown in my table3.3.R
  #  program.  
  #temp2<-c(22.69,  14,  14,
  #  23.84,  14,  20,
  #  24.77,  28,  67,
  #  25.84,  39, 105,
  #  26.79,  22,  63,
  #  27.74,  24,  93,
  #  28.67,  18,  71,
  # 30.41,  14,  72)

  #temp3<-matrix(data=temp2, nrow=8, ncol=3, byrow=T)
  #crab.tab3.3<-data.frame(width=temp3[,1], cases=temp3[,2], satell=temp3[,3])

  #Easier way to create data.frame here:
  crab.tab3.3<-data.frame(width = c(22.69, 23.84, 24.77, 25.84, 26.79, 27.74, 28.67, 30.41),
                          cases = c(14, 14, 28, 39, 22, 24, 18, 14),
                          satell = c(14, 20, 67, 105, 63, 93, 71, 72))

  #Average number of satellites per group
  mu.obs<-crab.tab3.3$satell/crab.tab3.3$cases

  points(x = crab.tab3.3$width, y = mu.obs, pch = 18, col = "darkgreen", cex = 2)
  legend(locator(1), legend="Diamonds are group mean", cex = 0.75)






#############################################################################
# Investigate residuals

  #Pearson residuals from Poisson regression model (w/o using offset)
  pearson1<-residuals(object = mod.fit, type="pearson")
 
  #Standardized Pearson residuals
  h<-lm.influence(model = mod.fit)$h  #Also can use hatvalues(model = mod.fit)
  head(h)
  standard.pearson<-pearson1/sqrt(1-h)
  head(standard.pearson)
   
  X<-model.matrix(mod.fit)
  mu.hat<-predict(object = mod.fit, type = "response")  #Also could use mu.hat<-mod.fit$fitted.values
  H<-diag(sqrt(mu.hat))%*%X%*%solve(t(X)%*%diag(mu.hat)%*%X)%*%t(X)%*%diag(sqrt(mu.hat))
  diag(H)[1:5]


  par(mfrow = c(2,1))  #2x1 grid of plots
  #Pearson residual vs observation number plot
  plot(x = 1:length(pearson1), y = pearson1, xlab="Observation number", ylab="Pearson residuals", 
     main = "Pearson residuals vs. observation number")
  abline(h = c(qnorm(0.975), qnorm(0.995)), lty=3, col="red")
  abline(h = c(qnorm(0.025), qnorm(0.005)), lty=3, col="red")

  #Standardized residual vs observation number plot
  plot(x = 1:length(standard.pearson), y = standard.pearson, xlab="Observation number", ylab="Standardized residuals", 
     main = "Standardized residuals vs. observation number")
  abline(h = c(qnorm(0.975), qnorm(0.995)), lty=3, col="red")
  abline(h = c(qnorm(0.025), qnorm(0.005)), lty=3, col="red")

  par(mfrow = c(1,1))  
  #Residual vs width plot - this plot can be done since only 1 explanatory variable is present
  plot(x = crab$width, y = standard.pearson, xlab="Width", ylab="Standardized residuals", main = "Standardized residuals vs. width")
  abline(h = c(qnorm(0.975), qnorm(0.995)), lty=3, col="red")
  abline(h = c(qnorm(0.025), qnorm(0.005)), lty=3, col="red")

  plot(x = crab$width, y = standard.pearson, xlab="Width", ylab="Standardized residuals", main = "Standardized residuals vs. width", type = "n")
  text(x = crab$width, y = standard.pearson, labels = crab$satellite, cex=0.75)
  abline(h = c(qnorm(0.975), qnorm(0.995)), lty=3, col="red")
  abline(h = c(qnorm(0.025), qnorm(0.005)), lty=3, col="red")
  
  
##############################################################
# Goodness of fit
      
  #LRT: -2log(lambda)
  mod.fit$null.deviance

  #p-value
  1-pchisq(q = mod.fit$deviance, df = mod.fit$df.residual)

  #Pearson statistic
  sum(pearson1^2)
  1-pchisq(q = sum(pearson1^2), df = mod.fit$df.residual)



#########################################################
# Negative binomial

  library(MASS)
  mod.fit.nb<-glm.nb(formula = satellite ~ width, data = crab, link = log)
  summary(mod.fit.nb)

  #See p. 65 of Thompson's S-Plus and R guide for Agresti (2002) for discussion of problems with LRTs

  pearson.nb<-residuals(object = mod.fit.nb, type="pearson")
  h.nb<-lm.influence(model = mod.fit.nb)$h  
  head(h.nb)
  standard.pearson.nb<-pearson.nb/sqrt(1-h.nb)

  par(mfrow = c(1,2))
  plot(x = 1:length(standard.pearson.nb), y = standard.pearson.nb, xlab="Obs. number", ylab="Standardized residuals", 
     main = "Stand. residuals (NB model) vs. obs. number")
  abline(h = c(qnorm(0.975), qnorm(0.995)), lty=3, col="red")
  abline(h = c(qnorm(0.025), qnorm(0.005)), lty=3, col="red")

  plot(x = crab$width, y = standard.pearson.nb, xlab="Width", ylab="Standardized residuals", 
     main = "Stand. residuals (NB model) vs. width", type = "n")
  text(x = crab$width, y = standard.pearson.nb, labels = crab$satellite, cex=0.75)
  abline(h = c(qnorm(0.975), qnorm(0.995)), lty=3, col="red")
  abline(h = c(qnorm(0.025), qnorm(0.005)), lty=3, col="red")




  #Another way to deal with overdispersion is discussed on p. 151 of Agresti (2002).  Here's 
  #  its implementation.
  mod.fit.nb2<-glm(formula = satellite ~ width, data = crab, family = quasipoisson(link = log), 
    na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
  sum.fit2<-summary(mod.fit.nb2)
  names(sum.fit2)
  sum.fit2$cov.unscaled
  sum.fit2$cov.scaled
  sum.fit2$cov.unscaled*sum.fit2$dispersion



#########################################################
# Using offsets
  
  library(nlme) #gsummary function is located here

  sum.rate.data<-gsummary(object = crab, FUN = sum, groups = crab$width) 
  length.rate.data<-gsummary(object = crab, FUN = length, groups=crab$width) 

  rate.data<-data.frame(y = sum.rate.data$satellite, t = length.rate.data$satellite, width = length.rate.data$width)
  rate.data[1:5,]

  mod.fit.rate<-glm(formula = y ~ width+offset(log(t)), data = rate.data, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
  summary(mod.fit.rate)

  par(mfrow = c(1,1))
  #Plot of data with estimated mu's; notice the use of the panel.first option to put grid lines behind plotting points
  plot(x = crab$width, y = crab$satellite, xlab = "Width (cm)", ylab = "Number of satellites", 
     panel.first = grid(col = "gray", lty = "dotted"), 
     main = "Horseshoe crab data set \n with poisson regression model fit (rate data)")
  points(x = rate.data$width, y = mod.fit.rate$fitted.values, pch = 18, col = "darkgreen", cex = 1)
  legend(locator(1), legend = "Diamonds are predicted values", cex = 0.75)


  #LRT: -2log(lambda)
  mod.fit.rate$null.deviance-mod.fit.rate$deviance

  #p-value
  1-pchisq(q = mod.fit.rate$null.deviance-mod.fit.rate$deviance, df = mod.fit.rate$df.null-mod.fit.rate$df.residual)




##########################################################################################
#Pearson residuals from model using offset

  pearson.rate<-resid(object = mod.fit.rate, type="pearson")
 
  #Standardized Pearson residuals
  h.rate<-lm.influence(model = mod.fit.rate)$h  
  head(h.rate)
  standard.pearson.rate<-pearson.rate/sqrt(1-h.rate)
  head(standard.pearson.rate)

  par(mfrow = c(1,2))
  plot(x = 1:length(standard.pearson.rate), y = standard.pearson.rate, xlab="Obs. number", ylab="Standardized residuals", 
     main = "Stand. residuals (rate model) vs. obs. numb.")
  abline(h = c(qnorm(0.975), qnorm(0.995)), lty=3, col="red")
  abline(h = c(qnorm(0.025), qnorm(0.005)), lty=3, col="red")

  plot(x = rate.data$width, y = standard.pearson.rate, xlab="Width", ylab="Standardized residuals", 
     main = "Stand. residuals (rate model) vs. width")
  abline(h = c(qnorm(0.975), qnorm(0.995)), lty=3, col="red")
  abline(h = c(qnorm(0.025), qnorm(0.005)), lty=3, col="red")
  identify(x = rate.data$width, y = standard.pearson.rate)

  par(mfrow = c(1,1))
  plot(x = rate.data$width, y = standard.pearson.rate, xlab="Width", ylab="Standardized residuals", 
     main = "Stand. residuals (rate model) vs. width", type = "n")
  text(x = rate.data$width, y = standard.pearson.rate, labels = rate.data$y, cex=0.75)
  abline(h = c(qnorm(0.975), qnorm(0.995)), lty=3, col="red")
  abline(h = c(qnorm(0.025), qnorm(0.005)), lty=3, col="red")





##############################################
# Do goodness-of-fit tests with offset model

  #LRT p-value
  1-pchisq(q = mod.fit.rate$deviance, df = mod.fit.rate$df.residual)

  #Pearson statistic and p-value
  sum(pearson.rate^2)
  1-pchisq(q = sum(pearson.rate^2), df = mod.fit.rate$df.residual)





###################################################################
# Model for table 4.3 data from Agresti (1996) - he showed more information in the first edition

  mod.fit <- glm(formula = satell ~ width + offset(log(cases)), data = crab.tab3.3, 
           family = poisson(link = log), na.action = na.exclude, 
           control = list(epsilon = 0.0001, maxit = 50, trace = T))
  summary(mod.fit)

  #LRT p-value
  1-pchisq(mod.fit$deviance, mod.fit$df.residual)

  #Pearson statistic and p-value
  pearson3<-residuals(mod.fit, type="pearson")
  sum(pearson3^2)
  1-pchisq(sum(pearson3^2), mod.fit$df.residual)
     

###############################################################################
#  Adjusted Pearson residuals and deviance residuals


  #Fit model
  mod.fit<-glm(formula = satellite ~ width, data = crab, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
  summary(mod.fit)


  #Deviance residuals
  dev.resid<-resid(object = mod.fit, type="deviance")

  #Standardized deviance residuals
  dev.resid/sqrt(1-h)
  rstandard(model = mod.fit) #Does not produce adjusted Pearson through any options :(

  #sqrt of delta deviance - see eq 5.17 of p. 174 of Hosmer and Lemeshow (2000)
  sign(dev.resid)*sqrt(dev.resid^2 + h*Pearson^2/(1-h))
  rstudent(model = mod.fit)

















#
