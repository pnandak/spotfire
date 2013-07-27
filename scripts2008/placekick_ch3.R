#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-8-02                                                     #
# UPDATE: 1-9-03, 12-23-05, 12-6-06                                 #
# Purpose: Find the logistic regression model for the placekick data#
#                                                                   #
# NOTES:                                                            #
#####################################################################


place.s<-read.table("C:\\chris\\UNL\\STAT875\\chapter3\\place.s.csv", header = TRUE, sep = ",")
head(place.s)

##############################################################
# Fit model

  mod.fit<-glm(formula = good1 ~ dist, data = place.s, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))

  names(mod.fit)
  mod.fit$coefficients
  mod.fit 
  summary(mod.fit)


###############################################################
# Estimate probability of success

  #Estimated probability of success for a 20 yard field goal
  lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*20
  exp(lin.pred)/(1+exp(lin.pred))

  #Estimated probability of success for a 50 yard field goal
  lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*50
  exp(lin.pred)/(1+exp(lin.pred))

  #Simple plot - not good enough for projects
  plot(place.s$dist, mod.fit$fitted.values, xlab="Distance (yards)", 
     ylab="Estimated probability", main = "Estimated probability of success of a placekick")

     
#######################################################
# Find plot with observed proportion of successes and 
#  model estimated number of successes

  library(nlme)
  place.small<-data.frame(good = place.s$good1, dist = place.s$dist)

  place.sum<-gsummary(object = place.small, FUN = sum, groups = place.small$dist) 
  place.length<-gsummary(object = place.small, FUN = length, groups = place.small$dist) 
  prop<-place.sum$good/place.length$good

  place.pattern<-data.frame(sum.y = place.sum$good, n = place.length$good, prop = prop, distance=place.sum$dist)
  head(place.pattern)

  #Find plot of the observed proportions 
  plot(x = place.pattern$distance, y = place.pattern$prop, xlab = "Distance (yards)", ylab = "Estimated probability", main = 
     "Estimated probability of success of a placekick \n with observed proportions", 
     panel.first=grid(col="gray", lty="dotted"))

  #Easiest way to get model on plot
  curve(expr = exp(mod.fit$coefficients[1]+mod.fit$coefficients[2]*x)/(1+exp(mod.fit$coefficients[1]+mod.fit$coefficients[2]*x)),
       col = "red", add = TRUE)
  #curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[2]*x), col = "red", add = TRUE)
 
 



#################################################################################
# Bubble plot version with bubble proportional to sample size

  #plots the plotting points
  symbols(x = place.pattern$distance, y = place.pattern$prop, circles = sqrt(place.pattern$n), inches = 1, 
        xlab = "Distance (yards)", ylab="Estimated probability",
        xlim = c(10,65), ylim = c(0, 1.5), main = "Estimated probability of success of a placekick \n with observed proportions",
        panel.first = grid(col = "gray", lty = "dotted"))
  #Puts the estimated logistic regression model on the plot
  curve(expr = exp(mod.fit$coefficients[1]+mod.fit$coefficients[2]*x)/(1+exp(mod.fit$coefficients[1]+mod.fit$coefficients[2]*x)),
       col = "red", add = TRUE)



###################################################################################
# Probit

  mod.fit.probit<-glm(formula = good1 ~ dist, data = place.s, family = binomial(link = probit), 
       na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
  summary(mod.fit.probit)

  #Estimated probability of success for a 20 yard field goal
  lin.pred<-mod.fit.probit$coefficients[1]+mod.fit.probit$coefficients[2]*20
  pnorm(q = lin.pred, mean = 0, sd = 1)

  #Estimated probability of success for a 50 yard field goal
  lin.pred<-mod.fit.probit$coefficients[1]+mod.fit.probit$coefficients[2]*50
  pnorm(q = lin.pred, mean = 0, sd = 1)


###################################################################################
#  Complementary log-log

  mod.fit.cloglog<-glm(formula = good1 ~ dist, data = place.s, family = binomial(link = cloglog), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
  summary(mod.fit)

  #Estimated probability of success for a 20 yard field goal
  lin.pred<-mod.fit.cloglog$coefficients[1]+mod.fit.cloglog$coefficients[2]*20
  1-exp(-exp(lin.pred))

  #Estimated probability of success for a 50 yard field goal
  lin.pred<-mod.fit.cloglog$coefficients[1]+mod.fit.cloglog$coefficients[2]*50
  1-exp(-exp(lin.pred))


  #easier way to find the estimated probabilities - note this used the cloglog model mod.fit above!
  predict.data<-data.frame(dist=20)
  predict(object = mod.fit.cloglog, newdata = predict.data, type = "response")

  #predict the linear predictor
  predict(object = mod.fit.cloglog, newdata = predict.data, type = "link")

  #Predict for 20 and 50 yards
  predict.data<-data.frame(dist = c(20, 50))
  save.pi.hat<-predict(object = mod.fit.cloglog, newdata = predict.data, type = "response")
  data.frame(predict.data, pi.hat = round(save.pi.hat,4))

  #Prediction with C.I.s
  predict.data<-data.frame(dist = c(20, 50))
  alpha<-0.05
  save.pi.hat<-predict(object = mod.fit.cloglog, newdata = predict.data, type = "response", se.fit = TRUE)
  lower<-save.pi.hat$fit-qnorm(1-alpha/2)*save.pi.hat$se
  upper<-save.pi.hat$fit+qnorm(1-alpha/2)*save.pi.hat$se

  data.frame(predict.data, pi.hat = round(save.pi.hat$fit,4), se = round(save.pi.hat$se,4), 
                         lower = round(lower,4), upper = round(upper,4))


#######################################################################################
# Plots of the estimated probability of success for all three models

  par(pty = "m") #plots over all of graph - not square
  plot(x = place.pattern$distance, y = place.pattern$prop, xlab="Distance (yards)", 
     ylab="Estimated probability", main = "Estimated probability of success of a placekick \n with observed proportions",
     panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[2]*x), col = "red", add = TRUE, lwd = 2, lty = 1)
  curve(expr = pnorm(mod.fit.probit$coefficients[1]+mod.fit.probit$coefficients[2]*x), col = "blue", add = TRUE, lty = 2, lwd = 2)
  curve(expr = 1-exp(-exp(mod.fit.cloglog$coefficients[1]+mod.fit.cloglog$coefficients[2]*x)), col = "green", add = TRUE, 
      lty = 4, lwd = 2)
  legend(locator(1), legend = c("Complementary log-log", "Logit", "Probit"), lty = c(4, 1, 2), lwd = c(2,2,2), 
       bty = "n", col=c("green", "red", "blue"), cex = 0.75)


  # Bubble plot version with bubble proportional to sample size
  symbols(x = place.pattern$distance, y = place.pattern$prop, circles=sqrt(place.pattern$n), xlab = "Distance (yards)", 
        ylab="Estimated probability", inches = 1,
        xlim = c(10,65), ylim = c(0, 1.2), main = "Estimated probability of success of a placekick \n with observed proportions",
        panel.first = grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted"))
  curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[2]*x), col = "red", add = TRUE, lwd = 2, lty = 1)
  curve(expr = pnorm(mod.fit.probit$coefficients[1]+mod.fit.probit$coefficients[2]*x), col = "blue", add = TRUE, lty = 2, lwd = 2)
  curve(expr = 1-exp(-exp(mod.fit.cloglog$coefficients[1]+mod.fit.cloglog$coefficients[2]*x)), col = "green", add = TRUE, 
      lty = 4, lwd = 2)
  legend(locator(1), legend = c("Complementary log-log", "Logit", "Probit"), lty = c(4, 1, 2), lwd = c(2,2,2), 
       bty = "n", col=c("green", "red", "blue"), cex = 0.75)





##############################################################
# Fit model again for the LRT later in Chapter 3 notes

  mod.fit<-glm(formula = good1 ~ dist, data = place.s, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
  names(mod.fit)

  #LRT: -2log(lambda)
  mod.fit$null.deviance - mod.fit$deviance

  #DF
  mod.fit$df.null-mod.fit$df.residual

  #p-value
  1 - pchisq(q = mod.fit$null.deviance - mod.fit$deviance, df = mod.fit$df.null-mod.fit$df.residual)


  #Show alpha_hat_0
  mod.fit<-glm(formula = good1 ~ 1, data = place.s, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T)) 
  mod.fit$coefficients

  #Predicted from model
  exp(mod.fit$coefficients)/(1+exp(mod.fit$coefficients))

  #Observed proportion of successes
  table(place.s$good1)[2]/(sum(table(place.s$good1)))










#
