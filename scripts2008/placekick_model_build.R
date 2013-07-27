#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  2-4-02                                                     #
# UPDATE: 1-10-03, 12-13-03 - R, 12-26-05, 1-1-08                   #
# Purpose: Go through whole model building process                  #
#                                                                   #
# NOTES:                                                            #
#####################################################################


placekick.mb<-read.table("C:\\chris\\UNL\\STAT875\\Chapter5\\placekick.mb.csv", header = TRUE, sep = ",")
placekick.mb[1:10,]

#####################################################################
# Step 1
#n<-1438  # no longer need 3-18-05
 
#Fit individual models
mod<-function(form, data) {
  mod.fit<-glm(formula = form, data = data, family = binomial(link = logit), 
               na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace=F))
  list(p = round(1-pchisq(mod.fit$null.deviance-mod.fit$deviance, mod.fit$df.null-mod.fit$df.residual),4),
       df = mod.fit$df.null-mod.fit$df.residual)
}


alt<-mod(form = good1~alt, data = placekick.mb)
change<-mod(good1~change, placekick.mb)
dist<-mod(good1~dist, placekick.mb)
type1<-mod(good1~type1, placekick.mb)
home<-mod(good1~home, placekick.mb)
pat1<-mod(good1~pat1, placekick.mb)
precip1<-mod(good1~precip1, placekick.mb)
field1<-mod(good1~field1, placekick.mb)
temp72<-mod(good1~temp72, placekick.mb)
elap30<-mod(good1~elap30, placekick.mb)
week<-mod(good1~week, placekick.mb)
wind<-mod(good1~wind, placekick.mb)

#Summarize results
data.frame(alt$p, change$p, dist$p, type1$p, home$p, pat1$p, precip1$p, field1$p, temp72$p, elap30$p, week$p, wind$p)
data.frame(alt$df, change$df, dist$df, type1$df, home$df, pat1$df, precip1$df, field1$df, temp72$df, elap30$df, week$df, wind$df)


#####################################################################
# Step 2

mod.fit<-glm(formula = good1~change+dist+type1+pat1+elap30+week+wind, 
        data = placekick.mb, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)


#Illustrate change test
Ho.mod<-glm(formula = good1~dist+type1+pat1+elap30+week+wind, data = placekick.mb, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace=F))
Ha.mod<-glm(formula = good1~change+dist+type1+pat1+elap30+week+wind, data = placekick.mb, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace=F))
cat("Gsq.o =", Ho.mod$deviance, "\n")
cat(" Gsq.a =", Ha.mod$deviance, "\n")
cat("Gsq.o-Gsq.a =", Ho.mod$deviance-Ha.mod$deviance, "\n")
cat("DF =", Ho.mod$df.residual - Ha.mod$df.residual, "\n")
cat("P-value =", 1-pchisq(Ho.mod$deviance - Ha.mod$deviance, Ho.mod$df.residual - Ha.mod$df.residual), "\n")


#Help with backward elimination
back<-function(Ho, Ha, data) {
  Ho.mod<-glm(formula = Ho, data = data, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace=F))
  Ha.mod<-glm(formula = Ha, data = data, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace=F))
  round(1-pchisq(Ho.mod$deviance - Ha.mod$deviance, Ho.mod$df.residual - Ha.mod$df.residual),4)
 }

Ha.model<-        good1~change+dist+type1+pat1+elap30+week+wind
change<-back(Ho = good1~       dist+type1+pat1+elap30+week+wind, 
             Ha = Ha.model, data = placekick.mb)
dist<-  back(good1~change     +type1+pat1+elap30+week+wind, Ha.model, placekick.mb)
type1<- back(good1~change+dist      +pat1+elap30+week+wind, Ha.model, placekick.mb)
pat1<-  back(good1~change+dist+type1     +elap30+week+wind, Ha.model, placekick.mb)
elap30<-back(good1~change+dist+type1+pat1       +week+wind, Ha.model, placekick.mb)
week<-  back(good1~change+dist+type1+pat1+elap30     +wind, Ha.model, placekick.mb)
wind<-  back(good1~change+dist+type1+pat1+elap30+week     , Ha.model, placekick.mb)
data.frame(change, dist, type1, pat1, elap30, week, wind)


#REMOVE elap30
Ha.model<-   good1~change+dist+type1+pat1+week+wind
change<-back(good1~       dist+type1+pat1+week+wind, Ha.model, placekick.mb)
dist<-  back(good1~change     +type1+pat1+week+wind, Ha.model, placekick.mb)
type1<- back(good1~change+dist      +pat1+week+wind, Ha.model, placekick.mb)
pat1<-  back(good1~change+dist+type1     +week+wind, Ha.model, placekick.mb)
week<-  back(good1~change+dist+type1+pat1+    +wind, Ha.model, placekick.mb)
wind<-  back(good1~change+dist+type1+pat1+week     , Ha.model, placekick.mb)
data.frame(change, dist, type1, pat1, week, wind)


#REMOVE type1
Ha.model<-   good1~change+dist+pat1+week+wind
change<-back(good1~       dist+pat1+week+wind, Ha.model, placekick.mb)
dist<-  back(good1~change     +pat1+week+wind, Ha.model, placekick.mb)
pat1<-  back(good1~change+dist     +week+wind, Ha.model, placekick.mb)
week<-  back(good1~change+dist+pat1+    +wind, Ha.model, placekick.mb)
wind<-  back(good1~change+dist+pat1+week     , Ha.model, placekick.mb)
data.frame(change, dist, pat1, week, wind)


#REMOVE week
Ha.model<-    good1~change+dist+pat1+wind
change<-back(good1~       dist+pat1+wind, Ha.model, placekick.mb)
dist<-  back(good1~change     +pat1+wind, Ha.model, placekick.mb)
pat1<-  back(good1~change+dist     +wind, Ha.model, placekick.mb)
wind<-  back(good1~change+dist+pat1     , Ha.model, placekick.mb)
data.frame(change, dist, pat1, wind)

mod.fit<-glm(formula = good1~change+dist+pat1+wind, 
        data = placekick.mb, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)


#####################################################################
# Step 3
 
 dist.sq<-back(good1~change+dist+      +pat1+wind, 
              good1~change+dist+I(dist^2)+pat1+wind, placekick.mb)
 cat("Distance^2 LRT p-value =", dist.sq)
 
 

#Check interaction terms
Ho.model<-good1~change+dist+pat1+wind
change.dist<-back(Ho.model, good1~change+dist+pat1+wind+change:dist, placekick.mb)
change.pat1<-back(Ho.model, good1~change+dist+pat1+wind+change:pat1, placekick.mb)
change.wind<-back(Ho.model, good1~change+dist+pat1+wind+change:wind, placekick.mb)
dist.pat1  <-back(Ho.model, good1~change+dist+pat1+wind+dist:pat1, placekick.mb)
dist.wind  <-back(Ho.model, good1~change+dist+pat1+wind+dist:wind, placekick.mb)
pat1.wind  <-back(Ho.model, good1~change+dist+pat1+wind+pat1:wind, placekick.mb)
data.frame(change.dist, change.pat1, change.wind, dist.pat1, dist.wind, pat1.wind)


#Perform backward elimination with the dist.pat1, dist.wind, and pat1.wind variables
dist.pat1  <-back(good1~change+dist+pat1+wind          +pat1:wind+dist:wind, 
                 good1~change+dist+pat1+wind+dist:pat1+pat1:wind+dist:wind, placekick.mb)
pat1.wind  <-back(good1~change+dist+pat1+wind+dist:pat1          +dist:wind, 
                 good1~change+dist+pat1+wind+dist:pat1+pat1:wind+dist:wind, placekick.mb)
dist.wind  <-back(good1~change+dist+pat1+wind+dist:pat1+pat1:wind          , 
                 good1~change+dist+pat1+wind+dist:pat1+pat1:wind+dist:wind, placekick.mb)
data.frame(dist.pat1, dist.wind, pat1.wind)

#Remove pat1.wind variable
dist.pat1  <-back(good1~change+dist+pat1+wind          +dist:wind, 
                 good1~change+dist+pat1+wind+dist:pat1+dist:wind, placekick.mb)
dist.wind  <-back(good1~change+dist+pat1+wind+dist:pat1          , 
                 good1~change+dist+pat1+wind+dist:pat1+dist:wind, placekick.mb)
data.frame(dist.pat1, dist.wind)

mod.fit<-glm(formula = good1~change+dist+pat1+wind+dist:pat1+dist:wind, 
        data = placekick.mb, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)



#################################################################
# Step 4
library(nlme)

placekick.mb.reduce<-data.frame(good1=placekick.mb$good1, 
   change=placekick.mb$change, dist=placekick.mb$dist, 
   pat1=placekick.mb$pat1, wind=placekick.mb$wind)

success<-gsummary(object = placekick.mb.reduce, FUN = sum, form = ~ good1 | dist / pat1 / change / wind)
total<-gsummary(object = placekick.mb.reduce, FUN = length, form= ~ good1 | dist / pat1 / change / wind)

place.pattern<-data.frame(success, total=total$good1)

#Check to make sure get same model as before
mod.fit<-glm(formula = good1/total ~ change+dist+pat1+wind+dist:pat1+dist:wind, data = place.pattern, 
        weight=total, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)




#################################################################
# Step 5 

diag<-examine.resid(mod.fit)
names(diag)


#Remove the 29 and 30 yard PATs from the data set
#Note use of logical operators - ! means "not", & means "and"
place.pattern.temp1<-place.pattern[!(place.pattern$dist==30 & place.pattern$pat1==1),]
sum(place.pattern.temp1$total)
place.pattern.temp2<-place.pattern.temp1[!(place.pattern.temp1$dist==29 & place.pattern.temp1$pat1==1),]
sum(place.pattern.temp2$total)


mod.fit<-glm(formula = good1/total ~ change+dist+pat1+wind+dist:pat1+dist:wind, data = place.pattern.temp2, 
        weight=total, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)




#Remove all not 20 yard placekicks from the data set
#Note use of logical operators - ! means "not", & means "and"
place.pattern.non20<-place.pattern[!(place.pattern$dist!=20 & place.pattern$pat1==1),]
sum(place.pattern.non20$total)
        


mod.fit<-glm(formula = good1/total ~ change+dist+pat1+wind+dist:wind, data = place.pattern.non20, 
        weight=total, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)
diag<-examine.resid(mod.fit)



#################################################################
# Step 6 

####
#Reproduce table 3 in Bilder and Loughin (1998)
theta.change<-exp(mod.fit$coefficients[2])
theta.pat1<-exp(mod.fit$coefficients[4])

theta.wind.dist20<-exp(mod.fit$coefficients[5]+mod.fit$coefficients[6]*20)
theta.wind.dist30<-exp(mod.fit$coefficients[5]+mod.fit$coefficients[6]*30)
theta.wind.dist40<-exp(mod.fit$coefficients[5]+mod.fit$coefficients[6]*40)
theta.wind.dist50<-exp(mod.fit$coefficients[5]+mod.fit$coefficients[6]*50)
theta.wind.dist60<-exp(mod.fit$coefficients[5]+mod.fit$coefficients[6]*60)

c<-10
theta.dist.wind1<-1/exp(c*mod.fit$coefficients[3]+c*mod.fit$coefficients[6]*1)
theta.dist.wind0<-1/exp(c*mod.fit$coefficients[3]+c*mod.fit$coefficients[6]*0)

#C.I. for betas and thetas
sum.mod.fit<-summary(mod.fit)
names(sum.mod.fit)

#Covariance matrix parameter estimates
sum.mod.fit$cov.unscaled




#C.I. function
OR.CI<-function(OR, alpha, var.log.OR) {
    lower<-log(OR) - qnorm(1-alpha/2)*sqrt(var.log.OR)
    upper<-log(OR) + qnorm(1-alpha/2)*sqrt(var.log.OR)
    list(OR = round(OR,4), lower = round(exp(lower),4), upper = round(exp(upper),4)) 
  }

change<-OR.CI(OR = exp(mod.fit$coefficients[2]), alpha = 0.10, var.log.OR = sum.mod.fit$cov.unscaled[2,2])
cat("The 90% C.I. for change OR is", round(change$lower,2), "to", round(change$upper,2), "\n")

pat<-OR.CI(OR = exp(mod.fit$coefficients[4]), alpha = 0.10, var.log.OR = sum.mod.fit$cov.unscaled[4,4])
cat("The 90% C.I. for PAT OR is", round(change$lower,2), "to", round(change$upper,2), "\n")

#Windy with distance=20
wind.dist20<-OR.CI(OR = exp(mod.fit$coefficients[5]+20*mod.fit$coefficients[6]), alpha = 0.10, 
                   var.log.OR = sum.mod.fit$cov.unscaled[5,5]+sum.mod.fit$cov.unscaled[6,6]*20^2+2*20*sum.mod.fit$cov.unscaled[5,6])
cat("The 90% C.I. for PAT OR is", round(wind.dist20$lower,2), "to", round(wind.dist20$upper,2), "\n")

#Do all at once - Is there an easy way to still use OR.CI here?
dist<-seq(20, 60, 10)
lower<-exp(mod.fit$coefficients[5]+dist*mod.fit$coefficients[6]
          -qnorm(1-alpha/2)*sqrt(sum.mod.fit$cov.unscaled[5,5]
          +sum.mod.fit$cov.unscaled[6,6]*dist^2+2*dist*sum.mod.fit$cov.unscaled[5,6]))
upper<-exp(mod.fit$coefficients[5]+dist*mod.fit$coefficients[6]
          +qnorm(1-alpha/2)*sqrt(sum.mod.fit$cov.unscaled[5,5]
          +sum.mod.fit$cov.unscaled[6,6]*dist^2+2*dist*sum.mod.fit$cov.unscaled[5,6]))
data.frame(dist=dist, lower=round(lower,2), upper=round(upper,2))


#distance for decrease c=10 and wind=1 - slightly different answer from paper
dist10.wind1<-OR.CI(OR = exp(10*mod.fit$coefficients[3]+10*mod.fit$coefficients[6]*1), alpha = 0.10, var.log.OR = 
              sum.mod.fit$cov.unscaled[3,3]*10^2+sum.mod.fit$cov.unscaled[6,6]*10^2+2*10*sum.mod.fit$cov.unscaled[3,6])
cat("The 90% C.I. for distance decrease of 10 and wind = 1 OR is", round(1/dist10.wind1$upper,2), "to", round(1/dist10.wind1$lower,2), "\n")

#distance for decrease c=10 and wind=0
dist10.wind1<-OR.CI(OR = exp(10*mod.fit$coefficients[3]), alpha = 0.10, var.log.OR = 
              sum.mod.fit$cov.unscaled[3,3]*10^2)
cat("The 90% C.I. for distance decrease of 10 and wind = 0 OR is", round(1/dist10.wind1$upper,2), "to", round(1/dist10.wind1$lower,2), "\n")




#Predict for Elliott's placekick
alpha<-0.1
elliott<-data.frame(change=1, dist=42, pat1=0, wind=0)
save.pi.hat<-predict(object = mod.fit, newdata = elliott, type = "response", se = TRUE)
lower<-save.pi.hat$fit-qnorm(1-alpha/2)*save.pi.hat$se
upper<-save.pi.hat$fit+qnorm(1-alpha/2)*save.pi.hat$se
data.frame(elliott, pi.hat = round(save.pi.hat$fit,4), lower = round(lower,4), upper = round(upper,4))

save.lp.hat<-predict(object = mod.fit, newdata = elliott, type = "link", se = TRUE)
lower.lp<-save.lp.hat$fit-qnorm(1-alpha/2)*save.lp.hat$se
upper.lp<-save.lp.hat$fit+qnorm(1-alpha/2)*save.lp.hat$se
lower<-exp(lower.lp)/(1+exp(lower.lp))
upper<-exp(upper.lp)/(1+exp(upper.lp))
data.frame(elliott, lp.hat = round(save.lp.hat$fit,4), 
           lower.lp = round(lower.lp,4), upper.lp = round(upper.lp,4),
           lower = round(lower,4), upper = round(upper,4))




################################################################################
# Plots

########
# Figure 1
par(mfrow=c(1,1))

#Find plot of the observed proportions - 
# I just used place.pattern.non20$good/place.pattern.non20$total to see what the 
# estimated proportions looked like.  I turned off the proportions from being
# plotted by using the type="n" command.  
plot(place.pattern.non20$dist, place.pattern.non20$good/place.pattern.non20$total,
     xlab="Distance in Yards", ylab="Estimated Probability of Success", type="n", 
     panel.first=grid(col = "gray", lty = "dotted"), main = 
     "Estimated probability of success of a field goal (PAT=0)")


#Put estimated logistic regression model on the plot - change=0, wind=0
#  This maybe could have been incorporated in the plot statement above.  
curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[3]*x), lwd=2, col = "red", add = TRUE)

#Put estimated logistic regression model on the plot - change=1, wind=0
curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[3]*x+mod.fit$coefficients[2]), lty=3, 
      lwd=2, col = "green", add = TRUE)

#Put estimated logistic regression model on the plot - change=0, wind=1
curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[3]*x+mod.fit$coefficients[5]+mod.fit$coefficients[6]*x),
      lty=4, lwd=2, col = "blue", add = TRUE)

#Put estimated logistic regression model on the plot - change=1, wind=1
curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[3]*x+mod.fit$coefficients[2]+mod.fit$coefficients[5]+mod.fit$coefficients[6]*x),
      lty=2, lwd=2, col = "purple", add = TRUE)

names1<-c("Change=0, Wind=0", "Change=1, Wind=0", "Change=0, Wind=1", "Change=1, Wind=1")
legend(locator(1), legend=names1, lty=c(1,3,4,2), col=c("red","green","blue","purple"), bty="n", cex=0.75, lwd=2)



####
#Figure 2

#Find plot of the observed proportions - 
# I just used place.pattern.non20$good/place.pattern.non20$total to see what the 
# estimated proportions looked like.  I turned off the proportions from being
# plotted by using the type="n" command.  
plot(place.pattern.non20$dist, place.pattern.non20$good/place.pattern.non20$total,
     xlab="Distance in Yards", ylab="Estimated Probability of Success", type = "n",
     panel.first=grid(col = "gray", lty = "dotted"), main = 
     "Estimated probability of success for a field goal (PAT=0)")


#Function for C.I.s - need in order to use with curve function
ci.pi<-function(newdata, mod.fit.obj, alpha){
  save.lp.hat<-predict(object = mod.fit.obj, newdata = newdata, type = "link", se = TRUE)
  lower.lp<-save.lp.hat$fit-qnorm(1-alpha/2)*save.lp.hat$se
  upper.lp<-save.lp.hat$fit+qnorm(1-alpha/2)*save.lp.hat$se
  lower.pi<-exp(lower.lp)/(1+exp(lower.lp)) #could also do plogis(lower.lp)
  upper.pi<-exp(upper.lp)/(1+exp(upper.lp))
  list(pi.hat = plogis(save.lp.hat$fit), lower = lower.pi, upper = upper.pi)  #Added the pi.hat to function from placekick_ch5.R
}

#Example
ci.pi(newdata = data.frame(change=0, dist=20:21, pat1=0, wind=0), mod.fit.obj = mod.fit, alpha = 0.05)

#Put lines on plot
curve(expr = ci.pi(newdata = data.frame(change=0, dist=x, pat1=0, wind=0), mod.fit.obj = mod.fit, alpha = 0.10)$pi.hat, 
      lty = 1, lwd = 2, col = "red", add = TRUE)
curve(expr = ci.pi(newdata = data.frame(change=0, dist=x, pat1=0, wind=0), mod.fit.obj = mod.fit, alpha = 0.10)$lower, 
      lty = 3, lwd = 2, col = "red", add = TRUE)
curve(expr = ci.pi(newdata = data.frame(change=0, dist=x, pat1=0, wind=0), mod.fit.obj = mod.fit, alpha = 0.10)$upper, 
      lty = 3, lwd = 2, col = "red", add = TRUE)


#Change=1 and Wind=1
curve(expr = ci.pi(newdata = data.frame(change=1, dist=x, pat1=0, wind=1), mod.fit.obj = mod.fit, alpha = 0.10)$pi.hat, 
      lty = 2, lwd = 2, col = "purple", add = TRUE)
curve(expr = ci.pi(newdata = data.frame(change=1, dist=x, pat1=0, wind=1), mod.fit.obj = mod.fit, alpha = 0.10)$lower, 
      lty = 3, lwd = 2, col = "purple", add = TRUE)
curve(expr = ci.pi(newdata = data.frame(change=1, dist=x, pat1=0, wind=1), mod.fit.obj = mod.fit, alpha = 0.10)$upper, 
      lty = 3, lwd = 2, col = "purple", add = TRUE)


text(25, 0.4, "Lowest Number of Risk Factors", cex=0.75)
names1<-c("Estimated Probability", "90% Confidence Interval") 
legend(17, 0.38, legend=names1, lty=c(1,3), col=c("red","red"), bty="n", lwd=2, cex=0.75)

text(25, 0.25, "Highest Number of Risk Factors", cex=0.75)
names1<-c("Estimated Probability", "90% Confidence Interval") 
legend(17, 0.23, legend=names1, lty=c(2,3), col=c("purple","purple"), bty="n", lwd=2, cex=0.75)



################################################################
#Extra:

#####
#Automated variable selection

mod.fit<-glm(formula = good1~change+dist+type1+pat1+elap30+week+wind, 
        data = placekick.mb, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = F))
summary(mod.fit)

res<-step(mod.fit, direction="backward", scope=list(upper=mod.fit$formula))
res$anova


#####
# automated influence measures!
library(boot)

mod.fit<-glm(formula = good1/total ~ change+dist+pat1+wind+dist*wind, data = place.pattern.non20, 
        weight=total, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
save.diag<-glm.diag(mod.fit)
names(save.diag)
glm.diag.plots(mod.fit, iden=TRUE)

#Other influence measures
save.influence<-influence.measures(mod.fit)
summary(save.influence)
 



###
