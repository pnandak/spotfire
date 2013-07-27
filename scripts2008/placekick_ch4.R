#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-11-02                                                    #
# UPDATE: 12-12-03, 12-24-05, 12-7-06                               #
# Purpose: Find the logistic regression model for the placekick data#
#                                                                   #
# NOTES:                                                            #
#####################################################################


place.s<-read.table(file = "C:\\chris\\UNL\\STAT875\\Chapter4\\place.s.csv", header = TRUE, sep = ",")

#Distance model
mod.fit<-glm(formula = good1 ~ dist, data = place.s, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)

#Find OR for c=1
theta1<-exp(mod.fit$coefficients[2])
cat("The estimated odds of a successful placekick are", round(theta1,4), "times larger for every 1 yard increase in distance \n")
#Invert
cat("The estimated odds of a successful placekick are", round(1/theta1,4), "times larger for every 1 yard decrease in distance \n")



#Find OR for c=10
theta10<-exp(mod.fit$coefficients[2]*10)
cat("The estimated odds of a successful placekick are", round(theta10,4), "times larger for every 10 yard increase in distance \n")
#Invert
cat("The estimated odds of a successful placekick are", round(1/theta10,4), "times larger for every 10 yard decrease in distance \n")


#Lethal distance
pi.vec<-c(0.25, 0.5, 0.75)
LD<-(log(pi.vec/(1-pi.vec))-mod.fit$coefficients[1])/mod.fit$coefficients[2]
LD.summary<-data.frame(pi.vec, LD)
LD.summary


#From the MASS package:
library(MASS)
save.LD<-dose.p(obj = mod.fit, p = pi.vec) #Be careful with cf option, will work here (see help)
save.LD
attributes(save.LD)  #Older way to view same thing that names(save.LK) would normally
#How can you pull the predicted dose level out?  May need to rewrite function


###########################################################################################
# C.I. for beta and theta
sum.mod.fit<-summary(mod.fit)
names(sum.mod.fit)

#Covariance matrix of alpha^ and beta^
sum.mod.fit$cov.unscaled

#C.I. for beta
alpha<-0.05
lower<-mod.fit$coefficients[2]-qnorm(1-alpha/2)*sqrt(sum.mod.fit$cov.unscaled[2,2])
upper<-mod.fit$coefficients[2]+qnorm(1-alpha/2)*sqrt(sum.mod.fit$cov.unscaled[2,2])
#Print in nice frame
data.frame(beta.hat=mod.fit$coefficients[2], lower, upper)

#C.I. for OR with c=10
alpha<-0.05
lower<-exp(10*mod.fit$coefficients[2]-qnorm(1-alpha/2)*10*sqrt(sum.mod.fit$cov.unscaled[2,2]))
upper<-exp(10*mod.fit$coefficients[2]+qnorm(1-alpha/2)*10*sqrt(sum.mod.fit$cov.unscaled[2,2]))
#Print in nice frame
data.frame(theta.hat=exp(10*mod.fit$coefficients[2]), lower, upper)

#Invert
data.frame(theta.hat.invert=1/exp(10*mod.fit$coefficients[2]), 
           lower=1/upper, upper=1/lower)

#C.I. function
OR.CI<-function(OR, alpha, var.log.OR) {
    lower<-log(OR) - qnorm(1-alpha/2)*sqrt(var.log.OR)
    upper<-log(OR) + qnorm(1-alpha/2)*sqrt(var.log.OR)
    list(OR = round(OR,4), lower = round(exp(lower),4), upper = round(exp(upper),4)) 
  }


dist10<-OR.CI(OR = exp(10*mod.fit$coefficients[2]), alpha = 0.05, var.log.OR = 10^2 * sum.mod.fit$cov.unscaled[2,2])
names(dist10)
data.frame(name = "theta-hat invert", OR = 1/dist10$OR, lower = 1/dist10$upper, upper = 1/dist10$lower)



#####################################################################################
# Profile likelihood C.I.s

library(MASS)
profile.ci<-confint(object = mod.fit, parm = "dist", level = 0.95)
profile.ci
exp(profile.ci)
exp(10*profile.ci)


###########################################################################################
# C.I.s for pi


#Example using the predict function and finding the C.I.s
obs<-c(20,50)
predict.data<-data.frame(dist=obs)

alpha<-0.05
#Confidence interval again - better way - guarantee >=0 and <=1
save.lp.hat<-predict(object = mod.fit, newdata = predict.data, type = "link", se = TRUE)
save.lp.hat
lower.lp<-save.lp.hat$fit-qnorm(1-alpha/2)*save.lp.hat$se
upper.lp<-save.lp.hat$fit+qnorm(1-alpha/2)*save.lp.hat$se
lower<-exp(lower.lp)/(1+exp(lower.lp))
upper<-exp(upper.lp)/(1+exp(upper.lp))
data.frame(predict.data, lp.hat = round(save.lp.hat$fit,4), 
           lower.lp = round(lower.lp,4), upper.lp = round(upper.lp,4), 
           pi.hat = exp(save.lp.hat$fit)/(1+exp(save.lp.hat$fit)),
           lower = round(lower,4), upper = round(upper,4))

#Confidence interval again - not as good method - NOT guarantee >=0 and <=1
save.pi.hat<-predict(object = mod.fit, newdata = predict.data, type = "response", se = TRUE)
save.pi.hat
lower<-save.pi.hat$fit-qnorm(1-alpha/2)*save.pi.hat$se
upper<-save.pi.hat$fit+qnorm(1-alpha/2)*save.pi.hat$se
data.frame(predict.data, pi.hat = round(save.pi.hat$fit,4), lower = round(lower,4), upper = round(upper,4))





################################################################################################
#Show how the C.I. was created
sum.mod.fit<-summary(mod.fit)
names(sum.mod.fit)

#Covariance matrix of alpha^ and beta^
sum.mod.fit$cov.unscaled

#Find var(alpha^ + beta^ * x)
var.lin.pred<-sum.mod.fit$cov.unscaled[1,1]+obs^2*sum.mod.fit$cov.unscaled[2,2]+
             2*obs*sum.mod.fit$cov.unscaled[1,2]
#SE
sqrt(var.lin.pred)

#Linear predictor
lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*obs

#C.I.
alpha<-0.05
upper<-exp(lin.pred+qnorm(1-alpha/2)*sqrt(var.lin.pred)) / 
   (1+exp(lin.pred+qnorm(1-alpha/2)*sqrt(var.lin.pred)))
lower<-exp(lin.pred-qnorm(1-alpha/2)*sqrt(var.lin.pred)) / 
   (1+exp(lin.pred-qnorm(1-alpha/2)*sqrt(var.lin.pred)))
pi.hat<-exp(lin.pred)/(1+exp(lin.pred))

#Print C.I. and pi.hat in a nice form
data.frame(pi.hat, lower, upper)




######################################################################################################
# C.I. plot

library(nlme)

place.small<-data.frame(good = place.s$good1, dist = place.s$dist)

place.sum<-gsummary(object = place.small, FUN = sum, groups = place.small$dist) 
place.length<-gsummary(object = place.small, FUN = length, groups = place.small$dist) 
prop<-place.sum$good/place.length$good

place.pattern<-data.frame(y = place.sum$good, n = place.length$good, prop = prop, distance=place.sum$dist)
place.pattern[1:5,]


#Find plot of the observed proportions 
plot(place.pattern$distance, place.pattern$prop, xlab="Distance (yards)", ylab="Estimated probability", main = 
     "Estimated probability of success of a placekick \n with observed proportions", 
     panel.first = grid(col = "gray", lty = "dotted"))

#Put estimated logistic regression model on the plot
curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[2]*x), col = "red", add = TRUE)


#Function for C.I.s - need in order to use with curve function
ci.pi<-function(newdata, mod.fit.obj, alpha){
  save.lp.hat<-predict(object = mod.fit.obj, newdata = newdata, type = "link", se = TRUE)
  lower.lp<-save.lp.hat$fit-qnorm(1-alpha/2)*save.lp.hat$se
  upper.lp<-save.lp.hat$fit+qnorm(1-alpha/2)*save.lp.hat$se
  lower.pi<-exp(lower.lp)/(1+exp(lower.lp)) #could also do plogis(lower.lp)
  upper.pi<-exp(upper.lp)/(1+exp(upper.lp))
  list(lower = lower.pi, upper = upper.pi)
}

ci.pi(newdata = data.frame(dist = 20), mod.fit.obj = mod.fit, alpha = 0.05)

curve(expr = ci.pi(newdata = data.frame(dist = x), mod.fit.obj = mod.fit, alpha = 0.05)$lower, col = "blue", 
      lty = 4, add = TRUE)
curve(expr = ci.pi(newdata = data.frame(dist = x), mod.fit.obj = mod.fit, alpha = 0.05)$upper, col = "blue", 
      lty = 4, add = TRUE)



#OLD way to put the estimated logistic regression model on plot and C.I.s
#distance<-18:66
#lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*distance
#pi.hat<-exp(lin.pred)/(1+exp(lin.pred))
#lines(distance, pi.hat, lty=1, col="red")

#Need data in a data.frame for predict to work
#distance.df<-data.frame(dist=distance)

#save.lp.hat<-predict(object = mod.fit, newdata = distance.df, type = "link", se = TRUE)
#lower.lp<-save.lp.hat$fit-qnorm(1-alpha/2)*save.lp.hat$se
#upper.lp<-save.lp.hat$fit+qnorm(1-alpha/2)*save.lp.hat$se
#lower<-exp(lower.lp)/(1+exp(lower.lp))
#upper<-exp(upper.lp)/(1+exp(upper.lp))

#Draw confidence interval lines on plot
#lines(distance, lower, lty=4, col="blue")
#lines(distance, upper, lty=4, col="blue")


#Legend
names1<-c("Estimated probability", "95% individual C.I.")
legend(locator(1), legend=names1, lty=c(1,4), col=c("red","blue"), bty="n")

######################################################################################################
# LD plot

#Find plot of the observed proportions 
plot(x = place.pattern$distance, y = place.pattern$prop, xlab="Distance (yards)", ylab="Estimated probability", main = 
     "Estimated probability of success of a placekick \n with observed proportions", 
     panel.first = grid(col = "gray", lty = "dotted"))

#Put estimated logistic regression model on the plot
curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[2]*x), col = "red", add = TRUE)

#Put LK lines on plot
segments(x0 = LD.summary$LD, y0 = -0.1, x1 = LD.summary$LD, y1 = LD.summary$pi.vec, lty=2, col=c("darkblue","darkred","darkgreen"))
segments(x0 = 15, y0 = LD.summary$pi.vec, x1 = LD.summary$LD, y1 = LD.summary$pi.vec, lty=2, col=c("darkblue","darkred","darkgreen"))
legend(locator(1), legend=c("LD25", "LD50", "LD75"), lty=2, col=c("darkblue","darkred","darkgreen"), bty="n", cex=0.75)











###############################################################################################
###############################################################################################
#Change model
mod.fit<-glm(formula = good1 ~ change, data = place.s, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)

#Find OR for c=1
theta1<-exp(mod.fit$coefficients[2])
cat("The estimated odds of a successful placekick are", round(theta1,4), "times larger for a lead-change placekick instead of a non-lead change placekick")

#Invert
cat("The estimated odds of a successful placekick are", round(1/theta1,4), "times larger for a non-lead-change placekick instead of a lead-change placekick")


#summary table
sum.table<-table(place.s$good1, place.s$change)
sum.table
summary(sum.table)

sum.table[1,1]*sum.table[2,2]/(sum.table[1,2]*sum.table[2,1])


#Find estimated probability of success
lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*1
exp(lin.pred)/(1+exp(lin.pred))

#Find estimated probability of success
lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*0
exp(lin.pred)/(1+exp(lin.pred))




#####################################################################################################
#Y is Binomial - use distance as explanatory variable
library(nlme)

place.small<-data.frame(good = place.s$good1, dist = place.s$dist)

place.sum<-gsummary(object = place.small, FUN = sum, groups = place.small$dist) 
place.length<-gsummary(object = place.small, FUN = length, groups = place.small$dist) 
prop<-place.sum$good/place.length$good

place.pattern<-data.frame(y = place.sum$good, n = place.length$good, prop = prop, distance=place.sum$dist)
place.pattern[1:5,]


mod.fit<-glm(formula = y/n ~ distance, data = place.pattern, weight=n, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)


#Find plot of the observed proportions 
plot(place.pattern$distance, place.pattern$prop, xlab="Distance (yards)", ylab="Estimated probability", main = 
     "Estimated probability of success of a placekick \n with observed proportions")

#Put estimated logistic regression model on the plot
distance<-18:66
lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*distance
pi.hat<-exp(lin.pred)/(1+exp(lin.pred))
lines(distance, pi.hat, lty=1, col="red")

#Put LK lines on plot
segments(x0 = LK.summary$LK, y0 = -0.1, x1 = LK.summary$LK, y1 = LK.summary$pi.vec, 
         lty=2, col=c("darkblue","darkred","darkgreen"))
segments(x0 = 15, y0 = LK.summary$pi.vec, x1 = LK.summary$LK, y1 = LK.summary$pi.vec, 
         lty=2, col=c("darkblue","darkred","darkgreen"))
names1<-c("LK25", "LK50", "LK75")
legend(locator(1), legend=names1, lty=2, col=c("darkblue","darkred","darkgreen"), bty="n", cex=0.75)










#####################################################################

#LRT: -2log(lambda)
mod.fit$null.deviance-mod.fit$deviance

n<-nrow(place.s)
#p-value
1-pchisq(mod.fit$null.deviance-mod.fit$deviance, n-1-mod.fit$df.residual)



#Estimated probability of success for a 20 yard field goal
lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*20
exp(lin.pred)/(1+exp(lin.pred))

#Estimated probability of success for a 50 yard field goal
lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*50
exp(lin.pred)/(1+exp(lin.pred))


#Find Pearson residuals
pearson<-resid(mod.fit, type="pearson")
#The call below will also work  
#pearson<-residuals.glm(mod.fit, type="pearson")


plot(1:1425, pearson, ylab="Pearson residual", xlab="Obs. Number") 
title("Pearson residual vs. observation number")

#Simple plot
plot(place.s$dist, mod.fit$fitted.values, xlab="Distance (yards)", 
     ylab="Estimated probability")
title("Estimated probability of success of a placekick")
 

#######################################################
# Find plot with observed proportion of successes and 
#  model estimated number of successes - SEE PREVIOUS BETTER WAYS TO DO THIS!

#Summary of the placekicks by distance
dist.good<-table(place.s$dist, place.s$good1)
dist.good

#Find total number of placekicks at each distance
tot<-rowSums(dist.good)

#Proportion of successes at each distance
prop<-dist.good[,2]/tot

plot(names(tot), prop, xlab="Distance (yards)", 
     ylab="Estimated probability")
title("Estimated probability of success of a placekick \n with observed proportions")


distance<-18:66
lin.pred<-mod.fit$coefficients[1]+mod.fit$coefficients[2]*distance
pi.hat<-exp(lin.pred)/(1+exp(lin.pred))
lines(distance, pi.hat, lty=1, col=8)

#save for later
logistic.pi<-pi.hat

######
# Bubble plot version with bubble proportional to sample size
#   The type="n" option says to creating the plot without anything in the plot.  
plot(names(tot), prop, type="n")

#Need to do this because symbols() can not change the characters to 
# numeric values
num.dist<-c(18:56, 59, 62:63, 66)

#plots the plotting points
symbols(num.dist, prop, circles=sqrt(tot))

#Puts the estimated logistic regression model on the plot
lines(distance, pi.hat, lty=1, col=8)

title("Estimated probability of success of a placekick \n with observed proportions",
     xlab="Distance (yards)", ylab="Estimated probability")


#
######################################################################
######################################################################
## Explanatory variable pattern form analysis

new.placekick<-data.frame(success=dist.good[,2], total=tot, distance=num.dist)
new.placekick

mod.fit<-glm(formula = success/total ~ distance, data = new.placekick, weight=total,
            family = binomial(link = logit), na.action = na.exclude, 
            control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)


#Still can not fit the probit model
mod.fit<-glm(formula = success/total ~ distance, data = new.placekick, weight=total,
            family = binomial(link = probit), na.action = na.exclude, 
            control = list(epsilon = 0.0001, maxit = 50, trace = T))
