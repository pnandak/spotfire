#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-9-02                                                     #
# UPDATE: 1-9-03, 12-13-03 for R, 12-31-07                          #
# PURPOSE: Find the logistic regression model for the placekick data#
#          with data in explanatory variable pattern form           #
#                                                                   #
# NOTES:                                                            #
#####################################################################


place.s<-read.table("C:\\chris\\UNL\\STAT875\\Chapter4\\place.s.csv", header = TRUE, sep = ",")


#Y is Bernoulli
mod.fit<-glm(formula = good1 ~ dist, data = place.s, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)

#another way to summarize with the LRT
anova(mod.fit)


pearson1<-resid(mod.fit, type="pearson")
plot(1:length(pearson1), pearson1, xlab="Observation number", ylab="Pearson residuals", 
     main = "Pearson residuals vs. observation number \n Y=Bernoulli")
abline(h = c(qnorm(0.975), qnorm(0.025)), lty = 3, col = "red")




#####################################################################
#Y is Binomial
library(nlme)

place.small<-data.frame(good=place.s$good1, dist=place.s$dist)


place.sum<-gsummary(object=place.small, FUN=sum, groups=place.small$dist) 
place.length<-gsummary(object=place.small, FUN=length, groups=place.small$dist) 

place.pattern<-data.frame(y=place.sum$good, fail=place.length$good-place.sum$good, n=place.length$good, distance=place.sum$dist)
place.pattern

mod.fit<-glm(formula = y/n ~ distance, data = place.pattern, weight=n, family = binomial(link = logit), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)
save<-examine.resid(mod.fit)



anova(mod.fit)

pearson<-resid(mod.fit, type="deviance")

#Adjusted pearson residual vs observation number plot
par(mfrow=c(1,1))
h<-lm.influence(mod.fit)$h 
pearson<-resid(mod.fit, type="pearson")
adj.pearson<-pearson/sqrt(1-h)
plot(place.pattern$distance, adj.pearson, xlab="Distance", ylab="Adj. Pearson residuals",
     main = "Adj. Pearson residuals vs. Distance")
abline(h = c(qnorm(0.975), qnorm(0.025)), lty = 3, col = "red")     


data.frame(place.pattern$distance, adj.pearson)

data.frame(place.pattern, pi.hat = round(mod.fit$fitted.values,2), adj.pearson = round(adj.pearson,2))





##################################################
#Investigate Pearson residuals


pred<-predict.glm(mod.fit, place.pattern, type = "response",  
        ci.fit = F, pi.fit = F) 
n.success<-pred*place.pattern$n

mod.fit$weights
sqrt(mod.fit$prior.weights*pred*(1-pred))

temp<-(place.pattern$y-pred*place.pattern$n)/sqrt(pred*place.pattern$n*(1-pred))
temp2<-resid(mod.fit, type="pearson")

temp[1:5]
temp2[1:5]

place.pattern[1:5,]

#21 yard placekicks Pearson resid
(19-0.967595*20)/sqrt(0.967595*20*(1-0.967595))
  
