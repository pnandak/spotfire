#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  3-21-02                                                    #
# UPDATE: 1-10-03, 12-14-03 for R, 1-3-08                           #
# PURPOSE: Larry Bird loglinear model example                       #
#                                                                   #
# NOTES:                                                            #
#####################################################################

####################################################################
#Data formats

#Create contingency table - notice the data is entered by columns
n.table<-array(c(251, 48, 34, 5), dim=c(2,2), dimnames=list(First = c("made", "missed"), 
             Second = c("made", "missed")))
n.table

#Data needs to be in a data.frame format for glm()
counts<-c(251, 48, 34, 5)
First<-c("made", "missed", "made", "missed") 
Second<-c("made", "made", "missed", "missed")
bird.data.frame<-data.frame(First, Second, counts)
bird.data.frame

#Show how to convert back to a contingency table
bird.table<-xtabs(counts ~ First + Second, data = bird.data.frame)
bird.table

#Easier way to get the data in the correct form
bird.data.frame2<-as.data.frame(as.table(n.table))
bird.data.frame2

####################################################################
#Fit model under independence using glm()

mod.fit<-glm(formula = counts ~ First + Second, data = bird.data.frame, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)
model.matrix(mod.fit)
save.predict<-predict(object=mod.fit, type="response") #Also could use mod.fit$fitted.values
save.pearson<-residuals(object=mod.fit, type="pearson")
h<-lm.influence(model = mod.fit)$h  
standard.pearson<-save.pearson/sqrt(1-h)

save.all<-data.frame(bird.data.frame, predict = round(save.predict,4), pearson = round(save.pearson,4), 
   standard.pearson = round(standard.pearson,4))
save.all

xtabs(predict ~ First + Second, data = save.all)
xtabs(pearson ~ First + Second, data = save.all)
xtabs(standard.pearson ~ First + Second, data = save.all)

##################################################################
# Below is a wy to check how R is coding the indicator varibales for the categorical variables.

#Create dummy factor to see how coding is done
test.levels<-factor(c("a","b","c","d"))
contrasts(test.levels)
#options(contrasts=c("contr.treatment", "contr.poly"))
#options(contrasts=c("contr.sum", "contr.poly"))

#Sets the coding to the "set first level equal to 0" way - this is the default.  
options(contrasts=c("contr.treatment", "contr.poly"))

#Using glm() and use sum to 0
options(contrasts=c("contr.sum", "contr.poly")) 
mod.fit<-glm(formula = counts ~ First + Second, data = bird.data.frame, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)
model.matrix(mod.fit)
predict(mod.fit, type="response")





#Calculate goodness-of-fit measures
pearson.stat<-sum(save.pearson^2)
cat("X^2 =", round(pearson.stat, 4), "with p-value =", round(1-pchisq(pearson.stat, mod.fit$df.residual),4), "\n")

dev<-mod.fit$deviance
cat("G^2 =", round(dev, 4), "with p-value =", round(1-pchisq(dev, mod.fit$df.residual),4), "\n")


####################################################################
# Fit saturated model using glm()
options(contrasts=c("contr.treatment", "contr.poly")
#Remember that formula = counts ~ First*Second produces the same model
mod.fit<-glm(formula = counts ~ First + Second + First:Second, data = bird.data.frame, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)
model.matrix(mod.fit)
predict(mod.fit, type="response")


####################################################################
#Fit model under independence using loglin()


#Create contingency table - notice the data is entered by columns
n.table<-array(c(251, 48, 34, 5), dim=c(2,2), dimnames=list(First = c("made", "missed"), 
             Second = c("made", "missed")))
n.table
loglin(n.table, list("First", "Second"), param=T, fit=T)


#Different way to specify the model
mod.fit<-loglin(n.table, list(1, 2), param=T, fit=T)
names(mod.fit)
summary(mod.fit)
mod.fit$pearson

# Fit saturated model using loglin() with
#  loglin(n.table, list(c("First","Second")), param=T, fit=T)
# or
#  loglin(n.table, list(c(1,2)), param=T, fit=T)
