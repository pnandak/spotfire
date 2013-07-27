#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-27-07                                                   #
# UPDATE:                                                           #
# Purpose: Investigate qualitative variables with Berry FG data     #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#Read in data set
berry.reduce.new<-read.table(file = "C:\\chris\\UNL\\STAT875\\Chapter4\\berry.reduce.new.csv", 
  header = TRUE, sep = ",")
head(berry.reduce.new)   

mod.fit<-glm(formula = Good1 ~ Weather.new, data = 
     berry.reduce.new, family = binomial(link = logit), 
     na.action = na.exclude, control = list(epsilon = 
     0.0001, maxit = 50, trace = T))
summary(mod.fit)
contrasts(berry.reduce.new$Weather.new)


#This is one way you can change the ordering of the levels
levels(berry.reduce.new$Weather.new)
berry.reduce.new2<-data.frame(berry.reduce.new, Weather.new2 = relevel(berry.reduce.new$Weather.new, ref = "2.Inside"))
mod.fit<-glm(formula = Good1 ~ Weather.new2, data = 
     berry.reduce.new2, family = binomial(link = logit), 
     na.action = na.exclude, control = list(epsilon = 
     0.0001, maxit = 50, trace = T))
summary(mod.fit)


############################################################################
#Show how to get R to treat a quantitative variable as qualitative.

  mod.fit.quant<-glm(formula = Good1 ~ Length, data = 
     berry.reduce.new2, family = binomial(link = logit), 
     na.action = na.exclude, control = list(epsilon = 
     0.0001, maxit = 50, trace = T))
  summary(mod.fit.quant)


  mod.fit.qualt<-glm(formula = Good1 ~ factor(Length), data = 
     berry.reduce.new2, family = binomial(link = logit), 
     na.action = na.exclude, control = list(epsilon = 
     0.0001, maxit = 50, trace = T))
  summary(mod.fit.qualt)










#If the newly created variables are not dummy variables, you can set them to be this way using the following command.  





#options(contrasts=c("contr.treatment", "contr.poly"))

NewVar <- relevel( factor(OldVar), ref = "b")
should create a dummy variable, and change the reference category for the model.

Reza
