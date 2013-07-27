############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-23-06                                                           #
# PURPOSE: Chapter 4 with respect to the GPA data set                      #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#Read in the data
gpa<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter1\\gpa.txt", header=TRUE, sep = "")
head(gpa)


#Fit the simple linear regression model and save the results in mod.fit
mod.fit<-lm(formula = College.GPA ~ HS.GPA, data = gpa)
sum.fit<-summary(mod.fit)
sum.fit$coefficients
g<-2
alpha<-0.05
qt(p = 1-alpha/(2*g), df = mod.fit$df.residual)

#############################################################################
# C.I.s for beta0 and beta1 using Bonferroni procedure

  mod.fit$coefficients[1]-qt(p = c(1-alpha/(2*g),alpha/(2*g)), df = mod.fit$df.residual)*sum.fit$coefficients[1,2]
  mod.fit$coefficients[2]-qt(p = c(1-alpha/(2*g),alpha/(2*g)), df = mod.fit$df.residual)*sum.fit$coefficients[2,2]


  #Another way
  confint(object = mod.fit, level = 1 - alpha/g) 

#############################################################################
# C.I. for E(Y) and P.I. for Y

  more.gpa<-data.frame(HS.GPA = c(2, 2.5, 3, 3.5, 4))
  g<-nrow(more.gpa)
  round(predict(object = mod.fit, newdata = more.gpa, interval = "confidence", level = 1-alpha/g),2)
  round(predict(object = mod.fit, newdata = more.gpa, interval = "prediction", level = 1-alpha/g),2)














#
