############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-27-06                                                           #
# PURPOSE: Multicollinearity example for Chapter 7                         #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#Simulate observations
set.seed(8171)  #set.seed(7110)  
X1<-seq(from = 1, to = 50, by = 1)
X2<-X1 + 0.1*rnorm(n = 50, mean = 0, sd = 1)
epsilon<-rnorm(n = 50, mean = 0, sd = 100)
Y<-2 + 3*X1 + 4*X2 + epsilon

set1<-data.frame(Y, X1, X2)
cor(x = set1, method = "pearson")
 
 
library(car)
scatterplot.matrix(formula = ~Y+X1+X2, data=set1, reg.line=lm, smooth=TRUE, span=0.5, diagonal = 'histogram')


mod.fit1<-lm(formula = Y ~ X1, data = set1)
summary(mod.fit1)$coefficients
mod.fit2<-lm(formula = Y ~ X2, data = set1)
summary(mod.fit2)$coefficients
mod.fit12<-lm(formula = Y ~ X1 + X2, data = set1)
summary(mod.fit12)$coefficients

#From Chapter 10
vif(mod.fit12)
