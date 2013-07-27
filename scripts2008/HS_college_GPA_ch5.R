############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-24-06                                                           #
# PURPOSE: HS and college GPA calculations for Chapter 5                   #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################


#Read in the data
gpa<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter1\\gpa.txt", header=TRUE, sep = "")
head(gpa)


######################################################################
#Section 5.3

  X<-cbind(1, gpa$HS.GPA)
  Y<-gpa$College.GPA

  X 
  Y

  t(X)%*%X
  t(X)%*%Y
  t(Y)%*%Y

  #Another way to get X matrix
  mod.fit<-lm(formula = College.GPA ~ HS.GPA, data = gpa)
  model.matrix(object = mod.fit)
  mod.fit$coefficients



######################################################################
#Section 5.6

  solve(t(X)%*%X)
  solve(t(X)%*%X) %*% t(X)%*%Y
  #Using the formulation of solve for x in Ax = b 
  solve(t(X)%*%X, t(X)%*%Y)

  #Another way to get (X'X)^(-1)
  sum.fit<-summary(mod.fit)
  sum.fit$cov.unscaled


######################################################################
#Section 5.12

  n<-length(Y)  #Can not use nrow() here since R does not know Y's dimension
                #Could use nrow(X) as well - X is a nx2 matrix
  b<-solve(t(X)%*%X) %*% t(X)%*%Y
  Y.hat<-X%*%b
  e<-Y-Y.hat
  H<-X%*%solve(t(X)%*%X)%*%t(X)

  J<-matrix(data = 1, nrow = n, ncol = n)  #Notice that R will repeat 1 the correct number of times to fill the matrix
  SSTO<-t(Y)%*%Y-1/n*t(Y)%*%J%*%Y
  SSE<-t(e)%*%e
  MSE<-SSE/(n-nrow(b))

  #Notice how MSE uses * to do the multiplying since it is a scalar
  #  Also diag(n) creates an identity matrix.  
  #  Diag(c(1,2)) would create a 2x2 diagonal matrix with elements 1 and 2 on the diagonal
  Cov.e<-MSE*(diag(n) - H) #Does not work!
  Cov.e<-as.numeric(MSE)*(diag(n) - H) #The as.numeric() removes 1x1 matrix meaning from MSE
  SSR = SSTO - SSE

  data.frame(X, Y, Y.hat, e)

  data.frame(n, SSTO, SSE, MSE, SSR)

  round(Cov.e[1:5, 1:5],6) #5x5 part of Cov(e) 


  #From past work
  data.frame(X = model.matrix(mod.fit), Y.hat = mod.fit$fitted.values, e = mod.fit$residuals)
  anova(mod.fit)



######################################################################
#Section 5.13

  cov.beta.hat<-as.numeric(MSE)*solve(t(X)%*%X)
  cov.beta.hat
  
  #From earlier 
  sum.fit<-summary(mod.fit)
  sum.fit$cov.unscaled #Another way to get (X'X)^(-1)
  sum.fit$sigma^2 * sum.fit$cov.unscaled
  
  #One more way to get the covariance matrix
  vcov(mod.fit)
  
  #Find Var(Y^)
  X.h<-c(1,1.88)
  as.numeric(MSE)*X.h%*%solve(t(X)%*%X)%*%X.h
  
  #Find Var(Y-Y^)
  as.numeric(MSE)* (1+X.h%*%solve(t(X)%*%X)%*%X.h)


  #From HS_college_GPA_ch2.R
  n<-nrow(gpa)
  X.h<-1.88
  Y.hat.h<-as.numeric(mod.fit$coefficients[1] + mod.fit$coefficients[2]*X.h) #as.numeric just removes an unneeded name
  ssx<-var(gpa$HS.GPA)*(n-1)  #SUM( (X_i - X_bar)^2 )
  X.bar<-mean(gpa$HS.GPA)
  MSE * (1/n + (X.h - X.bar)^2 / ssx)  #Taken from the C.I. formula
  MSE * (1 + 1/n + (X.h - X.bar)^2 / ssx)  #Taken from the P.I. formula
  



#
