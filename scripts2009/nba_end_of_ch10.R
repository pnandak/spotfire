############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  8-15-06                                                           #
# PURPOSE: End of Chapter 10 example with the NBA data                     #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

nba<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter6\\nba_data.txt", header=TRUE, sep = "")
head(nba)


############################################################################
# Step #2

  test.var<-function(Ho, Ha, data) {
    Ho.mod<-lm(formula = Ho, data = data)  
    Ha.mod<-lm(formula = Ha, data = data) 
    anova.fit<-anova(Ho.mod, Ha.mod)
    round(anova.fit$"Pr(>F)"[2], 5)
  }

  ################################################################
  # Forward
  
  Ho.model<-PPM ~ MPG + height + FGP + age  #NOTE: Had difficulty combining the Ha model extra variables with Ho.model
  MPG.height<-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + MPG:height, data = nba)
  MPG.FGP   <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + MPG:FGP   , data = nba)
  MPG.age   <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + MPG:age   , data = nba)
  height.FGP<-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + height:FGP, data = nba)
  height.age<-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + height:age, data = nba)
  FGP.age   <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + FGP:age, data = nba)
  MPG.sq    <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2), data = nba)
  age.sq    <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(age^2), data = nba) 
  data.frame(MPG.height, MPG.FGP, MPG.age, height.FGP, height.age, FGP.age, MPG.sq, age.sq)


  #ADDED MPG^2
  Ho.model<-PPM ~ MPG + height + FGP + age + I(MPG^2)
  MPG.height<-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:height, data = nba)
  MPG.FGP   <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:FGP   , data = nba)
  MPG.age   <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age   , data = nba)
  height.FGP<-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + height:FGP, data = nba)
  height.age<-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + height:age, data = nba)
  FGP.age   <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + FGP:age, data = nba)
  age.sq    <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + I(age^2), data = nba)
  data.frame(MPG.height, MPG.FGP, MPG.age, height.FGP, height.age, FGP.age, age.sq)


  #ADDED MPG:age
  Ho.model<-PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age
  MPG.height<-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + MPG:height, data = nba)
  MPG.FGP   <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + MPG:FGP   , data = nba)
  height.FGP<-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + height:FGP, data = nba)
  height.age<-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + height:age, data = nba)
  FGP.age   <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + FGP:age, data = nba)
  age.sq    <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + I(age^2), data = nba)
  data.frame(MPG.height, MPG.FGP, height.FGP, height.age, FGP.age, age.sq)


  #ADDED MPG:FGP - marginally significant (maybe should not add)
  Ho.model<-PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + MPG:FGP
  MPG.height<-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + MPG:FGP + MPG:height, data = nba)
  height.FGP<-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + MPG:FGP + height:FGP, data = nba)
  height.age<-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + MPG:FGP + height:age, data = nba)
  FGP.age   <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + MPG:FGP + FGP:age, data = nba)
  age.sq    <-test.var(Ho = Ho.model, Ha = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + MPG:FGP + I(age^2), data = nba)
  data.frame(MPG.height, height.FGP, height.age, FGP.age, age.sq)


  #Examine models
  mod.fit1<-lm(formula = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age, data = nba)
  summary(mod.fit1)

  mod.fit2<-lm(formula = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + MPG:FGP, data = nba)
  summary(mod.fit2)


  
  #Try using the step() function and AIC
  mod.fit.for<-lm(formula = PPM ~ MPG + height + FGP + age, data = nba) 
  step.both<-step(object = mod.fit.for, direction = "both", 
                 scope=list(lower = PPM ~ MPG + height + FGP + age, 
                            upper = PPM ~ MPG + height + FGP + age + MPG:height + MPG:FGP + MPG:age + height:FGP + 
                                          height:age + FGP:age + I(MPG^2) + I(age^2)), k = 2) 
  step.both$anova

 

############################################################################
# Step #3


  #Studentized and studentized deleted residuals
  r.i<-rstandard(model = mod.fit1)
  t.i<-rstudent(model = mod.fit1)
  r.i[1:5]
  t.i[1:5]
  r.i[abs(r.i)>qt(p = 1-0.01/2, df = mod.fit1$df.residual)]
  t.i[abs(t.i)>qt(p = 1-0.01/2, df = mod.fit1$df.residual-1)]
 
 
  save.it<-examine.mod.multiple.final(mod.fit.obj = mod.fit1, first.order = 4, const.var.test = TRUE, boxcox.find = TRUE)
  names(save.it)
  save.it$levene
  save.it$bp
  save.it$box.cox
  
  #Possible large |DFFITS|
  nba[c(1,7,21,24,37,52,53,72,73,99),]
  
  #Possible large |DFBETA|
  nba[c(1,7,10,14,21,24,33,37,46,47,48,52,53,59,72,73,95,97,98,99),]
 
 
  #What if #21 and/or #37 were removed from the data set?
  mod.fit1.wo21<-lm(formula = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age, data = nba[-21,])
  summary(mod.fit1.wo21)$coefficients
  round((mod.fit1.wo21$coefficients - mod.fit1$coefficients)/mod.fit1$coefficients,2)
 
  mod.fit1.wo37<-lm(formula = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age, data = nba[-37,])
  summary(mod.fit1.wo37)$coefficients
  round((mod.fit1.wo37$coefficients - mod.fit1$coefficients)/mod.fit1$coefficients,2)

  mod.fit1.wo21.37<-lm(formula = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age, data = nba[-c(21,37),])
  summary(mod.fit1.wo21.37)$coefficients
  round((mod.fit1.wo21.37$coefficients - mod.fit1$coefficients)/mod.fit1$coefficients,2)

 
 
#################################################################################
#What if limited the population to those players who played in at least 41 games?
  
  mod.fit.lim1<-lm(formula = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + MPG:FGP, data = nba[nba$games>40,])
  summary(mod.fit.lim1)

  #Go through forward stepwise selection
  mod.fit.lim2<-lm(formula = PPM ~ 1, data = nba[nba$games>40,]) 
  step.both<-step(object = mod.fit.lim2, direction = "both", 
                  scope=list(lower = PPM ~ 1, upper = PPM ~ MPG + height + FGP + age + FTP), k = 2)
  step.both$anova
  
  #Fit model with first order terms - FGP is marginally significant
  mod.fit.lim3<-lm(formula = PPM ~ MPG + height + FGP + FTP, data = nba[nba$games>40,])
  summary(mod.fit.lim3)
  save.it<-examine.mod.multiple.final(mod.fit.obj = mod.fit.lim3, first.order = 4, const.var.test = TRUE, boxcox.find = TRUE)
  
  #Investigate quadratic and interaction terms - kept age in the investigation due to past hypothesis
  #  Could also use plots to help determine what quadratic terms to investigate
  mod.fit.for<-lm(formula = PPM ~ MPG + height + FGP + FTP + age, data = nba) 
  step.both<-step(object = mod.fit.for, direction = "both", 
                 scope=list(lower = PPM ~ MPG + height + FGP + FTP + age, 
                            upper = PPM ~ (MPG + height + FGP + FTP + age)^2 + 
                                          I(MPG^2) + I(height^2) + I(FGP^2) + I(FTP^2) + I(age^2)), k = 2) 
  step.both$anova

  #Model suggested by AIC forward stepwise
  mod.fit.lim4<-lm(formula = PPM ~ MPG + height + FGP + FTP + age + I(MPG^2) + MPG:FTP + MPG:age + MPG:FGP, 
                   data = nba[nba$games>40,])
  summary(mod.fit.lim4)

  #MPG:Age interaction was not significant so try removing it
  mod.fit.lim5<-lm(formula = PPM ~ MPG + height + FGP + FTP + age + I(MPG^2) + MPG:FTP + MPG:FGP, 
                   data = nba[nba$games>40,])
  summary(mod.fit.lim5)

  #Age was not significant so try removing it
  mod.fit.lim6<-lm(formula = PPM ~ MPG + height + FGP + FTP + I(MPG^2) + MPG:FTP + MPG:FGP, 
                   data = nba[nba$games>40,])
  summary(mod.fit.lim6)
 
  #What if removed MPG:FGP since marginally significant?
  mod.fit.lim7<-lm(formula = PPM ~ MPG + height + FGP + FTP + I(MPG^2) + MPG:FTP, 
                   data = nba[nba$games>40,])
  summary(mod.fit.lim7)
 
  #What if removed I(MPG^2)?
  mod.fit.lim8<-lm(formula = PPM ~ MPG + height + FGP + FTP + MPG:FTP, 
                   data = nba[nba$games>40,])
  summary(mod.fit.lim8)
 
  #Overall, there are a lot of marginally significant variables.  I decided to play it safe and keep MPG:FGP and I(MPG^2).
  
  #Investigate diagnostic measures
  save.it<-examine.mod.multiple.final(mod.fit.obj = mod.fit.lim6, first.order = 4, const.var.test = TRUE, boxcox.find = TRUE)
  #45 is Michael Jordan - temp<-nba[nba$games>40,]; temp[45,]
  #R does not rename the rows so that's why Hornacek appears as nba[45,]
 
 
#################################################################################
#What if worked with the additional interaction term and the full data set?


 save.it<-examine.mod.multiple.final(mod.fit.obj = mod.fit2, first.order = 4, const.var.test = TRUE, boxcox.find = TRUE)

  mod.fit.temp<-lm(formula = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age + MPG:FGP, data = nba[-c(21,24),])
  summary(mod.fit.temp)$coefficients
  round((mod.fit.temp$coefficients - mod.fit2$coefficients)/mod.fit2$coefficients,2)

 
#################################################################################
#What if worked with square root transformation for Y?

  mod.fit1.sqrt<-lm(formula = sqrt(PPM) ~ MPG + height + FGP + age + I(MPG^2) + MPG:age, data = nba)
  summary(mod.fit1.sqrt)
  save.it<-examine.mod.multiple.final(mod.fit.obj = mod.fit1.sqrt, first.order = 4, const.var.test = TRUE, boxcox.find = TRUE)













#
