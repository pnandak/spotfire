############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-27-06                                                           #
# PURPOSE: NBA data example for Chapter 7                                  #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

nba<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter6\\nba_data.txt", header=TRUE, sep = "")
head(nba)


###########################################################################
# Section 7.2

  mod.fit.comp<-lm(formula = PPM ~ MPG + height + FTP + FGP, data = nba)
  anova.comp<-anova(object = mod.fit.comp)
  anova.comp #p-values are incorrect (except for last)
  
  names(anova.comp)
  summary(mod.fit.comp)
  
  mod.fit.red<-lm(formula = PPM ~ MPG + height, data = nba)
  anova.red<-anova(mod.fit.red)
  anova.red
 
 
  #Partial F test
  p<-length(mod.fit.comp$coefficients)  #Number of betas in model is p
  g<-length(mod.fit.red$coefficients)-1 #Number of variables remaining in the reduced model is g
  F.star<-(anova.red$"Sum Sq"[3] - anova.comp$"Sum Sq"[5])/(p-1-g) / anova.comp$"Mean Sq"[5]
  F.star
  alpha<-0.05
  qf(p = 1-alpha, df1 = p-1-g, df2 = mod.fit.comp$df.residual)
  1 - pf(q = F.star, df1 = p-1-g, df2 = mod.fit.comp$df.residual)

  #Easier way to do partial F test :)
  anova(mod.fit.red, mod.fit.comp, test = "F")  #test = "F" is default
 
  #Shows how to get just F statistic out
  anova.compare<-anova(mod.fit.red, mod.fit.comp, test = "F")  #test = "F" is default
  names(anova.compare)
  anova.compare$F
  anova.compare$F[2]
  anova.compare$Pr(>F) #Does not work
  anova.compare$"Pr(>F)" #Can always put quotes around list component
  anova.compare$"Pr(>F)"[2]

  ################################################### 
  #Test Ho: E(Y) = beta0 + beta1*MPG
  #     Ha: E(Y) = beta0 + beta1*MPG + beta2*height
  mod.fit.temp<-lm(formula = PPM ~ MPG + height, data = nba)
  anova.temp<-anova(mod.fit.temp)
  anova.temp



  
  #Using Anova function to reproduce the p-values in summary(mod.fit.comp)
  Anova(mod = mod.fit.comp)
  Anova(mod = mod.fit.comp, type = "II") #Type II is default
  Anova(mod = mod.fit.comp, type = "III")



###########################################################################
# Section 7.4

  mod.fit.comp1<-lm(formula = PPM ~ MPG + height + FTP + FGP, data = nba)
  anova.comp1<-anova(mod.fit.comp1)
  mod.fit.comp2<-lm(formula = PPM ~ MPG + height + FTP + age, data = nba)
  anova.comp2<-anova(mod.fit.comp2)
  mod.fit.red<-lm(formula = PPM ~ MPG + height + FTP, data = nba)
  anova.red<-anova(mod.fit.red)
  
  ssr.4.123<-anova.comp1$"Sum Sq"[4]
  ssr.5.123<-anova.comp2$"Sum Sq"[4]
  sse<-anova.red$"Sum Sq"[4]
  
  anova.comp1$"Sum Sq"[4]/sse  #Rsq.4.123
  anova.comp2$"Sum Sq"[4]/sse  #Rsq.5.123
  
  

##########################################################################
# Section 7.5

  #columns 1:3 are the name of the player and the number of games played
  round(cor(x = nba[,-(1:3)], method = "pearson"),2)
  
  
  #Investigate what happens to the b's when one variable is added at a time
  mod.fit1<-lm(formula = PPM ~ MPG                           , data = nba)
  summary(mod.fit1)$coefficients
  mod.fit2<-lm(formula = PPM ~ MPG + height                  , data = nba)
  summary(mod.fit2)$coefficients
  mod.fit3<-lm(formula = PPM ~ MPG + height + FTP            , data = nba)
  summary(mod.fit3)$coefficients
  mod.fit4<-lm(formula = PPM ~ MPG + height + FTP + FGP      , data = nba)
  summary(mod.fit4)$coefficients
  mod.fit5<-lm(formula = PPM ~ MPG + height + FTP + FGP + age, data = nba)
  summary(mod.fit5)$coefficients




#
