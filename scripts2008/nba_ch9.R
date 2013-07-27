############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  8-2-06                                                            #
# PURPOSE: NBA data example for Chapter 9                                  #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

nba<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter6\\nba_data.txt", header=TRUE, sep = "")
head(nba)


############################################################################
# My own function used to find the model evaluation measures

  fit.stat<-function(model, data, MSE.all) {
    mod.fit<-lm(formula = model, data = data)
    sum.fit<-summary(mod.fit)
    aic.mod<-AIC(object = mod.fit, k = 2)
    bic.mod<-AIC(object = mod.fit, k = log(length(mod.fit$residuals)))  
    sse<-anova(mod.fit)$"Sum Sq"[length(anova(mod.fit)$"Sum Sq")]
    p<-length(mod.fit$coefficients)
    n<-length(mod.fit$residuals)
    Cp<-sse/MSE.all - (n-2*p)
    press<-sum(mod.fit$residuals^2/(1 - lm.influence(mod.fit)$hat)^2)
    data.frame(Rsq = sum.fit$r.squared, AdjRsq = sum.fit$adj.r.squared, 
               AIC.stat = aic.mod, BIC.stat = bic.mod, PRESS = press, Cp = Cp)
  }

  mod.fit<-lm(formula = PPM ~ MPG + height + FTP + FGP + age, data = nba)
  sum.fit<-summary(mod.fit)
  MSE.all<-sum.fit$sigma^2

  #5 variable model
  var5<-cbind(model = 1, fit.stat(model = PPM ~ MPG + height + FTP + FGP + age, data = nba, MSE.all = MSE.all))  
  var5
  
  #4 variable model
  mod4.1<-fit.stat(model = PPM ~ MPG + height + FTP + FGP      , data = nba, MSE.all = MSE.all)
  mod4.2<-fit.stat(model = PPM ~ MPG + height + FTP +       age, data = nba, MSE.all = MSE.all)
  mod4.3<-fit.stat(model = PPM ~ MPG + height +       FGP + age, data = nba, MSE.all = MSE.all)
  mod4.4<-fit.stat(model = PPM ~ MPG +          FTP + FGP + age, data = nba, MSE.all = MSE.all)
  mod4.5<-fit.stat(model = PPM ~       height + FTP + FGP + age, data = nba, MSE.all = MSE.all)  
  var4<-cbind(model = 1:5, rbind(mod4.1, mod4.2, mod4.3, mod4.4, mod4.5))
  round(var4[rev(order(var4$Rsq)),],2)

  #3 variable model
  mod3.1<-fit.stat(model = PPM ~ MPG + height + FTP            , data = nba, MSE.all = MSE.all)
  mod3.2<-fit.stat(model = PPM ~ MPG + height +             age, data = nba, MSE.all = MSE.all)
  mod3.3<-fit.stat(model = PPM ~ MPG + height +       FGP      , data = nba, MSE.all = MSE.all)
  mod3.4<-fit.stat(model = PPM ~ MPG +          FTP + FGP      , data = nba, MSE.all = MSE.all)
  mod3.5<-fit.stat(model = PPM ~ MPG +          FTP +       age, data = nba, MSE.all = MSE.all)  
  mod3.6<-fit.stat(model = PPM ~ MPG +                FGP + age, data = nba, MSE.all = MSE.all)
  mod3.7<-fit.stat(model = PPM ~       height + FTP + FGP      , data = nba, MSE.all = MSE.all)
  mod3.8<-fit.stat(model = PPM ~       height + FTP +       age, data = nba, MSE.all = MSE.all)
  mod3.9<-fit.stat(model = PPM ~       height +       FGP + age, data = nba, MSE.all = MSE.all)
  mod3.10<-fit.stat(model =PPM ~                FTP + FGP + age, data = nba, MSE.all = MSE.all)
  var3<-cbind(model = 1:10, rbind(mod3.1, mod3.2, mod3.3, mod3.4, mod3.5, mod3.6, mod3.7, mod3.8, mod3.9, mod3.10))
  round(var3[rev(order(var3$Rsq)),],2)
  
  #2 variable model
  mod2.1<-fit.stat(model = PPM ~ MPG + height                  , data = nba, MSE.all = MSE.all)
  mod2.2<-fit.stat(model = PPM ~ MPG +                      age, data = nba, MSE.all = MSE.all)
  mod2.3<-fit.stat(model = PPM ~ MPG +                FGP      , data = nba, MSE.all = MSE.all)
  mod2.4<-fit.stat(model = PPM ~ MPG +          FTP            , data = nba, MSE.all = MSE.all)
  mod2.5<-fit.stat(model = PPM ~       height + FTP            , data = nba, MSE.all = MSE.all)  
  mod2.6<-fit.stat(model = PPM ~       height +       FGP      , data = nba, MSE.all = MSE.all)
  mod2.7<-fit.stat(model = PPM ~       height +             age, data = nba, MSE.all = MSE.all)
  mod2.8<-fit.stat(model = PPM ~                FTP +       age, data = nba, MSE.all = MSE.all)
  mod2.9<-fit.stat(model = PPM ~                FTP + FGP      , data = nba, MSE.all = MSE.all)
  mod2.10<-fit.stat(model =PPM ~                      FGP + age, data = nba, MSE.all = MSE.all)
  var2<-cbind(model = 1:10, rbind(mod2.1, mod2.2, mod2.3, mod2.4, mod2.5, mod2.6, mod2.7, mod2.8, mod2.9, mod2.10))
  round(var2[rev(order(var2$Rsq)),],2)

  #1 variable model
  mod1.1<-fit.stat(model = PPM ~ MPG, data = nba, MSE.all = MSE.all)
  mod1.2<-fit.stat(model = PPM ~ height, data = nba, MSE.all = MSE.all)
  mod1.3<-fit.stat(model = PPM ~ FTP, data = nba, MSE.all = MSE.all)
  mod1.4<-fit.stat(model = PPM ~ FGP, data = nba, MSE.all = MSE.all)
  mod1.5<-fit.stat(model = PPM ~ age, data = nba, MSE.all = MSE.all)  
  var1<-cbind(model = 1:5, rbind(mod1.1, mod1.2, mod1.3, mod1.4, mod1.5), deparse.level = 0)
  round(var1[rev(order(var1$Rsq)),],2)

 
  plot(x = rep(1, times = 5), y = var1$Rsq, ylim = c(0,1), xlim = c(1,5), type = "n", xlab = "Number of variables",
       ylab = expression(R^2), main = expression(paste(R^2, " vs. number of variables")))
  text(x = rep(1, times = 5),  y = var1$Rsq, labels = var1$model, cex = 0.75)
  text(x = rep(2, times = 10), y = var2$Rsq, labels = var2$model, cex = 0.75)
  text(x = rep(3, times = 10), y = var3$Rsq, labels = var3$model, cex = 0.75)
  text(x = rep(4, times = 5),  y = var4$Rsq, labels = var4$model, cex = 0.75)
  text(x = 5                ,  y = var5$Rsq, labels = var5$model, cex = 0.75)



#############################################################################
# Using car and leaps package

  library(leaps)
  select.var<-regsubsets(x = PPM ~ MPG + height + FTP + FGP + age, data = nba, method = "exhaustive", nbest=4)
  select.sum<-summary(select.var, matrix.logical=TRUE)
  select.sum
  names(select.sum)
  select.sum$rsq
  select.sum$outmat
  
  #Example plots
  plot(select.var, scale="r2") #From leaps package - how to interpret?
  library(car)
  win.graph(width = 6, height = 6, pointsize = 10)
  subsets(object = select.var, statistic = "bic", legend = TRUE, cex.subsets = 0.75)

  
  par(mfrow = c(2,2))
  subsets(object = select.var, statistic="rsq", legend = FALSE, cex.subsets = 0.75)
  subsets(object = select.var, statistic="adjr2", legend = FALSE, cex.subsets = 0.75)
  subsets(object = select.var, statistic="cp", legend = FALSE, cex.subsets = 0.75)
  subsets(object = select.var, statistic="adjr2", legend = FALSE, cex.subsets = 0.75)
  par(mfrow = c(1,1))
  subsets(object = select.var, statistic="bic", legend = FALSE, cex.subsets = 0.75)
  #NOTE: cex.subsets option only changes the font for the plotted characters, not the legend 
  

  #regsubsets() actually calls a form of the leaps() function to do its calculations
  leaps(x=nba[,5:9], y=nba[,4], method="Cp", nbest=10) 
  leaps(x=nba[,5:9], y=nba[,4], method="adjr2", nbest=2) 



############################################################################
# Using the step function

  #Need to fit a model first - choose the model corresponding to the lower bound
  mod.fit.for<-lm(formula = PPM ~ 1, data = nba) #Model with just intercept

  step.for<-step(object = mod.fit.for, direction = "forward", scope=list(lower = PPM ~ 1, upper = PPM ~ MPG + height + FTP + FGP + age),
                 k = 2)  #use k = log(n) for BIC)
  step.for  #Returns b's
  #Summary
  step.for$anova


  #Need to fit a model first - choose the model corresponding to the upper bound
  mod.fit.back<-lm(formula = PPM ~ MPG + height + FTP + FGP + age, data = nba) 
  step.back<-step(object = mod.fit.back, direction = "backward", scope=list(lower = PPM ~ 1, upper = PPM ~ MPG + height + FTP + FGP + age),
                 k = 2)  
  step.back$anova


  #Need to fit a model first - choose the model corresponding to the lower bound
  mod.fit.for<-lm(formula = PPM ~ 1, data = nba) 
  step.for<-step(object = mod.fit.for, direction = "both", scope=list(lower = PPM ~ 1, upper = PPM ~ MPG + height + FTP + FGP + age),
                 k = 2, trace = 0)  
  step.for$anova
  #Can use trace = 0 to remove some of the information being printed with step()

  #Notes:
  # 1) .^2' gets interactions 
  # 2) The stepAIC() function in the MASS package will provide similar results.
  # 3) Cp = AIC + constant for some models



  #Example using stepAIC
  library(MASS)
  mod.fit2<-lm(formula = PPM ~ 1, data = nba)  
  #Problem is that it will add squared terms without the main-effects (first-order terms) - same problem occurs with step()
  temp<-stepAIC(object = mod.fit2, scope = list(upper = ~ (MPG + height + FTP + FGP + age)^2 + I(MPG^2) + I(height^2) + I(FTP^2) 
              + I(FGP^2) + I(age^2), lower = ~1), direction = "forward", k = 2) #use k = log(n) for BIC)
  names(temp)
  temp$anova

  #Will not add an interacton without both main effects
  stepAIC(object = mod.fit2, scope = list(upper = ~ (MPG + height + FTP + FGP + age)^2 , lower = ~1), direction = "forward")



############################################################################
# Using hypothesis tests to do forward, backward, and stepwise

  test.var<-function(Ho, Ha, data) {
    Ho.mod<-lm(formula = Ho, data = data)  
    Ha.mod<-lm(formula = Ha, data = data) 
    anova.fit<-anova(Ho.mod, Ha.mod)
    round(anova.fit$"Pr(>F)"[2], 4)
  }


  ################################################################
  # Forward
  
  MPG<-   test.var(Ho = PPM ~ 1, Ha = PPM ~ MPG, data = nba)
  height<-test.var(Ho = PPM ~ 1, Ha = PPM ~ height, data = nba)
  FTP<-   test.var(Ho = PPM ~ 1, Ha = PPM ~ FTP, data = nba)
  FGP<-   test.var(Ho = PPM ~ 1, Ha = PPM ~ FGP, data = nba)
  age<-   test.var(Ho = PPM ~ 1, Ha = PPM ~ age, data = nba)
  data.frame(MPG, height, FTP, FGP, age)
  
  #ADDED FGP
  MPG<-   test.var(Ho = PPM ~ FGP, Ha = PPM ~ FGP + MPG, data = nba)
  height<-test.var(Ho = PPM ~ FGP, Ha = PPM ~ FGP + height, data = nba)
  FTP<-   test.var(Ho = PPM ~ FGP, Ha = PPM ~ FGP + FTP, data = nba)
  age<-   test.var(Ho = PPM ~ FGP, Ha = PPM ~ FGP + age, data = nba)
  data.frame(MPG, height, FTP, age)
  
  #ADDED height
  MPG<-   test.var(Ho = PPM ~ FGP + height, Ha = PPM ~ FGP + height + MPG, data = nba)
  FTP<-   test.var(Ho = PPM ~ FGP + height, Ha = PPM ~ FGP + height + FTP, data = nba)
  age<-   test.var(Ho = PPM ~ FGP + height, Ha = PPM ~ FGP + height + age, data = nba)
  data.frame(MPG, FTP, age)
  
  #ADDED MPG
  FTP<-   test.var(Ho = PPM ~ FGP + height + MPG, Ha = PPM ~ FGP + height + MPG + FTP, data = nba)
  age<-   test.var(Ho = PPM ~ FGP + height + MPG, Ha = PPM ~ FGP + height + MPG + age, data = nba)
  data.frame(FTP, age)
  
  #ADDED age
  FTP<-   test.var(Ho = PPM ~ FGP + height + MPG + age, Ha = PPM ~ FGP + height + MPG + age + FTP, data = nba)
  FTP
 
 
 
  ################################################################
  # Backward
  MPG<-   test.var(Ho = PPM ~       height + FTP + FGP + age, Ha = PPM ~ MPG + height + FTP + FGP + age, data = nba)
  height<-test.var(Ho = PPM ~ MPG +          FTP + FGP + age, Ha = PPM ~ MPG + height + FTP + FGP + age, data = nba)
  FTP<-   test.var(Ho = PPM ~ MPG + height +       FGP + age, Ha = PPM ~ MPG + height + FTP + FGP + age, data = nba)
  FGP<-   test.var(Ho = PPM ~ MPG + height + FTP +       age, Ha = PPM ~ MPG + height + FTP + FGP + age, data = nba)
  age<-   test.var(Ho = PPM ~ MPG + height + FTP + FGP      , Ha = PPM ~ MPG + height + FTP + FGP + age, data = nba)
  data.frame(MPG, height, FTP, FGP, age)

  #REMOVED FTP
  MPG<-   test.var(Ho = PPM ~       height + FGP + age, Ha = PPM ~ MPG + height + FGP + age, data = nba)
  height<-test.var(Ho = PPM ~ MPG +          FGP + age, Ha = PPM ~ MPG + height + FGP + age, data = nba)
  FGP<-   test.var(Ho = PPM ~ MPG + height +       age, Ha = PPM ~ MPG + height + FGP + age, data = nba)
  age<-   test.var(Ho = PPM ~ MPG + height + FGP      , Ha = PPM ~ MPG + height + FGP + age, data = nba)
  data.frame(MPG, height, FGP, age)
 
  
  
  
  
  
  
  
  
  
  
  
  #
  
  
  
  
  
 round(anova.fit$"Pr(>F)"[2], 4)


#
