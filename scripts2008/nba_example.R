############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-25-06                                                           #
# PURPOSE: NBA data example for Chapter 6                                  #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

nba<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter6\\nba_data.txt", header=TRUE, sep = "")
head(nba)


###########################################################################
# Chapter 6 example section

  mod.fit<-lm(formula = PPM ~ MPG + height + FTP, data = nba)
  sum.fit<-summary(mod.fit)
  sum.fit
  sum.fit$coefficients
  
  jordan.stockton<-nba[nba$last.name == "Jordan" | nba$last.name == "Stockton",]
  jordan.stockton

  save.pred<-predict(object = mod.fit, newdata = jordan.stockton)
  #Summary - notice how elementwise multiplication is used for two vectors
  data.frame(Name = jordan.stockton$last.name, PPM = jordan.stockton$PPM, 
            PPG = jordan.stockton$PPM*jordan.stockton$MPG,
            PPM.hat =save.pred, PPG.hat = save.pred*jordan.stockton$MPG)
       
  #see what could happen with rounding error
  cbind(1, jordan.stockton$MPG, jordan.stockton$height, jordan.stockton$FTP)%*%round(mod.fit$coefficients,4)



###########################################################################
# Section 6.5

  alpha<-0.05

  sum.fit$fstatistic
  qf(p = 1-alpha, df1 = sum.fit$fstatistic[2], df2 = sum.fit$fstatistic[3])
  1-pf(q = sum.fit$fstatistic[1], df1 = sum.fit$fstatistic[2], df2 = sum.fit$fstatistic[3])
  
  #Just another way
  qf(p = 1-alpha, df1 =length(mod.fit$coefficients)-1, df2 = mod.fit$df.residual)
  
  #ANOVA table part
  anova.fit<-anova(mod.fit)
  anova.fit
  names(anova.fit)
  sum(temp$"Sum Sq"[1:3]) #SSR

  #Examine the R^2 values for different models
  mod.fit1<-lm(formula = PPM ~ MPG, data = nba)
  sum.fit1<-summary(mod.fit1)
  mod.fit2<-lm(formula = PPM ~ MPG + height, data = nba)
  sum.fit2<-summary(mod.fit2)
  R.sq.values<-data.frame(model = c("MPG", "MPG, height", "MPG, height, FTP"),
                          R.sq = c(sum.fit1$r.squared, sum.fit2$r.squared, sum.fit$r.squared),
                          adj.R.sq = c(sum.fit1$adj.r.squared, sum.fit2$adj.r.squared, sum.fit$adj.r.squared))
  R.sq.values
  
  mod.fit3<-lm(formula = PPM ~ MPG + height + FTP, data = nba)


###########################################################################
# Section 6.7

  miller<-nba[nba$last.name == "Miller",]
  save.ci<-predict(object = mod.fit, newdata = miller, se.fit = TRUE, interval = "confidence", level = 0.95)
  save.ci
  save.pi<-predict(object = mod.fit, newdata = miller, interval = "prediction", level = 0.95)
  save.pi
  
  #PPG
  save.ci$fit*miller$MPG
  save.pi*miller$MPG


###########################################################################
# Section 6.8

  save.it<-examine.mod.multiple(mod.fit.obj = mod.fit, const.var.test = TRUE, boxcox.find = TRUE)
  save.it

  pairs(~age+FGP+FTP+height+MPG+PPM, data = nba)
   
  
  library(car)
  scatterplot.matrix(formula = ~age+FGP+FTP+height+MPG+PPM, data=nba, reg.line=lm, smooth=TRUE, span=0.5, diagonal = 'histogram')

  #Allows for brushing points
  library(TeachingDemos)
  tkBrush(mat = nba[,3:8], hscale=1.75, vscale=1.75)


  #Correlation matrix
  round(cor(x = nba[,4:8], method = "pearson"),4)
   
  #Only 2 variables at a time
  cor.test(formula = ~ age+FGP, data=nba, method = "pearson")
  

#
