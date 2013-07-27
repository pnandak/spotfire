############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  8-5-06                                                            #
# PURPOSE: Chapter 10 example with the GPA data set                        #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#Read in the data
gpa<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter1\\gpa.txt", header=TRUE, sep = "")
head(gpa)

gpa2<-rbind(gpa, data.frame(HS.GPA = 4.35, College.GPA = 1.5))
#gpa2<-rbind(gpa, data.frame(HS.GPA = 4, College.GPA = 1.5))
#gpa2<-rbind(gpa, data.frame(HS.GPA = 4.9, College.GPA = 1.5))  #Need to change the x-axis to c(0,5)
row.names(gpa2)<-1:21

mod.fit<-lm(formula = College.GPA ~ HS.GPA, data = gpa2)
summary(mod.fit)
h.ii<-hatvalues(model = mod.fit)
h.ii

plot(x = gpa2$HS.GPA, y = gpa2$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA",
     panel.first = grid(col = "gray", lty = "dotted"), lwd = 2, col = "red", xlim = c(0, 4.5), ylim = c(0,4))
text(x = gpa2$HS.GPA, y = gpa2$College.GPA+0.1, labels = round(h.ii,2), cex = 0.75) 


############################################################################
# Section 10.3

  #Put regression lines on plot
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, 
        col = "darkblue", lty = "solid", lwd = 1, add = TRUE, from = min(gpa$HS.GPA), to = max(gpa$HS.GPA))
  mod.fit.wo<-lm(formula = College.GPA ~ HS.GPA, data = gpa)
  curve(expr = mod.fit.wo$coefficients[1] + mod.fit.wo$coefficients[2]*x, 
        col = "darkgreen", lty = "dashed", lwd = 1, add = TRUE, from = min(gpa$HS.GPA), to = max(gpa$HS.GPA))
  legend(locator(1), legend = c("Sample model w/ obs.", "Sample model w/o obs."), col = c("darkblue", "darkgreen"), 
         lty = c("solid", "dashed"), bty = "n", cex = 0.75)


############################################################################
# Section 10.4

  dffits.i<-dffits(model = mod.fit)
  dffits.i[abs(dffits.i)>1]
  n<-length(mod.fit$residuals)
  p<-length(mod.fit$coefficients)
  dffits.i[abs(dffits.i)>2*sqrt(p/n)]  
    
  cook.i<-cooks.distance(model = mod.fit)
  cook.i[cook.i>qf(p=0.5,df1=p, df2=mod.fit$df.residual)]

  #Be careful - dfbeta() (without the "s") finds something a little different
  dfbeta.all<-dfbetas(model = mod.fit)
  dfbeta.all[abs(dfbeta.all[,2])>1,2] #Do not need to look at beta0, only beta1
  dfbeta.all[abs(dfbeta.all[,2])>2/sqrt(n),2]
  round(dfbeta.all,2)

  predict(mod.fit, newdata = data.frame(HS.GPA = 4.35))
























#
