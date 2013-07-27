########################################################################
# NAME:  Chris Bilder                                                  #
# DATE:  3-14-06                                                       #
# UPDATE:                                                              #
# PURPOSE: Section 11.6.2 example                                      #
#                                                                      #
# NOTES:                                                               #
########################################################################

########################################################################
# Read in the data and fit the model

  library(boot)

  mammals<-read.table(file = "C:\\chris\\UNL\\STAT_boot\\chapter6\\mammals.dat", header = TRUE)
  head(mammals)
  n<-nrow(mammals)

  mod.fit<-lm(log(brain) ~ log(body), data = mammals)
  sum.fit<-summary(mod.fit)
  sum.fit
  
  #Predictions
  #  R automatically finds log(body)
  newdata<-data.frame(body = c(52.16, 10.55))  
  pred.newdata<-predict(object = mod.fit, newdata = newdata, se.fit = TRUE, interval = "prediction", level = 0.95) 
  pred.newdata

  #Verify upper bound and other calculations for x_+ = 52.16
  se<-sum.fit$sigma*sqrt((1 + 1/n + (log(52.16) - mean(log(mammals$body)))^2/sum((log(mammals$body) - mean(log(mammals$body)))^2)))
  pred.newdata$fit[1,1]+qt(0.975, mod.fit$df.residual)*se
  pred.newdata$se.fit[1]
  sum.fit$sigma*sqrt((1/n + (log(52.16) - mean(log(mammals$body)))^2/sum((log(mammals$body) - mean(log(mammals$body)))^2)))
  

  #Get h_j
  influence.stat<-lm.influence(mod.fit)
  h.j<-influence.stat$hat
  
  #Modified residuals
  r.j<-mod.fit$residuals/sqrt(1-h.j)
  mean(r.j)



########################################################################
# Bootstrap

  #Notice the use of i.pred here - boot() will pass in a vector of indices of length m
  #  I put ".star" on some items here to make it clear what they represented for a resample
  #  so be careful with trying to understand this function!!!
  calc.t.modbased<-function(data, i, i.pred, mu.hat, body, newdata, y.hat) {
    epsilon.star<-data[i]
    epsilon.plus.star<-data[i.pred]
    
    y.star<-mu.hat + epsilon.star
    mod.fit.star<-lm(y.star ~ log(body)) 
    pred.newdata.star<-predict(object = mod.fit.star, newdata = newdata, se.fit = TRUE) 
    
    #Calculate standard error needed for P.I.
    s.sq.yhat.star<-pred.newdata.star$se.fit^2  #Var^(Y^_+)
    sum.fit.star<-summary(mod.fit.star)
    s.sq.star<-sum.fit.star$sigma^2 #Var^(Y_+)
    #s.sq.star<-sum(mod.fit.star$residuals^2)/(mod.fit.star$df.residual)  
    s.yhat.minus.y.star<-sqrt(s.sq.star + s.sq.yhat.star) #sqrt( Var^(Y_+ - Y^_+) )
  
    delta<-pred.newdata.star$fit - (y.hat + epsilon.plus.star)
    c(delta, s.yhat.minus.y.star)
  }

  #Try it 
  calc.t.modbased(data = r.j - mean(r.j), i = 1:n, i.pred = 46:47, mu.hat = mod.fit$fitted.values, body = mammals$body,
                  newdata = newdata, y.hat = pred.newdata$fit[,1])
  
  set.seed(2387)
  boot.res.modbased<-boot(data = r.j - mean(r.j), statistic = calc.t.modbased, R = 999, sim = "ordinary", m = 2, 
                          mu.hat = mod.fit$fitted.values, body = mammals$body, newdata = newdata, 
                          y.hat = pred.newdata$fit[,1])
  boot.res.modbased
 
 
 
########################################################################
# Intervals

 #Work for the Basic
  delta.quant<-apply(X = boot.res.modbased$t[,1:2], MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975), type = 1)
  delta.quant

  #For x_+ = 52.16
  log.int<-as.numeric(pred.newdata$fit[1,1]-rev(delta.quant[,1]))
  log.int
  exp(log.int)

  #For x_+ = 10.55
  log.int<-as.numeric(pred.newdata$fit[2,1]-rev(delta.quant[,2]))
  log.int
  exp(log.int)


  #Work for the Studentized
  z.star1<-boot.res.modbased$t[,1]/boot.res.modbased$t[,3]
  z.star1.quant<-quantile(x = z.star1, probs = c(0.025, 0.975), type = 1)
  lower<-pred.newdata$fit[1,1] - sqrt(sum.fit$sigma + pred.newdata$se[1]^2)*z.star1.quant[2]
  upper<-pred.newdata$fit[1,1] - sqrt(sum.fit$sigma + pred.newdata$se[1]^2)*z.star1.quant[1]
  log.scale1<-data.frame(predicted = pred.newdata$fit[1,1], lower = lower, upper = upper)
  log.scale1 
  
  z.star2<-boot.res.modbased$t[,2]/boot.res.modbased$t[,4]
  z.star2.quant<-quantile(x = z.star2, probs = c(0.025, 0.975), type = 1)
  lower<-pred.newdata$fit[2,1] - sqrt(sum.fit$sigma + pred.newdata$se[2]^2)*z.star2.quant[2]
  upper<-pred.newdata$fit[2,1] - sqrt(sum.fit$sigma + pred.newdata$se[2]^2)*z.star2.quant[1]
  log.scale2<-data.frame(predicted = pred.newdata$fit[2,1], lower = lower, upper = upper)
  log.scale2
 
  #On the correct scale
  exp(rbind(log.scale1, log.scale2))

  #Could boot.ci be used?  My unsuccesful try: 
  #boot.ci(boot.out = boot.res.modbased, conf = 0.95, type = "stud", var.t0 = sum.fit$sigma + pred.newdata$se[2]^2, 
  #      var.t = boot.res.modbased$t[,3], t=boot.res.modbased$t[,1], t0 = boot.res.modbased$t0[,1] )

  #Compare quantiles for z* with x_+ = 52.16
  qt(p = c(0.025, 0.975), df = mod.fit$df.residual)
  z.star1.quant
  
  
  #Plots for z* with x_+ = 52.16
  par(mfrow = c(1,2), pty = "s") 
  
  hist(z.star1, main = expression(paste("Histogram of ", z^{"*"}, " for ", x["+"], " = 52.16")), xlab=expression(paste(z^{"*"})), 
       freq = FALSE) 
  curve(expr = dnorm(x, mean = mean(z.star1), sd = sd(z.star1)), col = "red", add = TRUE)
  curve(expr = dt(x, df = mod.fit$df.residual), col = "darkblue", add = TRUE, lty = "dashed") #t with df = 60 which is what we would normally use
  legend(locator(1), legend = c(expression(paste("N(mean(", z^{"*"},"), var(", z^{"*"}, "))")), "t(60)"), lty = c("solid", "dashed"), 
         col = c("red", "darkblue"), cex = 0.6, bty = "n")

  #EDF
  plot.ecdf(z.star1, verticals = TRUE, do.p = FALSE, main = expression(paste("EDF of ", z^{"*"}, " for ", x["+"], " = 52.16")),
            lwd = 2, panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), 
            ylab = expression(hat(G)), xlab = expression(paste(z^{"*"})))
  curve(expr = pnorm(x, mean(z.star1), sd = sd(z.star1)), col = "red", add = TRUE)
  curve(expr = pt(x, df = mod.fit$df.residual), col = "darkblue", add = TRUE, lty = "dashed") #t with df = 60 which is what we would normally use





##################################################################
# BMA code need for this section

  mam.lm <- glm(log(brain)~log(body),data=mammals)  #Could also use lm()
  mam.diag <- glm.diag(mam.lm) #Works similar to lm.influence(), but provides a few extra items (in boot package)
  
  #mam.diag$res provides the jackknife deviance residuals for a GLM
  #  Here, these are just r.j/sqrt(1-h.j)
  #  mam.diag$sd = 1 so it has no effect
  res <- (mam.diag$res-mean(mam.diag$res))*mam.diag$sd
  mam <- data.frame(mammals, res=res, fit=fitted(mam.lm))

  d.pred <- mam[c(46,47),]
  pred <- function( data, d.pred ) {
    predict( glm(log(brain)~log(body), data=data), d.pred )
  }

  #Try it
  pred(data = mam, d.pred = d.pred)


  #i.pred = just another set of indices to pass in like i itself 
  mam.pred <- function( data, i, i.pred,  d.pred )
  { d <- data
    d$brain <- exp( d$fit+d$res[i] )
    pred( d, d.pred) - (d.pred$fit + d$res[i.pred] ) 
  }
  
  #Try it
  mam.pred(data = mam, i = 1:62, i.pred=c(46,47), d.pred = d.pred) 
  
    
  set.seed(1829) 
  #mam.boot.pred <- boot( mam, mam.pred, R=199, m=2, d.pred=d.pred )  #BMA code
  mam.boot.pred <- boot( mam, mam.pred, R=999, m=2, d.pred=d.pred )   #I increased R
  names(mam.boot.pred)  #new part returned is pred.i - shows the indices used in the m = 1, 2 resamples
  head(mam.boot.pred$pred.i)


  orig <- matrix(pred( mam, d.pred ), nrow = mam.boot.pred$R, ncol = 2, byrow=T)
  #exp(apply(orig+mam.boot.pred$t, 2, quantile, c(0.025,0.975) ))  #BMA have this (+ sign is an error)
  exp(apply(orig - mam.boot.pred$t, 2, quantile, c(0.025,0.975) ))  #Corrected (see p. 285 bottom)

  #On log-scale
  apply(orig - mam.boot.pred$t, 2, quantile, c(0.025,0.975) )





#
