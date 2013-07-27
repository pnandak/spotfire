########################################################################
# NAME:  Chris Bilder                                                  #
# DATE:  3-6-06                                                        #
# UPDATE:                                                              #
# PURPOSE: Example 6.3                                                 #
#                                                                      #
# NOTES:                                                               #
########################################################################

########################################################################
# Get the data and do some initial examinations

  library(boot)
  
  head(acme) 
  n<-nrow(acme)
  
  par(mfrow = c(1,2))
  plot(x = acme$market, y = acme$acme, xlab = "x (market return)", ylab = "y (acme return)", 
       main = "acme return vs. market return", panel.first = grid(col="gray", lty="dotted"))
  abline(lm(acme ~ market, data = acme), col = "red")
    
  mod.fit<-lm(acme ~ market, data = acme)
  sum.fit<-summary(mod.fit)
  sum.fit
  
  #Perform regular test of Ho: beta1 = 1 vs. beta1 > 0
  z.0<-as.numeric((mod.fit$coefficients[2]-1)/sum.fit$coefficients[2,2])
  #p-value
  1 - pt(q = z.0, df = n-2) 
  1 - pnorm(q = z.0, mean = 0, sd = 1)  #Given by BMA
  
  
  #Get h_j
  influence.stat<-lm.influence(mod.fit)
  h.j<-influence.stat$hat
  
  #Modified residuals
  r.j<-mod.fit$residuals/sqrt(1-h.j)
  mean(r.j)
  
  plot(x = acme$market, y = abs(r.j), xlab = "x (market return)", ylab = expression(abs(r)), 
       main = expression(paste(abs(r), " vs. market return")), panel.first = grid(col="gray", lty="dotted"))


  #Check for autocorrelation
  par(mfrow = c(1,2))
  #Usual plot from a regression class
  plot(x = 1:length(acme$market), y = mod.fit$residuals, ylab = "Residuals", xlab = "Time", main = "Residuals vs. time", 
       type = "o", panel.first = grid(col="gray", lty="dotted"))
  #Plot of the autocorrelation function - correlation between e_i and e_i-1 (lag 1) is the first autocorrelation
  save<-acf(x = mod.fit$residuals, type = c("correlation"), main = "Autocorrelation function plot", xlab = "lag")
  save



########################################################################
# Case resampling - Perform a test of Ho: beta1 = 1 vs. beta1 > 1

  #Similar code for the first three lines as in Chapter 4 bird bootstrap example
  calc.t.cases<-function(data, i) {
    d<-data[i,]
    x<-d[d$variable == "X",]  
    y<-d[d$variable == "Y.tilde",]
    mod.fit.cases<-lm(formula = y$value ~ x$value)  #No data option needed since x and y exist outside of data.frame
    sum.fit<-summary(mod.fit.cases)
    c(as.numeric(mod.fit.cases$coefficients), sum.fit$coefficients[2,2])
  }
    
  #Restructure data set in order to resample under Ho - notice how I modify the response (see p. 268)
  set1<-rbind(data.frame(value = acme$market, variable = "X"), 
              data.frame(value = acme$acme - 1*acme$market, variable = "Y.tilde"))
  head(set1)
  tail(set1)
  
  #Try it - also, examine the effect of y.tilde = acme$acme - 1*acme$market has on the beta^'s
  calc.t.cases(data = set1, i = 1:nrow(set1))
 
  set.seed(7101)
  boot.res.cases<-boot(data = set1, statistic = calc.t.cases, R = 999, sim = "ordinary", strata = set1$variable)
  boot.res.cases
  
  save.index<-boot.array(boot.out = boot.res.cases, indices = TRUE)
  save.index[1,]
  #set1[save.index[1,],]
  

  #Testing beta1 = 1 - BUT NOTICE THAT I PUT 0 IN AS HYPOTHESIZED VALUE SINCE I ADJUSTED THE RESPONSE
  z.0<-(boot.res.cases$t0[2] - 0)/boot.res.cases$t0[3]
  z.star.0<-(boot.res.cases$t[,2] - 0)/boot.res.cases$t[,3]
  (sum(z.star.0>=z.0)+1)/(boot.res.cases$R+1)
 

  #Plots of resampling distribution under Ho
  par(mfrow = c(1,2), pty = "s", xaxs = "i")

  #Histogram
  hist(z.star.0, main = expression(paste("Histogram of ", z[0]^{"*"})), xlab=expression(paste(z[0]^{"*"})), freq = FALSE) 
  abline(v = z.0, col = "darkgreen", lwd = 5)
  curve(dnorm(x, mean = mean(z.star.0), sd = sd(z.star.0)), col = "red", add = TRUE)

  #EDF
  plot.ecdf(z.star.0, verticals = TRUE, do.p = FALSE, main = expression(paste("EDF for ", z[0]^{"*"})), lwd = 2,
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), 
            ylab = expression(paste(hat(G), " under ", H[o])), xlab = expression(paste(z[0]^{"*"})))
  curve(expr = pnorm(x, mean(z.star.0), sd = sd(z.star.0)), col = "red", add = TRUE)
 
  #How about a standard normal on the plot?
  curve(expr = pnorm(x, mean = 0, sd = 1), col = "darkblue", add = TRUE)

  #How about a t-distribution with n-2 D.F. - of course, similar to standard normal
  curve(expr = pt(x, df = n-2), col = "darkgreen", add = TRUE)

  
  #Find p-value without a studentized quantity
  (sum(boot.res.cases$t[,2]>=boot.res.cases$t0[2])+1)/(boot.res.cases$R+1)
 


########################################################################
# Case resampling - Perform a test of Ho: beta1 = 0 vs. beta1 <> 0

  #Similar code for the first three lines in Chapter 4 for the bird bootstrap example
  calc.t.cases<-function(data, i) {
    d<-data[i,]
    x<-d[d$variable == "X",]  
    y<-d[d$variable == "Y",]
    mod.fit.cases<-lm(formula = y$value ~ x$value)  #No data option needed since x and y exist outside of data.frame
    sum.fit<-summary(mod.fit.cases)
    c(as.numeric(mod.fit.cases$coefficients), sum.fit$coefficients[2,2])
  }
    
  #Restructure data set in order to resample under Ho
  set1<-rbind(data.frame(value = acme$market, variable = "X"), 
              data.frame(value = acme$acme, variable = "Y"))
  head(set1)
  tail(set1)
  
  #Try it
  calc.t.cases(data = set1, i = 1:nrow(set1))
 
  set.seed(9817)
  boot.res.cases<-boot(data = set1, statistic = calc.t.cases, R = 999, sim = "ordinary", strata = set1$variable)
  boot.res.cases
  
  save.index<-boot.array(boot.out = boot.res.cases, indices = TRUE)
  save.index[1,]
  #set1[save.index[1,],]
  

  #Testing beta1 = 0 
  z.0<-(boot.res.cases$t0[2] - 0)/boot.res.cases$t0[3]
  z.star.0<-(boot.res.cases$t[,2] - 0)/boot.res.cases$t[,3]
  (sum(abs(z.star.0)>=abs(z.0))+1)/(boot.res.cases$R+1)
 

  #Plots of resampling distribution under Ho
  par(mfrow = c(1,2), pty = "s", xaxs = "i")
  
  #Histogram
  hist(z.star.0, main = expression(paste("Histogram of ", z[0]^{"*"})), xlab=expression(paste(z[0]^{"*"})), freq = FALSE) 
  abline(v = z.0, col = "darkgreen", lwd = 5)
  curve(dnorm(x, mean = mean(z.star.0), sd = sd(z.star.0)), col = "red", add = TRUE)

  #EDF
  plot.ecdf(z.star.0, verticals = TRUE, do.p = FALSE, main = expression(paste("EDF for ", z[0]^{"*"})), lwd = 2,
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), 
            ylab = expression(paste(hat(G), " under ", H[o])), xlab = expression(paste(z[0]^{"*"})))
  curve(expr = pnorm(x, mean(z.star.0), sd = sd(z.star.0)), col = "red", add = TRUE)





########################################################################
# Model-based resampling - Perform a test of Ho: beta1 = 1 vs. beta1 > 1 

  calc.t.modbased<-function(data, i, mu.hat, x) {
    epsilon<-data[i]
    y<-mu.hat + epsilon
    mod.fit.modbased<-lm(y ~ x)
    sum.fit<-summary(mod.fit.modbased)
    c(as.numeric(mod.fit.modbased$coefficients), sum.fit$coefficients[2,2])  #as.numeric() is just used to remove some not needed labels
  }

  #Find predicted values under Ho model
  beta.hat.0.Ho<-mean(acme$acme-1*acme$market)  #y = beta0 - 1*x implies y - 1*x = beta0
  mu.hat.Ho<-beta.hat.0.Ho + 1*acme$market 
  head(mu.hat.Ho)
  
  #Modified residuals where h_j = 1/n
  r.j<-(acme$acme-mu.hat.Ho)/sqrt(1-1/n)
  
  #Another way to find beta.hat.0 and h_j
  temp<-lm(acme ~ 1 + offset(market), data = acme) #offset() tells R to not estimate a parameter for market
  head(temp$fitted.values)  #Matches mu.hat.Ho
  hat.temp<-lm.influence(temp)$hat  #all are 1/n
  head(hat.temp)
  
  #Try it 
  calc.t.modbased(data = r.j - mean(r.j), i = 1:n, mu.hat = mu.hat.Ho, x = acme$market)
  
  set.seed(6510)
  boot.res.modbased<-boot(data = r.j - mean(r.j), statistic = calc.t.modbased, R = 999, sim = "ordinary", 
                          mu.hat = mu.hat.Ho, x = acme$market)
  boot.res.modbased
  
  #Notice the mean of the beta_hat1*'s is about 1 since resampled under Ho!
  mean(boot.res.modbased$t[,2])

  
  #Testing beta1 = 1 - note: boot.res.modbased$t0[2] <> mod.fit$coefficients[2] due to resampling from the r.j-rbar
  #z.0<-(boot.res.modbased$t0[2] - 1)/boot.res.modbased$t0[3]
  z.0<-as.numeric((mod.fit$coefficients[2]-1)/sum.fit$coefficients[2,2])
  z.star.0<-(boot.res.modbased$t[,2] - 1)/boot.res.modbased$t[,3]
  (sum(z.star.0>=z.0)+1)/(boot.res.modbased$R+1)
 

  #Plots of resampling distribution under Ho
  par(mfrow = c(1,2), pty = "s", xaxs = "i")
  
  #Histogram
  hist(z.star.0, main = expression(paste("Histogram of ", z[0]^{"*"})), xlab=expression(paste(z[0]^{"*"})), freq = FALSE) 
  abline(v = z.0, col = "darkgreen", lwd = 5)
  curve(dnorm(x, mean = mean(z.star.0), sd = sd(z.star.0)), col = "red", add = TRUE)

  #EDF
  plot.ecdf(z.star.0, verticals = TRUE, do.p = FALSE, main = expression(paste("EDF for ", z[0]^{"*"})), lwd = 2,
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), 
            ylab = expression(paste(hat(G), " under ", H[o])), xlab = expression(paste(z[0]^{"*"})))
  curve(expr = pnorm(x, mean(z.star.0), sd = sd(z.star.0)), col = "red", add = TRUE)


  #Find p-value without a studentized quantity
  (sum(boot.res.modbased$t[,2]>=boot.res.modbased$t0[2])+1)/(boot.res.modbased$R+1)



##########################################################################
# Model-based resampling - Perform a test of Ho: beta1 = 0 vs. beta1 <> 0

  calc.t.modbased<-function(data, i, mu.hat, x) {
    epsilon<-data[i]
    y<-mu.hat + epsilon
    mod.fit.modbased<-lm(y ~ x)
    sum.fit<-summary(mod.fit.modbased)
    c(as.numeric(mod.fit.modbased$coefficients), sum.fit$coefficients[2,2])  #as.numeric() is just used to remove some not needed labels
  }
  
  mu.hat.Ho<-mean(acme$acme) #Will get the same mu.hat.Ho from: lm(y~1)
  
  #Modified residuals
  r.j<-(acme$acme-mu.hat.Ho)/sqrt(1-1/n)
  
  #Try it 
  calc.t.modbased(data = r.j - mean(r.j), i = 1:n, mu.hat = mu.hat.Ho, x = acme$market)
  
  set.seed(7198)
  boot.res.modbased<-boot(data = r.j - mean(r.j), statistic = calc.t.modbased, R = 999, sim = "ordinary", 
                          mu.hat = mu.hat.Ho, x = acme$market)
  boot.res.modbased                    

   
  #Testing beta1 = 0 - note: boot.res.modbased$t0[2] <> mod.fit$coefficients[2] due to resampling from the r.j-rbar
  #z.0<-(boot.res.modbased$t0[2] - 0)/boot.res.modbased$t0[3]
  z.0<-as.numeric((mod.fit$coefficients[2]-0)/sum.fit$coefficients[2,2])
  z.star.0<-(boot.res.modbased$t[,2] - 0)/boot.res.modbased$t[,3]
  (sum(abs(z.star.0)>=abs(z.0))+1)/(boot.res.modbased$R+1)
 

  #Plots of resampling distribution under Ho
  par(mfrow = c(1,2), pty = "s", xaxs = "i")
  
  #Histogram
  hist(z.star.0, main = expression(paste("Histogram of ", z[0]^{"*"})), xlab=expression(paste(z[0]^{"*"})), freq = FALSE,
       xlim = c(min(z.star.0,z.0), max(z.star.0,z.0))) 
  abline(v = z.0, col = "darkgreen", lwd = 5)
  curve(dnorm(x, mean = mean(z.star.0), sd = sd(z.star.0)), col = "red", add = TRUE)

  #EDF
  plot.ecdf(z.star.0, verticals = TRUE, do.p = FALSE, main = expression(paste("EDF for ", z[0]^{"*"})), lwd = 2,
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), 
            ylab = expression(paste(hat(G), " under ", H[o])), xlab = expression(paste(z[0]^{"*"})))
  curve(expr = pnorm(x, mean(z.star.0), sd = sd(z.star.0)), col = "red", add = TRUE)



#
