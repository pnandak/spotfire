#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-5-06, update 10-13-07                                    #
# UPDATE:                                                           #
# PURPOSE: Example 2.5, 2.6, 2.9, 2.10, 2.11                        #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Example 1.1 and Figure 1.2

  y<-c(3,5,7,18,43,85,91,98,100,130,230,487)
  t<-mean(y)
  cat("My sample is", sort(y), "\n which produces an observed statistic of", t, "\n")


  #EDF
  par(pty = "s", mfrow=c(1,2))
  plot.ecdf(x = y, verticals = TRUE, do.p = FALSE, main = "EDF for AC failure times", lwd = 2, xlim = c(0,600),
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylab = expression(hat(F)), xlab = "y")
  #Notice use of q = x below - the curve function needs "x" to be used, also notice the "rate" parameter 1/scale parameter
  curve(expr = pexp(q = x, rate = 1/t), from = 0, to = 600, col = "red", add = TRUE)
  #Could use the lines function instead of curve
  #lines(x = seq(from = 0, to = 600, by = 1), 
  #      y = pexp(q = seq(from = 0, to = 600, by = 1), rate = 1/t), col = "red")  #Note parameterization uses exp(1/mu)  
 
  #QQ-Plot
  #  Note: Book uses different version of the x-axis of the plot
  #  Bottom of p. 18 gives justification for the p = seq( )
  exp.quant<-qexp(p = seq(from = 1/(length(y)+1), to = 1-1/(length(y)+1), by = 1/(length(y)+1)), rate = 1/t)
  plot(y = sort(y), x = exp.quant, main = "QQ-Plot for AC failure times", ylab = "y", xlab = "Exp. quantiles",
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylim = c(0,600))
  data.frame(exp.quant, y)
  abline(a = 0, b = 1, col = "red")
    
    
######################################################################
# Example 2.5

  R.max<-500
  set.seed(4130)
  y.star<-matrix(data = rexp(n = length(y)*R.max, rate = 1/t), nrow = R.max, ncol = length(y))

  #r = 1
  y.star[1,]
  
  t.star<-apply(X = y.star, FUN = mean, MARGIN = 1)
  mean(t.star) - t #B_500
  var(t.star)      #V_500
  
  
  
  R.max<-10000
  set.seed(2891)
  y.star<-matrix(data = rexp(n = length(y)*R.max, rate = 1/t), nrow = R.max, ncol = length(y))  
  t.star<-apply(X = y.star, FUN = mean, MARGIN = 1)
  mean(t.star) - t #B_10000
  var(t.star)      #V_10000
  
 
  R.max<-500  
  R.values<-c(10, 20, 50, 100, 200, 300, 400, 500)
  
  save2<-data.frame(R.values = NULL, bias = NULL, variance=NULL, sim=NULL)
  
  set.seed(4131)
  bias<-numeric(length(R.values))
  variance<-numeric(length(R.values))

  for (sim.numb in 1:4) {
  
    y.star<-matrix(data = rexp(n = length(y)*R.max, rate = 1/t), nrow = 500, ncol = length(y))
    t.star<-apply(X = y.star, FUN = mean, MARGIN = 1)

    counter<-1
    
    for (i in R.values) { 
      bias[counter] <- mean(t.star[1:i]) - t
      variance[counter] <- var(t.star[1:i])  
      counter <- counter + 1
    }
  
    save<-data.frame(R.values, bias, variance, sim = sim.numb)
    save2<-rbind.data.frame(save2, save)
  }
  
  head(save2, n = 10) #Check values in it.
  
  #Figure 2.1
  par(pty = "s", mfrow=c(1,2))
  plot(x = save2$R.values, y = save2$bias, 
       col = rep(c("red", "blue", "darkgreen", "lightblue"), times = 1, each = length(R.values)),
       pch = rep(c(1, 2, 3, 4), times = 1, each = length(R.values)), xlab = "R", ylab = "Bias",
       main = "4 simulation runs for bias",
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), type = "p") 
  abline(h = 0, col = "black", lwd = 2)   
  lines(x = R.values, y = save2$bias[save2$sim == 1], col = "red")
  lines(x = R.values, y = save2$bias[save2$sim == 2], col = "blue")
  lines(x = R.values, y = save2$bias[save2$sim == 3], col = "darkgreen")
  lines(x = R.values, y = save2$bias[save2$sim == 4], col = "lightblue")
      
  legend(locator(1), legend = c("1", "2", "3", "4"), col=c("red", "blue", "darkgreen", "lightblue"),
         pch = c(1, 2, 3, 4), bty="n", cex=0.75)
  
  
  plot(x = save2$R.values, y = save2$variance, 
       col = rep(c("red", "blue", "darkgreen", "lightblue"), times = 1, each = length(R.values)),
       pch = rep(c(1, 2, 3, 4), times = 1, each = length(R.values)), xlab = "R", ylab = "Variance",       
       main = "4 simulation runs for variance",
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), type = "p") 
  abline(h = t^2/length(y), col = "black", lwd = 2)   
  lines(x = R.values, y = save2$variance[save2$sim == 1], col = "red")
  lines(x = R.values, y = save2$variance[save2$sim == 2], col = "blue")
  lines(x = R.values, y = save2$variance[save2$sim == 3], col = "darkgreen")
  lines(x = R.values, y = save2$variance[save2$sim == 4], col = "lightblue")
      
  legend(locator(1), legend = c("1", "2", "3", "4"), col=c("red", "blue", "darkgreen", "lightblue"),
         pch = c(1, 2, 3, 4), bty="n", cex=0.75)
  
  
######################################################################
# Example 2.6
  
  
  R.max<-999
  set.seed(4130)
  y.star<-matrix(data = rexp(n = length(y)*R.max, rate = 1/t), nrow = R.max, ncol = length(y))
  t.star<-apply(X = y.star, FUN = mean, MARGIN = 1)
  
  #NEW CODE
  alpha<-c(0.05,0.95)
  sort(t.star)[alpha*(R.max+1)]-t  #0.05 and 0.95 estimated quantiles of T*-t
  qgamma(p = alpha, shape = length(y), scale = t/length(y)) - t #0.05 and 0.95 quantiles of T*-t
  #Remember from p. 2.9, beta in Casella and Berger and R is mu/kappa in BMA.  
  
  #Equivalent way to get quantiles using quantile() function
  quantile(x = t.star, probs = alpha, type = 1)
  sort(t.star)[alpha*(R.max+1)]
  
  
  #Figure 2.2
  #QQ-Plot
  par(pty = "s", mfrow=c(1,2))
  norm.quant<-qnorm(p = seq(from = 1/(length(t.star[1:99])+1), to = 1-1/(length(t.star[1:99])+1), by = 1/(length(t.star[1:99])+1)),
                    mean = mean(t.star[1:99]), sd = sd(t.star[1:99]))
  plot(y = sort(t.star[1:99]), x = norm.quant, main = "QQ-Plot for t.star, R=99", ylab = "t.star", xlab = "Normal quantiles",
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))
  abline(a = 0, b = 1, col = "red")
    
  norm.quant<-qnorm(p = seq(from = 1/(length(t.star)+1), to = 1-1/(length(t.star)+1), by = 1/(length(t.star)+1)),
                    mean = mean(t.star), sd = sd(t.star))
  plot(y = sort(t.star), x = norm.quant, main = "QQ-Plot for t.star, R=999", ylab = "t.star", xlab = "Normal quantiles",
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))
  abline(a = 0, b = 1, col = "red")

  #t* ~ Gamma(n, t/n)
  gam.quant<-qgamma(p = seq(from = 1/(length(t.star[1:99])+1), to = 1-1/(length(t.star[1:99])+1), by = 1/(length(t.star[1:99])+1)),
                    shape = length(y), scale = t/length(y))
  plot(y = sort(t.star[1:99]), x = gam.quant, main = "QQ-Plot for t.star, R=99", ylab = "t.star", xlab = "Gamma quantiles",
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))
  abline(a = 0, b = 1, col = "red")
    
  gam.quant<-qgamma(p = seq(from = 1/(length(t.star)+1), to = 1-1/(length(t.star)+1), by = 1/(length(t.star)+1)),
                    shape = length(y), scale = t/length(y))
  plot(y = sort(t.star), x = gam.quant, main = "QQ-Plot for t.star, R=999", ylab = "t.star", xlab = "Gamma quantiles",
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))
  abline(a = 0, b = 1, col = "red")
 

  #Figure 2.3
  par(pty = "s", mfrow=c(1,2))
  hist(x = t.star[1:99], main = "Histogram for t*, R=99", freq=FALSE, xlab = "t*")
  curve(expr = dnorm(x, mean = mean(t.star[1:99]), sd = sd(t.star[1:99])), col = 2, add = TRUE)
 
  hist(x = t.star, main = "Histogram for t*, R=999", freq=FALSE, xlab = "t*", ylim = c(0,0.014))
  curve(expr = dnorm(x, mean = mean(t.star), sd = sd(t.star)), col = 2, add = TRUE)


  #p quantiles
  p<-c(0.01, 0.05, 0.10, 0.5, 0.90, 0.95, 0.99)
  # p*(R+1) quantiles of t.star 
  data.frame(p, t.star = quantile(x = t.star, probs = p, type = 1), gamma.quant = qgamma(p = p, shape = length(y), scale = t/length(y))) 
  #sort(t.star)[p*(R.max+1)] also will do the same as the quantile() function.
  # p*(R+1) quantiles of t.star - t 
  data.frame(p, t.star = quantile(x = t.star, probs = p, type = 1) - t, gamma.quant = qgamma(p = p, shape = length(y), scale = t/length(y)) - t)
 
  
  
######################################################################
# Another way to do a parametric bootstrap using the boot() function
  
  #Need to load it first even though it is automatically installed with R (only need to do once during the R session)
  library(boot)
  
  #Function for the statistic
  #  First element is the original or resampled data.
  #  No second element for a parametric bootstrap - see p. 528 of BMA
  mean.t<-function(data) {
     mean(data) 
  }
  
  
  #Function for how to simulate the resamples
  sim.data<-function(data, mle) {
    out<-rexp(n = length(data), rate = 1/mle)
    out
  }
   
  #Do bootstrap
  set.seed(4162)
  boot.res<-boot(data = y, statistic = mean.t, R = 1000, sim = "parametric", ran.gen = sim.data, mle = mean(y)) 
  boot.res
  plot(boot.res)   
  
  

  
  
######################################################################
# Example 2.9

  #Need to load it first even though it is automatically installed with R (only need to do once during the R session)
  library(boot)
  
  #Function for the statistic
  #  First element is the original or resampled data.
  #  Second element represents the indices of the data.  For example, the indices will be 1:length(y) for the observed
  #    data.   
  calc.t<-function(data, i) {
     d<-data[i]
     mean(d) 
  }
  
  #Try it
  calc.t(data = y, i = 1:length(y))

  #Other implementations of calc.t:
  calc.t(data = y, i = rep(x = 1, times = 12))
  set.seed(7828)
  calc.t(data = y, i = sample(x = 1:length(y), size = 12, replace = TRUE))
 


  
  #Do bootstrap
  set.seed(9182)
  boot.res<-boot(data = y, statistic = calc.t, R = 1000, sim="ordinary") 
  boot.res
  plot(boot.res)   
  
  #What's available from the boot() function?
    names(boot.res)
    boot.res$t0
    head(boot.res$t)
    boot.res$statistic
  
    #List of indices from the resamples
    save.ind<-boot.array(boot.out=boot.res, indices=TRUE)
    head(save.ind)
  
    #First resample
    y[save.ind[1,]]
    mean(y[save.ind[1,]])

  #Work for Figure 2.7
  R.max<-999
  R.values<-c(19, 39, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999)
  save2<-data.frame(R.values = NULL, p05 = NULL, p95=NULL, sim=NULL)
  set.seed(9182)

  p05<-numeric(length(R.values))
  p95<-numeric(length(R.values))

  for (sim.numb in 1:4) {
  
    boot.res<-boot(data = y, statistic = calc.t, R = 999, sim="ordinary") 
    counter<-1
    
    for (i in R.values) { 
      p05[counter] = quantile(x = boot.res$t[1:i], probs = 0.05, type = 1) - t  
      # Instead of quantile(), could also use sort(boot.res$t[1:i])[0.05*(i+1)]
      p95[counter] = quantile(x = boot.res$t[1:i], probs = 0.95, type = 1) - t  #  with type 1 option
      counter = counter + 1
    }
  
    save<-data.frame(R.values, p05, p95, sim = sim.numb)
    save2<-rbind.data.frame(save2, save)
  }
 
 
  #Figure 2.7
  par(pty = "m", mfrow=c(1,1))
  plot(x = c(save2$R.values, save2$R.values), y = c(save2$p05, save2$p95), 
       col = rep(c("red", "blue", "darkgreen", "lightblue"), times = 1, each = length(R.values)),
       pch = rep(c(1, 2, 3, 4), times = 1, each = length(R.values)), xlab = "R", ylab = "Quantiles of t* - t",
       main = "4 simulation runs for quantiles (using nonpar boot)",
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), type = "p") 
  abline(h = qgamma(p = c(0.05, 0.95), shape = length(y), scale = t/length(y)) - t, col = "black", lwd = 2)  
  lines(x = R.values, y = save2$p05[save2$sim == 1], col = "red")
  lines(x = R.values, y = save2$p05[save2$sim == 2], col = "blue")
  lines(x = R.values, y = save2$p05[save2$sim == 3], col = "darkgreen")
  lines(x = R.values, y = save2$p05[save2$sim == 4], col = "lightblue")      
  lines(x = R.values, y = save2$p95[save2$sim == 1], col = "red")
  lines(x = R.values, y = save2$p95[save2$sim == 2], col = "blue")
  lines(x = R.values, y = save2$p95[save2$sim == 3], col = "darkgreen")
  lines(x = R.values, y = save2$p95[save2$sim == 4], col = "lightblue")      
  legend(locator(1), legend = c("1", "2", "3", "4"), col=c("red", "blue", "darkgreen", "lightblue"),
         pch = c(1, 2, 3, 4), bty="n", cex=2)
#Could do the similar plot for bias and variance

  
  #Figure 2.8 - left plot
  par(pty = "s", mfrow=c(1,2))
  #t* ~ Gamma(n, t/n)
  gam.quant<-qgamma(p = seq(from = 1/(boot.res$R+1), to = 1-1/(boot.res$R+1), by = 1/(boot.res$R+1)),
                    shape = length(y), scale = t/length(y))
  plot(y = sort(boot.res$t), x = gam.quant, main = "QQ-Plot for t*, R=999, Y~Exp", ylab = "t*", xlab = "Gamma quantiles",
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))
  abline(a = 0, b = 1, col = "red")
  
    
  #Figure 2.8 - right plot - Suppose started with a gamma instead of a exponential model
    
   #-log likelihood function for a gamma(kappa, mu)
    logL<-function(par.gam, data) {

      kappa<-par.gam[1]
      mu<-par.gam[2]
      n<-length(data)
  
      #optim() finds a minimum so I need to put a negative here since want a max
      -(-n*lgamma(kappa) + n*kappa*log(kappa) - n*kappa*log(mu) - kappa/mu * sum(data) + (kappa-1)*sum(log(data)))

    }

    #MOM estimators - used as starting points
    par.gam<-c(mean(y)^2/var(y), mean(y))
    #Find MLEs (default method did not produce convergence)
    save.opt<-optim(par = par.gam, fn = logL, data = y, control=list(trace = 0, maxit=10000), method = "BFGS",
                    hessian = TRUE)
    save.opt

    gam.quant<-qgamma(p = seq(from = 1/(boot.res$R+1), to = 1-1/(boot.res$R+1), by = 1/(boot.res$R+1)),
                      shape = length(y)*save.opt$par[1], scale = save.opt$par[2]/(length(y)*save.opt$par[1]))
    plot(y = sort(boot.res$t), x = gam.quant, main = "QQ-Plot for t*, R=999, Y~Gamma", ylab = "t*", xlab = "Gamma quantiles",
         panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))
    abline(a = 0, b = 1, col = "red")
  
    #Do a contour plot to examine the log likelihood function - investigate why convergence problems occured with
    #  default method
    i<-1
    j<-1
    mu.eval<-seq(from = 50, to = 150, by = 0.1)
    kappa.eval<-seq(from = 0.1, to = 2, by = 0.1)
    save.it<-matrix(NA, length(mu.eval), length(kappa.eval))
    for (mu in mu.eval) {
      for (kappa in kappa.eval) {
        save.it[i,j]<--logL(c(kappa, mu), y)
        j<-j+1
      }
      j<-1
      i<-i+1
    }
    
    win.graph(width = 7, height = 7, pointsize = 12)  #Open new plotting sheet
    #Note that contour() has a different data arrangement for the data to plot than PROC CONTOUR in SAS
    contour(x = mu.eval, y = kappa.eval, z = save.it, levels = c(-68, -70, -72, -80, -90),
            xlab = expression(mu), ylab = expression(kappa), main = "Contour plot of log(L)")
    abline(h = save.opt$par[1], col = "blue")
    abline(v = save.opt$par[2], col = "blue")
    max(save.it)
    
     
  
#######################################################################################################  
  
  #Code using the Casella and Berger parameterization of a gamma distribution
    #-log likelihood function for a gamma(alpha, beta)
    logL<-function(par.gam, data) {

      alpha<-par.gam[1]
      beta<-par.gam[2]
      n<-length(data)
  
      #optim() finds a minimum so I need to put a negative here since want a max
      -(-n*lgamma(alpha) - n*alpha*log(beta) - 1/beta * sum(data) + (alpha-1)*sum(log(data)))

    }

    #MOM estimators - used as starting points
    par.gam<-c(mean(y)^2/var(y), var(y)/mean(y))
    #Find MLEs (default method did not produce convergence)
    save.opt<-optim(par = par.gam, fn = logL, data = y, control=list(trace = 0, maxit=10000), method = "BFGS",
                    hessian = TRUE)
    save.opt

    gam.quant<-qgamma(p = seq(from = 1/(boot.res$R+1), to = 1-1/(boot.res$R+1), by = 1/(boot.res$R+1)),
                      shape = length(y)*save.opt$par[1], scale = save.opt$par[2]/length(y))
    plot(y = sort(boot.res$t), x = gam.quant, main = "QQ-Plot for t*, R=999, Y~Gamma", ylab = "t*", xlab = "Gamma quantiles",
         panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))
    abline(a = 0, b = 1, col = "red")

  

  #Another way to find the MLE is to use the mle function in the stats4 package - notice how I get a little 
  #  different answer - due to flatness of likelihood function
  logL2<-function(alpha, beta) {
    #optim() finds a minimum so I need to put a negative here since want a max
    -(-length(y)*lgamma(alpha) - length(y)*alpha*log(beta) - 1/beta * sum(y) + (alpha-1)*sum(log(y))) 
  }

  library(stats4)
  mle.res<-mle(minuslogl = logL2, start = list(alpha = (mean(y)/sd(y))^2, beta = sd(y)^2/mean(y)), method = "BFGS")
  summary(mle.res)
  names(mle.res) #None - possibly due to S version 4 being used instead of version 3?
  logLik(mle.res)
  vcov(mle.res)
  plot(profile(mle.res), absVal=FALSE) #notice error messages
  confint(mle.res)  #notice error messages
  
#######################################################################################################   


############################################################################
# How many different resamples are possible?  (p. 27)

choose(2*length(y)-1, length(y)-1)


#####################################################################
# Example 2.11

  #Studentized parametric bootstrap C.I.
  z.star.0.025<-sqrt(length(y))*(1-1/qgamma(p=0.025, shape = length(y), scale = 1/length(y)))
  z.star.0.975<-sqrt(length(y))*(1-1/qgamma(p=0.975, shape = length(y), scale = 1/length(y)))
  lower<-t - z.star.0.975*sqrt(t^2/length(y))
  upper<-t - z.star.0.025*sqrt(t^2/length(y))
  data.frame(interval = "Parametric studentized", lower, upper)

  #Basic parametric bootstrap C.I.
  lower<-2*t - qgamma(p=0.975, shape = length(y), scale = t/length(y))
  upper<-2*t - qgamma(p=0.025, shape = length(y), scale = t/length(y))
  data.frame(interval = "Parametric basic", lower, upper)

  #Regular t-distribution interval; remember that t is mean(y) here
  data.frame(name = "t-based interval",
                    lower = t - qt(0.975, length(y)-1)*sd(y)/sqrt(length(y)), 
                    upper = t + qt(0.975, length(y)-1)*sd(y)/sqrt(length(y)))




  #Nonparametric bootstrap
  calc.t<-function(data, i) {
     d<-data[i]
     n<-length(d)
     v.L<-1/n^2*(n-1)*var(d) #BMA p. 22 variance, also npar delta-method var
     t<-mean(d)
     c(t, v.L) 
  }
  
  calc.t(data = y, i = 1:length(y))
  
  set.seed(9182) #Same seed as earlier
  boot.res<-boot(data = y, statistic = calc.t, R = 999, sim="ordinary") 
  plot(boot.res) 
  head(boot.res$t)
  boot.res$t0
  
  #Easier way to get quantiles
  G.hat.q<-quantile(x = boot.res$t[,1], probs = c(0.025, 0.975), type = 1)
  G.hat.q
  #Harder way to get quantiles
  data.frame(q025 = sort(boot.res$t[,1])[(999+1)*(0.025)], q975 = sort(boot.res$t[,1])[(999+1)*(1-0.025)])
 
  #Basic interval
  basic<-data.frame(name = "Basic", lower = 2*boot.res$t0[1]-G.hat.q[2], upper = 2*boot.res$t0[1]-G.hat.q[1])
  basic
 
  #Studentized boot interval
  z.star<-(boot.res$t[,1] - t)/sqrt(boot.res$t[,2])
  z.star.quant<-quantile(x = z.star, probs = c(0.025, 0.975), type = 1)
  z.star.quant
  boot.stud.nonpar<-data.frame(name = "Studentized boot interval nonpar",
                        lower = t - z.star.quant[2]*sd(y)/sqrt(length(y)), 
                        upper = t - z.star.quant[1]*sd(y)/sqrt(length(y)))
 
  boot.stud.nonpar
 
  #One way to remove the "97.5%" from being shown.
  data.frame(name = "Basic", lower = 2*boot.res$t0[1]-as.numeric(G.hat.q[2]), upper = 2*boot.res$t0[1]-as.numeric(G.hat.q[1]))
 
 
 
  #When using the studentized interval, boot.ci assumes that boot.res$t has a first column containing
  #  the statistic of interest and the second column contains the variance.  If the variance is not the 
  #  second column, use var.t = ___ as an additional option to show where the variances are (like boot.res$t[,3]).  
  boot.ci(boot.out = boot.res, conf = 0.95, type = c("norm", "basic", "stud")) 
  
  #Using my own value of v
  boot.ci(boot.out = boot.res, conf = 0.95, type = c("norm", "basic", "stud"), var.t0 = var(y)/length(y)) 

  #Using variance from second element of calc.t()
  boot.ci(boot.out = boot.res, conf = 0.95, type = c("norm", "basic", "stud"), var.t0 = (length(y)-1)/length(y)*var(y)/length(y))
 
 
  #"Normal" interval in boot.ci() calculations
  b<-mean(boot.res$t[,1]) - t
  b
 
  #Reproduces  boot.ci(boot.out = boot.res, conf = 0.95, type = "norm")
  lower<-t - b - qnorm(0.975, mean = 0, sd = 1)*sd(boot.res$t[,1])  
  upper<-t - b - qnorm(0.025, mean = 0, sd = 1)*sd(boot.res$t[,1])
  data.frame(name = "Normal interval calc in boot.ci", lower, upper)

  #Reproduces  boot.ci(boot.out = boot.res, conf = 0.95, type = "norm"), var.t0 = (length(y)-1)/length(y)*var(y)/length(y))
  lower<-t - b - qnorm(0.975, mean = 0, sd = 1)*sqrt(calc.t(y, 1:length(y))[2])
  upper<-t - b - qnorm(0.025, mean = 0, sd = 1)*sqrt(calc.t(y, 1:length(y))[2])
  data.frame(name = "Normal interval calc in boot.ci", lower, upper)
 
  
  

###############################################################################
# Code from BMA for this problem  

#   Nonparametric confidence intervals for mean failure time 
#   of the air-conditioning data as in Example 5.4 of Davison
#   and Hinkley (1997)

library(boot)

mean.fun <- function(d, i) 
{    m <- mean(d$hours[i])
     n <- length(i)
     v <- (n-1)*var(d$hours[i])/n^2
     c(m, v)
}
set.seed(9182)
air.boot <- boot(y, mean.fun, R=999)
boot.ci(air.boot, type = c("norm", "basic", "perc", "stud"))






#
