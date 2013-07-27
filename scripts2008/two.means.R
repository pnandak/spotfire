#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-29-06                                                    #
# UPDATE: 10-19-07                                                  #
# PURPOSE: C.I. for the difference of two means                     #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#####################################################################
# Simulate data 
set.seed(5310)

n1<-20
n2<-15
mu1<-14
mu2<-10
alpha<-0.1

#Pop #1 is Exp(14) so that variance is 14^2 = 196
y1<-rexp(n = n1, rate = 1/mu1)

#Pop #2 is Exp(10) so that variance is 10^2 = 100
y2<-rexp(n = n2, rate = 1/mu2)

set1<-rbind(data.frame(y = y1, pop = 1), data.frame(y = y2, pop = 2))
head(set1)
tail(set1)



#####################################################################
# Bootstrap calculations

  library(boot)
  
  calc.t2<-function(data2, i2) {
    d2<-data2[i2,]
    group.means<-tapply(X = d2$y, INDEX = d2$pop, FUN = mean)
    diff.means<-group.means[1] - group.means[2]
    diff.means
  }
  
  calc.t<-function(data, i, M = 50) { 
    d<-data[i,]
    group.means<-tapply(X = d$y, INDEX = d$pop, FUN = mean)
    diff.means<-group.means[1] - group.means[2]
    
    boot.res.M<-boot(data = d, statistic = calc.t2, R = M, sim="ordinary", strata=d$pop)

    n<-tapply(X = d$y, INDEX = d$pop, FUN = length)  
    group.var<-tapply(X = d$y, INDEX = d$pop, FUN = var)
    
    #Variance from nonparametric delta method
    v.L<-(n[1]-1)/n[1]*group.var[1]/n[1] + (n[2]-1)/n[2]*group.var[2]/n[2]
    
    #Would have been reasonable to use the below as well
    #v.unbiased<-group.var[1]/n[1] + group.var[2]/n[2]
    
    #Variance from double boot
    v.boot<-var(boot.res.M$t)  
  
    #Variance from using the jackknife estimate of the empirical influence values
    #  This stype = "i" part is needed because of the second argument to calc.t2 ("i" for indices).
    l.jack<-empinf(data = d, statistic = calc.t2, stype = "i", type = "jack", strata = d$pop) 
    v.jack<-var.linear(l.jack, strata=d$pop)
           
    c(diff.means, v.L, v.jack, v.boot) 
  }


  #Try the calc.t function
  calc.t(data = set1, i = 1:nrow(set1), M = 100)

  #Find start time
  start.time<-proc.time()

  #Bootstrap it!
  set.seed(7153)
  boot.res<-boot(data = set1, statistic = calc.t, R = 999, sim = "ordinary", strata=set1$pop, M = 800)
  boot.res
  plot(boot.res)
  #plot(boot.res, jack = TRUE, stinf = FALSE) #Extra options are from Section 3.10

  #Find end time and total time elapsed
  end.time<-proc.time()
  save.time<-end.time-start.time
  cat("\n Number of minutes running:", save.time[3]/60, "\n \n")


  #Compare the variance measures using histograms
  par(mfrow=c(1,3))
  hist(boot.res$t[,2], xlim = c(0,35), ylim = c(0,230), main = expression(v[L]), xlab = "Variance")
  hist(boot.res$t[,3], xlim = c(0,35), ylim = c(0,230), main = expression(v[jack]), xlab = "Variance")
  hist(boot.res$t[,4], xlim = c(0,35), ylim = c(0,230), main = expression(v[boot]), xlab = "Variance")


##########################################################################################
# Parallel coordinate plot of the variance measures

  #My version of the parallel coordinate plot function
  #  Does not rescale variables and uses actual y-axis
  #  Coded first in fall 2004 for group testing regression model research
  parcoord2<-function (x, col = 1, lty = 1,  ...)  
  {
    #x <- apply(x, 2, function(x) (x - min(x))/(max(x) - min(x)))
    matplot(1:ncol(x), t(x), type = "l", col = col, lty = lty, 
        xlab = "", ylab = "", axes = FALSE, ...)
    axis(1, at = 1:ncol(x), labels = colnames(x))
    #axis(side = 2, at = round(seq(from = min(x), to = max(x), by = (max(x) - min(x))/4),2)) 
    axis(side = 2, at = pretty(x)) #Added 1-30-06
     
    for (i in 1:ncol(x)) lines(c(i, i), c(min(x), max(x)), col = "grey70")
    invisible()
  }

  par(mfrow = c(1,1))
  parcoord2(x = boot.res$t[1:99,2:4], main = expression(paste("First 99 variance measures:", 1 == v[L], ", ", 2 == v[jack], ", ", 3 == v[boot])), 
            col = 1:99)
  cor(boot.res$t[1:99,2:4])
  cor(boot.res$t[,2:4])
  abline(h = boot.res$t0[2:4], col = "black", lwd = 4)
  text(x = 0.94, y = boot.res$t0[2:4], label = c(expression(v[L]), expression(v[jack]), expression(v[boot])), cex = 0.75, 
       pos = 2, xpd = NA, col = "red") 



##########################################################################################
# C.I.s 

  #Usual C.I.s 
  save.normal<-t.test(x = y1, y = y2, var.equal = FALSE, conf.level = 0.95)
  names(save.normal)
  normal.ci<-save.normal$conf.int   #C.I.
  normal.ci
  save.normal$parameter  #DF
  save.normal$estimate[1]-save.normal$estimate[2]  #Diff of means
  save.normal$statistic #t test statistic for testing = 0
  v.unbias<-((save.normal$estimate[1]-save.normal$estimate[2])/save.normal$statistic)^2
  as.numeric(v.unbias)  #as.numeric is a quick way to strip an inappropriate name from the object 
 
  #Another way to calculate variance 
  var(y1)/length(y1)+var(y2)/length(y2)
  
  
  #Boot C.I.s - using different variance measures
  save1<-boot.ci(boot.out = boot.res, conf = 0.95, type = c("norm", "basic", "stud"), var.t0 = v.unbias, var.t = boot.res$t[,2]) 
  save2<-boot.ci(boot.out = boot.res, conf = 0.95, type = "stud", var.t0 = v.unbias, var.t = boot.res$t[,3]) 
  save3<-boot.ci(boot.out = boot.res, conf = 0.95, type = "stud", var.t0 = v.unbias, var.t = boot.res$t[,4]) 
 
  ci.names<-c("Normal Intro STAT", "Basic", "Studentized, v.unbias, v.L*", "Studentized, v.unbias, v.jack*", 
              "Studentized, v.unbias, v.boot*")
  ci<-rbind(normal.ci, save1$basic[,4:5], save1$student[,4:5], save2$student[,4:5], save3$student[,4:5])
  data.frame(ci.names, lower = ci[,1], upper = ci[,2])

  #This corresponds to var.t0 = boot.res$t0[2])
  boot.ci(boot.out = boot.res, conf = 0.95, type = c("norm", "basic", "stud"), var.t = boot.res$t[,2]) 

  
  




#
