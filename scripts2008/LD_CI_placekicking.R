#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-17-05                                                    #
# UPDATE: 12-24-05                                                  #
# Purpose: Find the LD C.I. for the placekick data                  #
#                                                                   #
# NOTES:                                                            #
#####################################################################


placekick<-read.table(file = "C:\\chris\\UNL\\STAT875\\Chapter4\\place.s.csv", header = TRUE, sep = ",")
mod.fit<-glm(formula = good1~dist, data = placekick, na.action = na.exclude, 
             family = binomial(link=logit), control = list(epsilon = .0001, maxit = 50, trace = T))
pi0<-0.5
LD.x<-(log(pi0/(1-pi0)) - mod.fit$coefficients[1])/mod.fit$coefficients[2]
LD.x
alpha<-0.1


###############################################################################################
#Simple code for C.I.
x<-seq(from = 18, to = 75, by = 0.01)

save<-summary(mod.fit)
left.side <- abs(mod.fit$coefficients[1] + mod.fit$coefficients[2]*x - log(pi0/(1-pi0))) / 
                      sqrt(save$cov.unscaled[1,1] + x^2*save$cov.unscaled[2,2] + 2*x*save$cov.unscaled[1,2]) 

check <- left.side < qnorm(1-alpha/2)
min(x[check == TRUE])
max(x[check == TRUE])


################################################################################################
#Better code for C.I.

#Function need to find root of
root.func<-function(x, beta0.hat, beta1.hat, pi0, var.alpha.hat, var.beta.hat, cov.alpha.beta, alpha) {
  abs(beta0.hat + beta1.hat*x - log(pi0/(1-pi0))) / 
   sqrt(var.alpha.hat + x^2*var.beta.hat + 2*x*cov.alpha.beta) - qnorm(1-alpha/2)
}

#Find C.I. lower bound - notice that I only search starting at minimum x
lower<-uniroot(f = root.func, interval = c(min(placekick$dist), LD.x), 
      beta0.hat = mod.fit$coefficients[1], beta1.hat = mod.fit$coefficients[2], 
      pi0 = 0.5, var.alpha.hat = save$cov.unscaled[1,1], 
      var.beta.hat = save$cov.unscaled[2,2], cov.alpha.beta = save$cov.unscaled[1,2], alpha = alpha, 
      tol = 10^(-8))
names(lower)
lower


#Find C.I. upper bound - notice that I only search starting at maximum x
upper<-uniroot(f = root.func, interval = c(LD.x, max(placekick$dist)), 
      beta0.hat = mod.fit$coefficients[1], beta1.hat = mod.fit$coefficients[2], 
      pi0 = 0.5, var.alpha.hat = save$cov.unscaled[1,1], 
      var.beta.hat = save$cov.unscaled[2,2], cov.alpha.beta = save$cov.unscaled[1,2], alpha = alpha, 
      tol = 10^(-8))
names(upper)
upper

#The LD and confidence interval
data.frame(LD.x = LD.x, lower = lower$root, upper = upper$root)


#Plot summarzing the root finding process.  
temp<-root.func(x = seq(from = 18, to = 75, by = 0.01), beta0.hat = mod.fit$coefficients[1], beta1.hat = mod.fit$coefficients[2], 
      pi0 = 0.5, var.alpha.hat = save$cov.unscaled[1,1], 
      var.beta.hat = save$cov.unscaled[2,2], cov.alpha.beta = save$cov.unscaled[1,2], alpha = alpha)
plot(x = seq(from = 18, to = 75, by = 0.01), y = temp, type = "l", main = "Plot of function solved for 0", 
     xlab = "x values", ylab = "function of x")
abline(h = 0, lty = "dotted", col = "red")






#
