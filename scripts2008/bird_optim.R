#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-11-06                                                   #
# UPDATE:                                                           #
# PURPOSE: Example of how one could use optim() instead of glm()    #
#          to find the MLEs.  Note that this serves as a general    #
#          example of how to find MLEs without the use of a standard#
#          function pre-programmed to find them                     #
#                                                                   #
# NOTES: This came out of my MRCV research in OCTOBER 2006          #
#####################################################################


counts<-c(251, 48, 34, 5)
first<-c("made", "missed", "made", "missed") 
second<-c("made", "made", "missed", "missed")
bird.data.frame<-data.frame(first, second, counts)
bird.data.frame

mod.fit<-glm(formula = counts ~ first + second, data = bird.data.frame, family = poisson(link = log), 
        na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = T))
summary(mod.fit)


###########################################################################
# Write own code to find parameter estimates using optim()


#Show can find likelihood function using Poisson distribution assumption
n.counts<-bird.data.frame$counts
X<-model.matrix(mod.fit)
loglik<-sum(n.counts*(X%*%mod.fit$coefficients)-exp(X%*%mod.fit$coefficients)-lgamma(bird.data.frame$counts+1))
loglik
logLik(object = mod.fit) #Checks - they are the same



#Obtain starting values for the estimating procedure
mod.fit.lm<-lm(formula = log(counts) ~ first + second, data = bird.data.frame)
#summary(mod.fit.lm)
beta<-mod.fit.lm$coefficients 


logL<-function(beta, X, n.counts) {

  #Need negative since optim() finds min
  -sum(n.counts*(X%*%beta)-exp(X%*%beta)-lgamma(n.counts+1))
  
}

logL(beta = beta, X = X, n.counts = n.counts) #Test

save.it<-optim(par = beta, fn = logL, method = "Nelder-Mead", X = X, n.counts = n.counts, 
                       control = list(trace = 0, maxit = 1000), 
                       hessian = TRUE)
save.it$par  #Parameter estimates - compare to those in summary(mod.fit)
save.it$convergence
cov.mat<-solve(save.it$hessian)
sqrt(diag(cov.mat)) #standard errors - compare to those in summary(mod.fit)
             
                       
 
               
                       
                       
                       
                       
                       
                       
#




















#
