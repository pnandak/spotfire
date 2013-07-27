

############################################################
############################################################
###                                                      ###
###         Bayesian Methods for Regression in R         ###
###                   Nels G. Johnson                    ###
### Laboratory for Interdiciplinary Statistical Analysis ###
###       Department of Statistics, Virginia Tech        ###
###                                                      ###
############################################################
############################################################

#####################
# 0.1 Built-in Data  
#####################

# To introduce regression, here is a simple example using the swiss 
# built-in dataset.

?swiss # Read help file for swiss
head(swiss) # Look at first few rows of swiss

################
# 0.2 Libraries
################

# The only library we'll be using today is MCMCglmm.

library(MCMCglmm)

# For more libraries relating to Bayes see:

# http://cran.r-project.org/web/views/Bayesian.html

############################
# 0.3 Pairwise Scatterplots  
############################

# It's always good to plot the data first, to get an idea of what is
# going on.

pairs(swiss, panel = panel.smooth, main = "swiss data", 
col = 3 + (swiss$Catholic > 50))

###################
# 1 MLE Regression 
###################

######################
# 1.1 MLE Regression 
######################

# The basic function for regression in R is lm().

?lm # Looks at help for lm()

# It handles regression when normal errors are assumed.

reg1 <- lm(Fertility ~ . ,data=swiss) # Assigns an lm object to reg1

# The . tell tells R to use all other columns besides Fertility as 
# X variables in the model.

#############################
# 1.2 Regression Diagnostics 
#############################

# We can use the plot() function on lm objects to plot regression 
# diagnostics.

win.graph()   	# New graph window.
par(mfrow=c(2,2))		# Makes 4 panels for graphs in 1 graph.
plot(reg1)			# Produces 4 regression diagnostic plots.

#######################
# 1.3 Inference 
#######################

# We can use the summary() function to return the regression results.

summary(reg1)

# It looks like we may not need Examination.

# We use the predict() function to get fitted values, confidence intervals
# and prediction intervals.

?predict

predict(reg1)				# Asks for just fitted values.
predict(reg1,interval="confidence") # Asks for confidence intervals too.

# Let's remove Examination, since it doesn't appear to need to be there.

reg2 <- lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, data=swiss)

# H0: no model fit improvement from reg1 (i.e. use reg2).
# Ha: model fit improvement from reg1 (i.e. use reg1).

anova(reg1,reg2)

# Use reg2.

#########################
# 1.4 Another diagnostic 
#########################

# If the model does a good job, the fitted values and fertility
# should appear to have a linear relationship.

plot(swiss$Fertility, predict(reg2),pch=16,xlab="Fertility",
     ylab="Fitted Fertility",xlim=c(35,95),ylim=c(35,95))
abline(0,1,lwd=1)
legend("topleft",c("MLE"),col=c(1),pch=c(16))

# Looks good.

###################################
# 2 Subjective vs Objective Priors
###################################

###############################
# 2.1 An Illustrative Function
###############################

# Let's generate some data from a N(True.Mean,1) for an example.
# Specifically, a sample of size Sample.Size.
# Want to estimate True.Mean, so place prior N(Prior.Mean, Prior.Variance)
# on True.Mean.

# I've written a function to do the comparisons.

compare.prior <- function(Sample.Size,True.Mean,Prior.Mean,Prior.Variance){

N <- Sample.Size
muL <- True.Mean
muP <- Prior.Mean
sig2P <- Prior.Variance

X <- rnorm(N,muL,1)

post.var = sig2P/(N*sig2P+1)
post.mean = (sig2P*sum(X)+muP)/(N*sig2P+1) 

PlotMin = min(mean(X)-3/sqrt(N),muP-3*sqrt(sig2P),post.mean-3*sqrt(post.var))
PlotMax = max(mean(X)+3/sqrt(N),muP+3*sqrt(sig2P),post.mean+3*sqrt(post.var))

c1 <- curve(dnorm(x,mean(X),1/sqrt(N)),PlotMin,PlotMax,col=1)
c2 <- curve(dnorm(x,muP,sqrt(sig2P)),col=2,add=T)
c3 <- curve(dnorm(x,post.mean,sqrt(post.var)),col=4,add=T)

yMax <- max(c1$y,c2$y,c3$y)

curve(dnorm(x,mean(X),1/sqrt(N)),PlotMin,PlotMax,col=1,ylim=c(0,yMax),ylab="Density",xlab=expression(mu),lwd=3)
curve(dnorm(x,muP,sqrt(sig2P)),col=2,add=T,lwd=3)
curve(dnorm(x,post.mean,sqrt(post.var)),col=4,add=T,lwd=3)
legend("topleft",c("Likelihood","Prior","Posterior"),col=c(1,2,4),lty=1,lwd=3)

print(post.mean)
}

# Now just change the values of the function for interesting results.

############################
# 2.2 Effect of Sample Size
############################

# See how the prior has less impact as Sample.Size increases from 15 to 100

compare.prior(15,3,0,.1)
compare.prior(100,3,0,.1)

###############################
# 2.2 Effect of Prior Variance
###############################

# See how the prior has less impact as we increase Prior.Variance

compare.prior(15,3,0,0.1)
compare.prior(15,3,0,5)

##########################
# 2.3 Strong Prior Belief
##########################

# Here I've chosen the Prior.Variance to be as strong as the likelihood.

compare.prior(25,3,0,1/25)
compare.prior(25,3,3,1/25)

########################
# 3 Bayesian Regression
########################

################
# 3.1 MCMC GLMM
################

# We're going to use the MCMCglmm library. It can handle a very large number of models and is
# regularly updated (as of this talk). Though it has so many options things may be daunting,
# It is still probably the best place to start.

library(MCMCglmm)

# MCMC stands for Markov Chain Monte Carlo. MCMCglmm uses MCMC methods to generate random draws
# from the joint posterior distribution of all the parameters. We then can summarize these 
# samples to make inference about the parameter.

# For a more in depth discussion MCMCglmm and Bayesian mixed models, see the following:

vignette("CourseNotes",package="MCMCglmm")
vignette("Overview",package="MCMCglmm")


########################
# 3.1 MCMCglmm function
########################

# The basic function for Bayesian regression is MCMCglmm(). Let's read more about it:

?MCMCglmm

# By default it assumes that the  distribution is normal (aka Gaussian) and that 
# I want to use uninformative, but proper priors. You'll see that this results 
# in estimation that is effectively equivelent to MLE regression.

reg3 <- MCMCglmm(Fertility ~ Agriculture + Examination + Education 
			+ Catholic + Infant.Mortality ,data=swiss)

# At this point the proper thing would be to check convergence diagnostics for
# the MCMC, but I'm going to put that off for later since there isn't much to 
# look at here.

# If you wanted to change the prior distribution, you do it like so:
# I have set it to the defaults.

reg3.prior <- list(B=list(mu=c(0,0,0,0,0),V=diag(10^10,5)))
reg3.1 <- MCMCglmm(Fertility ~ Agriculture + Examination + Education 
                 + Catholic + Infant.Mortality,
                 data=swiss,
                 prior=reg3.prior
                 )

#########################
# 3.2 Bayesian Inference
#########################

######################
# 3.2.1 Basic Summary
######################

# Use summary() to output a bunch of different summaries.

summary(reg3)

# pMCMC is min(P(beta>0),P(beta<0)). It is a lazy subsitute for a model 
# selection techniques. Use pMCMC to conclude maybe Examination should 
# not be in the model.

reg4 <- MCMCglmm(Fertility ~ Agriculture  + Education 
			+ Catholic + Infant.Mortality ,data=swiss)

summary(reg4)

# Notice DIC is lower for reg4 than for reg3. This is a sign that reg4 
# is a better fit than reg3. DIC is like AIC, AICc, and BIC - lower is 
# better. Also it is an asymptotic result and works best when the 
# posterior distribution is approximately multivariate normal.

######################
# 3.2.2 Density Plots
######################

# Density plots are an excellent way to visualize the posterior
# distribution of each parameter.

plot(reg4,trace=F,density=T)

########################
# 3.2.3 Point Estimates
########################

# Three popular posterior point estimates:

# Posterior mode (aka MAP)
BetaHat4.1 <- matrix(posterior.mode(reg4$Sol),5,1)
# Posterior mean (most popular)
BetaHat4.2 <- matrix(apply(reg4$Sol,2,mean),5,1)
# Posterior median
BetaHat4.3 <- matrix(apply(reg4$Sol,2,median),5,1)

# Let's compare the fits using each.

# Some operations prefer swiss to be a matrix class object 
# instead of data.frame class object.

swiss2 <- as.matrix(swiss)
colnames(swiss2) <- NULL
rownames(swiss2) <- NULL

# Now I can make the following plot:
{
win.graph()
plot(swiss$Fertility,predict(reg2),xlim=c(40,95),ylim=c(40,95),pch=20
	,ylab="Fitted Fertility",xlab="Fertility")
abline(lm(predict(reg2)~swiss$Fertility),lty=4)

points(swiss2[,1],cbind(matrix(1,47,1),swiss2[,c(2,4:6)])%*%BetaHat4.1,col=4,pch=20)
abline(lm(cbind(matrix(1,47,1),swiss2[,c(2,4:6)])%*%BetaHat4.1~swiss$Fertility),lty=4,col=4)

points(swiss2[,1],cbind(matrix(1,47,1),swiss2[,c(2,4:6)])%*%BetaHat4.2,col=3,pch=20)
abline(lm(cbind(matrix(1,47,1),swiss2[,c(2,4:6)])%*%BetaHat4.2~swiss$Fertility),lty=4,col=3)

points(swiss2[,1],cbind(matrix(1,47,1),swiss2[,c(2,4:6)])%*%BetaHat4.3,col=2,pch=20)
abline(lm(cbind(matrix(1,47,1),swiss2[,c(2,4:6)])%*%BetaHat4.3~swiss$Fertility),lty=4,col=2)

abline(0,1)
legend("topleft",c("MLE","MAP","Posterior Mean","Posterior Median"),col=c(1,4,3,2),pch=20)
}

# All methods give similar answers, the MAP being the most unlike the others.

###########################
# 3.2.4 Credible Intervals
###########################

# The summary() function returns 95% HPD intervals.

summary(reg4)

# For just the interval use HPDinterval()

HPDinterval(reg4$Sol) # Bayesian

# Confidence itervals in the frequentist paradigm can be found
# using confint().

confint(reg2)		# Frequentist

# In a frequentist paradigm, confidence intervals are interpretted as 
# long term relative frequencies, hence the moniker frequentist. That
# is, we expect the true parameter to be contained by that interval 
# 95% of the time if we repeated the experiment many times.

# In Bayesian statistics the interpretation is much simpler. We can 
# the the probability the true parameter is contained within that 
# interval is 95%. Statisticians will often call Bayesian confidence 
# intervals "credible intervals" to differentiate them from their 
# frequentist analogs.

##########################
# 3.2.5 Hypothesis Testing
##########################

# Suppose you wanted to test H0: beta1 > 0 vs H1: beta1 <=0:
# Find the average number of samples where beta1 > 0.
# This is an estimate of the P(H0).

mean(reg4$Sol[,2]>0)

# Interpret this as the probability H0 is true!
# Suppose you wanted to test H0: beta1 > 0.1 or beta1 < -0.1 vs H1: -0.1 <= beta1 <= 0.1:
# Do the same as before! Find P(H0), the proportion of samples where H0 is true.

mean(reg4$Sol[,2]>0.1 | reg4$Sol[,2]< -0.1)

####################### 
# 3.3 MCMC Diagnostics
#######################

######################### 
# 3.3.1 MCMC Diagnostics
#########################

# As said previously, we use MCMC methods to estimate the distribution of 
# our parameters. Another way to think of Monte Carlo methods is that we 
# get an random answer in a fixed amount of time. Essentially at each step
# of the algorithm a new value for each parameter is proposed (usually based
# on the value of the parameter in the previous step of the algorithm) and 
# then accepted according or rejected according to some rule. And after we 
# do this long enough, we'll start sampling from the correct posterior 
# distribution. This results in a few concerns for us:

# 1. We have not run the MCMC long enough to sample from the posterior distribution.
# 2. We are sampling from the posterior distribution, but do not have enough 
# samples to adequately summarize it.
# 3. We have highly correlated samples so we have less information in our 
# samples about the posterior than we think.

# To inspect 1 and 2, we can use trace plots:

plot(reg4,trace=T,density=F)

# We want it too look like a fuzzy catepillar without any drift or cycling.

# For inspecting 3, we can look at auto- and cross-correlation plots.

acf(reg4$Sol)

# Or we can look at the effective sample size "eff.samp" in the summary output.
# We want the eff.samp to be the same size as the number of MCMC samples used.

summary(reg4)

################################## 
# 3.3.2 Fixing Convergence Issues
##################################

# Some solutions to the three previously listed 
# problems are as follows:

# 1. Run the sampler longer and increase the "burn-in" time. Usually 
# the first few thousand samples or so are not used for estimation 
# since they aren't samples from the posterior distribution. The default
# burn-in is 3000 iterations.
# 2. Just increase the number of iterations in the sampler. The default 
# is 13000.
# 3. Instead of using every observation, use every 10th or 30th, etc. 
# This is called "thinning" and the default thinning is to keep every 
# 10th sample.

# To illustrate these issues let's look at the built-in InsectSprays 
# dataset. For more info:

?InsectSprays

# Below I've included the defaults for thinning, iterations, and burnin.

reg5 <- MCMCglmm(count ~ spray-1 ,data=InsectSprays, family="poisson",
   		thin=10,
			nitt=13000,
			burnin=3000,
			)

# For trace plots you want them to look like a "fuzzy caterpillar".

plot(reg5,trace=T,density=F)

# We only want correlation for low numbered lags. Preferably only a small amount too.

acf(reg5$Sol)

# We want the effective sample size (eff.samp) to be close to the number of
# simulations used.

summary(reg5)

# Let's run the chain longer, increase the burnin time and increase how much we thin.

reg6 <- MCMCglmm(count ~ spray-1 ,data=InsectSprays, family="poisson",
 			thin=100,
			nitt=140000,
			burnin=40000,
			)

# Most of our previous issues should have gone away by now.

plot(reg6,trace=T,density=F)
acf(reg6$Sol)
summary(reg6)

##############
# End of Talk
##############
