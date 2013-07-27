

############################################################
############################################################
###                                                      ###
###         Bayesian Methods for Regression in R         ###
###                     Nels Johnson                     ###
### Laboratory for Interdiciplinary Statistical Analysis ###
###       Department of Statistics, Virginia Tech        ###
###                                                      ###
############################################################
############################################################


################
# 1.1 Libraries
################

# The only library we'll be using today is MCMCglmm.

library(MCMCglmm)

# For more libraries relating to Bayes see:

# http://cran.r-project.org/web/views/Bayesian.html

######################
# 1.2 MLE Regression #
######################

# To introduce regression, here is a simple example using the swiss 
# built-in dataset.

?swiss
head(swiss)

# It's always good to plot the data first, to get an idea of what is
# going on.

pairs(swiss, panel = panel.smooth, main = "swiss data", 
col = 3 + (swiss$Catholic > 50))

# The basic function for regression in R is lm().

?lm

# It handles regression when normal errors are assumed.

reg1 <- lm(Fertility ~ . ,data=swiss)

# The . tell tells R to use all other columns besides Fertility as 
# X variables in the model.

# We can use the summary() function to return the regression results.

summary(reg1)

# We can use the plot() function on lm objects to plot regression 
# diagnostics.

win.graph() 		# New graph window.
par(mfrow=c(2,2))		# Makes 4 panels for graphs in 1 graph.
plot(reg1)			# Produces 4 regression diagnostic plots.

# We use the predict() function to get fitted values, confidence intervals
# and prediction intervals.

?predict

predict(reg1)				# Asks for just fitted values.
predict(reg1,interval="confidence") # Asks for confidence intervals too.

plot(swiss$Fertility, predict(reg1),pch=16)
legend("topleft",c("MLE"),col=c(1),pch=c(16))

# It looks like we may not need Examination.

reg2 <- lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, data=swiss)

# H0: no model fit improvement from reg1 (i.e. use reg2).
# Ha: model fit improvement from reg1 (i.e. use reg1).

anova(reg1,reg2)

# Use reg2.

##########################################
# 1.3 Informative vs Uninformative Priors
##########################################

# Here's an illustrative function.

# Let's generate some data from a N(True.Mean,1) for an example.
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

curve(dnorm(x,mean(X),1/sqrt(N)),PlotMin,PlotMax,col=1,ylim=c(0,yMax),ylab="Density")
curve(dnorm(x,muP,sqrt(sig2P)),col=2,add=T)
curve(dnorm(x,post.mean,sqrt(post.var)),col=4,add=T)
legend("topleft",c("Likelihood","Prior","Posterior"),col=c(1,2,4),lty=1)

}

# Now just change the values of the function for interesting results.

# See how the prior has less impact as sample size increases from 5 to 50

compare.prior(25,3,0,.1)
compare.prior(250,3,0,.1)

# See how the prior has less impact as we increase its variance

compare.prior(25,3,0,.1)
compare.prior(25,3,0,5)


##########################
# 1.4 Bayesian Regression
##########################

#########################
# 1.4.1 Basic Estimation
#########################


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

# The basic function for Bayesian regression is MCMCglmm(). Let's read more about it:

?MCMCglmm

# By default it assumes that the  distribution is normal (aka Gaussian) and that I want to use
# uninformative, but proper priors. You'll see that this results in estimation that is effectively
# equivelent to MLE regression.

reg3 <- MCMCglmm(Fertility ~ Agriculture + Examination + Education 
			+ Catholic + Infant.Mortality ,data=swiss)
summary(reg3)

# Use pMCMC to conclude maybe Examination should not be in the model.

reg4 <- MCMCglmm(Fertility ~ Agriculture  + Education 
			+ Catholic + Infant.Mortality ,data=swiss)

summary(reg4)

# Notice DIC is lower for reg4 than for reg3. This is a sign that reg4 is a better fit than reg3.
# DIC is like AIC, AICc, and BIC - lower is better. Also it is an asymptotic result and works best
# when the posterior distribution is approximately multivariate normal.

#
plot(reg4)

# Three popular posterior point estimates:

BetaHat4.1 <- matrix(posterior.mode(reg4$Sol),5,1) 	# Posterior mode (aka MAP).
BetaHat4.2 <- matrix(apply(reg4$Sol,2,mean),5,1)	# Posterior mean (most popular).
BetaHat4.3 <- matrix(apply(reg4$Sol,2,median),5,1)	# Posterior median.


# Some operations prefer swiss to be a matrix class object instead of data.frame class object.

swiss2 <- as.matrix(swiss)
colnames(swiss2) <- NULL
rownames(swiss2) <- NULL

# Now I can make the following plot

{
plot(swiss$Fertility,predict(reg2),xlim=c(40,95),ylim=c(40,95),pch=16
	,ylab="Fitted Fertility",xlab="Fertility")
abline(lm(predict(reg2)~swiss$Fertility),lty=4)

points(swiss2[,1],cbind(matrix(1,47,1),swiss2[,c(2,4:6)])%*%BetaHat4.1,col=4,pch=16)
abline(lm(cbind(matrix(1,47,1),swiss2[,c(2,4:6)])%*%BetaHat4.1~swiss$Fertility),lty=4,col=4)

points(swiss2[,1],cbind(matrix(1,47,1),swiss2[,c(2,4:6)])%*%BetaHat4.2,col=3,pch=16)
abline(lm(cbind(matrix(1,47,1),swiss2[,c(2,4:6)])%*%BetaHat4.2~swiss$Fertility),lty=4,col=3)

points(swiss2[,1],cbind(matrix(1,47,1),swiss2[,c(2,4:6)])%*%BetaHat4.3,col=2,pch=16)
abline(lm(cbind(matrix(1,47,1),swiss2[,c(2,4:6)])%*%BetaHat4.3~swiss$Fertility),lty=4,col=2)

abline(0,1)
legend("topleft",c("MLE","MAP","Posterior Mean","Posterior Median"),col=c(1,4,3,2),pch=16)
}

# Again, this illustrates that in this simple example our estimates are effectively the same.

########################### 
# 1.4.2 Bayesian Inference
###########################

# Naturally we will want to make inference with our model. If we look at the output again

summary(reg4)

# we can see that there is a 95% confidence interval for each parameter.

# In frequentist statistics confidence intervals are interpretted as long term relative 
# frequencies, hence the moniker frequentist. That is, we expect the true parameter to be
# contained by that interval 95% of the time if we repeated the experiment many times.

# In Bayesian statistics the interpretation is much simpler. We can the the probability the
# true parameter is contained within that interval is 95%. Statisticians will often call Bayesian
# confidence intervals "credible intervals" to differentiate them from their frequentist analogs.

HPDinterval(reg4$Sol)	# Bayesian
confint(reg2)		# Frequentist

# We can see they give very similar answers, but for more complicated situations this will just
# not be the case. This is due to the prior and how nuisance variables are treated.

# We could very easily get 90% CI by doing the following:

HPDinterval(reg4$Sol,.90)
confint(reg2,level=.90)

# Note: HPD stands for "Highest Posterior Density," which is the smallest credible interval
# obtainable for fixed posterior probability, e.g., .95, .90, etc.

# It's very easy to make inference about hypotheses that can be framed as intervals in Bayesian
# statistics. For instance, H0: 55 < intercept < 65, Ha:  intercept >= 65 or intercept <= 55.

# reg4$Sol contains the samples for the regression parameters

reg4$Sol[1:10,]

# To find P(H0) we just find the proportion of our samples that are true under H0.

mean(reg4$Sol[,1] > 55 & reg4$Sol[,1] < 65)

# We can get a plot of the estimated distribution of our parameters using the plot() function.

plot(reg4)

# This will output not only the estimated density, but a diagnostic plot called a "trace plot." More
# on that in section 1.4.3.

##################### 
# 1.4.3 Diagnostics
#####################

# As said previously, we use MCMC methods to estimate the distribution of our parameters. Another
# way to think of Monte Carlo methods is that we get an random answer in a fixed amount of time.
# Essentially at each step of the algorithm a new value for each parameter is proposed (usually
# based on the value of the parameter in the previous step of the algorithm) and then accepted 
# according or rejected according to some rule. And after we do this long enough, we'll start
# sampling from the correct posterior distribution. This results in a few concerns for us:

# 1. We have not run the MCMC long enough to sample from the posterior distribution.
# 2. We are sampling from the posterior distribution, but do not have enough samples to adequately
# summarize it.
# 3. We have highly correlated samples so we have less information in our samples about the posterior
# than we think.

# Some solutions to these problems are as follows:

# 1. Run the sampler longer and increase the "burn-in" time. Usually the first few thousand samples
# or so are not used for estimation since they aren't samples from the posterior distribution. The
# default burn-in is 3000 iterations.
# 2. Just increase the number of iterations in the sampler. The default is 13000.
# 3. Instead of using every observation, use every 10th or 30th, etc. This is called "thinning" and
# the default thinning is to keep every 10th sample.

# Illustrate these issues let's look at the built-in InsectSprays dataset. For more info

?InsectSprays

# Below I've included the defaults for thinning, iterations, and burnin.

reg5 <- MCMCglmm(count ~ spray-1 ,data=InsectSprays, family="poisson",
 			thin=10,
			nitt=13000,
			burnin=3000,
			)

# For trace plots you want them to look like a "fuzzy caterpillar".

plot(reg5)

# We only want correlation for low numbered lags. Preferably only a small amount too.

acf(reg5$Sol)

# We want the effective sample size (eff.samp) to be close to the sample size.

summary(reg5)

# Let's run the chain longer, increase the burnin time and increase how much we thin.

reg6 <- MCMCglmm(count ~ spray-1 ,data=InsectSprays, family="poisson",
 			thin=100,
			nitt=140000,
			burnin=40000,
			)

# Most of our previous issues should have gone away by now.

plot(reg6)
acf(reg6$Sol)
summary(reg6)

############################ 
# 1.4.4 Computing Contrasts
############################

# ANOVA-type models, such as this one, can can be rather difficult to estimate well using MCMC
# methods depending on how the model is parameterized.

# Now that we have our samples for each treatment effect, finding the posterior distribution of
# contrasts we wish to estimate can be done by performing the appropriate functions on the 
# samples of our treatment effects.

# For example, to find the contrast for sprayA - sprayB we do the following:

post.cont <- matrix(0,1000,choose(6,2))
post.cont.names <- c()
k <- 1
for(j in 1:5){
 	for(j2 in (j+1):6){
		post.cont[,k] <- reg6$Sol[,j]-reg6$Sol[,j2]
		post.cont.names[k] = paste(colnames(reg6$Sol)[j],"-",colnames(reg6$Sol)[j2])
		k <- k+1
	}
}
colnames(post.cont) <- post.cont.names
head(post.cont)
post.cont <- as.mcmc(post.cont)

HPDinterval(post.cont)
exp(HPDinterval(post.cont))

############################ 
# 1.4.5 Informative Priors
############################

# Let's use the swiss dataset again. You may have noticed from the pairs() plots that
# the sign of our effects is not what it should be.

pairs(swiss, panel = panel.smooth, main = "swiss data",
      col = 3 + (swiss$Catholic > 50))

# This is due to correlation between our covariates. Let's put a more informative 
# prior on the betas in hopes to shrink their covariance a priori.

reg7.prior <- list(B=list(mu=c(0,0,0,0,0),V=diag(36,5)))

# When using shrinkage priors, they work best for centered and scales variables.

swiss2 <- swiss
swiss2[,2:6] <- data.frame(scale(swiss[,2:6]))

reg7 <- MCMCglmm(Fertility ~ Agriculture  + Education 
			+ Catholic + Infant.Mortality,data=swiss2,
			prior=reg7.prior
)

# Let's redo the old analysis.

reg8 <- MCMCglmm(Fertility ~ Agriculture  + Education 
			+ Catholic + Infant.Mortality,data=swiss2
)

summary(reg7)
summary(reg8)

# It's actually very difficult to use this as a remedial measure in this case, but
# it is still illustrative of setting informative priors. That is, poor selection of
# informative priors ends with bad posterior estimates! So be careful.

plot(swiss2[,1],predict(reg8) ,xlim=c(40,95),ylim=c(40,95),pch=16
	,ylab="Fitted Fertility",xlab="Fertility")
abline(lm(predict(reg8)~swiss2$Fertility),lty=4)

points(swiss[,1],predict(reg7),col=2,pch=16)
abline(lm(predict(reg7)~swiss2$Fertility),lty=4,col=2)

abline(0,1)
legend("topleft",c("Non Informative Prior","Informative Prior"),col=c(1,2),pch=16)


