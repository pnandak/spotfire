# Script for lab #7 (May 10, 2007)

# The purpose of this lab will be to play more with glms using the 
#  binomial family, including computing the leverage values, and 
#  more plotting of the data with its predicted values, and looking
#  at residual plots

setwd("C://courses//5142007")
# Load in the data and assign column names
leuk.df<-read.table("leuk.dat")
names(leuk.df)<-c("wbcount", "agstatus", "surv", "n")
prop<-leuk.df$surv/leuk.df$n
leuk.df<-cbind(leuk.df, prop)
names(leuk.df)[5]<-"prop"
leuk.df$agstatus<-as.factor(leuk.df$agstatus)

# Ok, start by plotting the data
plot(leuk.df$wbcount, leuk.df$prop,  
	xlab="White Blood Cell Count /10", ylab="Death Rate",
      pch=as.numeric(leuk.df$agstatus), col= as.numeric(leuk.df$agstatus))

# Now Create a rough guess at a logistic model

# using the method from the notes:
leuk.glm<-glm(prop ~ wbcount + agstatus, family=binomial(link=logit), data=leuk.df, 
	weights=n, control=list(epsilon=0.001, maxit=50, trace=F))
# or using a simpler function call:
leuk.glm<-glm(prop ~ wbcount + agstatus, family=binomial, data=leuk.df, weights=n)

#get the hat matrix and cook's distance
influence(leuk.glm)$hat
cooks.distance(leuk.glm)
# So, it seems that point 15 may be an outlier

###############################################################################
# Now lets look at the results of a number of different models
# These simulations are modeled after those in the notes
leuk.glm1<-glm(prop ~ wbcount + agstatus, family=binomial, data=leuk.df, weights=n)

# Since we're comparing the residual deviance levels, use anova()
anova(leuk.glm1, test="Chisq")

# Does order or the predictor variables matter?
leuk.glm2<-glm(prop ~ agstatus + wbcount, family=binomial, data=leuk.df, weights=n)
anova(leuk.glm2, test="Chisq")

# Add an interaction in the preditor term, is it significant?
leuk.glm3<-glm(prop ~ agstatus * wbcount, family=binomial, data=leuk.df, weights=n)
anova(leuk.glm3, test="Chisq")

# Replace the wbcount by log(wbcount), plus interaction
leuk.glm4<-glm(prop ~ agstatus * log(wbcount), family=binomial, data=leuk.df, 
	weights=n)
anova(leuk.glm4, test="Chisq")

# Same, but no interaction term
leuk.glm5<-glm(prop ~ agstatus + log(wbcount), family=binomial, data=leuk.df, 
	weights=n)
anova(leuk.glm5, test="Chisq")

#get the hat matrix and cook's distance
influence(leuk.glm5)$hat
cooks.distance(leuk.glm5)
# So, it seems that point 15 may be OK now


# instead of retyping all of this, you can use the update() command,
#  and see also the addterm() and dropterm() functions when dealing
#  with more than two predictor variables... but for example if we 
#  start with the first model above, we could use
leuk.glm3<-update(leuk.glm1, ~ . + wbcount:agstatus)

# Ok, now can we plot these result?  Lets try the method from last week
wbc.range<-seq(from=0, to=10000, by=100)
lines(wbc.range, predict(leuk.glm, data.frame(wbcount=wbc.range, 
	agstatus=as.factor(rep(1, length(wbc.range)))), type="response"), lty=2, col=2)
lines(wbc.range, predict(leuk.glm, data.frame(wbcount=wbc.range, 
	agstatus=as.factor(rep(0, length(wbc.range)))), type="response"), lty=3, col=3)
	
# But, the graphs in the notes use a log predictor scale... so let's redo 
#  the original plots
# Ok, start by plotting the data
plot(leuk.df$wbcount, leuk.df$prop, type="n", 
	xlab="White Blood Cell Count /10", ylab="Death Rate", log="x")
points(leuk.df$wbcount[leuk.df$agstatus==0], leuk.df$prop[leuk.df$agstatus==0], 
	pch=1)
points(leuk.df$wbcount[leuk.df$agstatus==1], leuk.df$prop[leuk.df$agstatus==1], 
	pch=2)
lines(wbc.range, predict(leuk.glm, data.frame(wbcount=wbc.range, 
	agstatus=as.factor(rep(1, length(wbc.range)))), type="response"), lty=2, col=2)
lines(wbc.range, predict(leuk.glm, data.frame(wbcount=wbc.range, 
	agstatus=as.factor(rep(0, length(wbc.range)))), type="response"), lty=3, col=3)
# Now this looks better (somewhat!?!)

# What if we want to redo the analysis with case 15 modified?  Just create 
#  a new data frame, say leuk2.df and manually modify the point so that the
#  y variable is now 0.  Then, rerun the analysis.   
leuk2.df<-leuk.df
leuk2.df[15,]$surv<-0
leuk2.df[15,]$prop<-0
leuk2.glm<-glm(prop ~ wbcount + agstatus, family=binomial, data=leuk2.df, weights=n)

lines(wbc.range, predict(leuk2.glm, data.frame(wbcount=wbc.range, 
	agstatus=as.factor(rep(1, length(wbc.range)))), type="response"), lty=4, col=2)
lines(wbc.range, predict(leuk2.glm, data.frame(wbcount=wbc.range, 
	agstatus=as.factor(rep(0, length(wbc.range)))), type="response"), lty=4, col=3)

# Lastly, let's look at some residual plots...
win.graph()		# opens a new plotting window
par(mfrow=c(1, 2))
plot(fitted(leuk.glm), resid(leuk.glm, type = "response"), 
	ylab="response residuals") 
	
# Or do a qqnorm plot
qqnorm(resid(leuk.glm), type="pearson", ylab="Pearson Residuals")
qqline(resid(leuk.glm), type="pearson")

#############################################################################
# Now a quick introduction into Poisson glms and more examples of update() and
#  addterm(), using another data set from V&R.  See Section 7.3 page 200
library(MASS)
names(housing)
# This data set concerns a four-way classification of 1681 householders in 
#  Copenhagen who were surveyed on the type (Type) of rental accomodation they 
#  occupied, the degree of contact (Cont) they had with other residents, their 
#  feeling of influence (Infl) on apartment management and their level of 
#  satisfaction (Sat) with their housing conditions.  Freq is the number of 
#  people in each category

# As a first model: 
house.glm0<-glm(Freq~Infl*Type*Cont+Sat, family=poisson, data=housing)
summary(house.glm0, cor=F)		# just suppress the correlation output

# The high residual deviance suggests that this simple model is inadequate

# So look at adding another term to the model... This command doesn't add it,
#  but displays the hypothetical results of adding any of the given terms.
# Note that addterm() is a function from MASS
addterm(house.glm0, ~. + Sat:(Infl+Type+Cont), test="Chisq")

# In this case, look at the Deviance column - this is the resid deviance 
#  achieved by adding each of these terms individually.

# Since all the terms look atleast borderline significant, let's add them to 
#  the model:
house.glm1<-update(house.glm0, ~.+ Sat:(Infl+Type+Cont))
summary(house.glm1, cor=F)

# This clearly looks much better!  

# Just as an introduction to the dropterm() command, first look at the help
?dropterm
# then as an example...  the possible single terms which could be dropped
#  are displayed
dropterm(house.glm1, test="Chisq")

#############################################################################



