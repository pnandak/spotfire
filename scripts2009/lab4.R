# QERM 514 Lab #4 (4/19/07)

# The purpose of this lab is to cover:
# - run an ANCOVA
# - advanced plotting in S+


####################################################################
#  we'll be covering a number of new plotting functions
setwd("c://qerm514")
# First, download the data frame "grazing.dat" from website
grazing.df<-read.table("grazing.dat")
names(grazing.df)<-c("fruit","roots","grazetrt")
grazing.df$grazetrt<-as.factor(grazing.df$grazetrt)
attach(grazing.df)

# when you have a datset with more than one predictor, first plot the response vs. predictor one by one.
win.graph() # open a new graph for you
par(mfrow=c(1,2))
# first plot two continuous variable
plot(roots, fruit, xlab="root size", ylab="friut production")
# then plot continuous variable vs categorical variable, boxplot is usually the choice in this situation
plot.factor(grazetrt,fruit)
coplot(fruit~roots|grazetrt)
# what can we get from these plots? --- choosing predictor!
#the response variable has relationship with which variable?

# Can we plot all the data on one graph?
par(mfrow=c(1,1))
plot(roots[grazetrt=="1"], fruit[grazetrt=="1"],xlim=c(min(roots), max(roots)), ylim=c(min(fruit),max(fruit)), type="p",pch=1,xlab="root size", ylab="fruit production")
points(roots[grazetrt=="2"], fruit[grazetrt=="2"],pch=2,col=2)
# Or easier:
plot(roots, fruit, pch=as.numeric(grazetrt), col=as.numeric(grazetrt))
# Notes: 
# 1) the 'type' variable allows you to select "p" for points, "l" for lines, 
#   "b" for both, or other options.  See ?par for more info.  
# 2) points() allows you to add new points to an existing plot. See also lines()
# 3) pch = X allows you to change the plotted character.  See ?par for more 

# what can we get from the plot? --- observe interaction!
#does the two groups have interaction?

# Let's add the regression lines to the graph.  
# First create the linear model without interaction 
grazing.lm1<-lm(fruit~roots+grazetrt)
#  the coefficients for the given model:
(grazing.coef<-grazing.lm1$coef)

# and then use abline to draw lines for the regression from each group
abline(a=grazing.coef[1], b=grazing.coef[2], lty=1)  
abline(a=grazing.coef[1]+grazing.coef[3], b=grazing.coef[2], lty=2,col=2)
# In this case I'm changing the intercept for each new line, but not the slope.

#if we hope to have different slope for two groups, what would we do?
# First fit a model with interaction
grazing.lm2<-lm(fruit~roots*grazetrt)
#  the coefficients for the given model:
(grazing.coef2<-grazing.lm2$coef)

abline(a=grazing.coef2[1], b=grazing.coef2[2], lty=3,col=3)  
abline(a=grazing.coef2[1]+grazing.coef2[3], b=grazing.coef2[2]+grazing.coef2[4],  lty=4,col=4)
legend("topleft",legend=c("ungrazed w/o interaction","grazed w/o interaction","ungrazed w/ interaction","grazed w/ interaction"),lty=c(1,4), col=1:4)

# In this case, what model do we use? with or without interaction?

####################################################################
# First, generate the data
# Generate the predictor (Xi) data using a uniform dist 
	predictor <- runif(30, 4, 10)
	
	# Generate the error (Ei) distribution.  
	e <- rnorm(30, mean=0, sd=1)

	# Generate the factor data, and randomize it
	treatment <- sample(c(rep("A", (30/3)), rep("B", (30/3)), rep("C", (30/3))))
b0<-2
b1<-4
bs<-c(2, 0)
response <- vector("numeric", 30)
	for (i in 1:30) {
		if (treatment[i]=="A")
			response[i]<- b0 + b1 * predictor[i] + e[i]
		else if (treatment[i]=="B")
			response[i]<- b0 -2 + b1 * predictor[i] * bs[1] + e[i]
		else
			response[i]<- b0 +4 + b1 * predictor[i] * bs[2] + e[i]
	}


#The example below assumes you have stored it in my.data
my.data<-data.frame(predictor,response,treatment)

# First analyze the data, ignoring the factor variable  
plot(my.data$predictor, my.data$response, xlab="Predictor", ylab="Response")
my.lm1 <- lm(response~predictor, data=my.data)
abline(my.lm1)
anova(my.lm1)
# This method (i.e. first creating the linear model and then calling ANOVA() 
#  gives comparable results to that described in the notes.  Since we're using
#  the results of the linear model anyway, to me, this way is easier.  
#  To compare, the other method is:
summary(aov(response~predictor, data=my.data))

# Clearly there is a strong linear relationship between y and x, but can we 
#  do better?  What is the MSE for this model?

# So, now let's look at the data based on the factor.  Start by computing the 
#  means of each group
tapply(my.data$response, my.data$treatment, mean)

# Now do a a box plot of the response sorted by each group
plot(my.data$treatment, my.data$response, name=my.data$treatment,	xlab="Group", ylab="Response")

# Now to plot all of the data - 

plot(my.data$predictor, my.data$response, xlab="Predictor", ylab="Response", col=as.numeric(my.data$treatment),type="n")
text(my.data$predictor, my.data$response, my.data$treatment)


# Create the simple linear model 
my.lm2 <- lm(response~predictor+treatment, data=my.data)
anova(my.lm2)
summary(my.lm2)
# Note the values of the coefficients (intercept), f1, and f2
# How does the MSE of this model compare with our first version?

 
#  (Intercept) corresponds to the intercept for group A only, 
#  fB is the offset from (Intercept) for group B, and 
#  fC is the offset from (Intercept) for group C, and 

# Let's add the regression lines to the graph.  First, fetch (and look at) 
#  the coefficients for the given model:
(my.coef<-my.lm2$coef)

# and then use abline to draw lines for the regression from each group
abline(a=my.coef[1], b=my.coef[2], lty=1)  # adds the default (i.e. group A) regression line
abline(a=my.coef[1]+my.coef[3], b=my.coef[2], lty=2,col=2)
abline(a=my.coef[1]+my.coef[4], b=my.coef[2], lty=3,col=3)
# In this case I'm changing the intercept for each new line, but not the slope.

###################################
#Model with interaction
######################################
my.lm3 <- lm(response~predictor*treatment, data=my.data)
anova(my.lm3)
summary(my.lm3)
# Note the values of the coefficients (intercept), f1, and f2
# How does the MSE of this model compare with our first version?


# Let's add the regression lines to the graph.  First, fetch (and look at) 
#  the coefficients for the given model:
(my.coef<-my.lm3$coef)

# and then use abline to draw lines for the regression from each group
abline(a=my.coef[1], b=my.coef[2], lty=4,col=4)  # adds the default (i.e. group A) regression line
abline(a=my.coef[1]+my.coef[3], b=my.coef[2]+my.coef[5], lty=5,col=5)
abline(a=my.coef[1]+my.coef[4], b=my.coef[2]+my.coef[6], lty=6,col=6)

legend("topleft",legend=c("A w/o interaction","B w/o interaction","C w/o interaction","A w/ interaction","B w/ interaction","C w/ interaction"),lty=c(1,6), col=1:6)
