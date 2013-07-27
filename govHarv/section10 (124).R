############################################

#   Goverment 1001
#   Section 10 - Logistic Regression
#                 
#   April 26, 2007
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################

#   Goals for today:

#   1) Component-Residual Plots

#   2) What are odds?  What is a logistic curve?

#   3) Logistic Regression

############################################


#   1) Component-Residual Plots

#   We have one last diagnostic tool for linear regression that 
#   we need to look at: the component-residual plot.  This is another
#   tool for looking at the linearity assumption in multiple regression.
#   remember that we don't need the relationship between Y and each X to 
#   be linear in a bivariate scatterplot; what we need is for them to be 
#   linear *after* we control for the other variables in the model.

#   Here is an example with some fake data:

XX <- rnorm(200)
ZZ <- XX^2 + rnorm(200, sd=.1)
YY <- -1 + 2*XX + -1*ZZ + rnorm(200)

#   Here are the bivariate scatterplots

plot(YY ~ XX)
plot(YY ~ ZZ)

#   These don't look so good, but we know that the data was generated
#   by a linear model, so is there some way that we can visualize it?
#   We could try a 3d scatterplot:

library(lattice)
cloud(YY ~ XX + ZZ, screen=c(10, 0, 0))

#   But even this is hard to see unless you get the perspective just right.

#   A better way in this case, particularly as the dimensions increase, is to 
#   use the CR plots in the car library.  First you fit the regression model
#   that you want to check:

lmout <- lm(YY ~ XX + ZZ)

#   Then you use the cr.plots() function:

cr.plots(lmout)

#   This is one case where the added variable plots don't help you:

av.plots(lmout)

#   You are looking for the same things that we look for in the bivariate case;
#   a linear relationship between the component plus residual and the variable
#   that you are looking at.

#   As with most diagnostics, if it looks bad, you have a problem, but if it
#   doesn't look bad, you still might have a problem.


#   2) What are odds?  What is a logistic curve?

#   What are odds?  You hear them a lot if you are interested in betting.
#   What does it mean if the odds are 2:1?  What about 5:4?  

#   [look at william hill]

#   What is an odds ratio?  It is just the ratio of two odds (surprise!).
#   So if the odds of Hillary being the democratic nominee are 3.25:1 and 
#   the odds of Mitt being the republican nominee are 9:1, what is the odds 
#   ratio?

#   What is true if the odds ratio = 1?  What is true if the log odds ratio 
#   is 0?

log(1)

#   We are going to write down a model where the log of the odds ratio is 
#   a linear function of some X variables.

#   Let's say logit(pi) = 2 + 3*XX

#   Here is a plot of the log odds as a function of XX:

XXa <- sort(XX)
plot(XXa, 2 + 3*XXa, type = "l")

#   And here are the odds:

plot(XXa, exp(2 + 3*XXa), type = "l")

#   And here is the probability:

plot(XXa, exp(2 + 3*XXa)/(1 + exp(2 + 3*XXa)), type = "l")



#   3) Logistic Regression

#   Fitting a logistic regression is easy; it is almost the same as
#   a linear regression, but we have to make two changes: use the glm() 
#   function instead of the lm() function, and add the family="binomial" 
#   argument inside the function.

load("H:/vote04.RData")
head(vote04)

lmout <- lm(bush ~ partyid)
summary(lmout)

glmout <- glm(bush ~ partyid, family = binomial)
summary(glmout)

#   The interpretation of much of the output is the same; we still have point 
#   estimates, standard errors, and p-values.  Note that R^2 has been replaced
#   by the AIC that we talked about in lecture today.

n <- length(bush)
B <- 100
newideo <- data.frame(afram=0, female = 1, ideology=seq(0,1, by=.05))

plot(jitter(ideology),jitter(bush))

for(bsam in 1:B){
	boot.ind <- sort(sample(1:n,replace=TRUE))
	glmBoot.out <- glm(bush~ideology + afram + female + afram*female,family=binomial(),data=vote04[boot.ind,])
	lines(seq(0,1, by=.05),predict(glmBoot.out,newdata=newideo,type="response"),type="l",col="yellow")
	}
lines(seq(0,1, by=.05),predict(glm1.out,newdata=newpid,type="response"),type="l",col="red")
