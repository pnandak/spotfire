## Lecture 8 R script
## CSSS 508

data(LifeCycleSavings)
attach(LifeCycleSavings)
head(LifeCycleSavings)
rownames(LifeCycleSavings)
summary(LifeCycleSavings)
plot(LifeCycleSavings)

attach(LifeCycleSavings)
mod1=lm(sr ~ pop15)
summary(mod1)

mod2 = lm(sr ~ pop15 + pop75)
## Does model 2 have more predictive power than model 1?
anova(mod1, mod2)

mod3 = lm(sr ~ pop15 + dpi)
anova(mod1, mod3)

mod4 = lm(sr ~ pop15 + ddpi )
anova(mod1, mod4)

## As long as the models are nested, they can have arbitrary #s of variables:
mod4 = lm(sr ~ pop15 + dpi + ddpi)
anova(mod4, mod3)

## Added variable plots.
## To see relationship between sr and pop75, controlling for pop15, first regress each of 
## sr and pop75 separately on pop15: 

mod1=lm(sr~pop15)
mod6=lm(pop75~pop15)
plot(resid(mod1)~resid(mod6))
abline(lm(resid(mod1)~resid(mod6)))

## Add a smoother to the plot to investigate
## whether the relationship is linear:

lines(lowess(resid(mod1)~resid(mod6))

## relationship between sr and dpi, controlling for pop15:
mod1=lm(sr~pop15)
mod6=lm(dpi~pop15)
plot(resid(mod1)~resid(mod6))
lm(resid(mod1)~resid(mod6))
lm(sr~pop15+dpi)

## relationship between sr and ddpi, controlling for pop15:
mod1=lm(sr~pop15)
mod6=lm(ddpi~pop15)
plot(resid(mod1)~resid(mod6))
lm(sr~pop15+ddpi)

## Does adding dpi+ddpi significantly improve model fit to sr~pop15?
 mod1=lm(sr~pop15)
 mod8=lm(sr~pop15+dpi+ddpi)
 anova(mod1,mod8)

## compute AIC: (smaller means better fit)
extractAIC(mod1)

## compute BIC (smaller is better):
n=dim(LifeCycleSavings)[1]  ## number of rows
extractAIC(mod1, k=log(n))

detach(LifeCycleSavings)

## Interaction terms
library(Ecdat)
data(Wages)
dim(Wages)
summary(Wages)
attach(Wages)

## regress log wages on education level
m1 = lm(lwage~ed)

## Add black to the model
m2=lm(lwage~ed+black)

anova(m1,m2) ## adding black improves model fit

m3=lm(lwage~ed+ black + ed*black)

anova(m3,m2) ## adding the interaction improves model fit.

summary(m3)
## The predicted log(wages) for blacks with no years of education is 0.49 less than that
## for whites with no years of education.  

## The predicted log(wages) for blacks with one year of education is 0.47 less than that
## for whites with one year of education.  

## The predicted log(wages) for blacks with k years of education is (0.49 - k*0.02) less than that
## for whites with k years of education.  

## Note: R automatically creates dummy variables when your variables are coded as factors.  
## R chooses the first category (alphabetically) as the reference category.

## You can change the reference category with relevel():
black=relevel(black, ref="yes")
lm(lwage~black)



############  Lab 8 ############  

## Multiple Regression

## We'll make some plots to explore the meaning of the interaction term.

## Run the following code to plot the relationship between
## log wage and education for blacks only:

plot(lwage[black=="yes"]~ed[black=="yes"],col="red")

## Now modify the above code to plot the relationship between
## log wage and education for non-blacks only, and make the points blue.

plot(lwage[black=="no"]~ed[black=="no"],col="blue")


## Type:

par(mfrow=c(1,2)) 

## so that both plots will appear on the same 
## plotting window, and modify your above code so that the two plots
## have the same x-axis limits and y-axis limits.


par(mfrow=c(1,2))
plot(lwage[black=="yes"]~ed[black=="yes"],col="red",ylim=c(4.5,9))
plot(lwage[black=="no"]~ed[black=="no"],col="blue",ylim=c(4.5,9))


## Add titles to each of the two plots, and add labels for the x-axis and 
## y-axis.

par(mfrow=c(1,2))
plot(lwage[black=="yes"]~ed[black=="yes"],col="red",ylim=c(4.5,9),
main="Relationship between log(Wages) \n and Education for blacks",cex.main=.85,ylab="log(Wages)",
xlab="Education")

plot(lwage[black=="no"]~ed[black=="no"],col="blue",ylim=c(4.5,9),
main="Relationship between log(Wages) \n and Education for non-blacks",cex.main=.85,ylab="log(Wages)",
xlab="Education")

## Add the linear regression line to each plot (in black)
plot(lwage[black=="yes"]~ed[black=="yes"],col="red",ylim=c(4.5,9),
main="Relationship between log(Wages)
 and Education for blacks",cex.main=.85,ylab="log(Wages)",
xlab="Education")
abline(lm(lwage[black=="yes"]~ed[black=="yes"]))

## Add a smoother if you like:
## lines(lowess(lwage[black=="yes"]~ed[black=="yes"]))

plot(lwage[black=="no"]~ed[black=="no"],col="blue",ylim=c(4.5,9),
main="Relationship between log(Wages)
 and Education for non-blacks",cex.main=.85,ylab="log(Wages)",
xlab="Education")

abline(lm(lwage[black=="no"]~ed[black=="no"]))
## lines(lowess(lwage[black=="no"]~ed[black=="no"]))

## Based on the relationships in these plots, 
## discuss interpretation of the interaction term.




## Wages increase with education for both whites and blacks.  At any of the 
## observed values of education, whites have higher wages than blacks.  However
## as education increases, the magnitude of the racial disparity decreases.  

## Let's put them all on a single plot to better see the relationships:
par(mfrow=c(1,1))

plot(lwage[black=="no"]~ed[black=="no"],col="blue",ylim=c(4.5,9),
main="Relationship between log(Wages) and Education",ylab="log(Wages)",
xlab="Education")

points(lwage[black=="yes"]~ed[black=="yes"],col="red")
abline(lm(lwage[black=="yes"]~ed[black=="yes"]), col="green",lwd=2)
abline(lm(lwage[black=="no"]~ed[black=="no"]),col="brown",lwd=2)

legend(4,9,c("Non-Black","Black"),col=c("brown","green"),lwd=2)

## The interaction term represents the difference in the slopes of the two lines.

## Type:

mod=lm(lwage ~.,data=Wages)

## to perform linear regression of the log(wages) on all other
## variables in the data set.  

## Examine coefficients and p-values.  Which predictor variables
## are not significant?

summary(mod)

## They are all significant!

## Now add an interaction between black and south to your model.
mod2=lm(lwage~exp+wks+bluecol+ind+south+smsa+married+sex+union+ed+black+black*south)
summary(mod2)

## Use anova() to test whether this improves model fit.
anova(mod,mod2)  #it doesn't

## State the scientific hypothesis you just tested.
## Is the association between black and log(wages) different in the south than in the north?

