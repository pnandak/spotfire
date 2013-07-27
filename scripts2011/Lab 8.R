## Lab 8, CSSS 508

## Multiple Regression

## We'll continue working with the "Wages" data set that we saw in lecture.
library(Ecdat)
data(Wages)
dim(Wages)
summary(Wages)

attach(Wages)
m2=lm(lwage~ed+black)
m3=lm(lwage~ed+ black + ed*black)
summary(m3)

## We'll make some plots to explore the meaning of the interaction term.

## Run the following code to plot the relationship between
## log wage and education for blacks only:

plot(lwage[black=="yes"]~ed[black=="yes"],col="red")

## Now modify the above code to plot the relationship between
## log wage and education for non-blacks only, and make the points blue.


## Type:

par(mfrow=c(1,2)) 

## so that both plots will appear on the same 
## plotting window, and modify your above code so that the two plots
## have the same x-axis limits and y-axis limits.


## Add titles to each of the two plots, and add labels for the x-axis and 
## y-axis.


## Add the appropriate linear regression line to each plot (in black)

## Based on the relationships in these plots, 
## discuss interpretation of the interaction term.

## Type:

mod=lm(lwage ~.,data=Wages)

## to perform linear regression of the log(wages) on all other
## variables in the data set.  

## Examine coefficients and p-values.  Which predictor variables
## are not significant?

## Now add an interaction between black and south to your model.

## Use anova() to test whether this improves model fit.

## State the scientific hypothesis you just tested.




