## Lecture 9 R script
## CSSS 508

library(MASS)
data(birthwt) 
attach(birthwt) 

table(race, low)
boxplot(age~low)
table(ht,low)

## Compute proportions falling into each cell:
prop.table(table(ht,low)
round(prop.table(table(ht,low)),2)
sum(prop.table(table(ht,low))  ## sum to 1

## compute margins of table:
?margin.table
tab=table(ht,low)
margin.table(tab,1)
margin.table(tab,2)

mod = glm(low ~ age,family=binomial)
summary(mod)

mod2=glm(low~smoke,family=binomial)
summary(mod2)
mod2

mod3=glm(low~ptl,family=binomial)
summary(mod3)
mod3

mod4=glm(low~ptl+age+as.factor(race)+smoke,
family=binomial)
summary(mod4)
mod4

## logistic regression with data in contingency table format
## MAKE SURE OUTCOME VARIABLE IS: column of # successes, column of failures:

birthwtmat = cbind (c(23,11,25), c(73, 15, 42))

## independent variable:
race.factor = factor(0:2)
levels(race.factor ) = c("white","black","other")
race.factor

mod5 = glm(birthwtmat ~ race.factor,family=binomial)
summary(mod5)

mod6 = glm(low~ factor(race),family=binomial)
summary(mod6)

## Note that estimates and standard errors are the same.  Degrees of
## freedom are different.


## CSSS 508 Lab 9

## We'll continue working with the birthwt data
# library(MASS)
# data(birthwt)
 
## Practice with logistic regression:
## Perform logistic regression of low birth weight on history of hypertension (ht).
## Compute confidence intervals for your model and interpret them.
## Create a contingency table and perform the logistic regression using the
## contingency table form of the data.  Verify that you get the same results.
 
## Pearson's chi square test is used to test whether rows and columns of 
## a table are independent.  
tab=table(low,smoke)
tab

## Is smoking status independent of low birth weight?

chisq.test(tab) ## No - we have evidence for dependence (p=0.03)

## Perform a chi square test to test whether race is independent 
## of birth weight

chisq.test(table(low, as.factor(race)))

## Not enough evidence to prove dependence.

## Perform a chi square test to test whether history of hypertension
## is independent of birth weight

chisq.test(table(low, ht))

## Not enough evidence to prove dependence.



## The chi-square statistic approaches a chi-square distribution as the 
## sample size increases, under the null hypothesis.  So the p-value is 
## only an approximation -- one which improves as sample size increases.  
## For small sample sizes, the approximation is bad, so instead you can use
## Fisher's Exact Test, which does not rely on an approximation.

tab=table(low,smoke)
fisher.test(tab)
## still find a relationship, p=0.03

## Perform Fisher's Exact Test to test whether race is independent 
## of birth weight

tab=table(low,as.factor(race))
fisher.test(tab)
## still not enough evidence to find a relationship, p=0.07

## Perform Fisher's Exact Test to test whether history of hypertension is independent 
## of birth weight

tab=table(low,ht)
fisher.test(tab)
## still not enough evidence to find a relationship, p=0.051

## Both Pearson's chi-square test and Fisher's Exact Test assume independent observations