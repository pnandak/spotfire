## CSSS 508 Lab 9


## We'll continue working with the birthwt data
library(MASS)
data(birthwt)



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

chisq.test(tab)

## Perform a chi square test to test whether race is independent 
## of birth weight


## Perform a chi square test to test whether history of hypertension
## is independent of birth weight


## The chi-square statistic approaches a chi-square distribution as the 
## sample size increases, under the null hypothesis.  So the p-value is 
## only an approximation -- one which improves as sample size increases.  

## For small sample sizes, the approximation is bad, so instead you can use
## Fisher's Exact Test, which does not rely on an approximation.

tab=table(low,smoke)
fisher.test(tab)

## Perform Fisher's Exact Test to test whether race is independent 
## of birth weight


## Perform Fisher's Exact Test to test whether history of hypertension is independent 
## of birth weight


## Both Pearson's chi-square test and Fisher's Exact Test assume independent observations





