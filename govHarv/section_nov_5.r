## Section November 5th, 2008
## Multivariate regression









## Let's load some data and take a look at multivariate regression



## Load the Pakistan data from problem set 5, making sure
## your working directory is set to the place where you've saved the data

pakistan <- read.csv("pakistan.csv", header = TRUE)




## On problem set 5, you were asked to create a new variable recording
## the share of total expenditures spent on food.  We'll do that now:

pakistan$foodshare <- pakistan$food / pakistan$totexp



## We can use the standard commands to familiarize ourselves with the data

head(pakistan)

dim(pakistan)

summary(pakistan)


## Suppose we want to know the relationship between monthly household
## income and the share of the budget spent on food, controlling for
## the total number of children

## The idea would be that maybe families with a lot of children face
## economies of scale with other expenses (like furniture?) but not
## with food, so families with more food may have to spend a higher
## proportion of their budget on food.



## One approach is to look at an added variable plot

## 1. 'remove' the influence of the number of kids from household income

out.income <- lm(hhincome ~ nfkids, data = pakistan)

## 2. 'remove' the influence of the number of kids from the share of 
##   household budget spent on food

out.food <- lm(foodshare ~ nfkids, data = pakistan)


## 3. Store the residuals from both regressions just run

resids.income <- residuals(out.income)

resids.food <- residuals(out.food)


## 4. Regress the residuals from the first regression on the residuals
##   from the second

out.removed <- lm(resids.food ~ resids.income)




## And we can plot the residuals and the last regression line to form 
## our 'Added Variable' plot



plot(x = resids.income, y = resids.food)
abline(out.removed, col = "red")




## Can we learn anything else from looking at the AV plot?








## Another approach is to run a multiple OLS regression:

out <- lm(foodshare ~ hhincome + nfkids, data = pakistan)
summary(out)

## Once you know the formula, creating your own confidence intervals
## takes virtually no time.  But a method that takes slightly less than 
## virtually no time is

confint(out)





## Interpretation exercise

library(car)

head(SLID)

help(SLID)


summary(lm(wages ~ education + age, data=SLID))

## What are the estimated hourly wages for an Ontarian who
##   - is zero years old and has no education?
##   - is zero years old and has 10 years of formal education?
##   - is 10 years old and has 10 years of formal education?


## What is the expected difference in hourly wages between two Ontarians who
##   - both had 10 years of education, one is age 20, the other is age 21?
##   - both had 2 years of education, one is age 20, the other is age 21?

##   - both are age 20, one had 10 years of education, one had 11?
##   - both are age 20, one had 5 years of education, one had 6?

##   - both are age 20, both had 5 years of education?

##   - both had 2 years of education, one is age 20, the other is age 30?



