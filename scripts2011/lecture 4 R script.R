# Lecture 4 R Script

x = sample(1:100, size=20)
x

sample(1:1000,5)

x=sample(1:6, 23, replace=T)
table(x)

age=sample(20:25, 20, replace=T)
age
sex=sample(c("M","F"),size=20, replace=T)
sex

table(age)
table(sex)
table(age, sex)

rnorm(n=35, mean=50, sd=10)
x=rnorm(100,10,1)
x=round(rnorm(n=100,mean=10,sd=1))

pnorm(q=6, mean=10, sd=2)
1-pnorm(q=6, mean=10, sd=2)
pnorm(q=6, mean=10, sd=2, lower.tail=F)
pnorm(1) - pnorm(-1)
pnorm(12, 10,2) - pnorm(8,10,2)
pnorm(2) - pnorm(-2)
qnorm(0.25, mean=33, sd=5)
qnorm(.95)
qnorm(.05)
qnorm(.995)
qnorm(.005)

x=rnorm(100)
hist(x, freq=F)
curve(dnorm(x), add=T)
runif(n=25, min=0, max=10) 
## samples 25 points between 0 and 10
x=rt(100,df=5)
hist(x)
pt(4,df=5)

## Lab 4


#  CSSS 508
#  Lab 4 - sampling

## Run the following code to create the ages and IDs of a
## population of size 10,591 from which you will draw a sample.

n=10591
id = 1:n
age = sample(1:100, n, replace=T, prob=c(rep(1,49), seq(1,.05,by = -.019)))
sex = sample(c("M","F"), n, replace=T)
race = sample(c("Black","White","Asian","Hispanic","Native Am"),
	 n, replace=T,prob=c(.05,.9,.02, .025,.005))
hist(age)

## Tabulate age, sex, and age by sex.
table(age)
table(sex)
table(age,sex)

## Create a Boolean variable called "child", which is true when age<=18
## and false when age>18
child=age<=18
table(child)

## Combine these variables into a data frame and summarize it.
dat = data.frame(id,age,sex,race,child)
summary(dat)
dim(dat)

## Draw a random sample of size 100 from this population.
s=sample(id,100) 
dat[s,]

## Now draw a weighted sample of size 100, where children (18 and under)
## are twice as likely to be sampled as adults.
## Hint: first create a "probability weight" vector which is 2 for kids
## and 1 for adults.  Then send this as a parameter to the sample function
## Store the IDs of your sample in a vector.

weight = child+1  # Recall that R treats TRUE as 1 and FALSE as 0.
table(weight)
table(weight,child)

s2 = sample(id, 100, prob=weight)

## For your sample, tabulate child, sex, race, and child by race.
table(child[s2])
table(sex[s2])
table(race[s2])
table(child[s2],race[s2])

## Chi-squared distribution
## The chi-squared distribution is used for many statistical tests.
## The functions pchisq, qchisq, rchisq, and dchisq can be used for 
## computations.

# type ?rchisq.  What parameter(s) does this distribution take?
?rchisq

## df = degrees of freedom, ncp = noncentrality parameter (optional)

# Draw a sample of size 1000 from a chi-square distribution with 
# degrees of freedom = 2.

x=rchisq(1000,df=2)

# Make a histogram of your sample.
hist(x)

# The Chi-square distribution is used to test whether one model fits
# better than another.  You compute the chi-squared test statistic 
# and compare it to the chi-squared distribution.  Supposed your
# test statistic is 6.27.

# If X follows a chi-squared distribution with 3 degrees of freedom
# what is the probability that X is greater than 6.27?  This is the p-value.

pchisq(6.27,df=3,lower.tail=F)

# What test statistic value do you need for a p-value of 0.05 or less?

qchisq(0.05, df=3, lower.tail=F)

## Test it:
pchisq(7.81, df=3, lower.tail=F)
pchisq(7.9, df=3, lower.tail=F)
pchisq(7.7, df=3, lower.tail=F)

## We need a statistic value of 7.81 or higher.
