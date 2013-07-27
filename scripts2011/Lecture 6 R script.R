## Lecture 6 R script: Programming in R

## CSSS 508


hist.with.normal <- function(my.data) {
## plots a histogram of my.data and overlays it with a normal curve in red ##
 hist(my.data,freq=F)
 curve(dnorm(x, mean=mean(my.data), sd=sd(my.data)), col="red", add=T)
}

hist.with.normal <- function(x) {
## plots a histogram of my.data and overlays it with a normal curve in red ##
 hist(x,freq=F)
 curve(dnorm(x, mean=mean(x), sd=sd(x)), col="red", add=T)
}
save(hist.with.normal,file="hist.with.normal")

hist.with.normal(rnorm(100))
hist.with.normal(rgamma(100,2))
hist.with.normal(runif(100))


qqplot=function(x) {
## make qq normal plot with qq line in red
 qqnorm(x)
 qqline(x,col="red")
}

x=rnorm(100)
qqplot(x)
qqplot(rgamma(100,2))
qqplot(runif(100,0,1))

data(swiss)
attach(swiss)
Mod1=lm(Fertility~Agriculture)
summary(Mod1)
reg.table(Mod1)
 
reg.table <- function(mod) {
 ## Given a linear model, returns a table of estimates and confidence intervals ##
 ## Values are rounded to the nearest hundredth. ##
 ## First retrieve the coefficient matrix of the linear model ##
 Est. = round(mod$coef,1)
 ## Next get confidence intervals for each coefficient estimate ##
 conf.interval = round(confint(mod),1)
 ## Retrieve the p-value from the summary of output
 p.value = round(summary(mod)$coef[,4],3)
 ## Bind them all together into a matrix and return it ##
 return(cbind(Est., conf.interval,p.value))
}

## Here's how we test each line.
## First assign a value to mod
mod=Mod1

## skip the first line with the function name

## test line 2:
mod
mod$coef
round(mod$coef,1)
Est. = round(mod$coef,1)
Est.

# test line 3
confint(mod)
?confint
round(confint(mod),1)
conf.interval = round(confint(mod),1)
conf.interval 

## test line 4
summary(mod)  ## list of objects including a matrix 
   ## with estimates, coefs, and p-values
summary(mod)$coef ## just the coef matrix 
summary(mod)$coef[,4] ## the column of p-values
round(summary(mod)$coef[,4],3) ## the rounded p-values
p.value = round(summary(mod)$coef[,4],3)
p.value 

## we can't test the return() function, but let's 
## check what we're returning:
cbind(Est., conf.interval,p.value)


reg.table(Mod1)
mod2 = lm(Fertility~Examination+Agriculture+Catholic)
reg.table(mod2)
mod3=lm(Fertility~Examination+Agriculture+Catholic+Infant.Mortality)
reg.table(mod3)
 

x=rnorm(10)
if (length(x)<20) print("Small Sample")
x=rnorm(10)
if (length(x)<20) print("Small Sample") else print("Large Sample")
x=rnorm(100)
if (length(x)<20) print("Small Sample") else print("Large Sample")
if (length(x)<20) print("Small Sample") 
else print("Large Sample")

x=(1:10)^4
for (i in 1:length(x)) {
 print(x[i])
}
for (i in 1:length(x)) {
 if (i==3 | i==5 | i==7) print(x[i])
}

for (i in c(3,5,7)) {
 print(x[i])
}

data.means = NULL 
for (i in 1:50) {
 data.means[i] = mean(rnorm(100))
}

marginal.means = function(my.matrix) {
 nrow = dim(my.matrix)[1]
 my.matrix.means=NULL
 for (i in 1:nrow) {
  my.matrix.means[i] = mean(my.matrix[i,])
 }
 return(my.matrix.means)
}

mat=cbind(1:10, 3:22, 10:29)
mat

marginal.means(mat)

## the apply() function!!!!!
apply(mat, 1, mean) 
apply(mat, 1, median) 
apply(mat, 2, median) 


library(MASS)
anorexia
summary(anorexia)
attach(anorexia)

mean(Prewt[Treat=="Cont"])
tapply(Prewt, Treat, mean) 
tapply(Postwt, Treat, mean) 

weight.gain = Postwt-Prewt
tapply(weight.gain, Treat, mean)
tapply(weight.gain, Treat, sd) 


######################################################

## Lab 6

## CSSS 508

## Programming in R

## Functions

## The logit function (the log odds) is often used in statistics.  Here it is:
logit = function(p) log(p/(1-p))

## Note that the range of logit is (0,1)

## Plot the logit function on the range 0,1
plot(logit,0,1)


## The inverse of the logit function is the expit function.  That is, 
## logit(expit(x)) = x and expit(logit(p))=p.  Derive the expit function
## and program it in R.  Test that it is the inverse of the logit.

expit=function(x) exp(x)/(exp(x)+1)
logit(expit(3))
expit(logit(.25))

## Plot the expit function on the range -10, 10
plot(expit,-10,10)

## loops

## Write a loop to plot 11 normal curves, each with standard deviation 1, but 
## with means ranging from 0 to 10,
## overlaid on the same plot with range -5 to 15 on the x-axis.
## Set the color of each curve equal to the loop index so that the 
## curves are different colors.

curve(dnorm(x), xlim=c(-5,15))  ## has mean=0, sd=1
for (i in 1:10) {
	curve(dnorm(x,mean=i),add=T,col=i)
}

## apply

## Create a matrix with 100 rows and 100 columns, containing
## 100 samples from a chi squared distribution with 4 degrees of freedom

mat = matrix(rchisq(100*100,df=4),nrow=100)
dim(mat)

## Use the apply() function to compute the 2.5% and 97.5% quantiles of 
## each row.  You will need the quantile() function.

apply(mat, 1, quantile, c(0.025, 0.975))


## tapply

## load the chickwts data set with 
data(chickwts)

## look at the data
summary(chickwts)

## This data set contains chick weights (grams) by feed type.

## Make a box plot of weights by feed type.
boxplot(weight~feed)

## Use tapply() to compute the mean chick weight for each feed type.
tapply(weight,feed,mean)

