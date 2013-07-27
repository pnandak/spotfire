## Lecture 7 R script
## HW 2 review:

s=sample(1:1000, 50)
sum(s<50)
sum(s>950)
sum(s>100 & s<300)


## Lecture 7

library(ISwR)
data(thuesen)
attach(thuesen)
plot(short.velocity ~ blood.glucose)
mod = lm(short.velocity  ~ blood.glucose)
mod

summary(mod)
abline(mod)

sum.mod=summary(mod)
sum.mod$coef
mat = sum.mod$coef[,1:2]
mat
est=sum.mod$coef[,1]
se=sum.mod$coef[,2]
low.ci = est-1.96*se
up.ci = est+1.96*se
mat=round(cbind(est,low.ci,up.ci),2)
round(cbind(confint(mod)),2)

mat
fitted(mod)
plot(short.velocity ~ blood.glucose)
points(fitted(mod) ~ blood.glucose[complete.cases(thuesen)],col="blue")
plot(resid(mod) ~ fitted(mod))
abline(h=0)
qqnorm(resid(mod))
qqline(resid(mod))

plot(mod) ## plot 4 diagnostic plots for the model
par(mfrow=c(2,2)) ## divide plotting screen into 2 by 2 matrix
plot(mod)

par(mfrow=c(1,1))
cor(blood.glucose, short.velocity) ## need to handle the missing value.
cor(blood.glucose, short.velocity, use="pairwise.complete.obs")

## Lab 7 -- Linear Regression
## CSSS 508

## Read in the states data set from the homework page of the class website.  Recall: this is a 
## CSV file, and the first column gives the rownames.  Read in the data set so that the first column
## is stored as rownames instead of as a variable.

states = read.csv("http://students.washington.edu//gailp2//CSSS508//homework//state%20data.csv",row.names=1)
summary(states)
head(states)

## Attach the data set.
attach(states)

## Make a scatter plot of HS Graduation rate (y-axis) against life expectancy.

plot(HS.Grad~Life.Exp)

## Perform a linear regression of HS Graduation rate (y-axis) on life expectancy.

mod=lm(HS.Grad~Life.Exp)

## Is the relationship statistically significant?
summary(mod)

## yes because p values < 0.05

## Add the regression line to your scatter plot in red.
abline(mod)

## Note that there are two outliers in the plot.  Use identify() to 
## identify which states these are.  

identify(Life.Exp, HS.Grad, rownames(states))
## Nevada and Alaska

## Plot the residuals against fitted values and add a horizontal line at y=0.
plot(resid(mod)~fitted(mod))
abline(h=0)

## Is the assumption of constant variance met?
## yes

## Make a QQ normal plot of residuals and add a QQ line.
qqnorm(resid(mod))
qqline(resid(mod))

## Now go through the same process to explore the relationship between income and illiteracy.
## Plot Income (y-axis) against Illiteracy.

plot(Income~Illiteracy)

## Perform linear regression and add the regression line in red.

mod2= lm(Income~Illiteracy)


## Is the relationship statistically significant?
summary(mod2)

## yes because p<0.05

## There is one outlier in the plot: identify which state that is.  
identify(Illiteracy,Income,rownames(states))
## Alaska

## Plot the residuals against fitted values and add a horizontal line at y=0.

plot(resid(mod2)~fitted(mod2))
abline(h=0)

## Is the assumption of constant variance met?  Yes.

## Make a QQ normal plot of residuals and add a QQ line.
qqnorm(resid(mod2))
qqline(resid(mod2))


## Now do the same regression, but omit the outlier.  Show this regression line on the same plot in blue.
which(rownames(states)=="Alaska")
## omit row 2:
mod3=lm(Income[-2]~Illiteracy[-2])

plot(Illiteracy, Income)
abline(mod2)
abline(mod3,col="red")

## How much does the outlier shift the regression line?
## qualitatively, the slope shifts by a small amount.  