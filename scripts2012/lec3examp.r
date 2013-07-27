# The following example is done in R. Most everything should work in S-PLUS
# although S-PLUS has built in routines for robust regression and robust 
# covariance matrix estimation negating the need to use the lqs library.
# 
# The comments here are minimal-- if you have a question please ask me
#
 
> library(MASS)

> data(Boston)

> attach(Boston)

> lm.out <- lm(medv~rm+age+dis+tax+ptratio, data=Boston)

> summary(lm.out)

Call:
lm(formula = medv ~ rm + age + dis + tax + ptratio, data = Boston)

Residuals:
     Min       1Q   Median       3Q      Max 
-14.7661  -2.7937  -0.7407   1.9414  41.5410 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.400480   4.219980   1.517     0.13    
rm           7.168110   0.393948  18.196  < 2e-16 ***
age         -0.076715   0.013930  -5.507 5.84e-08 ***
dis         -0.835210   0.189244  -4.413 1.25e-05 ***
tax         -0.010767   0.001986  -5.422 9.17e-08 ***
ptratio     -0.871862   0.137207  -6.354 4.72e-10 ***
---
Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

Residual standard error: 5.712 on 500 degrees of freedom
Multiple R-Squared: 0.6182,	Adjusted R-squared: 0.6143 
F-statistic: 161.9 on 5 and 500 DF,  p-value:     0 

> plot(lm.out)

# Notice the curvilinear pattern in the residuals plotted on the fitted 
# values. The following makes this more clear


> plot(fitted(lm.out), studres(lm.out))

> lines(lowess(fitted(lm.out), studres(lm.out)))

# let's look at a scatterplot matrix

> pairs(cbind(medv, rm, age, dis, tax, ptratio))

# let's look at a robust influence plot a la Rousseeuw and van Zomeren

> library(lqs)

> lts.out <- ltsreg(medv~rm+age+dis+tax+ptratio)

> lts.res <- residuals(lts.out)

> lts.scale <- lts.out$scale[1]

> mve.out <- cov.mve(cbind(rm,age,dis,tax,ptratio))

> mah.robust <- mahalanobis(cbind(rm,age,dis,tax,ptratio), mve.out$center,
+ mve.out$cov)

> plot(mah.robust, lts.res/lts.scale, xlab="Robust Distance", 
+ ylab="Standardized LTS Residuals")

> abline(h=c(-2.5, 2.5))

> abline(v=sqrt(qchisq(.975, 5)))

# looking at this plot it appears there are a lot of bad leverage points
# (data points in the upper right and lower right rectangles 
# 
# let's compare this plot with it's counterpart based on OLS residuals and 
# classical (non-robust) Mahalanobis distance:


> xbar <- apply(cbind(rm,age,dis,tax,ptratio), 2, mean)

> SigmaX <- cov(cbind(rm,age,dis,tax,ptratio))
> mah.classic <- mahalanobis(cbind(rm,age,dis,tax,ptratio), xbar, SigmaX)

> plot(mah.classic, studres(lm.out), xlab="Classical Mahalanobis Distance",
+ ylab="Studentized OLS Residuals")

> abline(h=c(-2.5, 2.5))

> abline(v=sqrt(qchisq(.975, 5)))

# here too we see some bad leverage points and regression outliers, although
# here the problem appears much less severe