#### Q1.1: Newspaper Circulation #######

attach( news <- read.csv("newspaper.csv") )
# plot the data points
plot(Daily, Sunday, pch=20)
# alternatively, plot the data with labels...
plot(Daily, Sunday, col=0, xlim=c(100,1300))
text(x=Daily, y=Sunday, labels=Newspaper, cex=.7)
## Things look linear, and this makes sense:
## more daily readers leads to more potential sunday readers
# (ii) The regression and confidence intervals:
newsreg <- lm(Sunday ~ Daily)
confint(newsreg, level=.95)
# Huge variability in the intercept, but the slope is solidly >1.
## (iii)
# H0: B1 = 0; HA: B1 != 0
# The test statistic is 1.33971/0.07075 = 18.935.
# P(Z_32 > 18.935) = 0 (basically), so you would reject the null.
# That is, daily circulation is significantly correlated with sunday.
## (iv)
# You can use the predict function to get everything:
predict(newsreg, newdata=data.frame(Daily=200),
        se.fit=TRUE, interval="prediction", level=.95)
# in the output, you have:
#    - the prediction mean and 95\% interval ($fit)
#    - the standard error of the mean ($se.fit)
#    - the degrees of freedom ($df)
#    - the standard error of the residuals ($residual.scale)
# Alternatively, it might be handy to go through the steps
# using the equations in class (as we did in the lecture)
# and compare your results to make sure you are on track.

#### Q1.3: Crime Rates #####

attach(crime <- read.csv("crime.csv"))
pairs(crime)
cor(crime)
# There is a high correlation between CR and POLICE,
# but it is most plausible that CR causes high POLICE;
# hence the police budget is a sort of derivitive response
# and POLICE is not very useful for understanding crime.
# (iii)
summary( crimereg <- lm(CR ~ INC) )
# The t-value for the LS slope coefficient (.018) leads to
# a p-value of 0.002, which is significant at the alpha=0.05 level.
# (iv)
plot(crimereg$fitted, crimereg$resid, pch=20)
# No obvious problems; looks like a decent fit
# (v)
predict(crimereg, newdata=data.frame(INC=2800),
        interval="prediction", level=.9)
## The interval includes negative crime rates!
#  This exposes the danger of prediction at the boundaries.
#  Perhaps log(CR) would be a better thing to look at...
