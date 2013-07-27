#### Again, House price v. size data.
size <- c(.8,.9,1,1.1,1.4,1.4,1.5,1.6,1.8,2,2.4,2.5,2.7,3.2,3.5)
price <- c(70,83,74,93,89,58,85,114,95,100,138,111,124,161,172)
n <- 15

#### Run the regression
house.reg <- lm(price ~ size)

####  Look at the summary:
summary(house.reg)
## Read the following values off of the summary:
b0 <- 38.885
sb0 <- 9.094
b1 <- 35.386
sb1 <- 4.494

#### Estimated error variance (s^2)
(summary(house.reg)$sigma)^2
sum(house.reg$resid^2)/(n-2)

#### Confidence intervals example (alpha = .05)
## Calculate everything yourself:
t025 <- qt(.975, df=n-2) # this is t_{n-p,alpha/2}... 
c(b0 - sb0*t025, b0 + sb0*t025) # 95% CI for intercept
b1 + c(-1,1)*sb1*t025 # 95% CI for slope (using c(-1,1) to get + or -)
 # 95% CIs using R's function:
confint(house.reg,level=0.95)


#### Windsor CAPM example ####

mfund <- read.csv("MFUNDS.csv")

## Testing for zero intercept
summary(capm <- lm(mfund$windsor ~ mfund$valmrkt))
# Read the values off of the summary:
b0 <- 0.003647
sb0 <- .001409
b1 <- .935717
sb1 <- .029150

# R's "t-value" is our z_b from class, with null beta=0 
zb0 <- 0.003647/.001409 # number of standard errors away from the null
# To get p-value, find the area out in the tails of the standard t_{n-p}
2*pt(-abs(zb0),df=178) # p-value = 2 * area in left tail
2*(1-pt(abs(zb0),178)) # p-valu = 2 * area in right tail

## Now consider testing for beta=1
zb1 <- (b1 - 1)/sb1  # number of standard errors away from the null
2*pt(-abs(zb1), df=178) # p-value = 2 * area in left tail


#### Back to the Housing Data ####

# Prediction for places of size Xf = 1.48, 2, or 3
# You must use the same name as the data ('size')
Xf <- data.frame(size=c(1.48,2,3))
# This gives you the 95% prediction interval
predict(house.reg, newdata=Xf, interval="prediction", level=.95)
# You can use the same function to get the se.fit (i.e. standard error of Y.hat at Xf)
predict(house.reg, newdata=Xf, se.fit=TRUE)
# calculate the same things ourself...
round(s <- summary(house.reg)$sigma, 4) # s
round(sfit <- s*sqrt( 1/n + (Xf$size-mean(size))^2/((n-1)*var(size)) ), 3) # se(Yhat)
# pred interval at Xf=1.48: 
house.reg$coeff[1] + house.reg$coeff[2]*1.48 + c(0,-1, 1)*qt(.975, df=n-2)*sqrt(s^2+sfit^2) 


