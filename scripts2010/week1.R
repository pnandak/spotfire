### Pickup data visualization
data <- read.csv("pickup.csv")
attach(data)
names(data)

# R's summary function is pretty clever
summary(data)

# Histograms
par(mfrow=c(1,3)) # break the plot into 1x3 matrix
hist(year, col=grey(.4), border=grey(.9))
hist(miles, col=grey(.4), border=grey(.9))
hist(price, col=grey(.4), border=grey(.9))

# Scatterplots
par(mfrow=c(1,2))
# B&W
plot(year,price,pch=20)
plot(miles,price,pch=20)
# Color
plot(year,price,pch=20, col=make)
legend("topleft", fill=c(1:3), legend=levels(make), cex=.7) 
plot(miles,price,pch=20, col=make)

# 'pairs' scatterplot matrix
pairs(data[,1:3], pch=20, col=make)

### Boxplots with the cars data
library(MASS) # Ripley's MASS library has lots of good data
data(Cars93) # Load data on cars for sale in 1993
par(mfrow=c(1,2))
# R's boxplot function is super nice
boxplot(Price ~ Type, data=Cars93,
        ylab="price", col=8, main="Type")
boxplot(Price ~ Origin, data=Cars93,
        ylab="price", col=8, main="Origin")

### ANOVA with simulated data
par(mfrow=c(1,1))
boxplot( Y <- data.frame(Y0 = rnorm(n=10, mean=0),
                         Y1 = rnorm(n=10, mean=5),
                         Y2 = rnorm(n=10, mean=10)) )
# data.frame is clever, so mean() applies to each column
print( groupYbar <- mean(Y) )
# unlist removes this cleverness
print( totalYbar <- mean( unlist(Y) ) )
#Sums of Squares:
print( SSR <- sum( 10*(groupYbar - totalYbar)^2 ) )
print( SST <- sum( (unlist(Y) - totalYbar)^2 ) )
SSR/SST # proportion of explained variability

### ANOVA example: back to pickups
boxplot(price ~ make, ylab="price")
# Our first real modeling!
# lm stands for 'linear model'
print(model <- lm(price ~ as.factor(make)))
mean(price[make=="Dodge"])
mean(price[make=="Ford"])
mean(price[make=="GMC"])
#
anova(model)
round( 29571553/(1373653582+29571553), 2 )

#########  Wage Data Example ##########

D <- read.csv("wages.csv")
# Rename for convenience
Y <- D$HRS
X <- D$RATE
# These are the coefficients we found in class
round(b1 <- cor(X,Y)*sd(Y)/sd(X)) # Use correlation for slope
round(b0 <- mean(Y) - mean(X)*b1) # Put [X.bar,Y.bar] on the line
# Plot the data and our line
plot(X,Y)
xx <- seq(-2,4,length=4)
lines(xx, b0 + b1*xx, col=2)
## Plot the residuals Y - Y.hat
e <- Y - (b0+b1*X)
plot(X, e, type="h", xlab="Hours", 
	ylab="Residuals", col=(e>0)+1, lwd=3)
abline(h=0, col=8, lwd=3) # Just to make it pretty
