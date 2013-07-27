#### 1.1: Stereo sales ####

sales <- c(420,380,350,400,440,380,450,420)
price <- c(5.5,6,6.5,6,5,6.5,4.5,5)

## (i)
# divide by n rather than (n-1), since we know the mean
print( s2 <- mean( (sales-400)^2 ) )

## (ii)
print( s2 <- mean( (sales - 630 +  40*price)^2 ) )

## (iii)
print( r <- cor(sales, price) )
print( b1 <- r*sd(sales)/sd(price) )
print( b0 <- mean(sales) - b1*mean(price) )
plot(price, sales, pch=20, main = "sales vs price regression")
lines(4:7, b0 + b1*4:7, col=2)

## (iv)
e <- sales - (b0 + b1*price)
mean(e) # This should be very close to zero!

#### 1.2: Rent Problem ####

attach(rent <- read.csv("rent.csv"))

## (i) 
par(mfrow=c(1,3))
boxplot(Rent ~ Bathrooms, xlab="Bathrooms")
boxplot(Rent ~ AC, xlab="AC")
boxplot(Rent ~ Parking, xlab="Parking")
# Bathrooms looks like the most influential factor
pairs(cbind(Rent, YearBuilt, SqFt, Rooms),
      pch=20, col=Bathrooms+1,
      main="Bathrooms: 0=black, 1=red, 2=green" )
# Sq Ft looks the most influential, and it also 
# correlates well with the # of bathrooms
# There are some very high SqFt places which might be outliers

## (ii)
par(mfrow=c(1,3)) 
boxplot(Rent, col=grey(.5), xlab="marginal")
boxplot(Rent ~ Bathrooms, xlab="Bathrooms",
        main = "Rent Distribution", col=grey(.5))
boxplot(Rent ~ Rooms, xlab="Rooms", col=grey(.5))
# Looks like rent increases with the # of (bath)rooms

## (iii)
## You need to do as.factor() to get R to treat the variable
## as a categorical grouping (rather than continuous level)
## Bonus marks for those who figured it out!
# proportion Bathrooms explain
anova(lm(Rent ~ as.factor(Bathrooms)))
5797635/(33519946 + 5797635)
# proportion Rooms explain
anova(lm(Rent ~ as.factor(Rooms)))
5241103/(34076478 + 5241103) 
# Both explain a high percentage of variability in Rent.
# Also, the Pr(>F) is very small for each, indicating
# that the room/bathroom groupings do indeed matter for rent
# This all agrees with what we found from the boxplots in (ii)

## (iv)
print( r <- cor(Rent, SqFt) )
print( b1 <- r*sd(Rent)/sd(SqFt) ) # Rent increases with SqFt
print( b0 <- mean(Rent) - b1*mean(SqFt) )
# The model is not believable at SqFt=0; why pay $600 for 0 SqFt
# this exposes the risk of extrapolation.

## (v)
par(mfrow=c(1,3))
plot(SqFt, Rent, pch=20, main = "Rent vs SqFt regression")
xx <- 0:100
lines(xx, b0 + b1*xx, col=2)
e <- Rent - b0 - b1*SqFt
hist(e)
plot(SqFt, e)
# We see that the high rent outliers are causing us lots of trouble.
# So lets remove them! 
newRent <- Rent[SqFt < 25]
newSqFt <- SqFt[SqFt < 25]
print( r <- cor(newRent, newSqFt) )
print( b1 <- r*sd(newRent)/sd(newSqFt) ) 
print( b0 <- mean(newRent) - b1*mean(newSqFt) )
## (v)
par(mfrow=c(1,3))
plot(newSqFt, newRent, pch=20, main = "Rent vs SqFt regression")
xx <- 0:25
lines(xx, b0 + b1*xx, col=2)
e <- newRent - b0 - b1*newSqFt
hist(e)
plot(newSqFt, e)

## (vi)

b0 + 14.8*b1
