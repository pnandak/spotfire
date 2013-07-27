## regression analysis

## verzani Example 3.2, pp82ff
library(UsingR)
attach(homedata)
plot(y1970,y2000)
plot(y2000 ~ y1970)

## run a regression
reg1 <- lm(y2000 ~ y1970,   ## formula with twiddle
           data=homedata)   ## data set

summary(reg1)               ## print a summary of regression


## Example 3.3, on p84
## does the weather predict the stock market
attach(maydow)
plot(max.temp,DJA)
reg2 <- lm(DJA ~ max.temp)
summary(reg2)

## Jackman's elaboration 
x <- c(NA,DJA[-21])
plot(DJA ~ x,
     type="b")
plot(DJA ~ x)
reg3 <- lm(DJA ~ x)
summary(reg3)
abline(reg3)
