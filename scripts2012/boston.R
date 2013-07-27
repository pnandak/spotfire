# Try-it-on-your-own Boston Housing Prices Example
# Import the data, and try to recreate the regression done by Harrison, et al. 
# in the article I've given you.
# The start of my code is below...


boston<-read.csv("F:/RShortcourse/boston.csv",header=T)
names(boston)

hist(MEDV)
qqnorm(MEDV)
qqline(MEDV)

boston$LMEDV<-log(boston$MEDV)