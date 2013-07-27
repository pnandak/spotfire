## Linear Regression, PS 206 Class 1

happydat <- read.table("happy.txt", header=TRUE)

#library(foreign)
#happydat <- read.dta("happy.dta", convert.factors=FALSE)

happydata2 <- subset(happydat, year>1998, select=c(meanhap, lgdp, country, oecd))

happydata <- na.omit(happydata2) 
happydata <- as.data.frame(happydata)
attach(happydata)  

## linear regression

model1 <- lm(meanhap ~ lgdp, data=happydata)
summary(model1)

## plot data and regression line

plot(lgdp, meanhap, xlab="Real GDP per Capita (thousands of dollars, log scale)", ylab="Mean Happiness", xlim=c(6,11.5), ylim=c(2.4,3.65), mgp=c(2,.5,0))
curve(coef(model1)[1] + coef(model1)[2]*x, add=TRUE)
text(x=lgdp, y=meanhap, labels=country, pos=4, offset=0.5, cex=0.7) 


## rescale to 10s of thousands of dollars, logged, to make intercept visible

gdp2 <- gdp/1000  # GDP in 1000s
lgdp2 <- log(gdp2)
model2 <- lm(meanhap ~ lgdp2, data=happydata)

plot(lgdp2, meanhap, xlab="Real GDP per Capita (thousands of dollars, log scale)", ylab="Mean Happiness", xlim=c(0,4), ylim=c(2.0,3.65), xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve(coef(model2)[1] + coef(model2)[2]*x, add=TRUE)

## add OECD variable and interaction to model

oecd.interact <- lgdp2*oecd

model3 <- lm(meanhap ~ lgdp2+oecd+oecd.interact, data=happydata)
summary(model3)

curve(coef(model3)[1] + coef(model3)[2]*x + coef(model3)[3] + coef(model3)[4]*x, col="red", add=TRUE)
text(x=lgdp2, y=meanhap, labels=country, pos=4, offset=0.5, cex=0.7) 

country.oecd <- country
country.oecd[oecd==0] <- NA
text(x=lgdp2, y=meanhap, labels=country.oecd, pos=4, offset=0.5, cex=0.7, col="red") 


## Tests for heteroskedasticity and robust standard errors

e <- model1$residuals
e2 <- e^2
plot(lgdp, e2)

install.packages("sandwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)

bptest(model1)

coeftest(model1, vcov=sandwich)
