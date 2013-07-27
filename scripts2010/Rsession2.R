################
# Friday, November 6, 2009
# Introduction to R
################

# Read in auto.dta
library(foreign)
auto <- read.dta("auto.dta")
summary(auto)
head(auto)
str(auto)
attr(auto,"var.labels")
attr(auto,"value.labels")
attr(auto,"label.table")$origin
attr(attr(auto,"label.table")$origin,"names")

# Simple Linear Regression
auto.lm <- lm(mpg ~ weight,data=auto)
deviance(auto.lm)
yhat.lm <- predict(auto.lm,se.fit=T) 
yhat.lm 
ser2 <- deviance(auto.lm)/yhat.lm$df
ser <- sqrt(ser2)
yhat.lm$residual.scale 

# Scatterplot
attach(auto)
plot(mpg ~ weight,xlab="Weight",ylab="Miles per Gallon")
abline(auto.lm)
rug(weight)
rug(mpg,side=2)

# Jitter Rugs 
par(mfrow=c(1,2))
plot(mpg ~ weight,xlab="Weight",ylab="Miles per Gallon")
abline(auto.lm)
rug(weight)
rug(mpg,side=2)
plot(mpg ~ weight,xlab="Weight",ylab="Miles per Gallon")
abline(auto.lm)
rug(jitter(weight))
rug(jitter(mpg),side=2)

# Residual Plot
plot(residuals(auto.lm)~weight,xlab="Weight",ylab="Residuals",
	 ylim=c(-10,10))
abline(h=0,col="red")

# Transform Data
auto.lm2 <- lm(log(mpg) ~ log(weight),data=auto)
plot(log(mpg) ~ log(weight),xlab="Weight, logged",ylab="Miles per Gallon, logged")
abline(auto.lm2)
# Diagnostic Plots
par(mfrow=c(2,2))
plot(auto.lm2)

# Quadratic form
auto.lm3 <- lm(mpg ~ weight + I(weight^2),data=auto)
coef(auto.lm3)
confint(auto.lm3)
vcov(auto.lm3)

plot(mpg ~ weight, xlab="Weight", ylab="Miles per Gallon")
yhat.lm3 <- predict(auto.lm3)
#lines(weight,yhat.lm3)
index.wgt <- order(weight)
lines(weight[index.wgt],yhat.lm3[index.wgt],lwd=1.25)
ci.lm3 <- predict(auto.lm3,interval="confidence")
head(ci.lm3)
lines(weight[index.wgt],ci.lm3[index.wgt,"lwr"],lty=2)
lines(weight[index.wgt],ci.lm3[index.wgt,"upr"],lty=2)

# Use prediction interval instead
plot(mpg ~ weight, xlab="Weight", ylab="Miles per Gallon")
lines(weight[index.wgt],yhat.lm3[index.wgt],lwd=1.25)
ci.lm3.alt <- predict(auto.lm3,interval="prediction")
lines(weight[index.wgt],ci.lm3.alt[index.wgt,"lwr"],lty=2)
lines(weight[index.wgt],ci.lm3.alt[index.wgt,"upr"],lty=2)

# Adjust range
plot(mpg ~ weight, xlab="Weight", ylab="Miles per Gallon",
	ylim=range(ci.lm3.alt))
lines(weight[index.wgt],fitted(auto.lm3)[index.wgt],lwd=1.25)
lines(weight[index.wgt],ci.lm3.alt[index.wgt,"lwr"],lty=2)
lines(weight[index.wgt],ci.lm3.alt[index.wgt,"upr"],lty=2)

# Add dummy variable
auto.lm4 <- lm(mpg ~ weight + I(weight^2) + foreign,data=auto)
par(mfrow=c(2,2))
plot(auto.lm)
anova(auto.lm)

# Hypothesis Test
# F-test: Difference in RSS
anova(auto.lm4,auto.lm3)

# Unconstrained only
# Hard way 1 
vcov(auto.lm4)
foreign.tvalue <- coef(auto.lm4)["foreignForeign"]/sqrt(vcov(auto.lm4)["foreignForeign","foreignForeign"])
help(pt)
# All distributions
# dNAME --- PDF: f(y)
# pNAME --- CDF: F(y)
# qNAME --- inverse CDF: F^{-1}(y)
#   i.e., y = G(p)
# rNAME --- random generation
predict.lm4 <- predict(auto.lm4,se.fit=TRUE)
2*pt(foreign.tvalue,predict.lm4$df)
# Plot this t-distribution if time 
y <- seq(-4,4,by=0.01)
pdf.y <- dt(y,predict.lm4$df)
pdf("tvalue.pdf",height=3.5)
par(mar=c(4.1,0.4,3.1,0.2))
plot(pdf.y ~ y,type="l",
			   xlab="",
			   ylab="",
			   yaxs="i",
			   ylim=c(0,dt(0,predict.lm4$df)+0.005),
			   #main="Probability Density Function",
			   axes=F,lwd=1.5)
axis(1,at=c(0,2))
segments(-3.5,0,3.5,0,lwd=1.25,font=2)
segments(foreign.tvalue,0,foreign.tvalue,dnorm(foreign.tvalue))
region <- y < foreign.tvalue
polygon(x=c(y[region],rev(y[region])),
	   y=c(pdf.y[region],rep(0,length(y[region]))),
	   col=gray(0.7),border=F)
text(-2.3,dt(-2.6,predict.lm4$df),round(pt(foreign.tvalue,predict.lm4$df),4),cex=0.5,font=2)
dev.off()




# Hard Way 2 
R <- matrix(c(0,0,0,1),nrow=1)
VB <- vcov(auto.lm4)
W <- t(R%*%coef(auto.lm4)-0)%*%solve(R%*%VB%*%t(R))%*%(R%*%coef(auto.lm4)-0)
help(pf)
pf(W,1,predict.lm4$df)
1-pf(W,1,predict.lm4$df)
pf(W,1,predict.lm4$df,lower.tail=FALSE)

# Functions from user packages
library(car)
help(linear.hypothesis)
linear.hypothesis(auto.lm4,c("foreignForeign=0"))

library(lmtest)  
coeftest(auto.lm4)
waldtest(auto.lm4,auto.lm3)


# Robust Covariance Estimate 
library(sandwich)
help(vcovHC)
vcov(auto.lm4)
vcovHC(auto.lm4,type="const")
vcovHC(auto.lm4,type="HC0")  #uncorrected
vcovHC(auto.lm4,type="HC1")  #in Stata, vce(robust)
vcovHC(auto.lm4,type="HC2")  #in Stata, vce(hc2)
vcovHC(auto.lm4,type="HC3")  #in Stata, vce(hc3)
vcovHC(auto.lm4,type="HC4")  #recent 

coeftest(auto.lm4,vcov=vcovHC)
coeftest(auto.lm4,vcov=vcovHC(auto.lm4,type="HC2"))

cov.types <- c("const",paste("HC",0:4,sep=""))
for(i in 1:length(cov.types)){
cat(paste("Variance Covariance Type", cov.types[i]))
print(coeftest(auto.lm4,vcov=vcovHC(auto.lm4,type=cov.types[i])))
	}

# Also available for Time Series
help(vcovHAC)
help(NeweyWest)
help(kernelHAC)
help(weave)

# Spline function  
library(splines)
help(bs)
auto.lm5 <- lm(mpg ~ bs(weight,degree=3) + foreign,data=auto)
summary(auto.lm5)


plot(mpg ~ weight,xlab="Weight",ylab="Miles per Gallon")
abline(auto.lm)
lines(fitted(auto.lm3)[index.wgt] ~ weight[index.wgt],lty=2)
lines(fitted(auto.lm5)[index.wgt] ~ weight[index.wgt],lty=3)
legend(3800,40,c("Linear","Quadratic","Cubic Spline"),lty=1:3)



detach()
