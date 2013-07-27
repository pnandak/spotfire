## baisc linear regression:
library(arm)

# there are some problems reading this data set directly from online:
# http://privatewww.essex.ac.uk/~caox/teaching/teaching.htm
# so save it into your own working directory first and read in:
# for example, my data are saved at "C:\XunCao\Teaching\Method_Stat\multilevel\Lec\lec2"
# so: 

# or set the working directory as follows:
dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec2") 
setwd(dd)

kidiq <- read.dta(file="kidiq.dta")
kidiq[1:10, ]
attach(kidiq)

### fitting and summarizing regressions in R
fit.1 <- lm (kid_score ~ mom_hs )
display(fit.1)



### graphical displays of data and fitted models
fit.2 <- lm (kid_score ~ mom_iq)
display(fit.2)
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score")
curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE)

fit.3 <- lm (kid_score ~ mom_hs + mom_iq)
display(fit.3)
print(fit.3)
summary(fit.3)



### two fitted regression lines
## model with no interaction
fit.3 <- lm (kid_score ~ mom_hs + mom_iq)
colors <- ifelse (mom_hs==1, "black", "gray")    # a simple way for the "if()" function. 
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score",
  col=colors, pch=20)
curve (cbind (1, 1, x) %*% coef(fit.3), add=TRUE, col="black")
curve (cbind (1, 0, x) %*% coef(fit.3), add=TRUE, col="gray")
# this is exactly:
lines(mom_iq, cbind (1, 0, mom_iq) %*% coef(fit.3))


# alternative sequence of commands
plot(mom_iq,kid_score,xlab="Mother IQ score",ylab="Child test score",type="n")
points (mom_iq[mom_hs==1], kid_score[mom_hs==1], pch=20, col="black")
points (mom_iq[mom_hs==0], kid_score[mom_hs==0], pch=20, col="gray")
curve (cbind (1, 1, x) %*% coef(fit.3), add=TRUE, col="black")
curve (cbind (1, 0, x) %*% coef(fit.3), add=TRUE, col="gray")


## model with interaction
fit.4 <- lm (kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq)
display(fit.4)
mean(mom_iq, na.rm=T)

colors <- ifelse (mom_hs==1, "black", "gray")
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score",
  col=colors, pch=20)
curve (cbind (1, 1, x, 1*x) %*% coef(fit.4), add=TRUE, col="black")
curve (cbind (1, 0, x, 0*x) %*% coef(fit.4), add=TRUE, col="gray")

# get the covariance matrix for beta:
display(fit.4)
vcov(fit.4)
summary(fit.4)$cov.unscaled*summary(fit.4)$sigma^2



### displaying uncertainty in the fitted regression
fit.2 <- lm (kid_score ~ mom_iq)
display(fit.2)

fit.2.sim <- sim (fit.2)
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score")
for (i in 1:100){
  curve (fit.2.sim$coef[i,1] + fit.2.sim$coef[i,2]*x, add=TRUE,col="gray")
}

curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE, col="black")




### displaying using one plot for each input variable
fit.3 <- lm (kid_score ~ mom_hs + mom_iq)
beta.hat <- coef (fit.3)
beta.sim <- sim (fit.3)$coef

par (mfrow=c(1,2))
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score")
for (i in 1:100){
  curve (cbind (1, mean(mom_hs), x) %*% beta.sim[i,], lwd=.5, col="gray",
    add=TRUE)
} 
curve (cbind (1, mean(mom_hs), x) %*% beta.hat, col="black", add=TRUE) # does it make sense to do mean(mom.hs):
  
        
plot (mom_hs, kid_score, xlab="Mother completed high school",
  ylab="Child test score")
for (i in 1:10){
  curve (cbind (1, x, mean(mom_iq)) %*% beta.sim[i,], lwd=.5, col="gray",
    add=TRUE)
}
curve (cbind (1, x, mean(mom_iq)) %*% beta.hat, col="black", add=TRUE)


### Prediction
x.new <- data.frame (mom_hs=1, mom_iq=100)
predict (fit.3, x.new, interval="prediction", level=0.95)
