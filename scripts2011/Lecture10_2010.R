###################################################
### chunk number 1: resdens
###################################################
Weakliem <- read.table("Weakliem.txt")
mod1 <- lm(secpay~gini + democrat, data=Weakliem)
sm.density(rstudent(mod1), model="normal")


###################################################
### chunk number 2: qq1
###################################################
library(car)
qq.plot(mod1)


###################################################
### chunk number 3: qq2
###################################################
mod2 <- lm(secpay ~ gini + democrat, data=Weakliem,
    subset=-c(25,49))
qq.plot(mod2, simulate=T, labels=FALSE)


###################################################
### chunk number 4: resdens2
###################################################
sm.density(rstudent(mod2), model="normal")


###################################################
### chunk number 5: assesshet1
###################################################
plot(fitted.values(mod1), rstudent(mod1),
    main="Studentized Residuals versus Fitted Values")


###################################################
### chunk number 6: remout
###################################################
infl.outliers <- which(rownames(Weakliem) %in% 
    c("Slovakia", "CzechRepublic"))
mod2 <- lm(secpay~gini*democrat, data=Weakliem,
    subset=-infl.outliers)

plot(fitted.values(mod2), rstudent(mod2),
    main="Studentized Residuals versus Fitted Values")
abline(h=0, lty=2)


###################################################
### chunk number 7: dhs
###################################################
library(apsrtable)
dat <- read.dta("dhs_sl.dta")
mod <- lm(ceb ~ age + christian + educ + notv, data=dat)
apsrtable(mod, model.names="", Sweave=T, digits=3)


###################################################
### chunk number 8: rvf1
###################################################
plot(fitted.values(mod), rstudent(mod),
	main="Studentized Residuals vs Fitted Values")
abline(h=0, lty=2)


###################################################
### chunk number 9: spreadlevel1
###################################################
spread.level.plot(mod)


###################################################
### chunk number 10: sl2
###################################################
sl2 <- spread.level.plot(mod)


###################################################
### chunk number 11: ncv
###################################################
ncv.test(mod, ~age + christian + educ + notv, data=dat)


###################################################
### chunk number 12: robse.fun
###################################################
robust.se <- function( model, type="hc3") {
  require(car)
  s <- summary( model)
  wse <- sqrt( diag( hccm( model, type=type)))
  t <- model$coefficients/wse
  p <- 2*pt(abs( t), model$df.residual, lower.tail=F)
  results <- round(cbind( model$coefficients, wse, t, p), 3)
  dimnames(results) <- dimnames( s$coefficients)
  results
}


###################################################
### chunk number 13: coef1
###################################################
with(summary(mod), coefficients)


###################################################
### chunk number 14: coef2
###################################################
robust.se(mod)


###################################################
### chunk number 15: wls
###################################################
mod.wls <- lm(ceb ~ age + christian + educ + notv, data=dat, 
     weight=1/age)
with(summary(mod.wls), coefficients)


###################################################
### chunk number 16: fgls
###################################################
mod1.ols <- lm(ceb ~ age + christian + educ + notv, data=dat)
aux.mod1 <- lm(log(resid(mod1.ols)^2) ~ age + christian + educ + notv, data=dat)
h <- exp(predict(aux.mod1))
mod.fgls <- lm(ceb ~ age + christian + educ + notv, data=dat, weights=1/h)
with(summary(mod.fgls), coefficients)


###################################################
### chunk number 17: compmod
###################################################
apsrtable(mod1.ols, mod.wls, mod.fgls, model.names=c("OLS", "WLS", "FGLS"), 
	digits=3, Sweave=T)


