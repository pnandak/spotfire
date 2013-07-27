###################################################
### chunk number 1: q1.1
###################################################
library(foreign)
dat <- read.dta("http://www.quantoid.net/lab1_nes.dta")
dat[["pidfac"]] <- as.factor(dat[["pid"]])
mod.pid <- lm(demtherm ~ age + racerec + pid, data=dat)
mod.pid2 <- lm(demtherm ~ age + racerec + pidfac, data=dat)
anova(mod.pid, mod.pid2, test="F")


###################################################
### chunk number 2: q1.2
###################################################
mod.pid3 <- lm(demtherm ~ age + racerec + pid3, data=dat)
anova(mod.pid2, mod.pid3, test="F")


###################################################
### chunk number 3: q2.1
###################################################
library(car)
mod <- lm(demtherm ~ age + racerec + income, data=dat)
Anova(mod)
dat[["newinc"]] <- as.numeric(dat[["income"]])
mod2 <- lm(demtherm ~ age + racerec + newinc,data=dat)
anova(mod2, mod, test="F")


###################################################
### chunk number 4: q21fig1
###################################################
plot(effect("income", mod))


###################################################
### chunk number 5: q21fig2
###################################################
plot(effect("newinc", mod2))


###################################################
### chunk number 6: q22
###################################################
library(splines)
AICs <- NULL
	for(i in 1:10){
		tmp <- lm(demtherm ~ age + racerec + 
			bs(newinc, knots=i, degree=3), data=dat)
		AICs <- c(AICs, AIC(tmp))	
	}
	which.min(AICs)
	mod <- lm(demtherm ~ age + racerec + 
		bs(newinc, knots=3, degree=3), data=dat)
	summary(mod)


###################################################
### chunk number 7: q22fig1
###################################################
	plot(effect("bs(newinc, knots=3, degree=3)", mod, default.levels=100))


###################################################
### chunk number 8: q22anova
###################################################
anova(mod, mod2, test="F")


###################################################
### chunk number 9: q23
###################################################
	library(mgcv)
	gam.mod <- gam(demtherm ~ age + racerec + s(newinc),data=dat)
	summary(gam.mod)


###################################################
### chunk number 10: q31
###################################################
mod <- lm(demtherm ~ racerec + pid3 + libcon*age, data=dat)


###################################################
### chunk number 11: q31fig1
###################################################
plot(effect("libcon*age", mod))


###################################################
### chunk number 12: q31fig2
###################################################
plot(effect("libcon*age", mod), x.var="age")


###################################################
### chunk number 13: q32
###################################################
DAintfun2(mod, c("libcon","age"), name.stem="q32", plot.type="pdf")


