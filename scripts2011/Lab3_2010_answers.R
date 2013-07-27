###################################################
### chunk number 1: q1mod
###################################################
library(nlme)
library(foreign)
dat <- read.dta("wvs_lab3.dta")
newdata <- na.omit(dat)
mod.list <- lmList(satisfied ~ married + org_mem + ind_democ +
 	weekrel | nation, data=newdata)


###################################################
### chunk number 2: q1fig
###################################################
print(plot(intervals(mod.list)))


###################################################
### chunk number 3: q2mods
###################################################
X <- as.matrix(dat[,-1])
by.X <- by(X, list(dat[["nation"]]), apply, 2, mean, na.rm=T)
between.dat <- as.data.frame(do.call(rbind, by.X))
summary(b.mod1 <- lm(satisfied ~ org_mem, data=between.dat))
summary(b.mod2 <- lm(satisfied ~ married, data=between.dat))
summary(b.mod3 <- lm(satisfied ~ ind_democ, data=between.dat))
summary(b.mod4 <- lm(satisfied ~ weekrel, data=between.dat))
summary(b.mod5 <- lm(satisfied ~ gdppc, data=between.dat))
summary(b.mod6 <- lm(satisfied ~ civlibs, data=between.dat))


###################################################
### chunk number 4: q3mods
###################################################
lme.mod <- lme(satisfied ~ married + org_mem + ind_democ  + 
	weekrel + gdppc + civlibs, random = ~ 1|nation, 
	data=newdata, method="ML" )
summary(lme.mod)


###################################################
### chunk number 5: q4mods
###################################################
newdata[["mean_ind_democ"]] <- between.dat[["ind_democ"]][match(newdata[["nation"]],
 	rownames(between.dat))]

lme.mod2 <- lme(satisfied ~ married + org_mem +  weekrel + gdppc +
 	civlibs + ind_democ*mean_ind_democ, 
	random = ~ 1 + ind_democ|nation, 
	data=newdata , method="ML")
	summary(lme.mod2)
anova(lme.mod, lme.mod2)


###################################################
### chunk number 6: q5mods
###################################################
library(Amelia)
library(Zelig)
dat.am <- amelia(dat, idvars ="nation")

fixef.mod <- zelig(satisfied ~ married + org_mem + ind_democ +
 	weekrel + as.factor(nation), data=dat.am[["imputations"]], 	
	model="normal")
summary(fixef.mod)


