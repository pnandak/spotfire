###################################################
### chunk number 1: sbmodels
###################################################
dat <- matrix(c(
1,  1,  1,  2,  5,  6,
1,  2,  3,  2,  7,  6,
2,  1,  2,  3,  4,  5,
2,  2,  4,  3,  6,  5,
3,  1,  3,  4,  3,  4,
3,  2,  5,  4,  5,  4,
4,  1,  4,  5,  2,  3,
4,  2,  6,  5,  4,  3,
5,  1,  5,  6,  1,  2,
5,  2,  7,  6,  3,  2), ncol=6, byrow=T)
reg1 <- lm(dat[,5] ~ dat[,3])
reg2 <- lm(dat[,6] ~ dat[,4])
reg3 <- lm(I(dat[,5]-dat[,6]) ~ I(dat[,3] - dat[,4]))
library(nlme)
mix1 <- lme(dat[,5] ~ dat[,4] +dat[,3] , random = ~ 1|dat[,1])
b1 <- coef(reg1)
b2 <- coef(reg2)
b3 <- coef(reg3)
bmix <- fixef(mix1)


###################################################
### chunk number 2: sbexfig
###################################################
plot(c(0,8), c(0,8), type="n", xlab="x", ylab="y")
abline(lm(dat[1:2, 5] ~ dat[1:2, 3]), lty=3)
abline(lm(dat[3:4, 5] ~ dat[3:4, 3]), lty=3)
abline(lm(dat[5:6, 5] ~ dat[5:6, 3]), lty=3)
abline(lm(dat[7:8, 5] ~ dat[7:8, 3]), lty=3)
abline(lm(dat[9:10, 5] ~ dat[9:10, 3]), lty=3)
points(dat[1:2, c(3,5)], pch=1, cex=2)
points(dat[3:4, c(3,5)], pch=2, cex=2)
points(dat[5:6, c(3,5)], pch=3, cex=2)
points(dat[7:8, c(3,5)], pch=4, cex=2)
points(dat[9:10, c(3,5)], pch=5, cex=2)
abline(8,-1,lty=2, lwd=2)
abline(lm(dat[,5] ~ dat[,3]), lty=1, lwd=2)
legend(8,8, c("Total Regression", "Regression Between Groups", 
    "Regressions Within Groups"), lty=c(1,2,3), xjust=1)


###################################################
### chunk number 3: coplot
###################################################
library(nlme)
library(foreign)
dat <- read.spss("context.sav",
    use.value.labels=T, to.data.frame=T)
sample30 <- with(dat, sample(unique(PANO), 30))
sample.new <- dat[which(dat[["PANO"]] %in% 
	sample30), ]
sample.new[["PANO"]] <- as.factor(as.character(
	sample.new[["PANO"]]))
library(car)
coplot(LRSCALE ~ INCOME | PANO, panel=panel.car,
    span=1, data=sample.new)


###################################################
### chunk number 4: mod1
###################################################
mod1 <- lmList(LRSCALE ~ INCOME | PANO, data=sample.new)



###################################################
### chunk number 5: mod1plot
###################################################
print(plot(intervals(mod1)))


###################################################
### chunk number 6: betweendat
###################################################
numdat <- dat[,c("LRSCALE", "AGE", "SEX", "INCOME", "DEGREE")]
for(i in 1:ncol(numdat)){
    numdat[,i] <- as.numeric(numdat[,i])
}
numdat[,c(3,5)] <- numdat[,c(3,5)]-1
between.dat <- by(numdat, list(dat[["PANO"]]), 
    function(x)apply(x, 2, mean, na.rm=T))
between.dat <- do.call(rbind, between.dat)
colnames(between.dat) <- colnames(numdat)
between.dat <- as.data.frame(between.dat)

between.mod <- lm(LRSCALE ~ AGE + SEX + INCOME + DEGREE,
    data=between.dat)


###################################################
### chunk number 7: sumbet
###################################################
summary(between.mod)


###################################################
### chunk number 8: winmodel
###################################################
unpano <- unique(dat[["PANO"]])
numdat.mean <- between.dat[match(as.numeric(as.character(dat[["PANO"]])), 
    as.numeric(rownames(between.dat))), ]
within.dat <- numdat - numdat.mean
within.mod <- lm(LRSCALE ~ AGE + SEX + INCOME + DEGREE, data=within.dat)
within.mod2 <- lm(LRSCALE ~ AGE + SEX + INCOME + DEGREE + dat[["PANO"]], 
    data=numdat)

mat <- cbind(round(coef(within.mod),5), round(coef(within.mod2)[1:5], 5))
colnames(mat) <- c("mod","mod2")
library(xtable)
xtable(mat)


###################################################
### chunk number 9: empty
###################################################
library(lme4)
empty.mod <- lmer(LRSCALE ~ (1|PANO), data=dat)
summary(empty.mod)


###################################################
### chunk number 10: onex
###################################################
onex.mod <- lmer(LRSCALE ~ INCOME + (1|PANO), data=dat)
summary(onex.mod)


###################################################
### chunk number 11: varints
###################################################
plot(range(dat[["INCOME"]]), c(12.5,19), type="n",
	xlab="Income", ylab="Predicted LRSCALE")
b <- coef(onex.mod)[["PANO"]]
for(i in 1:nrow(b)){
	abline(a=b[i,1], b=b[i,2])
}


###################################################
### chunk number 12: intdens
###################################################
sm.density(b[,1], xlab="Intercepts", ylab="Density", 
	model="normal")


###################################################
### chunk number 13: bweff
###################################################
bet.with.mod <- lmer(LRSCALE ~ INCOME + 
	numdat.mean[["INCOME"]] + (1|PANO), data=dat)
summary(bet.with.mod)


###################################################
### chunk number 14: ranslope
###################################################
inc.mod3 <- lmer(LRSCALE ~ INCOME + numdat.mean[["INCOME"]] + 
	(1+INCOME|PANO), data=dat)
summary(inc.mod3)


###################################################
### chunk number 15: compmods
###################################################
slope.mod <- lme(LRSCALE ~ INCOME + PROFMAN + AGE + SEX + DEGREE, 
    random = ~ 1+ INCOME | PANO, data=dat, method="ML")
int.mod <- lme(LRSCALE ~ INCOME+ PROFMAN + AGE + SEX + DEGREE, 
    random = ~ 1 | PANO, data=dat, method="ML")
ols.mod <- lm(LRSCALE ~ INCOME+ PROFMAN + AGE + SEX + DEGREE, 
	data=dat)


###################################################
### chunk number 16: anovas
###################################################
anova(slope.mod, ols.mod)
anova(slope.mod, int.mod)
anova(int.mod, ols.mod)


###################################################
### chunk number 17: diag1
###################################################
print(plot(int.mod))


###################################################
### chunk number 18: diag2
###################################################
print(plot(int.mod, resid(., type="p") ~
    fitted(.)|PANO[PANO %in% unique(PANO)[1:20]]))


###################################################
### chunk number 19: diag3
###################################################
print(qqnorm(int.mod))


###################################################
### chunk number 20: diag4
###################################################
print(plot(int.mod, PANO~resid(.), abline=0))


###################################################
### chunk number 21: final_mods
###################################################
int.mod <- lmer(LRSCALE ~ INCOME+ PROFMAN + AGE + SEX + DEGREE +  
    (1 | PANO), data=dat, REML=T)
int.modML <- lmer(LRSCALE ~ INCOME+ PROFMAN + AGE + SEX + DEGREE +  
    (1 | PANO), data=dat, REML=F)


###################################################
### chunk number 22: moremods
###################################################
int.mod2 <- lme(LRSCALE ~ INCOME+ PROFMAN + AGE + SEX + DEGREE, 
    random = ~ 1 | PANO, data=dat, method="REML")
int.modML2 <- lme(LRSCALE ~ INCOME+ PROFMAN + AGE + SEX + DEGREE, 
    random = ~ 1 | PANO, data=dat, method="ML")


###################################################
### chunk number 23: intervals
###################################################
intervals(int.mod2, which="var-cov")
intervals(int.modML2, which="var-cov")


###################################################
### chunk number 24: panel.mods
###################################################
library(car)
data(Blackmoor)
panel.mod <- lme(log(exercise+1) ~ age*group,
    random =~ age|subject, data=Blackmoor)
panel.mod2 <- lme(log(exercise+1) ~ age*group,
    random =~ age|subject, data=Blackmoor,
    correlation=corAR1(form=~age|subject))


###################################################
### chunk number 25: makepred
###################################################
plot.dat <- data.frame(preds = panel.mod2[["fitted"]][,2], 
	age = Blackmoor[["age"]], subject=Blackmoor[["subject"]])
plot.dat <- plot.dat[which(plot.dat[["subject"]] %in% 
	sample(unique(plot.dat[["subject"]]), 25)), ]
print(xyplot(preds ~ age | subject, data=plot.dat, type="l", col="black"))


###################################################
### chunk number 26: anova_panel
###################################################
anova(panel.mod, panel.mod2)


