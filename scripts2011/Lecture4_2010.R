###################################################
### chunk number 1: asfac
###################################################
library(car)
data(Duncan)
type2 <- with(Duncan, as.factor(type))
contrasts(type2)


###################################################
### chunk number 2: relev
###################################################
type2 <- relevel(type2, ref="bc")
contrasts(type2)


###################################################
### chunk number 3: lm1
###################################################
mod1<-lm(prestige~income+education+type, data=Duncan)
summary(mod1)


###################################################
### chunk number 4: xtanova
###################################################
library(xtable)
xtable(Anova(mod1))


###################################################
### chunk number 5: qv
###################################################
library(qvcalc)
qvtype<-qvcalc(mod1,"type")
summary(qvtype)


###################################################
### chunk number 6: plotqv
###################################################
plot(qvtype)


###################################################
### chunk number 7: facplot1
###################################################
factorplot(mod1,"type")


###################################################
### chunk number 8: contrsum
###################################################
contrasts(Duncan[["type"]])<-'contr.sum'
contrasts(Duncan[["type"]])


###################################################
### chunk number 9: duncanmod
###################################################
Duncan.mod <- lm(prestige ~ income*type, data=Duncan)


###################################################
### chunk number 10: duncantab
###################################################
library(apsrtable)
apsrtable(Duncan.mod, model.names="", Sweave=T)


###################################################
### chunk number 11: xtanduncan
###################################################
xtable(Anova(Duncan.mod))


###################################################
### chunk number 12: effprest
###################################################
data(Prestige)
mod <- lm(prestige ~ income * type, data=Prestige)
mod.effects <- effect("income*type", mod)
names(mod.effects)


###################################################
### chunk number 13: intypeeff1
###################################################
mod2<-lm(prestige~education+income*type, data=Prestige)
plot(effect("income*type", mod2))


###################################################
### chunk number 14: intypeeff2
###################################################
plot(effect("income*type", mod2), multiline=T)


###################################################
### chunk number 15: intfigs1
###################################################
set.seed(123)
x <- mvrnorm(250, c(0,0), matrix(c(1,.4,.4, 1), ncol=2))
y <- 2 + 3*x[,1] - 4*x[,2] + 3*x[,1]*x[,2] + 
	rnorm(250, 0,3)
df <- data.frame(y=y, x1=x[,1], x2=x[,2])
mod <- lm(y ~ x1*x2, data=df)
DAintfun2(mod, c("x1","x2"), name.stem="x12", 
	plot.type="pdf")


###################################################
### chunk number 16: intfun2
###################################################
DAintfun(mod, c("x1","x2"), theta=245, phi=30)


###################################################
### chunk number 17: cent_data
###################################################
set.seed(123)
X <- mvrnorm(1000, c(10,10), matrix(c(1,.4,.4,1), ncol=2), empirical=T)
X <- cbind(1, X, apply(X, 1, prod))
b <- matrix(c(2,3,-4,3), ncol=1)
Y <- X %*% b + rnorm(1000, 0, 4)
df <- data.frame(Y=Y, x1=X[,2], x2=X[,3])
mod1 <- lm(Y ~ x1*x2, data=df)
df2 <- as.data.frame(apply(df, 2, function(x)x-mean(x)))
mod2 <- lm(Y ~ x1*x2, data=df2)


###################################################
### chunk number 18: tabcent
###################################################
apsrtable(mod1, mod2, model.names=c("Not Cent","Cent"), Sweave=T)


###################################################
### chunk number 19: viftab
###################################################
tab <- cbind(vif(mod1), vif(mod2))
colnames(tab) <- c("No Cent","Cent")
xtable(tab, caption="VIF Statistics")


###################################################
### chunk number 20: pctiletab
###################################################
library(xtable)
tab <- cbind(
	with(df, quantile(x1, probs=c(.25,.5,.75))), 
	with(df2, quantile(x1, probs=c(.25,.5,.75))), 
	with(df, quantile(x2, probs=c(.25,.5,.75))), 
	with(df2, quantile(x2, probs=c(.25,.5,.75))))
rownames(tab) <- c("25th","50th","75th")
colnames(tab) <- c("x1","x1 (cent)","x2","x2 (cent)")
xtable(tab)


###################################################
### chunk number 21: condeff
###################################################
b1 <- coef(mod1)
b2 <- coef(mod2)
v1 <- vcov(mod1)
v2 <- vcov(mod2)
eff1.2 <- round(cbind(1, tab[,1]) %*% b1[c(3,4)], 3)
eff2.2 <- round(cbind(1, tab[,2]) %*% b2[c(3,4)], 3)
eff1.1 <- round(cbind(1, tab[,3]) %*% b1[c(2,4)], 3)
eff2.1 <- round(cbind(1, tab[,4]) %*% b2[c(2,4)], 3)

se1.2 <- round(sqrt(diag(cbind(1, tab[,1]) %*% v1[c(3,4), c(3,4)] %*% t(cbind(1, tab[,1])))), 3)
se2.2 <- round(sqrt(diag(cbind(1, tab[,2]) %*% v2[c(3,4), c(3,4)] %*% t(cbind(1, tab[,2])))), 3)
se1.1 <- round(sqrt(diag(cbind(1, tab[,3]) %*% v1[c(2,4), c(2,4)] %*% t(cbind(1, tab[,3])))), 3)
se2.1 <- round(sqrt(diag(cbind(1, tab[,4]) %*% v2[c(2,4), c(2,4)] %*% t(cbind(1, tab[,4])))), 3)

tab2 <- cbind(c(rbind(c(eff1.1), c(se1.1))), c(rbind(c(eff2.1), c(se2.1))),c(rbind(c(eff1.2), c(se1.2))), c(rbind(c(eff2.2), c(se2.2))))

rownames(tab2) <- c("25th","","50th"," ","75th","  ")
colnames(tab2) <- c("eff x1",  "eff x1 (cent)", "eff x2", "eff x2 (cent)")
xtable(tab2, caption = "Conditional Effects of x1 and x2")


###################################################
### chunk number 22: modrelimp
###################################################
mod1<-lm(prestige~income+education+type, data=Duncan)
apsrtable(mod1, model.names="", Sweave=T)


###################################################
### chunk number 23: relimpsum
###################################################
library(relimp)
relimp(mod1, set1=2, set2=4:5)


