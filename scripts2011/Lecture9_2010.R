###################################################
### chunk number 1: plot1
###################################################
weakliem <- read.table("Weakliem2.txt")
plot(secpay ~ gini, data=weakliem)
outs <- which(rownames(weakliem) %in% 
	c("Slovakia","CzechRepublic"))
with(weakliem, text(gini[outs], secpay[outs], 
	rownames(weakliem)[outs], pos=4))


###################################################
### chunk number 2: modsum1
###################################################
mod <- lm(secpay ~ gini, data=weakliem)
library(apsrtable)
apsrtable(mod, model.names = "", Sweave=T, digits=4)


###################################################
### chunk number 3: infl
###################################################
library(car)
influencePlot(mod, identify="auto")


###################################################
### chunk number 4: modsum2
###################################################
mod2 <- lm(secpay ~ gini, data=weakliem, subset=-c(7,26))
apsrtable(mod2, model.names="", Sweave=T, digits=4)


###################################################
### chunk number 5: rob1
###################################################
library(MASS)
mod3 <- rlm(secpay ~ gini, data=weakliem) 
summary(mod3)


###################################################
### chunk number 6: plotres
###################################################
plot(residuals(mod3))
text(outs, residuals(mod3)[outs], 
	rownames(weakliem)[outs], pos=2)


###################################################
### chunk number 7: loww1
###################################################
loww <- with(mod3, which(w < 1))
with(mod3, plot(w))
text(loww, mod3[["w"]][loww], 
	rownames(weakliem)[loww], pos=2)


###################################################
### chunk number 8: rob2
###################################################
mod4 <- rlm(secpay ~ gini, data=weakliem, psi= psi.bisquare)
summary(mod4)


###################################################
### chunk number 9: plotres4
###################################################
with(mod4, plot(residuals))
text(outs, mod4[["residuals"]][outs], 
	rownames(weakliem)[outs], pos=2)


###################################################
### chunk number 10: wcompare
###################################################
plot(mod4[["w"]], mod3[["w"]], xlim=c(0,1),
    ylim=c(0,1), xlab="bisquare weights",
    ylab="Huber Weights")
abline(0,1)
text(mod4[["w"]][loww], mod3[["w"]][loww], 
	rownames(weakliem)[loww], pos=2)


###################################################
### chunk number 11: robmm
###################################################
mod5 <- rlm(secpay ~ gini, data=weakliem, method="MM")
summary(mod5)


###################################################
### chunk number 12: compmod
###################################################
mat <- matrix(NA, ncol=3, nrow=5)
mat[1,1:2] <- c(coef(mod)[2], sqrt(diag(vcov(mod))[2]))
mat[2,1:2] <- c(coef(mod2)[2], sqrt(diag(vcov(mod2))[2]))
mat[3,1:2] <- c(coef(mod3)[2], sqrt(diag(vcov(mod3))[2]))
mat[4,1:2] <- c(coef(mod4)[2], sqrt(diag(vcov(mod4))[2]))
mat[5,1:2] <- c(coef(mod5)[2], sqrt(diag(vcov(mod5))[2]))
mat[,3] <- mat[,1]/mat[,2]
colnames(mat) <- c("Estimate", "SE", "t")
rownames(mat) <- c("OLS (with outliers)", "OLS (no outliers)", 
	"M (Huber)", "M (bisquare)", "MM (Huber)")
library(xtable)
xtable(mat, digits=4)


###################################################
### chunk number 13: compfig
###################################################
plot(secpay ~ gini, data=weakliem)
abline(mod, lty=1)
abline(mod2, lty=2)
abline(mod3, lty=3)
abline(mod4, lty=4)
abline(mod5, lty=5)
legend("topright", rownames(mat), lty=1:5, inset=.01)


###################################################
### chunk number 14: rr1
###################################################
plot(residuals(mod) ~ residuals(mod3))
abline(lm(residuals(mod) ~ residuals(mod3)), lty=3)
abline(0,1, col="gray60")
legend("bottomright", c("LM Line", "45-degree"), lty=c(3,1), 
	col=c("black", "gray60"), inset=.01)


###################################################
### chunk number 15: rr2
###################################################
plot(residuals(mod) ~ residuals(mod4))
abline(lm(residuals(mod) ~ residuals(mod4)), lty=3)
abline(0,1, col="gray60")
legend("bottomright", c("LM Line", "45-degree"), lty=c(3,1), 
	col=c("black", "gray60"), inset=.01)


###################################################
### chunk number 16: rr3
###################################################
plot(residuals(mod) ~ residuals(mod5))
abline(lm(residuals(mod) ~ residuals(mod5)), lty=3)
abline(0,1, col="gray60")
legend("bottomright", c("LM Line", "45-degree"), lty=c(3,1), 
	col=c("black", "gray60"), inset=.01)


###################################################
### chunk number 17: assocmod1
###################################################
assoc <- read.spss("assoc.sav", 
	use.value.labels=T, to.data.frame=T)[1:1000, ]
assoc.mod <- glm(ASSOC ~ SEX + AGE + SES, 
	data=assoc, family=poisson(link=log))
apsrtable(assoc.mod, model.names="", Sweave=T, digits=4)


###################################################
### chunk number 18: glmcook
###################################################
plot(cookd(assoc.mod))
bigd <- which(cookd(assoc.mod) > .05)
text(bigd, cookd(assoc.mod)[bigd], 
	as.character(bigd), pos=1)


###################################################
### chunk number 19: glmdfb
###################################################
dfb <- dfbetas(assoc.mod)
plot(dfb[,6], ylab = "DFBetas for Unskilled")
text(bigd, dfb[bigd, 6], 
	as.character(bigd), pos=1)


###################################################
### chunk number 20: bigdobs
###################################################
assoc[bigd, ]


###################################################
### chunk number 21: glmrob
###################################################
library(robustbase)
glm2 <- glmrob(ASSOC ~ SEX + AGE + SES, data=assoc, family=poisson(link=log))
summary(glm2)


###################################################
### chunk number 22: exptab
###################################################
mat <- cbind(exp(coef(assoc.mod)), exp(coef(glm2)))
colnames(mat) <- c("GLM", "Robust GLM")
xtable(mat)


