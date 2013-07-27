###################################################
### chunk number 1: 
###################################################
ufc <- read.csv(file="t:/R-workshop/workshop/data/ufc.csv")


###################################################
### chunk number 2: see.ufc
###################################################
dim(ufc)         # Reports the number of rows and columns
names(ufc)       # Lists the column names
ufc[1:5,]        # Prints the first 10 rows of ufc


###################################################
### chunk number 3: ufc.dataframe
###################################################
ufc <- read.csv("t:/R-workshop/workshop/data/ufc.csv")      # ufc is a dataframe
is.data.frame(ufc)                      # we hope
dim(ufc)                                # the size of the dimensions (r,c)
names(ufc)                              # the labels of the columns
ufc$Height[1:5]                         # first 10 heights
ufc$Species[1:5]                        # first 10 species


###################################################
### chunk number 4: ufc.dataframe
###################################################
ufc$dbh.cm <- ufc$Dbh/10            # Height now in metres
ufc$height.m <- ufc$Height/10            # Height now in metres


###################################################
### chunk number 5: ufc.dataframe
###################################################
temp <- data.frame(my.species=ufc$Species,
                   my.dbh=ufc$dbh.cm)
temp[1:5,]


###################################################
### chunk number 6: ufc.refs
###################################################
ufc$height.m[ufc$Species=="LP"]         # Heights of lodgepole pine    
mean(ufc$height.m[ufc$Species=="LP"])   # Average height of lodgepole pine


###################################################
### chunk number 7: ufc.refs
###################################################
ufc$Species[order(ufc$height.m, decreasing = T)][1:3]


###################################################
### chunk number 8: ufc.refs
###################################################
tapply(ufc$height.m, ufc$Species, mean)   # Average height by species


###################################################
### chunk number 9: ufc.refs
###################################################
tapply(ufc$height.m, ufc$Species, mean, na.rm=T)   # Average height by species


###################################################
### chunk number 10: ufc.refs
###################################################
format(tapply(ufc$height.m, ufc$Species, mean, na.rm=T), dig=3) 


###################################################
### chunk number 11: ufc.refs
###################################################
ht.bar.by.species <- tapply(ufc$height.m, ufc$Species, mean, na.rm=T)  
ht.bar.by.species


###################################################
### chunk number 12: ufc.refs
###################################################
species.order.by.ht <- order(ht.bar.by.species, decreasing = T)
species.order.by.ht


###################################################
### chunk number 13: ufc.refs
###################################################
species.by.ht <- levels(ufc$Species)[species.order.by.ht]
species.by.ht


###################################################
### chunk number 14: ufc.refs
###################################################
species.by.ht[2]


###################################################
### chunk number 15: ufc.refs
###################################################
median(ufc$height.m[ufc$Species == species.by.ht[2]], na.rm=T)


###################################################
### chunk number 16: ufc.refs
###################################################
median(ufc$height.m[ufc$Species == levels(ufc$Species)[order(tapply(ufc$height.m, ufc$Species, mean, na.rm=T), decreasing = T)][2]], na.rm=T)


###################################################
### chunk number 17: fig-ufc
###################################################
plot(ufc$dbh.cm, ufc$height.m, xlab = "Diameter (cm)", ylab = "Height (m)")


###################################################
### chunk number 18: ufc
###################################################
plot(ufc$dbh.cm, ufc$height.m, xlab = "Diameter (cm)", ylab = "Height (m)")


###################################################
### chunk number 19: 
###################################################
rm(list=ls())
ufc <- read.csv("t:/R-workshop/workshop/data/ufc.csv")


###################################################
### chunk number 20: 
###################################################
dim(ufc)


###################################################
### chunk number 21: 
###################################################
names(ufc)


###################################################
### chunk number 22: 
###################################################
ufc[1:10, ]


###################################################
### chunk number 23: 
###################################################
ufc$dbh.cm <- ufc$Dbh/10
ufc$height.m <- ufc$Height/10


###################################################
### chunk number 24: 
###################################################
table(ufc$Species)
tapply(ufc$dbh.cm, ufc$Species, length)
aggregate(x=list(num.trees=ufc$dbh.cm),
          by=list(species=ufc$Species),
          FUN=length)


###################################################
### chunk number 25: 
###################################################
ufc$Species[is.na(ufc$Dbh)] <- NA
ufc$Species <- factor(ufc$Species)


###################################################
### chunk number 26: 
###################################################
ufc <- ufc[!is.na(ufc$height.m), ]


###################################################
### chunk number 27: 
###################################################
sd(ufc$height.m, na.rm=T)


###################################################
### chunk number 28: 
###################################################
tapply(ufc$height.m, ufc$Species, sd, na.rm = TRUE)


###################################################
### chunk number 29: 
###################################################
hd.lm.1 <- lm(height.m ~ dbh.cm, data = ufc)


###################################################
### chunk number 30: diagnostic
###################################################
opar <- par(mfrow = c(2, 2), mar=c(4, 4, 4, 1))
plot(hd.lm.1)
par(opar)


###################################################
### chunk number 31: diagnostic
###################################################
opar <- par(mfrow = c(2, 2), mar=c(4, 4, 4, 1))
plot(hd.lm.1)
par(opar)


###################################################
### chunk number 32: 
###################################################
ufc[order(abs(residuals(hd.lm.1)), decreasing=TRUE), ][1:2, ]


###################################################
### chunk number 33: 
###################################################
hd.res.1 <- abs(residuals(hd.lm.1))
hd.lm.1a <- lm(height.m ~ dbh.cm, data = ufc, 
subset = (hd.res.1 < hd.res.1[order(hd.res.1, decreasing=TRUE)][2]))


###################################################
### chunk number 34: params
###################################################
opar <- par(las=1)
plot(coef(hd.lm.1), coef(hd.lm.1a), xlab="Parameters (all data)",
     ylab="Parameters (without outliers)")
text(coef(hd.lm.1)[1]-2, coef(hd.lm.1a)[1]-0.5, expression(hat(beta)[0]), cex=2, col="blue")
text(coef(hd.lm.1)[2]+2, coef(hd.lm.1a)[2]+0.5,  expression(hat(beta)[1]), cex=2, col="blue")
points(summary(hd.lm.1)$sigma, summary(hd.lm.1a)$sigma)
text(summary(hd.lm.1)$sigma+1, summary(hd.lm.1a)$sigma, 
     expression(hat(sigma)[epsilon]), cex=2, col="darkgreen")
abline(0, 1, col="darkgrey")
par(opar)


###################################################
### chunk number 35: params
###################################################
opar <- par(las=1)
plot(coef(hd.lm.1), coef(hd.lm.1a), xlab="Parameters (all data)",
     ylab="Parameters (without outliers)")
text(coef(hd.lm.1)[1]-2, coef(hd.lm.1a)[1]-0.5, expression(hat(beta)[0]), cex=2, col="blue")
text(coef(hd.lm.1)[2]+2, coef(hd.lm.1a)[2]+0.5,  expression(hat(beta)[1]), cex=2, col="blue")
points(summary(hd.lm.1)$sigma, summary(hd.lm.1a)$sigma)
text(summary(hd.lm.1)$sigma+1, summary(hd.lm.1a)$sigma, 
     expression(hat(sigma)[epsilon]), cex=2, col="darkgreen")
abline(0, 1, col="darkgrey")
par(opar)


###################################################
### chunk number 36: 
###################################################
summary(hd.lm.1)


###################################################
### chunk number 37: 
###################################################
names(hd.lm.1)
names(summary(hd.lm.1))


###################################################
### chunk number 38: 
###################################################
hd.lm.1$call
summary(hd.lm.1)$sigma


###################################################
### chunk number 39: 
###################################################
hd.lm.2 <- lm(height.m ~ dbh.cm + Species, data = ufc) 
hd.lm.3 <- lm(height.m ~ dbh.cm * Species, data = ufc) 


###################################################
### chunk number 40: 
###################################################
objective.function <- function(parameters, x, y) {
  - sum( dnorm(y - parameters[1] - 
               parameters[2] * x, 0, parameters[3], log = T))
}
good.fit <- optim(c(1, 1, 1), 
                  objective.function, hessian=TRUE, 
                  x = ufc$dbh.cm, 
                  y = ufc$height.m)
good.fit$par
sqrt(diag(solve(good.fit$hessian)))


###################################################
### chunk number 41: fig-colours
###################################################
opar <- par(oma=c(0,0,0,0), mar=c(0,0,0,0))
x1 <- rep(1:10, 10)
x2 <- rep(1:10, each=10)
x3 <- 1:100
interesting.colour.numbers <- c(1:152,253:259,362:657)
plot.us <- sample(interesting.colour.numbers, size=max(x3))
plot(x1, x2, col=colors()[plot.us], pch=20, cex=10, axes=F,
     ylim=c(0,10), xlim=c(0, 10.5))
text(x1, x2-0.5, colors()[plot.us], cex=0.3)
text(x1+0.4, x2-0.4, plot.us, cex=0.5)
par(opar)


###################################################
### chunk number 42: colours
###################################################
opar <- par(oma=c(0,0,0,0), mar=c(0,0,0,0))
x1 <- rep(1:10, 10)
x2 <- rep(1:10, each=10)
x3 <- 1:100
interesting.colour.numbers <- c(1:152,253:259,362:657)
plot.us <- sample(interesting.colour.numbers, size=max(x3))
plot(x1, x2, col=colors()[plot.us], pch=20, cex=10, axes=F,
     ylim=c(0,10), xlim=c(0, 10.5))
text(x1, x2-0.5, colors()[plot.us], cex=0.3)
text(x1+0.4, x2-0.4, plot.us, cex=0.5)
par(opar)


###################################################
### chunk number 43: require
###################################################
require(lattice)


###################################################
### chunk number 44: fig-trellis
###################################################
ufc$height.hat <- fitted(hd.lm.1)
top.nine <- levels(ufc$Species)[order(table(ufc$Species), decreasing=T)][1:9]

print(xyplot(height.m ~ height.hat | Species, data = ufc, 
             xlab="Predicted Height (m)", ylab="Measured Height (m)",
             panel = function(x, y) {
               panel.xyplot(x, y)
               panel.abline(lm(y~x), col="red")
               panel.abline(0, 1, col="blue", lty=2)
             },
             subset=ufc$Species %in% top.nine
))


###################################################
### chunk number 45: trellis
###################################################
ufc$height.hat <- fitted(hd.lm.1)
top.nine <- levels(ufc$Species)[order(table(ufc$Species), decreasing=T)][1:9]

print(xyplot(height.m ~ height.hat | Species, data = ufc, 
             xlab="Predicted Height (m)", ylab="Measured Height (m)",
             panel = function(x, y) {
               panel.xyplot(x, y)
               panel.abline(lm(y~x), col="red")
               panel.abline(0, 1, col="blue", lty=2)
             },
             subset=ufc$Species %in% top.nine
))


###################################################
### chunk number 46: 
###################################################
rm(list = ls())
stage <- read.csv("t:/R-workshop/workshop/data/stage.csv")
stage$Tree.ID <- factor(stage$Tree.ID)
stage$Forest.ID <- factor(stage$Forest, labels = c("Kaniksu", 
    "Coeur d'Alene", "St. Joe", "Clearwater", "Nez Perce", "Clark Fork", 
    "Umatilla", "Wallowa", "Payette"))
stage$HabType.ID <- factor(stage$HabType, labels = c("Ts/Pac", 
    "Ts/Op", "Th/Pach", "AG/Pach", "PA/Pach"))
stage$dbhib.cm <- stage$Dbhib * 2.54
stage$height.m <- stage$Height / 3.2808399
stage[1:10,]


###################################################
### chunk number 47: fig-stage-1
###################################################
opar <- par(las=1)
plot(stage$dbhib.cm, stage$height.m, xlab="Dbhib (cm)", ylab="Height (m)")
par(opar)


###################################################
### chunk number 48: stage1
###################################################
opar <- par(las=1)
plot(stage$dbhib.cm, stage$height.m, xlab="Dbhib (cm)", ylab="Height (m)")
par(opar)


###################################################
### chunk number 49: fig-stage-2
###################################################
colours <- c("deepskyblue","goldenrod","purple",
             "orangered2","seagreen")
par(mfrow=c(3,3), pty="m", mar=c(3, 2, 3, 1) + 0.1)
for (i in 1:length(levels(stage$Forest.ID))) {
  thisForest <- levels(stage$Forest.ID)[i]
  forestData <- stage[stage$Forest.ID==thisForest,]
  plot(stage$dbhib.cm, stage$height.m, xlab = "", ylab = "",
       main = thisForest, type="n")
  theseTrees <- factor(forestData$Tree.ID)
  legend(0, max(stage$height.m),
         unique(as.character(forestData$HabType.ID)), 
         xjust=0, yjust=1, bty="n",
         col=colours[unique(forestData$HabType)],
         lty=unique(forestData$HabType)+1)
  for (j in 1:length(levels(theseTrees))) {
    thisTree <- levels(theseTrees)[j]
    lines(forestData$dbhib.cm[forestData$Tree.ID==thisTree],
          forestData$height.m[forestData$Tree.ID==thisTree],
          col=colours[forestData$HabType[forestData$Tree.ID==thisTree]],
          lty=forestData$HabType[forestData$Tree.ID==thisTree]+1)
  }
}
mtext("Height (m)", outer=T, side=2, line=2)
mtext("Diameter (cm)", outer=T, side=1, line=2)


###################################################
### chunk number 50: stage2
###################################################
colours <- c("deepskyblue","goldenrod","purple",
             "orangered2","seagreen")
par(mfrow=c(3,3), pty="m", mar=c(3, 2, 3, 1) + 0.1)
for (i in 1:length(levels(stage$Forest.ID))) {
  thisForest <- levels(stage$Forest.ID)[i]
  forestData <- stage[stage$Forest.ID==thisForest,]
  plot(stage$dbhib.cm, stage$height.m, xlab = "", ylab = "",
       main = thisForest, type="n")
  theseTrees <- factor(forestData$Tree.ID)
  legend(0, max(stage$height.m),
         unique(as.character(forestData$HabType.ID)), 
         xjust=0, yjust=1, bty="n",
         col=colours[unique(forestData$HabType)],
         lty=unique(forestData$HabType)+1)
  for (j in 1:length(levels(theseTrees))) {
    thisTree <- levels(theseTrees)[j]
    lines(forestData$dbhib.cm[forestData$Tree.ID==thisTree],
          forestData$height.m[forestData$Tree.ID==thisTree],
          col=colours[forestData$HabType[forestData$Tree.ID==thisTree]],
          lty=forestData$HabType[forestData$Tree.ID==thisTree]+1)
  }
}
mtext("Height (m)", outer=T, side=2, line=2)
mtext("Diameter (cm)", outer=T, side=1, line=2)


###################################################
### chunk number 51: 
###################################################
require(nlme)


###################################################
### chunk number 52: 
###################################################
straw <- data.frame(y = c(10.1, 14.9, 15.9, 13.1, 4.2, 4.8, 5.8, 1.2),
                    x = c(1, 2, 3, 4, 1, 2, 3, 4),
                    group = factor(c(1, 1, 1, 1, 2, 2, 2, 2)))


###################################################
### chunk number 53: fig-simple1
###################################################
colours = c("red", "blue")
plot(straw$x, straw$y, col = colours[straw$group])


###################################################
### chunk number 54: simple1
###################################################
colours = c("red", "blue")
plot(straw$x, straw$y, col = colours[straw$group])


###################################################
### chunk number 55: 
###################################################
basic.1 <- lm(y ~ x, data=straw)


###################################################
### chunk number 56: 
###################################################
basic.2 <- lm(y ~ x + group, data=straw)


###################################################
### chunk number 57: 
###################################################
basic.3 <- lm(y ~ x * group, data=straw)


###################################################
### chunk number 58: 
###################################################
straw.mixed <- groupedData(y ~ x | group, data=straw)


###################################################
### chunk number 59: 
###################################################
basic.4 <- lme(y ~ x, random = ~1 | group, data = straw.mixed)


###################################################
### chunk number 60: fig-simple4
###################################################
print(plot(augPred(basic.4)))


###################################################
### chunk number 61: 
###################################################
basic.5 <- lme(y ~ x, random = ~1 | group, 
               weights = varIdent(form = ~1 | group), 
               data = straw.mixed)


###################################################
### chunk number 62: 
###################################################
basic.6 <- lme(y ~ x, random = ~1 | group, 
               weights = varIdent(form = ~1 | group), 
               correlation = corAR1(), 
               data = straw.mixed)


###################################################
### chunk number 63: fig-simple7
###################################################
opar <- par(las=1)
colours <- c("blue", "darkgreen", "plum")
plot(straw$x, straw$y)
for (g in 1:2) lines(straw$x[straw$group == levels(straw$group)[g]], 
                     fitted(basic.1)[straw$group == 
                                     levels(straw$group)[g]], 
                     col = colours[1])
for (g in 1:2) lines(straw$x[straw$group == levels(straw$group)[g]], 
                     fitted(basic.2)[straw$group == 
                                     levels(straw$group)[g]], 
                     col = colours[2])
for (g in 1:2) lines(straw$x[straw$group == levels(straw$group)[g]], 
                     fitted(basic.4)[straw$group == 
                                     levels(straw$group)[g]], 
                     col = colours[3])
legend(2.5, 13, lty = rep(1, 3), col = colours, 
       legend = c("Mean Only", "Intercept Fixed", "Intercept Random"))
par(opar)


###################################################
### chunk number 64: simple4
###################################################
print(plot(augPred(basic.4)))


###################################################
### chunk number 65: simple7
###################################################
opar <- par(las=1)
colours <- c("blue", "darkgreen", "plum")
plot(straw$x, straw$y)
for (g in 1:2) lines(straw$x[straw$group == levels(straw$group)[g]], 
                     fitted(basic.1)[straw$group == 
                                     levels(straw$group)[g]], 
                     col = colours[1])
for (g in 1:2) lines(straw$x[straw$group == levels(straw$group)[g]], 
                     fitted(basic.2)[straw$group == 
                                     levels(straw$group)[g]], 
                     col = colours[2])
for (g in 1:2) lines(straw$x[straw$group == levels(straw$group)[g]], 
                     fitted(basic.4)[straw$group == 
                                     levels(straw$group)[g]], 
                     col = colours[3])
legend(2.5, 13, lty = rep(1, 3), col = colours, 
       legend = c("Mean Only", "Intercept Fixed", "Intercept Random"))
par(opar)


###################################################
### chunk number 66: 
###################################################
rm(list = ls())
stage <- read.csv("t:/R-workshop/workshop/data/stage.csv")
dim(stage)
names(stage)
sapply(stage, class)


###################################################
### chunk number 67: 
###################################################
stage$Tree.ID <- factor(stage$Tree.ID)
stage$Forest.ID <- factor(stage$Forest, labels = c("Kaniksu", 
    "Coeur d'Alene", "St. Joe", "Clearwater", "Nez Perce", "Clark Fork", 
    "Umatilla", "Wallowa", "Payette"))
stage$HabType.ID <- factor(stage$HabType, labels = c("Ts/Pac", 
    "Ts/Op", "Th/Pach", "AG/Pach", "PA/Pach"))


###################################################
### chunk number 68: 
###################################################
stage$dbhib.cm <- stage$Dbhib * 2.54
stage$height.m <- stage$Height / 3.2808399
stage[1:10,]


###################################################
### chunk number 69: 
###################################################
stage.old <- stage[stage$Decade == 0, ]


###################################################
### chunk number 70: 
###################################################
hd.lm.1 <- lm(height.m ~ dbhib.cm * HabType.ID, 
        data = stage.old, 
        subset = HabType.ID !=  "Ts/Op")


###################################################
### chunk number 71: fig-hd-lm-1
###################################################
opar <- par(mfrow=c(2,2), mar=c(4, 4, 4, 1))
plot(hd.lm.1)
par(opar)


###################################################
### chunk number 72: s-hd-lm-1
###################################################
opar <- par(mfrow=c(2,2), mar=c(4, 4, 4, 1))
plot(hd.lm.1)
par(opar)


###################################################
### chunk number 73: 
###################################################
summary(hd.lm.1)


###################################################
### chunk number 74: 
###################################################
sd(stage.old$height.m)
summary(hd.lm.1)$sigma


###################################################
### chunk number 75: 
###################################################
summary(lm(height.m ~ dbhib.cm, data = stage.old, 
        subset = HabType.ID !=  "Ts/Op"))$sigma


###################################################
### chunk number 76: 
###################################################
require(nlme)
stage.old <- groupedData(height.m ~ dbhib.cm | Forest.ID, 
                         data = stage.old)


###################################################
### chunk number 77: 
###################################################
hd.lme.1 <- lme(height.m ~ dbhib.cm, random = ~1 | Forest.ID, 
      data = stage.old)


###################################################
### chunk number 78: fig-hd-lme-1
###################################################
opar <- par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), las = 1, cex.axis = 0.9)
plot(fitted(hd.lme.1, level=0), stage.old$height.m, 
    xlab = "Fitted Values (height, m.)", 
    ylab = "Observed Values (height, m.)",  main = "Model Structure (I)")
abline(0, 1, col = "blue")
scatter.smooth(fitted(hd.lme.1), residuals(hd.lme.1, type="pearson"), 
    main = "Model Structure (II)", 
    xlab = "Fitted Values", ylab = "Innermost Residuals")
abline(h = 0, col = "red")
ref.forest <- ranef(hd.lme.1)[[1]]
ref.var.forest <- tapply(residuals(hd.lme.1, type="pearson", level=1),
                      stage.old$Forest.ID,  var)
qqnorm(ref.forest, main="Q-Q Normal - Forest Random Effects")
qqline(ref.forest, col="red")
qqnorm(residuals(hd.lme.1, type="pearson"), main="Q-Q Normal - Residuals")
qqline(residuals(hd.lme.1, type="pearson"), col="red")
boxplot(residuals(hd.lme.1, type="pearson", level=1) ~ stage.old$Forest.ID, 
        ylab = "Innermost Residuals", xlab = "National Forest",
        notch=T, varwidth = T, at=rank(ref.forest))
axis(3, labels=format(ref.forest, dig=2), cex.axis=0.8,
     at=rank(ref.forest))
abline(h=0, col="darkgreen")
plot(ref.forest, ref.var.forest, xlab="Forest Random Effect",
     ylab="Variance of within-Forest Residuals")
abline(lm(ref.var.forest ~ ref.forest), col="purple")
par(opar)


###################################################
### chunk number 79: diag-lme-1
###################################################
opar <- par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), las = 1, cex.axis = 0.9)
plot(fitted(hd.lme.1, level=0), stage.old$height.m, 
    xlab = "Fitted Values (height, m.)", 
    ylab = "Observed Values (height, m.)",  main = "Model Structure (I)")
abline(0, 1, col = "blue")
scatter.smooth(fitted(hd.lme.1), residuals(hd.lme.1, type="pearson"), 
    main = "Model Structure (II)", 
    xlab = "Fitted Values", ylab = "Innermost Residuals")
abline(h = 0, col = "red")
ref.forest <- ranef(hd.lme.1)[[1]]
ref.var.forest <- tapply(residuals(hd.lme.1, type="pearson", level=1),
                      stage.old$Forest.ID,  var)
qqnorm(ref.forest, main="Q-Q Normal - Forest Random Effects")
qqline(ref.forest, col="red")
qqnorm(residuals(hd.lme.1, type="pearson"), main="Q-Q Normal - Residuals")
qqline(residuals(hd.lme.1, type="pearson"), col="red")
boxplot(residuals(hd.lme.1, type="pearson", level=1) ~ stage.old$Forest.ID, 
        ylab = "Innermost Residuals", xlab = "National Forest",
        notch=T, varwidth = T, at=rank(ref.forest))
axis(3, labels=format(ref.forest, dig=2), cex.axis=0.8,
     at=rank(ref.forest))
abline(h=0, col="darkgreen")
plot(ref.forest, ref.var.forest, xlab="Forest Random Effect",
     ylab="Variance of within-Forest Residuals")
abline(lm(ref.var.forest ~ ref.forest), col="purple")
par(opar)


###################################################
### chunk number 80: hackknife
###################################################
all.betas <- data.frame(labels=names(unlist(hd.lme.1$coefficients)))
cook.0 <- cook.1 <- rep(NA, dim(stage.old)[1])

p.sigma.0 <- length(hd.lme.1$coefficients$fixed) *
         var(residuals(hd.lme.1, level=0))
p.sigma.1 <- length(hd.lme.1$coefficients$fixed) *
         var(residuals(hd.lme.1, level=1))

for (i in 1:dim(stage.old)[1]) {
  try({ hd.lme.n <- update(hd.lme.1, data = stage.old[-i,])
        new.betas <- data.frame(labels=names(unlist(hd.lme.n$coefficients)),
                                coef=unlist(hd.lme.n$coefficients))
        names(new.betas)[2] <- paste("obs", i, sep=".")
        all.betas <- merge(all.betas, new.betas, all.x = TRUE)
        cook.0[i] <- sum((predict(hd.lme.1, level=0, newdata=stage.old) -
                         predict(hd.lme.n, level=0, newdata=stage.old))^2) /
                           p.sigma.0
        cook.1[i] <- sum((predict(hd.lme.1, level=1, newdata=stage.old) -
                         predict(hd.lme.n, level=1, newdata=stage.old))^2) /
                           p.sigma.1
      })
}


###################################################
### chunk number 81: 
###################################################
all.betas <- t(all.betas[,-1])

len.all <- length(unlist(hd.lme.1$coefficients))
len.fixed <- length(hd.lme.1$coefficients$fixed) # 2
len.ran <- length(hd.lme.1$coefficients$random$Forest.ID) # 9


###################################################
### chunk number 82: fig-hd-lme-1-param
###################################################
opar <- par(mfrow=c(len.all, 1), oma=c(2,0,1,0), mar=c(0,4,0,0), las=1)
for (i in 1:len.fixed) {
  plot(all.betas[,i], type="l", axes=F, xlab="", ylab="")
  text(length(all.betas[,i])-1, max(all.betas[,i], na.rm=T),
       names(unlist(hd.lme.1$coefficients))[i],
       adj=c(1,1), col="red")
  axis(2)
  box()
}
for (i in (len.fixed+1):(len.all)) {
  plot(all.betas[,i], type="l", axes=F, xlab="", ylab="")
  text(length(all.betas[,i])-1, max(all.betas[,i], na.rm=T),
       names(unlist(hd.lme.1$coefficients))[i],
       adj=c(1,1), col="red")
  axis(2)
  box()
}
axis(1)
par(opar)


###################################################
### chunk number 83: diag-lme-1-p
###################################################
opar <- par(mfrow=c(len.all, 1), oma=c(2,0,1,0), mar=c(0,4,0,0), las=1)
for (i in 1:len.fixed) {
  plot(all.betas[,i], type="l", axes=F, xlab="", ylab="")
  text(length(all.betas[,i])-1, max(all.betas[,i], na.rm=T),
       names(unlist(hd.lme.1$coefficients))[i],
       adj=c(1,1), col="red")
  axis(2)
  box()
}
for (i in (len.fixed+1):(len.all)) {
  plot(all.betas[,i], type="l", axes=F, xlab="", ylab="")
  text(length(all.betas[,i])-1, max(all.betas[,i], na.rm=T),
       names(unlist(hd.lme.1$coefficients))[i],
       adj=c(1,1), col="red")
  axis(2)
  box()
}
axis(1)
par(opar)


###################################################
### chunk number 84: fig-hd-lme-1-cook
###################################################
cook <- data.frame(id=stage.old$Tree.ID, fixed=cook.0, forest=cook.1)
influential <- apply(cook[,2:3], 1, max) > 1
plot(cook$fixed, cook$forest, type="n",
    xlab="Outermost (Fixed effects only)", 
    ylab="Innermost (Fixed effects and random effects)")
points(cook$fixed[!influential], cook$forest[!influential])
if(sum(influential) > 0)
   text(cook$fixed[influential], cook$forest[influential], 
      cook$id[influential], col="red", cex=0.85)


###################################################
### chunk number 85: diag-lme-1-cook
###################################################
cook <- data.frame(id=stage.old$Tree.ID, fixed=cook.0, forest=cook.1)
influential <- apply(cook[,2:3], 1, max) > 1
plot(cook$fixed, cook$forest, type="n",
    xlab="Outermost (Fixed effects only)", 
    ylab="Innermost (Fixed effects and random effects)")
points(cook$fixed[!influential], cook$forest[!influential])
if(sum(influential) > 0)
   text(cook$fixed[influential], cook$forest[influential], 
      cook$id[influential], col="red", cex=0.85)


###################################################
### chunk number 86: 
###################################################
summary(hd.lme.1)


###################################################
### chunk number 87: 
###################################################
anova(hd.lme.1)


###################################################
### chunk number 88: 
###################################################
stage <- groupedData(height.m ~ dbhib.cm | Forest.ID/Tree.ID, data = stage)


###################################################
### chunk number 89: 
###################################################
hd.lme.3 <- lme(height.m ~ dbhib.cm, 
   random = ~1 | Forest.ID/Tree.ID, 
     data = stage)


###################################################
### chunk number 90: hd-lme-3a
###################################################
opar <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), las = 1, 
        cex.axis = 0.9)
  plot(fitted(hd.lme.3, level=0), stage$height.m, 
      xlab = "Fitted Values", ylab = "Observed Values", 
      main = "Model Structure (I)")
  abline(0, 1, col = "gray")
  scatter.smooth(fitted(hd.lme.3), residuals(hd.lme.3, type="pearson"), 
      main = "Model Structure (II)", 
      xlab = "Fitted Values", ylab = "Innermost Residuals")
  abline(h = 0, col = "gray")
acf.resid <- ACF(hd.lme.3, resType = "normal")
plot(acf.resid$lag[acf.resid$lag < 10.5], 
     acf.resid$ACF[acf.resid$lag < 10.5], 
     type="b", main="Autocorrelation", 
     xlab="Lag", ylab="Correlation")
stdv <- qnorm(1 - 0.01/2)/sqrt(attr(acf.resid, "n.used"))
lines(acf.resid$lag[acf.resid$lag < 10.5], 
      stdv[acf.resid$lag < 10.5], 
      col="darkgray")
lines(acf.resid$lag[acf.resid$lag < 10.5], 
      -stdv[acf.resid$lag < 10.5], 
      col="darkgray")
abline(0,0,col="gray")
par(opar)


###################################################
### chunk number 91: hd-lme-3a-d
###################################################
opar <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), las = 1, 
        cex.axis = 0.9)
  plot(fitted(hd.lme.3, level=0), stage$height.m, 
      xlab = "Fitted Values", ylab = "Observed Values", 
      main = "Model Structure (I)")
  abline(0, 1, col = "gray")
  scatter.smooth(fitted(hd.lme.3), residuals(hd.lme.3, type="pearson"), 
      main = "Model Structure (II)", 
      xlab = "Fitted Values", ylab = "Innermost Residuals")
  abline(h = 0, col = "gray")
acf.resid <- ACF(hd.lme.3, resType = "normal")
plot(acf.resid$lag[acf.resid$lag < 10.5], 
     acf.resid$ACF[acf.resid$lag < 10.5], 
     type="b", main="Autocorrelation", 
     xlab="Lag", ylab="Correlation")
stdv <- qnorm(1 - 0.01/2)/sqrt(attr(acf.resid, "n.used"))
lines(acf.resid$lag[acf.resid$lag < 10.5], 
      stdv[acf.resid$lag < 10.5], 
      col="darkgray")
lines(acf.resid$lag[acf.resid$lag < 10.5], 
      -stdv[acf.resid$lag < 10.5], 
      col="darkgray")
abline(0,0,col="gray")
par(opar)


###################################################
### chunk number 92: hd-lme-3b
###################################################
opar <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), las = 1, 
        cex.axis = 0.9)
  ref.forest <- ranef(hd.lme.3, level=1, standard=T)[[1]]
  ref.tree <- ranef(hd.lme.3, level=2, standard=T)[[1]]

  ref.tree.frame <- ranef(hd.lme.3, level=2, augFrame=T, standard=T)

  ref.var.tree <- tapply(residuals(hd.lme.3, type="pearson", level=1),
                      stage$Tree.ID,  var)
  ref.var.forest <- tapply(ref.tree, ref.tree.frame$Forest, var)  

  qqnorm(ref.forest, main = "QQ plot: Forest")
  qqline(ref.forest)
  qqnorm(ref.tree, main = "QQ plot: Tree")
  qqline(ref.tree)
  qqnorm(residuals(hd.lme.3, type="pearson"), main="QQ plot: Residuals")
  qqline(residuals(hd.lme.3, type="pearson"), col="red")
par(opar)


###################################################
### chunk number 93: hd-lme-3b-d
###################################################
opar <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), las = 1, 
        cex.axis = 0.9)
  ref.forest <- ranef(hd.lme.3, level=1, standard=T)[[1]]
  ref.tree <- ranef(hd.lme.3, level=2, standard=T)[[1]]

  ref.tree.frame <- ranef(hd.lme.3, level=2, augFrame=T, standard=T)

  ref.var.tree <- tapply(residuals(hd.lme.3, type="pearson", level=1),
                      stage$Tree.ID,  var)
  ref.var.forest <- tapply(ref.tree, ref.tree.frame$Forest, var)  

  qqnorm(ref.forest, main = "QQ plot: Forest")
  qqline(ref.forest)
  qqnorm(ref.tree, main = "QQ plot: Tree")
  qqline(ref.tree)
  qqnorm(residuals(hd.lme.3, type="pearson"), main="QQ plot: Residuals")
  qqline(residuals(hd.lme.3, type="pearson"), col="red")
par(opar)


###################################################
### chunk number 94: hd-lme-3c
###################################################
opar <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), las = 1, 
        cex.axis = 0.9)

boxplot(ref.tree ~ ref.tree.frame$Forest, 
        ylab = "Tree Effects", xlab = "National Forest",
        notch=T, varwidth = T, at=rank(ref.forest))
axis(3, labels=format(ref.forest, dig=2), cex.axis=0.8,
     at=rank(ref.forest))
abline(h=0, col="darkgreen")

boxplot(residuals(hd.lme.3, type="pearson", level=1) ~ stage$Tree.ID, 
        ylab = "Innermost Residuals", xlab = "Tree",
        notch=T, varwidth = T, at=rank(ref.tree))
axis(3, labels=format(ref.tree, dig=2), cex.axis=0.8,
     at=rank(ref.tree))
abline(h=0, col="darkgreen")

plot(ref.forest, ref.var.forest, xlab="Forest Random Effect",
     ylab="Variance of within-Forest Residuals")
abline(lm(ref.var.forest ~ ref.forest), col="purple")

plot(ref.tree, ref.var.tree, xlab="Tree Random Effect",
     ylab="Variance of within-Tree Residuals")
abline(lm(ref.var.forest ~ ref.forest), col="purple")

par(opar)


###################################################
### chunk number 95: hd-lme-3c-d
###################################################
opar <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), las = 1, 
        cex.axis = 0.9)

boxplot(ref.tree ~ ref.tree.frame$Forest, 
        ylab = "Tree Effects", xlab = "National Forest",
        notch=T, varwidth = T, at=rank(ref.forest))
axis(3, labels=format(ref.forest, dig=2), cex.axis=0.8,
     at=rank(ref.forest))
abline(h=0, col="darkgreen")

boxplot(residuals(hd.lme.3, type="pearson", level=1) ~ stage$Tree.ID, 
        ylab = "Innermost Residuals", xlab = "Tree",
        notch=T, varwidth = T, at=rank(ref.tree))
axis(3, labels=format(ref.tree, dig=2), cex.axis=0.8,
     at=rank(ref.tree))
abline(h=0, col="darkgreen")

plot(ref.forest, ref.var.forest, xlab="Forest Random Effect",
     ylab="Variance of within-Forest Residuals")
abline(lm(ref.var.forest ~ ref.forest), col="purple")

plot(ref.tree, ref.var.tree, xlab="Tree Random Effect",
     ylab="Variance of within-Tree Residuals")
abline(lm(ref.var.forest ~ ref.forest), col="purple")

par(opar)


###################################################
### chunk number 96: hd.tree
###################################################
trees.in.forests <- aggregate(x=list(measures=stage$height.m), 
    by=list(tree=stage$Tree.ID, forest=stage$Forest.ID), FUN=length)
panel.order <- rank(as.numeric(as.character(trees.in.forests$tree)))
print(plot(augPred(hd.lme.3), index.cond=list(panel.order),
strip = strip.custom(par.strip.text=list(cex=0.5))))


###################################################
### chunk number 97: hd-tree
###################################################
trees.in.forests <- aggregate(x=list(measures=stage$height.m), 
    by=list(tree=stage$Tree.ID, forest=stage$Forest.ID), FUN=length)
panel.order <- rank(as.numeric(as.character(trees.in.forests$tree)))
print(plot(augPred(hd.lme.3), index.cond=list(panel.order),
strip = strip.custom(par.strip.text=list(cex=0.5))))


###################################################
### chunk number 98: hd.lme.4
###################################################
hd.lme.4 <- lme(height.m ~ dbhib.cm + I(dbhib.cm^2), 
   random = ~ dbhib.cm + I(dbhib.cm^2) | Forest.ID/Tree.ID, 
     data = stage)


###################################################
### chunk number 99: hd-lme-4
###################################################
opar <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), las = 1, 
        cex.axis = 0.9)
  plot(fitted(hd.lme.4, level=0), stage$height.m, 
      xlab = "Fitted Values", ylab = "Observed Values", 
      main = "Model Structure (I)")
  abline(0, 1, col = "gray")
  scatter.smooth(fitted(hd.lme.4), residuals(hd.lme.4, type="pearson"), 
      main = "Model Structure (II)", 
      xlab = "Fitted Values", ylab = "Innermost Residuals")
  abline(0, 0, col = "gray")
acf.resid <- ACF(hd.lme.4, resType = "n")
plot(acf.resid$lag[acf.resid$lag < 10.5], 
     acf.resid$ACF[acf.resid$lag < 10.5], 
     type="b", main="Autocorrelation", 
     xlab="Lag", ylab="Correlation")
stdv <- qnorm(1 - 0.01/2)/sqrt(attr(acf.resid, "n.used"))
lines(acf.resid$lag[acf.resid$lag < 10.5], 
      stdv[acf.resid$lag < 10.5], 
      col="darkgray")
lines(acf.resid$lag[acf.resid$lag < 10.5], 
      -stdv[acf.resid$lag < 10.5], 
      col="darkgray")
abline(0,0,col="gray")
par(opar)


###################################################
### chunk number 100: hd-lme-4-d
###################################################
opar <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), las = 1, 
        cex.axis = 0.9)
  plot(fitted(hd.lme.4, level=0), stage$height.m, 
      xlab = "Fitted Values", ylab = "Observed Values", 
      main = "Model Structure (I)")
  abline(0, 1, col = "gray")
  scatter.smooth(fitted(hd.lme.4), residuals(hd.lme.4, type="pearson"), 
      main = "Model Structure (II)", 
      xlab = "Fitted Values", ylab = "Innermost Residuals")
  abline(0, 0, col = "gray")
acf.resid <- ACF(hd.lme.4, resType = "n")
plot(acf.resid$lag[acf.resid$lag < 10.5], 
     acf.resid$ACF[acf.resid$lag < 10.5], 
     type="b", main="Autocorrelation", 
     xlab="Lag", ylab="Correlation")
stdv <- qnorm(1 - 0.01/2)/sqrt(attr(acf.resid, "n.used"))
lines(acf.resid$lag[acf.resid$lag < 10.5], 
      stdv[acf.resid$lag < 10.5], 
      col="darkgray")
lines(acf.resid$lag[acf.resid$lag < 10.5], 
      -stdv[acf.resid$lag < 10.5], 
      col="darkgray")
abline(0,0,col="gray")
par(opar)


###################################################
### chunk number 101: 
###################################################
hd.lme.5 <- update(hd.lme.4, correlation = corCAR1())


###################################################
### chunk number 102: hd-lme-5
###################################################
opar <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), las = 1, 
        cex.axis = 0.9)
  plot(fitted(hd.lme.5, level=0), stage$height.m, 
      xlab = "Fitted Values", ylab = "Observed Values", 
      main = "Model Structure (I)")
  abline(0, 1, col = "gray")
  scatter.smooth(fitted(hd.lme.5), residuals(hd.lme.5, type="pearson"), 
      main = "Model Structure (II)", 
      xlab = "Fitted Values", ylab = "Innermost Residuals")
  abline(0, 0, col = "gray")
acf.resid <- ACF(hd.lme.5, resType = "n")
plot(acf.resid$lag[acf.resid$lag < 10.5], 
     acf.resid$ACF[acf.resid$lag < 10.5], 
     type="b", main="Autocorrelation", 
     xlab="Lag", ylab="Correlation")
stdv <- qnorm(1 - 0.01/2)/sqrt(attr(acf.resid, "n.used"))
lines(acf.resid$lag[acf.resid$lag < 10.5], 
      stdv[acf.resid$lag < 10.5], 
      col="darkgray")
lines(acf.resid$lag[acf.resid$lag < 10.5], 
      -stdv[acf.resid$lag < 10.5], 
      col="darkgray")
abline(0,0,col="gray")
par(opar)


###################################################
### chunk number 103: hd-lme-5-d
###################################################
opar <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), las = 1, 
        cex.axis = 0.9)
  plot(fitted(hd.lme.5, level=0), stage$height.m, 
      xlab = "Fitted Values", ylab = "Observed Values", 
      main = "Model Structure (I)")
  abline(0, 1, col = "gray")
  scatter.smooth(fitted(hd.lme.5), residuals(hd.lme.5, type="pearson"), 
      main = "Model Structure (II)", 
      xlab = "Fitted Values", ylab = "Innermost Residuals")
  abline(0, 0, col = "gray")
acf.resid <- ACF(hd.lme.5, resType = "n")
plot(acf.resid$lag[acf.resid$lag < 10.5], 
     acf.resid$ACF[acf.resid$lag < 10.5], 
     type="b", main="Autocorrelation", 
     xlab="Lag", ylab="Correlation")
stdv <- qnorm(1 - 0.01/2)/sqrt(attr(acf.resid, "n.used"))
lines(acf.resid$lag[acf.resid$lag < 10.5], 
      stdv[acf.resid$lag < 10.5], 
      col="darkgray")
lines(acf.resid$lag[acf.resid$lag < 10.5], 
      -stdv[acf.resid$lag < 10.5], 
      col="darkgray")
abline(0,0,col="gray")
par(opar)


###################################################
### chunk number 104: hab5a
###################################################
print(plot(hd.lme.5, resid(.) ~ fitted(.) | HabType.ID, layout=c(1, 5)))


###################################################
### chunk number 105: hab5b
###################################################
print(qqmath(~ resid(hd.lme.5) | stage$HabType.ID,
            prepanel = prepanel.qqmathline,
            panel = function(x, y) {
               panel.qqmathline(y, distribution = qnorm)
               panel.qqmath(x, y)
            }))


###################################################
### chunk number 106: habitat5a
###################################################
print(plot(hd.lme.5, resid(.) ~ fitted(.) | HabType.ID, layout=c(1, 5)))


###################################################
### chunk number 107: habitat5b
###################################################
print(qqmath(~ resid(hd.lme.5) | stage$HabType.ID,
            prepanel = prepanel.qqmathline,
            panel = function(x, y) {
               panel.qqmathline(y, distribution = qnorm)
               panel.qqmath(x, y)
            }))


###################################################
### chunk number 108: hd.tree.5
###################################################
print(plot(augPred(hd.lme.5), index.cond=list(panel.order),
 strip = strip.custom(par.strip.text=list(cex=0.5))))


###################################################
### chunk number 109: hd-tree-5
###################################################
print(plot(augPred(hd.lme.5), index.cond=list(panel.order),
 strip = strip.custom(par.strip.text=list(cex=0.5))))


###################################################
### chunk number 110: 
###################################################
summary(hd.lme.5)


###################################################
### chunk number 111: addedv
###################################################
age.lme.1 <- lme(Age ~ dbhib.cm, random = ~1 | Forest.ID/Tree.ID, 
      data = stage)
res.Age <- residuals(age.lme.1, level = 0)
res.HD <- residuals(hd.lme.5, level = 0)
scatter.smooth(res.Age, res.HD, xlab = "Variation unique to Age", 
     ylab = "Variation in Height after all but Age")


###################################################
### chunk number 112: added
###################################################
age.lme.1 <- lme(Age ~ dbhib.cm, random = ~1 | Forest.ID/Tree.ID, 
      data = stage)
res.Age <- residuals(age.lme.1, level = 0)
res.HD <- residuals(hd.lme.5, level = 0)
scatter.smooth(res.Age, res.HD, xlab = "Variation unique to Age", 
     ylab = "Variation in Height after all but Age")


###################################################
### chunk number 113: addedhab
###################################################
print(xyplot(stage$height.m ~ fitted(hd.lme.5, level=0) | HabType.ID,
        xlab="Predicted height (m)",
        ylab="Observed height (m)",
        data=stage,
        panel = function(x, y, subscripts) {
                panel.xyplot(x, y)
                panel.abline(0, 1)
                panel.abline(lm(y ~ x), lty=3)
        }
))


###################################################
### chunk number 114: hd-hab
###################################################
print(xyplot(stage$height.m ~ fitted(hd.lme.5, level=0) | HabType.ID,
        xlab="Predicted height (m)",
        ylab="Observed height (m)",
        data=stage,
        panel = function(x, y, subscripts) {
                panel.xyplot(x, y)
                panel.abline(0, 1)
                panel.abline(lm(y ~ x), lty=3)
        }
))


###################################################
### chunk number 115: 
###################################################
require(nlme) 


###################################################
### chunk number 116: packages
###################################################
ip <- installed.packages()     # Save the output as an object!
class(ip)                      # It is a matrix
ip <- as.data.frame(ip)        # Now it is a dataframe
names(ip)                      # These are the variable names
length(ip$Package)             # And the packages are numerous
ip$Package


###################################################
### chunk number 117: gettingEquivalence
###################################################
#get.equivalence <- download.packages("equivalence", 
#               destdir="t:/R-workshop/workshop/library",
#               repos="http://cran.stat.sfu.ca/")
#get.equivalence
#install.packages(get.equivalence[1,2], repos=NULL, 
#              lib="t:/R-workshop/workshop/library")
#library(equivalence, lib.loc="t:/R-workshop/workshop/library")


###################################################
### chunk number 118: gettingBoot
###################################################
#get.boot <- download.packages("boot", destdir="t:/R-workshop/workshop/library",
#                              repos="http://cran.stat.sfu.ca/")
#get.boot
#install.packages(get.boot[1,2], repos=NULL, 
#               lib="/R-workshop/workshop/library")
#library(boot, lib.loc="t:/R-workshop/workshop/library")
#library(equivalence, lib.loc="t:/R-workshop/workshop/library")


###################################################
### chunk number 119: 
###################################################
my.function <- function(arguments) {
}


###################################################
### chunk number 120: 
###################################################
cm.to.inches <- function(data) {
      data / 2.54
  }


###################################################
### chunk number 121: 
###################################################
cm.to.inches(25)


###################################################
### chunk number 122: 
###################################################
cm.to.inches(c(25, 35, 45))


###################################################
### chunk number 123: scoping
###################################################
x <- pi
radius.2.area <- function(radius) {
  x * radius^2
}
radius.2.area(4)


###################################################
### chunk number 124: 
###################################################
x <- c(1:10)
class(x)
class(x) <- "eh"
class(x)


###################################################
### chunk number 125: 
###################################################
print.eh <- function(x) print(x[length(x)]) 
print(x)
x


###################################################
### chunk number 126: 
###################################################
length(x)


###################################################
### chunk number 127: 
###################################################
x * x
class(x * x)
length(x * x)


###################################################
### chunk number 128: 
###################################################
class(x) <- "integer"
x


###################################################
### chunk number 129: 
###################################################
dim(stage)
max.age.stage <- aggregate(x=list(maxAge = stage$Age),
                           by=list(Tree.ID = stage$Tree.ID),
                           FUN = max, na.rm=TRUE)
dim(max.age.stage)
max.age.stage[1:10,]
temp <- merge(stage, max.age.stage)
dim(temp)
temp[1:10,]
stage <- temp
rm(temp, max.age.stage)
stage$year <- stage

