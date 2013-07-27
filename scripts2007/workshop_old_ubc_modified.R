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
ufc$dbh.cm <-ufc$Dbh/10            # Height now in metres
ufc$height.m <-ufc$Height/10            # Height now in metres


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
ht.bar.by.species <- tapply(ufc$height.m, ufc$Species, mean, na.rm=T)  
ht.bar.by.species


###################################################
### chunk number 11: ufc.refs
###################################################
species.order.by.ht <- order(ht.bar.by.species, decreasing = T)
species.order.by.ht


###################################################
### chunk number 12: ufc.refs
###################################################
species.by.ht <- levels(ufc$Species)[species.order.by.ht]
species.by.ht


###################################################
### chunk number 13: ufc.refs
###################################################
species.by.ht[2]


###################################################
### chunk number 14: ufc.refs
###################################################
median(ufc$height.m[ufc$Species == species.by.ht[2]], na.rm=T)


###################################################
### chunk number 15: ufc.refs
###################################################
median(ufc$height.m[ufc$Species == levels(ufc$Species)[order(tapply(ufc$height.m, ufc$Species, mean, na.rm=T), decreasing = T)][2]], na.rm=T)


###################################################
### chunk number 16: fig-ufc
###################################################
plot(ufc$dbh.cm, ufc$height.m, xlab = "Diameter (cm)", ylab = "Height (m)")


###################################################
### chunk number 17: ufc
###################################################
plot(ufc$dbh.cm, ufc$height.m, xlab = "Diameter (cm)", ylab = "Height (m)")


###################################################
### chunk number 18: 
###################################################
rm(list=ls())
ufc <- read.csv("t:/R-workshop/workshop/data/ufc.csv")


###################################################
### chunk number 19: 
###################################################
dim(ufc)


###################################################
### chunk number 20: 
###################################################
names(ufc)


###################################################
### chunk number 21: 
###################################################
ufc[1:10, ]


###################################################
### chunk number 22: 
###################################################
ufc$dbh.cm <- ufc$Dbh/10
ufc$height.m <- ufc$Height/10


###################################################
### chunk number 23: 
###################################################
table(ufc$Species)
tapply(ufc$dbh.cm, ufc$Species, length)
aggregate(x=list(num.trees=ufc$dbh.cm),
          by=list(species=ufc$Species),
          FUN=length)


###################################################
### chunk number 24: 
###################################################
ufc$Species[is.na(ufc$Dbh)] <- NA
ufc$Species <- factor(ufc$Species)


###################################################
### chunk number 25: 
###################################################
ufc <- ufc[!is.na(ufc$height.m), ]


###################################################
### chunk number 26: 
###################################################
sd(ufc$height.m, na.rm=T)


###################################################
### chunk number 27: 
###################################################
tapply(ufc$height.m, ufc$Species, sd, na.rm = TRUE)


###################################################
### chunk number 28: 
###################################################
hd.lm.1 <- lm(height.m ~ dbh.cm, data = ufc)


###################################################
### chunk number 29: diagnostic
###################################################
opar <- par(mfrow = c(2, 2), mar=c(4, 4, 4, 1))
plot(hd.lm.1)
par(opar)


###################################################
### chunk number 30: diagnostic
###################################################
opar <- par(mfrow = c(2, 2), mar=c(4, 4, 4, 1))
plot(hd.lm.1)
par(opar)


###################################################
### chunk number 31: 
###################################################
ufc[order(abs(residuals(hd.lm.1)), decreasing=TRUE), ][1:2, ]


###################################################
### chunk number 32: 
###################################################
hd.res.1 <- abs(residuals(hd.lm.1))
hd.lm.1a <- lm(height.m ~ dbh.cm, data = ufc, 
subset = (hd.res.1 < hd.res.1[order(hd.res.1, decreasing=TRUE)][2]))


###################################################
### chunk number 33: params
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
### chunk number 35: 
###################################################
summary(hd.lm.1)


###################################################
### chunk number 36: 
###################################################
names(hd.lm.1)
names(summary(hd.lm.1))


###################################################
### chunk number 37: 
###################################################
hd.lm.1$call
summary(hd.lm.1)$sigma


###################################################
### chunk number 38: 
###################################################
hd.lm.2 <- lm(height.m ~ dbh.cm + Species, data = ufc) 
hd.lm.3 <- lm(height.m ~ dbh.cm * Species, data = ufc) 


###################################################
### chunk number 39: 
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
### chunk number 40: fig-colours
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
### chunk number 41: colours
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
### chunk number 42: require
###################################################
require(lattice)


###################################################
### chunk number 43: fig-trellis
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
### chunk number 44: trellis
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
### chunk number 45: 
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
### chunk number 46: fig-stage-1
###################################################
opar <- par(las=1)
plot(stage$dbhib.cm, stage$height.m, xlab="Dbhib (c)", ylab="Height (m)")
par(opar)


###################################################
### chunk number 47: stage1
###################################################
opar <- par(las=1)
plot(stage$dbhib.cm, stage$height.m, xlab="Dbhib (c)", ylab="Height (m)")
par(opar)


###################################################
### chunk number 48: fig-stage-2
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
### chunk number 49: stage2
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
### chunk number 50: 
###################################################
require(nlme)


###################################################
### chunk number 51: 
###################################################
straw <- data.frame(y = c(10.1, 14.9, 15.9, 13.1, 4.2, 4.8, 5.8, 1.2),
                    x = c(1, 2, 3, 4, 1, 2, 3, 4),
                    group = factor(c(1, 1, 1, 1, 2, 2, 2, 2)))


###################################################
### chunk number 52: fig-simple1
###################################################
colours = c("red", "blue")
plot(straw$x, straw$y, col = colours[straw$group])


###################################################
### chunk number 53: simple1
###################################################
colours = c("red", "blue")
plot(straw$x, straw$y, col = colours[straw$group])


###################################################
### chunk number 54: 
###################################################
basic.1 <- lm(y ~ x, data=straw)


###################################################
### chunk number 55: 
###################################################
basic.2 <- lm(y ~ x + group, data=straw)


###################################################
### chunk number 56: 
###################################################
basic.3 <- lm(y ~ x * group, data=straw)


###################################################
### chunk number 57: 
###################################################
straw.mixed <- groupedData(y ~ x | group, data=straw)


###################################################
### chunk number 58: 
###################################################
basic.4 <- lme(y ~ x, random = ~1 | group, data = straw.mixed)


###################################################
### chunk number 59: fig-simple4
###################################################
print(plot(augPred(basic.4)))


###################################################
### chunk number 60: 
###################################################
basic.5 <- lme(y ~ x, random = ~1 | group, 
               weights = varIdent(form = ~1 |  group), 
               data = straw.mixed)


###################################################
### chunk number 61: 
###################################################
basic.6 <- lme(y ~ x, random = ~1 | group, 
               weights = varIdent(form = ~1 | group), 
               correlation = corAR1(), 
               data = straw.mixed)


###################################################
### chunk number 62: fig-simple7
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
### chunk number 63: simple4
###################################################
print(plot(augPred(basic.4)))


###################################################
### chunk number 64: simple7
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
### chunk number 65: 
###################################################
rm(list = ls())
stage <- read.csv("t:/R-workshop/workshop/data/stage.csv")
dim(stage)
names(stage)
sapply(stage, class)


###################################################
### chunk number 66: 
###################################################
stage$Tree.ID <- factor(stage$Tree.ID)
stage$Forest.ID <- factor(stage$Forest, labels = c("Kaniksu", 
    "Coeur d'Alene", "St. Joe", "Clearwater", "Nez Perce", "Clark Fork", 
    "Umatilla", "Wallowa", "Payette"))
stage$HabType.ID <- factor(stage$HabType, labels = c("Ts/Pac", 
    "Ts/Op", "Th/Pach", "AG/Pach", "PA/Pach"))


###################################################
### chunk number 67: 
###################################################
stage$dbhib.cm <- stage$Dbhib * 2.54
stage$height.m <- stage$Height / 3.2808399
stage[1:10,]


###################################################
### chunk number 68: 
###################################################
tapply(as.numeric(stage$Decade), stage$Tree.ID, min)


###################################################
### chunk number 69: 
###################################################
stage[stage$Tree.ID == 32, ]


###################################################
### chunk number 70: 
###################################################
stage.hd <- stage[stage$Decade == 0, ]
stage.hd <- rbind(stage.hd, stage[stage$Tree.ID == 32 & 
        stage$Decade == 1, ])
stage.hd$HD <- stage.hd$Height/stage.hd$Dbhib


###################################################
### chunk number 71: 
###################################################
lm.hd.1 <- lm(HD ~ HabType.ID + Forest.ID, data = stage.hd, 
         subset = HabType.ID !=  "Ts/Op")


###################################################
### chunk number 72: fig-hd-lm-1
###################################################
opar <- par(mfrow=c(2,2), mar=c(4, 4, 4, 1))
plot(lm.hd.1)
par(opar)


###################################################
### chunk number 73: s-hd-lm-1
###################################################
opar <- par(mfrow=c(2,2), mar=c(4, 4, 4, 1))
plot(lm.hd.1)
par(opar)


###################################################
### chunk number 74: 
###################################################
require(MASS)


###################################################
### chunk number 75: boxcox
###################################################
boxcox(HD ~ HabType.ID + Forest.ID, data = stage.hd)


###################################################
### chunk number 76: boxcox
###################################################
boxcox(HD ~ HabType.ID + Forest.ID, data = stage.hd)


###################################################
### chunk number 77: 
###################################################
summary(lm.hd.1)


###################################################
### chunk number 78: 
###################################################
sd(stage.hd$HD)


###################################################
### chunk number 79: 
###################################################
require(nlme)
lme.hd.1 <- lme(HD ~ Forest.ID, random = ~1 | HabType.ID, 
      data = stage.hd)


###################################################
### chunk number 80: fig-hd-lme-1
###################################################
opar <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), las = 1, cex.axis = 0.9)
plot(fitted(lme.hd.1), fitted(lme.hd.1) + residuals(lme.hd.1), 
    xlab = "Fitted Values", ylab = "Observed Values", 
    main = "Model Structure (I)")
abline(0, 1, col = "gray")
plot(fitted(lme.hd.1), residuals(lme.hd.1), main = "Model Structure (II)", 
    xlab = "Fitted Values", ylab = "Conditional Residuals")
abline(h = 0, col = "gray")
qqnorm(ranef(lme.hd.1)[[1]], main = "QQ plot: Habitat Type")
qqline(ranef(lme.hd.1)[[1]])
boxplot(residuals(lme.hd.1) ~ stage.hd$HabType.ID, 
    ylab = "Innermost Residuals", xlab = "Habitat Type", varwidth = T, 
    main = "Within-group Residuals")
par(opar)


###################################################
### chunk number 81: diag-lme-1
###################################################
opar <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), las = 1, cex.axis = 0.9)
plot(fitted(lme.hd.1), fitted(lme.hd.1) + residuals(lme.hd.1), 
    xlab = "Fitted Values", ylab = "Observed Values", 
    main = "Model Structure (I)")
abline(0, 1, col = "gray")
plot(fitted(lme.hd.1), residuals(lme.hd.1), main = "Model Structure (II)", 
    xlab = "Fitted Values", ylab = "Conditional Residuals")
abline(h = 0, col = "gray")
qqnorm(ranef(lme.hd.1)[[1]], main = "QQ plot: Habitat Type")
qqline(ranef(lme.hd.1)[[1]])
boxplot(residuals(lme.hd.1) ~ stage.hd$HabType.ID, 
    ylab = "Innermost Residuals", xlab = "Habitat Type", varwidth = T, 
    main = "Within-group Residuals")
par(opar)


###################################################
### chunk number 82: 
###################################################
summary(lme.hd.1)


###################################################
### chunk number 83: 
###################################################
anova(lme.hd.1)


###################################################
### chunk number 84: 
###################################################
lm.hd.2 <- lm(HD ~ Forest.ID, data = stage.hd)
anova(lme.hd.1, lm.hd.2)


###################################################
### chunk number 85: 
###################################################
lme.hd.2 <- lme(HD ~ Forest.ID + Age, random = ~1 | HabType.ID, 
      data = stage.hd)


###################################################
### chunk number 86: 
###################################################
stage$HD <- stage$height.m/stage$dbhib.cm
stage <- groupedData(HD ~ Age | HabType.ID/Tree.ID, data = stage)


###################################################
### chunk number 87: 
###################################################
lme.hd.3 <- lme(HD ~ Age + Forest.ID, random = ~1 | HabType.ID/Tree.ID, 
     data = stage)


###################################################
### chunk number 88: hd-lme-3
###################################################
opar <- par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), las = 1, 
        cex.axis = 0.9)
  plot(fitted(lme.hd.3), fitted(lme.hd.3) + residuals(lme.hd.3), 
      xlab = "Fitted Values", ylab = "Observed Values", 
      main = "Model Structure (I)")
  abline(0, 1, col = "gray")
  plot(fitted(lme.hd.3), residuals(lme.hd.3), 
      main = "Model Structure (II)", 
      xlab = "Fitted Values", ylab = "Conditional Residuals")
  abline(0, 0, col = "gray")
  qqnorm(random.effects(lme.hd.3)[[1]][,1], main = "QQ plot: Habitat Type")
  qqline(random.effects(lme.hd.3)[[1]][,1])
  qqnorm(random.effects(lme.hd.3)[[2]][,1], main = "QQ plot: Tree")
  qqline(random.effects(lme.hd.3)[[2]][,1])
  randomEffects = as.data.frame.table(table(stage$Tree.ID, stage$HabType.ID))
  randomEffects <- randomEffects[randomEffects$Freq > 0, ]
  randomEffects$b.hat <- as.data.frame(random.effects(lme.hd.3)[[2]])[,1]
  names(randomEffects) <- c("Tree.ID", "HabType.ID", "Count", "b.hat")
  boxplot(randomEffects$b.hat ~ randomEffects$HabType.ID, 
      ylab = "Tree Random Effects", 
      xlab = "Habitat Type", varwidth = T, main = "Within-tree Residuals")
  boxplot(residuals(lme.hd.3) ~ stage$Tree.ID, ylab = "Innermost Residuals", 
      xlab = "Tree", varwidth = T, main = "Within-tree Residuals")
  par(opar)


###################################################
### chunk number 89: hd-lme-3-d
###################################################
opar <- par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), las = 1, 
        cex.axis = 0.9)
  plot(fitted(lme.hd.3), fitted(lme.hd.3) + residuals(lme.hd.3), 
      xlab = "Fitted Values", ylab = "Observed Values", 
      main = "Model Structure (I)")
  abline(0, 1, col = "gray")
  plot(fitted(lme.hd.3), residuals(lme.hd.3), 
      main = "Model Structure (II)", 
      xlab = "Fitted Values", ylab = "Conditional Residuals")
  abline(0, 0, col = "gray")
  qqnorm(random.effects(lme.hd.3)[[1]][,1], main = "QQ plot: Habitat Type")
  qqline(random.effects(lme.hd.3)[[1]][,1])
  qqnorm(random.effects(lme.hd.3)[[2]][,1], main = "QQ plot: Tree")
  qqline(random.effects(lme.hd.3)[[2]][,1])
  randomEffects = as.data.frame.table(table(stage$Tree.ID, stage$HabType.ID))
  randomEffects <- randomEffects[randomEffects$Freq > 0, ]
  randomEffects$b.hat <- as.data.frame(random.effects(lme.hd.3)[[2]])[,1]
  names(randomEffects) <- c("Tree.ID", "HabType.ID", "Count", "b.hat")
  boxplot(randomEffects$b.hat ~ randomEffects$HabType.ID, 
      ylab = "Tree Random Effects", 
      xlab = "Habitat Type", varwidth = T, main = "Within-tree Residuals")
  boxplot(residuals(lme.hd.3) ~ stage$Tree.ID, ylab = "Innermost Residuals", 
      xlab = "Tree", varwidth = T, main = "Within-tree Residuals")
  par(opar)


###################################################
### chunk number 90: 
###################################################
summary(lme.hd.3)


###################################################
### chunk number 91: 
###################################################
variance.details <- function(model) {
      if (class(model) != "lme") 
          stop("Input must be an lme!")
      dimension <- floor(dim(VarCorr(model))[1])/2
      j <- c(1:dimension) * 2
      if (length(j) == 1) 
          j <- 1
      i <- c(j, dim(VarCorr(model))[1])
      variances <- as.numeric(VarCorr(model)[i, 1])
      mse <- sum(variances)
      icc <- rep(0, length(j))
      for (k in 1:length(j)) icc[k] <- as.numeric(VarCorr(model)[j[k], 
          1])/mse
      rmse <- sqrt(mse)
      list(icc = icc, rmse = rmse)
  }
lme.hd.4 <- lme(HD ~ Forest.ID, 
      random = ~1 | HabType.ID/Tree.ID, 
      data = stage)
variance.details(lme.hd.3)
variance.details(lme.hd.4)


###################################################
### chunk number 92: addedv
###################################################
lme.age.1 <- lme(Age ~ Forest.ID, random = ~1 | HabType.ID/Tree.ID, 
      data = stage)
res.Age <- residuals(lme.age.1, level = 0)
res.HD <- residuals(lme.hd.4, level = 0)
scatter.smooth(res.Age, res.HD, xlab = "Variation unique to Age", 
     ylab = "Variation in HD after all but Age")


###################################################
### chunk number 93: added
###################################################
lme.age.1 <- lme(Age ~ Forest.ID, random = ~1 | HabType.ID/Tree.ID, 
      data = stage)
res.Age <- residuals(lme.age.1, level = 0)
res.HD <- residuals(lme.hd.4, level = 0)
scatter.smooth(res.Age, res.HD, xlab = "Variation unique to Age", 
     ylab = "Variation in HD after all but Age")


###################################################
### chunk number 94: habitat
###################################################
print(plot(lme.hd.3, resid(.) ~ fitted(.) | HabType.ID, layout=c(1, 5)))


###################################################
### chunk number 95: 
###################################################
lme.hd.7 <- lme(HD ~ Age + Forest.ID, 
      random = ~Age + I(Age^2) | HabType.ID/Tree.ID, data = stage)
anova(lme.hd.7, lme.hd.3)


###################################################
### chunk number 96: hab6
###################################################
print(plot(lme.hd.7, resid(.) ~ fitted(.) | HabType.ID, layout=c(1, 5)))


###################################################
### chunk number 97: habitat3
###################################################
print(plot(lme.hd.3, resid(.) ~ fitted(.) | HabType.ID, layout=c(1, 5)))


###################################################
### chunk number 98: habitat6
###################################################
print(plot(lme.hd.7, resid(.) ~ fitted(.) | HabType.ID, layout=c(1, 5)))


###################################################
### chunk number 99: lme.hd.8
###################################################
lme.hd.8 <- lme(HD ~ Age + Forest.ID, 
      random = list(HabType.ID = ~Age + I(Age^2), Tree.ID = ~Age), 
      data = stage)
anova(lme.hd.8, lme.hd.7)


###################################################
### chunk number 100: lme.hd.9
###################################################
lme.hd.9 <- lme(HD ~ Age + Forest.ID, 
      random = ~Age + I(Age^2) | HabType.ID/Tree.ID, 
      weights = varIdent(form = ~1 | HabType.ID), 
      data = stage)
anova(lme.hd.7, lme.hd.9)


###################################################
### chunk number 101: lme.hd.10
###################################################
lme.hd.10 <- lme(HD ~ Age + Forest.ID, 
      random = ~Age + I(Age^2) | HabType.ID/Tree.ID, 
      weights = varPower(), data = stage)
anova(lme.hd.7, lme.hd.10)


###################################################
### chunk number 102: homo
###################################################
print(plot(lme.hd.10, resid(., type = "p") ~ fitted(.), abline = 0))


###################################################
### chunk number 103: acf
###################################################
print(plot(ACF(lme.hd.10, form = ~Age | Tree.ID), alpha = 0.01))


###################################################
### chunk number 104: homoplot
###################################################
print(plot(lme.hd.10, resid(., type = "p") ~ fitted(.), abline = 0))


###################################################
### chunk number 105: acfplot
###################################################
print(plot(ACF(lme.hd.10, form = ~Age | Tree.ID), alpha = 0.01))


###################################################
### chunk number 106: hd.tree
###################################################
print(plot(augPred(lme.hd.10)))


###################################################
### chunk number 107: hd-tree
###################################################
print(plot(augPred(lme.hd.10)))


###################################################
### chunk number 108: bigdiag
###################################################
opar <- par(mfrow=c(4,3), mar=c(4,4,3,1), las=1, 
     cex.axis=0.9, cex.main=1)
plot(fitted(lme.hd.10), fitted(lme.hd.10) + residuals(lme.hd.10),
     xlab="Fitted Values", ylab="Observed Values", 
     main="Model Structure (I)")
abline(0, 1, col="gray")
plot(fitted(lme.hd.10), residuals(lme.hd.10), 
     main="Model Structure (II)",
     xlab="Fitted Values", ylab="Conditional Residuals")
abline(0, 0, col="gray")
plot(fitted(lme.hd.10), residuals(lme.hd.10, type="p"), 
     main="Normalized residuals", xlab="Fitted Values", 
     ylab="Conditional Residuals")
abline(0, 0, col="gray")
qqnorm(random.effects(lme.hd.10)[[1]][,1], 
       main="QQ plot: Habitat Type Intercept")
qqline(random.effects(lme.hd.10)[[1]][,1])
qqnorm(random.effects(lme.hd.10)[[1]][,2], 
       main="QQ plot: Habitat Type Slope (Age)")
qqline(random.effects(lme.hd.10)[[1]][,2])
qqnorm(random.effects(lme.hd.10)[[1]][,3], 
       main="QQ plot: Habitat Type Slope (Age.2)")
qqline(random.effects(lme.hd.10)[[1]][,3])
qqnorm(random.effects(lme.hd.10)[[2]][,1], 
       main="QQ plot: Tree Intercept")
qqline(random.effects(lme.hd.10)[[2]][,1])
qqnorm(random.effects(lme.hd.10)[[2]][,2], 
       main="QQ plot: Tree Slope (Age)")
qqline(random.effects(lme.hd.10)[[2]][,2])
qqnorm(random.effects(lme.hd.10)[[2]][,3], 
       main="QQ plot: Tree (Age.2)")
qqline(random.effects(lme.hd.10)[[2]][3])
acf.resid <- ACF(lme.hd.10, resType = "n")
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
randomEffects <- as.data.frame.table(table(stage$Tree.ID, 
     stage$HabType.ID))
randomEffects <- randomEffects[randomEffects$Freq > 0,]
randomEffects$b.hat=as.data.frame(random.effects(lme.hd.10)[[2]])[,1]
names(randomEffects) <- c("Tree.ID","HabType.ID","Count","b.hat")
boxplot(randomEffects$b.hat ~ randomEffects$HabType.ID, 
        ylab="Tree Random Effects",
        xlab="Habitat Type", varwidth=T, 
        main="Within-Tree Residuals")
boxplot(residuals(lme.hd.10) ~ stage$Tree.ID, 
        ylab="Innermost Residuals", xlab="Tree", 
        varwidth=T, main="Within-Tree Residuals")
par(opar)


###################################################
### chunk number 109: bigdiagplot
###################################################
opar <- par(mfrow=c(4,3), mar=c(4,4,3,1), las=1, 
     cex.axis=0.9, cex.main=1)
plot(fitted(lme.hd.10), fitted(lme.hd.10) + residuals(lme.hd.10),
     xlab="Fitted Values", ylab="Observed Values", 
     main="Model Structure (I)")
abline(0, 1, col="gray")
plot(fitted(lme.hd.10), residuals(lme.hd.10), 
     main="Model Structure (II)",
     xlab="Fitted Values", ylab="Conditional Residuals")
abline(0, 0, col="gray")
plot(fitted(lme.hd.10), residuals(lme.hd.10, type="p"), 
     main="Normalized residuals", xlab="Fitted Values", 
     ylab="Conditional Residuals")
abline(0, 0, col="gray")
qqnorm(random.effects(lme.hd.10)[[1]][,1], 
       main="QQ plot: Habitat Type Intercept")
qqline(random.effects(lme.hd.10)[[1]][,1])
qqnorm(random.effects(lme.hd.10)[[1]][,2], 
       main="QQ plot: Habitat Type Slope (Age)")
qqline(random.effects(lme.hd.10)[[1]][,2])
qqnorm(random.effects(lme.hd.10)[[1]][,3], 
       main="QQ plot: Habitat Type Slope (Age.2)")
qqline(random.effects(lme.hd.10)[[1]][,3])
qqnorm(random.effects(lme.hd.10)[[2]][,1], 
       main="QQ plot: Tree Intercept")
qqline(random.effects(lme.hd.10)[[2]][,1])
qqnorm(random.effects(lme.hd.10)[[2]][,2], 
       main="QQ plot: Tree Slope (Age)")
qqline(random.effects(lme.hd.10)[[2]][,2])
qqnorm(random.effects(lme.hd.10)[[2]][,3], 
       main="QQ plot: Tree (Age.2)")
qqline(random.effects(lme.hd.10)[[2]][3])
acf.resid <- ACF(lme.hd.10, resType = "n")
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
randomEffects <- as.data.frame.table(table(stage$Tree.ID, 
     stage$HabType.ID))
randomEffects <- randomEffects[randomEffects$Freq > 0,]
randomEffects$b.hat=as.data.frame(random.effects(lme.hd.10)[[2]])[,1]
names(randomEffects) <- c("Tree.ID","HabType.ID","Count","b.hat")
boxplot(randomEffects$b.hat ~ randomEffects$HabType.ID, 
        ylab="Tree Random Effects",
        xlab="Habitat Type", varwidth=T, 
        main="Within-Tree Residuals")
boxplot(residuals(lme.hd.10) ~ stage$Tree.ID, 
        ylab="Innermost Residuals", xlab="Tree", 
        varwidth=T, main="Within-Tree Residuals")
par(opar)


###################################################
### chunk number 110: 
###################################################
summary(lme.hd.10)


###################################################
### chunk number 111: 
###################################################
require(nlme) 


###################################################
### chunk number 112: packages
###################################################
ip <- installed.packages()     # Save the output as an object!
class(ip)                      # It is a matrix
ip <- as.data.frame(ip)        # Now it is a dataframe
names(ip)                      # These are the variable names
length(ip$Package)             # And the packages are numerous
ip$Package


###################################################
### chunk number 113: gettingEquivalence
###################################################
get.equivalence <- download.packages("equivalence", 
         destdir="t:/R-workshop/workshop/library",
         repos="http://cran.stat.sfu.ca/")
get.equivalence
install.packages(get.equivalence[1,2], repos=NULL, 
    lib="t:/R-workshop/workshop/library")
library(equivalence, lib.loc="t:/R-workshop/workshop/library")


###################################################
### chunk number 114: gettingBoot
###################################################
get.boot <- download.packages("boot", 
           destdir="t:/R-workshop/workshop/library",
           repos="http://cran.stat.sfu.ca/")
get.boot
install.packages(get.boot[1,2], repos=NULL, 
       lib="t:/R-workshop/workshop/library")
library(boot, lib.loc="t:/R-workshop/workshop/library")
library(equivalence, lib.loc="t:/R-workshop/workshop/library")


###################################################
### chunk number 115: 
###################################################
my.function <- function(arguments) {
}


###################################################
### chunk number 116: 
###################################################
cm.to.inches <- function(data) {
      data / 2.54
  }


###################################################
### chunk number 117: 
###################################################
cm.to.inches(25)


###################################################
### chunk number 118: 
###################################################
cm.to.inches(c(25, 35, 45))


###################################################
### chunk number 119: scoping
###################################################
x <- pi
radius.2.area <- function(radius) {
  x * radius^2
}
radius.2.area(4)


###################################################
### chunk number 120: 
###################################################
x <- c(1:10)
class(x)
class(x) <- "eh"
class(x)


###################################################
### chunk number 121: 
###################################################
print.eh <- function(x) print(x[length(x)]) 
print(x)
x


###################################################
### chunk number 122: 
###################################################
length(x)


###################################################
### chunk number 123: 
###################################################
x * x
class(x * x)
length(x * x)


###################################################
### chunk number 124: 
###################################################
class(x) <- "integer"
x


###################################################
### chunk number 125: 
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


