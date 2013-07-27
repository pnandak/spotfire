library(car)
library(lattice)
library(modreg)

data(singer)

# univariate regular graphics
x <- rnorm(1000)
hist(x, nclass=30, col=5)
boxplot(x)


# univariate lattice graphics
print(histogram(~height|voice.part, data=singer, layout=c(2,4), aspect=1,
          xlab = "Height (inches)"))

print(bwplot(voice.part~height, data=singer, xlab="Height (inches)"))

print(stripplot(voice.part~jitter(height), data=singer, aspect=1, jitter=TRUE,
                xlab="Height (inches)"))



# regular qq plot
y <- rnorm(1000)
qqplot(x,y)
qqline(y)


# lattice qq plot
print(qq(voice.part~height, aspect=1, data=singer,
         subset = (voice.part == "Tenor 1" | voice.part == "Bass 2")))



# regular qqnorm plots
par(mfrow=c(1,2), pty="s")
hist(x, col=5)
qqnorm(x)
qqline(x)

x <- rt(1000,3)
hist(x, col=5)
qqnorm(x)
qqline(x)

x <- rchisq(1000, 3)
hist(x, col=5)
qqnorm(x)
qqline(x)

# lattice qqnorm plot
print(qqmath(~height | voice.part, data=singer, aspect=1, layout=c(2,4)))




# banking
data(sunspot)
print(xyplot(sunspot~1:37, type="l", aspect=1))

print(xyplot(sunspot~1:37, type="l", aspect="xy"))



# scatterplot smoothing, SL, RF, and residual dependence plots
par(mfrow=c(1,1))
data(Robey)
scatter.smooth(Robey$contraceptors, Robey$tfr)

Robey.lo <- loess(tfr~contraceptors, data=Robey)
tfr.hat <- fitted(Robey.lo)
tfr.res <- residuals(Robey.lo)

# residual dependence
par(mfrow=c(1,2))
plot(Robey$contraceptors, Robey$tfr)
lines(Robey$contraceptors[order(Robey$contraceptors)],
      tfr.hat[order(Robey$contraceptors)],
      col="red")
scatter.smooth(tfr.hat, tfr.res)
abline(h=0)


# RF
print(rfs(Robey.lo))


# SL
par(mfrow=c(1,2))
plot(Robey$contraceptors, Robey$tfr)
lines(Robey$contraceptors[order(Robey$contraceptors)],
      tfr.hat[order(Robey$contraceptors)],
      col="red")
scatter.smooth(tfr.hat, sqrt(abs(tfr.res)))
par(mfrow=c(1,1))


# identifying points
plot(Robey$contraceptors, Robey$tfr)
identify(Robey$contraceptors, Robey$tfr, labels=row.names(Robey), n=2)



# conditioning in lattice xyplot
print(xyplot(tfr~contraceptors|region, data=Robey))

# panel functions
print(xyplot(tfr~contraceptors|region, data=Robey, aspect="xy",
             panel=function(x, y, ...) {
               panel.xyplot(x, y, ...)
               panel.loess(x, y, ...)
             }
             ))


print(xyplot(tfr~contraceptors|region, data=Robey,
             panel=function(x, y, ...) {
               panel.xyplot(x, y, ...)
               panel.lmline(x, y, type="l", ...)
             }
             ))


# conditioning on continuous variables
data(Freedman)
Freedman.clean <- na.omit(Freedman)
NonWhite <- equal.count(Freedman.clean$nonwhite, number=3, overlap=.25)
Popul    <- equal.count(Freedman.clean$population, number=3, overlap=.25)
print(xyplot(log(crime)~log(density)|NonWhite*Popul, data=Freedman.clean,
             panel=function(x, y, ...) {
               panel.xyplot(x, y, ...)
               panel.loess(x, y, span=.9, degree=1, ...)
             }
             ))



# scatterplot matrices (regular)
pairs(Freedman.clean)
Freedman.log <- Freedman.clean
Freedman.log$density <- log(Freedman.log$density)
Freedman.log$population <- log(Freedman.log$population)
Freedman.log$crime <- log(Freedman.log$crime)
pairs(Freedman.log)

# scatterplot matrices (lattice)
print(splom(~Freedman.log,
            panel=function(x,y,...){
              panel.xyplot(x, y, ...)
              panel.loess(x, y, span=.9, degree=1, ...)
            }
            ))

data(Angell)
print(splom(~Angell[,1:3]|region, data=Angell,
            panel=function(x,y,...){
              panel.xyplot(x,y,...)
              panel.loess(x,y, span=.9, degree=1, ...)
            }
            ))
