###################################################
### chunk number 1: L2_Exercise1
###################################################
for (i in 0:10) print (2^i)


###################################################
### chunk number 2: L2_Exercise2
###################################################
p1<-0
pLim=1000
while (2^p1 < pLim)  { 
    cat(p1, ":", 2^p1, "\n")
    p1= p1 +1
    }


###################################################
### chunk number 3: SummaryStats
###################################################
vec1= rnorm(100)
summary(vec1)
mean(vec1);sd(vec1);length(vec1)


###################################################
### chunk number 4: Recap on factors
###################################################
dept <- c("Epi", "Biostats", "Epi", "Env Hlth", 
         "other","Epi", "Epi", "Biostats", "Biostats", 
         "Env Hlth", "Biostats", "Biostats", "Env Hlth", 
         "other", "other")


###################################################
### chunk number 5: Sorting factors
###################################################
  dept.fac <- factor(dept)
  dept.fac


###################################################
### chunk number 6: Levels of factor
###################################################
levels(dept.fac)


###################################################
### chunk number 7: Run a demo on graphics eval=FALSE
###################################################
## demo(graphics)


###################################################
### chunk number 8: simple plot
###################################################
x<-1:10
plot(x)


###################################################
### chunk number 9: Scatterplot of x and y
###################################################
set.seed(13)
x <- -30:30
y <- 3*x + 2 + rnorm(length(x), sd=20)
plot(x, y)


###################################################
### chunk number 10: plotting_factor
###################################################
fac <- factor(c(rep(1,10),rep(5,30),rep(10,21)))
plot(fac)  # barplot
plot(fac, x) # boxplot of x.vec for each level of f.vec


###################################################
### chunk number 11: Plot data.frame
###################################################
airquality[1:2,]
plot(airquality) # all variables plotted against each other
plot(airquality$Ozone, airquality$Temp)
plot(Ozone~Temp, data =airquality)
plot(Temp~Ozone, data =airquality)
attach(airquality)
plot(Ozone, Temp)
detach(airquality)


###################################################
### chunk number 12: Plots_df
###################################################
attach(airquality)
par(mfrow=c(2,2))
plot(airquality$Ozone, main="Plot of Ozone")  #One variable
plot(Ozone, main="Attach df, now Plot of Ozone")
plot(Solar.R, Ozone, main="Plot of Solar.R, Ozone")
plot(Ozone~Solar.R, main="Plot of Ozone~Solar.R")
detach(2)


###################################################
### chunk number 13: boxplot
###################################################
par(mfrow=c(2,2))
boxplot(airquality)
boxplot(airquality$Ozone)
boxplot(airquality$Ozone~airquality$Month, col=2:6)


###################################################
### chunk number 14: pie
###################################################
pie(1:10, col=rainbow(10))


###################################################
### chunk number 15: Plotting qqnorm qqline qqplot
###################################################
xt <- rt(100, 3)
qqnorm(xt)
qqline(xt)


###################################################
### chunk number 16: Plotting histogram
###################################################
hist(xt)
plot(density(xt))


###################################################
### chunk number 17: plotting xyx plots- image contour persp
###################################################
x <- y <- seq(-4*pi, 4*pi, len=27)
r <- sqrt(outer(x^2, y^2, "+"))
z <- cos(r^2)*exp(-r/6)

image(z, col=heat.colors(64),
    xlab = expression(cos(r^2) * e^{-r/6}))
contour(z, add = TRUE, drawlabels = FALSE)
persp(x,y,z)
persp(x,y,z, theta=30, phi=30)



###################################################
### chunk number 18: Multivariate data- plotting using pairs
###################################################
pairs(airquality)
pairs(ChickWeight)


###################################################
### chunk number 19: Multivariate data - plotting using coplot
###################################################
coplot(weight~Time|Diet, data=ChickWeight) # Diet is a factor
coplot(Ozone~Temp|Solar.R, data=airquality) # Solar.R is numeric


###################################################
### chunk number 20: Removing missing values
###################################################
airquality[1:6,]
filNa<- apply(airquality, 1, function(x) any(is.na(x)))
airquality[!filNa,][1:6,]
airTemp<- airquality[!filNa,]
coplot(Ozone~Temp|Solar.R, data=airTemp)


###################################################
### chunk number 21: moreExamplesPlotting
###################################################

xp <- 1:100/100
yp <- 3*xp^2 - 2*xp + rnorm(100,sd=.2)

par(mfrow=c(3,2))
for (i in c("l", "b", "o", "h")) plot(xp, yp, type = i, main=paste("Plot type:", i))

plot(xp, yp, type='o',
     xlab='index', ylab='values',
     main='BIO503 simple plot')
plot(xp,yp, type='l', axes=FALSE)
axis(1)
axis(2, at=c(-0.6, 0, 0.6, 1.2), col='blue')
axis(3, at=c(0, 0.25, 0.5, 0.75, 1.0), col='red')
axis(4, col = "violet", col.axis="dark violet", lwd = 2)


###################################################
### chunk number 22: Greek
###################################################
plot(x, cos(x), main=expression(paste("A random eqn ",bar(x)) == sum(frac(alpha[i]+beta[z], n))), sub="This is the subtitle")


###################################################
### chunk number 23: PointsLinesLegend
###################################################
attach(cars)
plot(cars, type='n', xlab='Speed [mph]', ylab='Distance [ft]')
points(speed[speed<15], dist[speed<15], pch='s', col='blue')
points(speed[speed>=15], dist[speed>=15], pch='f', col='green')
lines(lowess(cars), col='red')
legend(5,120, pch=c('s','f'), col=c('blue', 'green'), legend=c('Slow','Fast'))
title('Breaking distance of old cars')
detach(2)


###################################################
### chunk number 24: PlotGreek
###################################################
# Mean and Median Plot
x <- rexp(100, rate = .5)
hist(x, main = "Mean and Median of a Skewed Distribution")
abline(v = mean(x),   col=2, lty=2, lwd=2)
abline(v = median(x), col=3, lty=3, lwd=2)
ex1 <- expression(bar(x) == sum(over(x[i], n), i==1, n),  hat(x) == median(x[i], i==1,n))
legend(4.1, 30, ex1, col = 2:3, lty=2:3, lwd=2)


###################################################
### chunk number 25: PlotGreek2
###################################################
x <- seq(-pi, pi, len = 65)
plot(x, sin(x), type="l", col = "blue", xlab = expression(phi),   ylab = expression(f(phi)))
lines(x, cos(x), col = "magenta", lty = 2)
abline(h=-1:1, v=pi/2*(-6:6), col="gray90")
ex2 <- expression(plain(sin) * phi,  paste("cos", phi))
legend(-3, .9, ex2, lty=1:2, col=c("blue", "magenta"),    adj = c(0, .6))


###################################################
### chunk number 26: Interactive plotting using locator eval=FALSE
###################################################
## plot(1:20, rt(20,1))
## text(locator(1), 'outlier', adj=0)


###################################################
### chunk number 27: interative plotting using identify eval=FALSE
###################################################
## attach(ChickWeight)
## plot(Time, weight)
## identify(Time, weight, Chick)
## detach(2)


###################################################
### chunk number 28: interative dendrogram using identify eval=FALSE
###################################################
## set.seed(123)
## m <- matrix(rnorm(100),nrow=10)
## d <- dist(m)
## hc <- hclust(d)
## plot(hc)
## cl <- identify(hc,N=3,MAXCLUSTER=10)
## print(cl)


###################################################
### chunk number 29: interative dendrogram using identify compute centroids eval=FALSE
###################################################
## plot(hc)
## cl.means <- identify(hc,FUN=function(k) apply(m,2,mean),N=3,MAXCLUSTER=10)
## print(cl.means)


###################################################
### chunk number 30: The par function
###################################################
oldpar <- par(col=4, lty=2)
plot(cars)
lines(lowess(cars))
par(oldpar)


###################################################
### chunk number 31: Passing par parameter to a plot
###################################################
plot(1:20, rnorm(20), pch='+')


###################################################
### chunk number 32: Graphical elements using pch and text
###################################################
x <- matrix(rep(1:5,5), nrow=5)
y <- t(x)
names <- 0:25
plot(0, 0, pch=names[1], xlim=c(0,6), ylim=c(0,6),
        xlab='', ylab='', main='Point symbols in R')
for (i in 1:25)
  points(x[i],y[i],pch=i)
text(x, (y+.4), names[-1])


###################################################
### chunk number 33: FontLinePoint
###################################################
# fonts
plot(c(0,31), c(1,22), type="n", ylab="", xlab="", axes=FALSE)
title("Fonts, line and point types")
for (i in 1:5) {
    par(font=i)
    ypos<-22-i
    points(0:31, rep(ypos,32), pch=64:95)
    axis(2,at=ypos, labels=paste("font =", i), las=1)
    }

par(font=1)
for (i in 9:15){
    lines(c(0,31), c(i,i), lty=i)
    axis(2,at=i, labels=paste("lty =",i), las=1)
    }

points(0:25, rep(4,26), pch=0:25)
text(0:25, rep(5,26), paste(0:25), srt=90, cex=0.65)
axis(2,at=4:5, labels=c("Symbol", "pch no."), las=1, tick=FALSE)


###################################################
### chunk number 34: DefaultColors
###################################################
image(1:12, 1, as.matrix(1:12), col=0:11, main="Default 9 Colors", ylab="",xlab="", yaxt="n")


###################################################
### chunk number 35: Colors
###################################################
colors()
grep("yellow", colors(), value=TRUE)


###################################################
### chunk number 36: inbuilt color palettes eval=FALSE
###################################################
## example(rainbow)


###################################################
### chunk number 37: Create a pdf of colors
###################################################
 source("http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.R")


###################################################
### chunk number 38: RColorBrewer eval=FALSE
###################################################
## library(RColorBrewer)
## example(brewer.pal)


###################################################
### chunk number 39: heatmap
###################################################
library(RColorBrewer)
hmcol <- colorRampPalette(brewer.pal(10, "RdBu"))(256)
heatmap(state.x77)
heatmap(t(state.x77))
heatmap(t(state.x77), col=hmcol)


###################################################
### chunk number 40: Example on volcano dataset
###################################################
par(mfrow=c(2, 2))
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
# Don't draw the grid lines :  border = NA
par(mar=rep(0, 4))
persp(x, y, z, theta = 135, phi = 30, col = "light grey", scale = FALSE,
      ltheta = -120, shade = 0.75, border = NA, box = FALSE)
mtext("persp()", side=3, line=-2)
par(mar=rep(0.5,4))
contour(x, y, z, asp=1, labcex=0.35, axes=FALSE)
rect(0, 0, 870, 620)
mtext("contour()", side=3, line=-1.5)
image(x, y, z, asp=1, col=grey(0.5 + 1:12/24), xlab="", ylab="", axes=FALSE)
rect(min(x)-5, min(y)-5, max(x)+5, max(y)+5)
mtext("image()", side=3, line=-1.5)
image(x, y, z, asp=1, col=rainbow(100), xlab="", ylab="", axes=FALSE)
mtext("image()", side=3, line=-1.5)


###################################################
### chunk number 41: cleanup
###################################################
rm(list=ls())
data(ChickWeight)


###################################################
### chunk number 42: LatticeSimplexyplotC
###################################################
library(lattice)
xyplot(weight~Time | Diet, data=ChickWeight) # Simple use of xyplot


###################################################
### chunk number 44: latticeBarchartC
###################################################
x <- 1:10
y <- 1:10
g <- factor(1:10)
barchart(y~g|1)

###################################################
### chunk number 46: Lattice density histogram splom and more
###################################################
angle <- seq(0, 2*pi, length=21)[-21]
xx <- cos(angle)
yy <- sin(angle)
gg <- factor(rep(1:2, each=10))

bwplot(yy~gg|1)
densityplot(~ yy | 1)
histogram(~yy | 1)
qqmath(~yy|1)
xyplot(xx~yy|1)
splom(~ data.frame(x=xx[1:10], y=yy[1:10]) | 1,
                     pscales=0)
parallel(~ data.frame(x=xx[1:10], y=yy[1:10]) | 1)


###################################################
### chunk number 47: Lattice more than 2 variables
###################################################
aaa <- seq(0, pi, length=10)
xxx <- rep(aaa, 10)
yyy <- rep(aaa, each=10)
zzz <- sin(xxx) + sin(yyy)

levelplot(zzz ~ xxx + yyy | 1, colorkey=FALSE)
contourplot(zzz ~ xxx + yyy | 1, labels=FALSE, cuts=8)
cloud(zzz ~ xxx + yyy | 1, zlab=NULL, zoom=0.9,
                     par.settings=list(box.3d=list(lwd=0.01)))
wireframe(zzz ~ xxx + yyy | 1, zlab=NULL, zoom=0.9,
                     drape=TRUE, par.settings=list(box.3d=list(lwd=0.01)),
                     colorkey=FALSE)


###################################################
### chunk number 48: SavingPlots eval=FALSE
###################################################
## myPath <- file.path("P:/Bio503/Plots")
## pdf(file=paste(myPath,'nicePlot.pdf', sep=''))
## x <- seq(0,2*pi,length=100)
## y <- sin(3*x) + cos(x) + rnorm(100,sd=.2)
## plot(x,y)
## dev.off()


###################################################
### chunk number 49: Compare jpeg and pdf eval=FALSE
###################################################
## load("geneExprMat.rdat")
## pdf("heatmap.pdf",12,12)
## heatmap(gene.expr.mat,cexRow=0.1)
## dev.off()
## jpeg("heatmap.jpg",2000,2000)
## heatmap(gene.expr.mat,cexRow=0.1)
## dev.off()


