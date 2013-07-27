### R code from vignette source 'C:/Users/aedin/Dropbox/Talks/Bio503/winter2012/L3/Lecture3.rnw'

###################################################
### code chunk number 1: Run a demo on graphics (eval = FALSE)
###################################################
## demo(graphics)


###################################################
### code chunk number 2: dev (eval = FALSE)
###################################################
## dev.list()
## dev.cur()
## dev.off()


###################################################
### code chunk number 3: par
###################################################
par(mfrow=c(2,1))


###################################################
### code chunk number 4: simplePlot
###################################################
x<-1:10
plot(x, main="Plot of 1:10")


###################################################
### code chunk number 5: Scatterplot
###################################################
set.seed(13)
x <- -30:30
y <- 3*x + 2 + rnorm(length(x), sd=20)
plot(x, y, main="XY plot")


###################################################
### code chunk number 6: Plotdf
###################################################
airquality[1:2,]
plot(airquality) # all variables plotted against each other pairs()


###################################################
### code chunk number 7: Plotequiv
###################################################
par(mfrow=c(2,2))
plot(airquality$Ozone, airquality$Temp, main="plot(airquality$Ozone, airquality$Temp)")
plot(Ozone~Temp, data =airquality, main="plot(Ozone~Temp, data =airquality)")
plot(Temp~Ozone, data =airquality, main="plot(Temp~Ozone, data =airquality)")
attach(airquality)
plot(Ozone, Temp, main="plot(Ozone, Temp)")
detach(airquality)


###################################################
### code chunk number 8: boxplot
###################################################
par(mfrow=c(2,2))
boxplot(airquality)
boxplot(airquality$Ozone)
boxplot(airquality$Ozone~airquality$Month, col=2:6)


###################################################
### code chunk number 9: boxplot1
###################################################
par(mfrow=c(2,2))
boxplot(airquality$Ozone~airquality$Month, col=2:6)
plot(factor(airquality$Month), airquality$Ozone, col=2:6)
plot(airquality$Ozone~factor(airquality$Month), col=2:6)


###################################################
### code chunk number 10: barplot
###################################################
OzMonthMean<-tapply(airquality$Ozone, factor(airquality$Month), mean, na.rm=TRUE)
par(mfrow=c(1,2))
barplot(OzMonthMean,col=2:6, main="Mean Ozone by month")


###################################################
### code chunk number 11: pie
###################################################
pie(1:10, col=rainbow(10))


###################################################
### code chunk number 12: PlottingQQ
###################################################
par(mfrow=c(2,1))
xt <- rt(100, 3)
qqnorm(xt)
qqline(xt)


###################################################
### code chunk number 13: PlotHistogram
###################################################
hist(xt)
plot(density(xt))


###################################################
### code chunk number 14: plotImageContourPersp
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
### code chunk number 15: Recap on factors
###################################################
dept <- c("Epi", "Biostats", "Epi", "Env Hlth", 
         "other","Epi", "Epi", "Biostats", "Biostats", 
         "Env Hlth", "Biostats", "Biostats", "Env Hlth", 
         "other", "other")


###################################################
### code chunk number 16: Sorting factors
###################################################
  dept.fac <- factor(dept)
  dept.fac


###################################################
### code chunk number 17: Ordered factor
###################################################
  dept.fac1 <- factor(dept, ordered=TRUE)
  dept.fac1
  dept.fac2 <- factor(dept, ordered=TRUE, levels=c("other", "Epi", "Env Hlth", "Biostats"))
  dept.fac2


###################################################
### code chunk number 18: Levels of factor
###################################################
levels(dept.fac)


###################################################
### code chunk number 19: plotting_factor
###################################################
par(mfrow=c(2,3))
plot(dept.fac, main="dept.fac, factor")  # barplot
plot(dept.fac1, main="dept.fac1, Ordered factor")  # barplot
plot(dept.fac2, main="dept.fac2, Ordered factor with reversed levels")  # barplot 
x=rnorm(15)
plot(dept.fac, x) # boxplot of x.vec for each level of f.vec
plot(dept.fac1, x) # boxplot of x.vec for each level of f.vec
plot(dept.fac2, x) # boxplot of x.vec for each level of f.vec



###################################################
### code chunk number 20: Multivariate data- plotting using pairs
###################################################
pairs(airquality)
pairs(ChickWeight)


###################################################
### code chunk number 21: Multivariate data - plotting using coplot
###################################################
coplot(weight~Time|Diet, data=ChickWeight) # Diet is a factor
coplot(Ozone~Temp|Solar.R, data=airquality) # Solar.R is numeric


###################################################
### code chunk number 22: RemovingNA
###################################################
airquality[1:6,]
filNa<- apply(airquality, 1, function(x) any(is.na(x)))
airquality[!filNa,][1:6,]
airTemp<- airquality[!filNa,]
coplot(Ozone~Temp|Solar.R, data=airTemp)


###################################################
### code chunk number 23: moreExamplesPlotting
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
### code chunk number 24: CH
###################################################
ChickWeight[1:2,]


###################################################
### code chunk number 25: Greek
###################################################
plot(x, cos(x), main=expression(paste("A random eqn ",bar(x)) == sum(frac(alpha[i]+beta[z], n))), sub="This is the subtitle")


###################################################
### code chunk number 26: Points-Lines-Legend
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
### code chunk number 27: PlotGreek
###################################################
par(mfrow=c(2,1))
# Mean and Median Plot
x <- rexp(100, rate = .5)
hist(x, main = "Mean and Median of a Skewed Distribution")
abline(v = mean(x),   col=2, lty=2, lwd=2)
abline(v = median(x), col=3, lty=3, lwd=2)
ex1 <- expression(bar(x) == sum(over(x[i], n), i==1, n),  hat(x) == median(x[i], i==1,n))
legend(4.1, 30, ex1, col = 2:3, lty=2:3, lwd=2)

x <- seq(-pi, pi, len = 65)
plot(x, sin(x), type="l", col = "blue", xlab = expression(phi),   ylab = expression(f(phi)))
lines(x, cos(x), col = "magenta", lty = 2)
abline(h=-1:1, v=pi/2*(-6:6), col="gray90")
ex2 <- expression(plain(sin) * phi,  paste("cos", phi))
legend(-3, .9, ex2, lty=1:2, col=c("blue", "magenta"),    adj = c(0, .6))


###################################################
### code chunk number 28: Interactive plotting using locator (eval = FALSE)
###################################################
## plot(1:20, rt(20,1))
## text(locator(1), 'outlier', adj=0)


###################################################
### code chunk number 29: interative plotting using identify (eval = FALSE)
###################################################
## attach(ChickWeight)
## plot(Time, weight)
## identify(Time, weight, Chick)
## detach(2)


###################################################
### code chunk number 30: identifyhclust
###################################################
hca<-hclust(eurodist)
plot(hca, main="Distance between European Cities")


###################################################
### code chunk number 31: identify hclust (eval = FALSE)
###################################################
## (x<-identify(hca))
## x


###################################################
### code chunk number 32: The par function
###################################################
oldpar <- par(col=4, lty=2)
plot(cars)
lines(lowess(cars))
par(oldpar)


###################################################
### code chunk number 33: Passing par parameter to a plot
###################################################
plot(1:20, rnorm(20), pch='+')


###################################################
### code chunk number 34: DefaultColors
###################################################
image(1:12, 1, as.matrix(1:12), col=0:11, main="Default 9 Colors", ylab="",xlab="", yaxt="n")


###################################################
### code chunk number 35: Colors
###################################################
colors()
grep("yellow", colors(), value=TRUE)


###################################################
### code chunk number 36: inbuilt color palettes (eval = FALSE)
###################################################
## example(rainbow)


###################################################
### code chunk number 37: Create a pdf of colors
###################################################
 #source("http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.R")


###################################################
### code chunk number 38: RColorBrewer (eval = FALSE)
###################################################
## library(RColorBrewer)
## example(brewer.pal)


###################################################
### code chunk number 39: heatmap
###################################################
library(RColorBrewer)
hmcol <- colorRampPalette(brewer.pal(10, "RdBu"))(256)
heatmap(t(state.x77), col=hmcol)


###################################################
### code chunk number 40: Graphical elements using pch and text
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
### code chunk number 41: FontLinePoint
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
### code chunk number 42: Example on volcano dataset
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


###################################################
### code chunk number 43: cleanup
###################################################
rm(list=ls())
data(ChickWeight)


###################################################
### code chunk number 44: LatticeSimplexyplot
###################################################
library(lattice)
xyplot(weight~Time | Diet, data=ChickWeight) # Simple use of xyplot


###################################################
### code chunk number 45: LatticeMorexyplot
###################################################
xyplot(weight~Time|Diet,
       data=ChickWeight,
       panel=panel.superpose,
       groups=Chick,
       type='b')


###################################################
### code chunk number 46: latticeBarchart
###################################################
x <- 1:10
y <- 1:10
g <- factor(1:10)
barchart(y~g|1)


###################################################
### code chunk number 47: Lattice density histogram splom and more
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
### code chunk number 48: Lattice more than 2 variables
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
### code chunk number 49: PackageInstall (eval = FALSE)
###################################################
## #setRepositories()
## install.packages("igraph")
## install.packages("RColorBrewer")
## install.packages("RJSONIO")
## install.packages("googleVis")
## install.packages("network")
## #install.packages("googleVis",dep=T)


###################################################
### code chunk number 50: google
###################################################
library(googleVis)
M <- gvisMotionChart(Fruits, "Fruit", "Year")
plot(M)
cat(M$html$chart, file="tmp.html")


###################################################
### code chunk number 51: network
###################################################
#install.packages(network)
library(network)
m<-matrix(rbinom(100,1,1.5/9),10)
diag(m)<-0
g<-network(m)
#Plot the graph
plot(g)

#Load Padgett's marriage data
data(flo)
nflo<-network(flo)
#Display the network, indicating degree and flagging the Medicis
plot(nflo, vertex.cex=apply(flo,2,sum)+1, usearrows=FALSE,
    vertex.sides=3+apply(flo,2,sum),
    vertex.col=2+(network.vertex.names(nflo)=="Medici"))



###################################################
### code chunk number 52: igraph
###################################################
#install.packages(igraph)
library(igraph)
adj.mat <- matrix(sample(c(0,1), 9, replace=TRUE), nr=3)
g <- graph.adjacency(adj.mat)
plot(g)


###################################################
### code chunk number 53: SavingPlots (eval = FALSE)
###################################################
## myPath <- file.path("P:/Bio503/Plots")
## pdf(file=paste(myPath,'nicePlot.pdf', sep=''))
## x <- seq(0,2*pi,length=100)
## y <- sin(3*x) + cos(x) + rnorm(100,sd=.2)
## plot(x,y)
## dev.off()


###################################################
### code chunk number 54: Ex1
###################################################
 data(cars)
 plot(cars)
 lines(lowess(cars))


###################################################
### code chunk number 55: Lecture3.rnw:1129-1130
###################################################
methods("plot")


###################################################
### code chunk number 56: Ex2
###################################################
unlist(par())


###################################################
### code chunk number 57: Ex3
###################################################
 plot(1:4, rnorm(4), axes=FALSE)
 axis(1, 1:4, LETTERS[1:4])
 axis(2)


###################################################
### code chunk number 58: Ex4
###################################################
par(bg="gray")
pie(rep(1,24), col=rainbow(24), radius=0.9)
title(main="A Sample Color Wheel", cex.main=1.4, font.main=3)


###################################################
### code chunk number 59: Ex5
###################################################
opar <- par(no.readonly = TRUE)
x <- rnorm(50)
opar <- c(opar, par(bg="white"))
plot(x, ann=FALSE, type="n")
abline(h=0, col=gray(.90))


###################################################
### code chunk number 60: Ex6
###################################################
opar <- par(no.readonly = TRUE)
x <- rnorm(50)
opar <- c(opar, par(bg="white"))
plot(x, ann=FALSE, type="n")
abline(h=0, col=gray(.90))
lines(x, col="green4", lty="dotted")
points(x, bg="limegreen", pch=21)


###################################################
### code chunk number 61: Ex7
###################################################
opar <- par(no.readonly = TRUE)
x <- rnorm(50)
opar <- c(opar, par(bg="white"))
plot(x, ann=FALSE, type="n")
abline(h=0, col=gray(.90))
lines(x, col="green4", lty="dotted")
points(x, bg="limegreen", pch=21)
title(main="Simple Use of Color In a Plot",
      xlab="Just a Whisper of a Label",
      col.main="blue", col.lab=gray(.8),
      cex.main=1.2, cex.lab=1.0, font.main=4, font.lab=3)


###################################################
### code chunk number 62: Ex8
###################################################
seq.norm <- seq(-3,3,by=.05)
plot(seq.norm,dnorm(seq.norm),type='l',ann=FALSE)
text(0, .05, expression(paste(frac(1, sigma*sqrt(2*pi)), " ",
     plain(e)^{frac(-(x-mu)^2, 2*sigma^2)})),
     cex= 1.2)


###################################################
### code chunk number 63: Ex9
###################################################
     data(LifeCycleSavings)
     lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
     par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
     plot(lm.SR)


###################################################
### code chunk number 64: Ex10
###################################################
data(iris)
colnames(iris)
iris[1:2,]
newCols<-c("red", "green3", "blue")[iris$Species]
class(iris$Species)
table(newCols, iris$Species)
pairs(iris[1:4], main="Edgar Anderson's Iris Data", pch=21, bg=newCols)


###################################################
### code chunk number 65: Ex11
###################################################
     x <- pmin(3, pmax(-3, rnorm(50)))
     y <- pmin(3, pmax(-3, rnorm(50)))
     xhist <- hist(x, breaks=seq(-3,3,0.5), plot=FALSE)
     yhist <- hist(y, breaks=seq(-3,3,0.5), plot=FALSE)
     top <- max(c(xhist$counts, yhist$counts))
     xrange <- c(-3,3)
     yrange <- c(-3,3)
     nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
     layout.show(nf)

     par(mar=c(3,3,1,1))
     plot(x, y, xlim=xrange, ylim=yrange, xlab="", ylab="")
     par(mar=c(0,3,1,1))
     barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
     par(mar=c(3,0,1,1))
     barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)


###################################################
### code chunk number 66: Ex12
###################################################
     data(quakes)
     coplot(lat ~ long | depth, data = quakes)

     given.depth <- co.intervals(quakes$depth, number=4, overlap=.1)
     coplot(lat ~ long | depth, data = quakes, given.v=given.depth, rows=1)


