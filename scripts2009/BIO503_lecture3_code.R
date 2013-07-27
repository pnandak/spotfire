# Code used in Lecture 3
weight <- c(115, 120, 142, 100)
height <- c(151, 170, 184, 140)
names <- c("jess", "meredith", "erick", "liz")

bio.df <- data.frame(id = names, h = height, w = weight)
write.table(bio.df, file="bio503info.txt", sep="\t", quote=F, col.names=T, row.names=F)

# 
write.table(bio.df, file="bio503info.txt", sep="\t", quote=T, col.names=T, row.names=F)
write.table(bio.df, file="bio503info.txt", sep=",", quote=F, col.names=T, row.names=F)
write.table(bio.df, file="bio503info.txt", sep=",", quote=F, col.names=T, row.names=T)

#
write(names, file="justNames.txt")
write(bio.df, n=3, file="bioFile.txt")
write(bio.df, n=3, file="bioFile.txt", sep=",")

#
coins <- c("H", "H", "T", "H", "T", "H", "T")
sample(coins, 4)	# n = 4 

#
dbinom(x=4, size=7, prob=0.5)
qbinom(p=.25, size=7, prob=0.5)
rbinom(n=10, size=7, prob=0.5)

#
dbinom(0:7, size=7, prob=0.5)

#
sum(dbinom(0:7, size=7, prob=0.5))
pbinom(3, size=7, prob=0.5, lower.tail=F) 
pbinom(3, size=7, prob=0.5, lower.tail=F) 

#
pbinom(q=3, size=7, prob=0.5, lower.tail=F)
1- pbinom(q=3, size=7, prob=0.5, lower.tail=T)

#
X <- rnorm(1000)
A <- rnorm(1000)
C <- rt(1000, df=2)
D <- rt(1000, df=100)

#
qqplot(X, A, main="A and X are the same")

#
qqplot(C, A, main="A and C are different")

#
qqplot(X, D, main="X and D are starting to look more similar")

#
rnorm(1) 
args(rnorm) 
rnorm(1, mean=a, sd=sqrt(b))

#
set.seed(123)

#
normNumbers <- rnorm(1000)
summary(normNumbers)
mean(normNumbers)
median(normNumbers)
min(normNumbers)
max(normNumbers)
sd(normNumbers)

quantile(normNumbers, 0.25)
quantile(normNumbers, 0.75)

#
xy <- rnorm(1000)
boxplot(xy)
zz <- rt(1000, df=2)
boxplot(xy,zz)

#
boxplot(xy, xlab="Group1", ylab="Scores", main="Title")
boxplot(xy,zz, names=c("Group1", "Group2"), ylab="Scores")
myDF <- data.frame(Monday=xy, Saturday=zz)
boxplot(myDF, ylab="Scores")

#
xy <- rnorm(1000)
hist(xy)
hist(xy, freq=F)

#
hist(xy, col="orange", xlab="Scores", main="A Very Orange Plot")

#
tnum <- rt(1000, df=10)
plot(density(tnum))

#
dObj <- density(tnum)
plot(dObj, lwd=3, col="blue", main="Two Densities in One Graph")
nnum <- rnorm(1000) + 10
ndObj <- density(nnum)
lines(ndObj, col="red", lwd=3)

#
names(dObj)
?density
xdensity <- dObj$x
ydensity <- dObj$y
summary(ydensity)

#
xVals <- seq(from=-10, to=10, by=0.5)
yDat <- 0.1*xVals^2 + 5
zDat <- rnorm(length(xVals))
plot(xVals, yDat, type="l")
lines(xVals, zDat, lwd=3, col="red")

#
range(yDat)
range(zDat)
plot(xVals, yDat, ylim=range(yDat, zDat), type="l")
lines(xVals, zDat, lwd=3, col="red")

#
some.counts <- rpois(20, lambda=2)
dummyLabels <- sample(letters, 20)
barplot(some.counts, names=dummyLabels)

#
barplot(some.counts, col=rainbow(20))
example(barplot)

#
group.sizes <- c(18, 30, 32, 10, 10)
labels <- letters[1:5]
pie(group.sizes, labels, col=rainbow(5))
example(pie)

#
xVals <- seq(from=-10, to=10, by=0.5)
yDat <- 0.1*xVals^2 + 5 
plot(xVals, yDat)
plot(xVals, yData, xlab="X", ylab="Y")
plot(xVals, yData, main="my title goes here")
plot(xVals, yDat, type="l")  # line plot
plot(xVals, yDat, type="p")  # just points
plot(xVals, yDat, type="h")  # histogram-like

#
plot(1:10, 1:10, axes=F)
axis(1, at=1:10, label=letters[1:10])
axis(2)
box()

#
plot(xVals, yDat)
lines(xVals, yDat, lwd=3, lty=2, col="dark green")
lines(xVals, yDat, type="h", col=rainbow(length(yDat))

#
ranMat <- matrix(rnorm(1000), ncol=10, nrow=10)
avgMat <- apply(ranMat, 2, mean)
plot(avgMat, type="l", lwd=4, ylim=range(ranMat))
for( i in 1:nrow(ranMat) ){
	points(ranMat[i,], col=rainbow(i), pch=20)
}

#
plot(0,0)
abline(a=0, b=1)
abline(h=2)
abline(v=1)

#
plot(rnorm(1000), pch=20, col="red") 
plot(rnorm(1000), pch=20, ylim=c(-5,5)) 
locator()
legend(locator(), c("Point"), col="red", pch=20)
plot(1:10, pch=25)
identify()
text(2,6, "label")

#
?par 
par()

#
par(mfrow=c(1,3))

#
par(cex=2)
par(cex.axis=1.4)
par(cex.main=1.9)

#
jpeg("myPic.jpg")
plot(1:10)
dev.off()

x11()
win.graph(32, 16) 

#
win.graph(32, 16) 

#
pdf("myFirstPDF.pdf")
plot(1:10, main="Plot 1")
plot(rnorm(1000), main="Plot 2")
dev.off() 









