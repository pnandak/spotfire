
#Basic Graphics in R.

setwd("/myRfolder")
load(file="mydata100.Rdata")
attach(mydata100) 
options(width=64)

# Request it to ask you to click for new graph.
par(ask=FALSE, mfrow=c(1,1) )

#---Barplots---

# Barplots of counts via table

barplot( c(40,60) )

barplot(q4)
table(q4)
barplot( table(q4) )

barplot( workshop )
barplot( table(workshop) )
barplot(gender)
barplot( table(gender) )

barplot( table(workshop), horiz=TRUE)

barplot( as.matrix( table(workshop) ),
  beside = FALSE)

# Grouped barplots & mosaic plots

barplot( table(gender,workshop) )

plot( workshop,gender )

mosaicplot( table(workshop,gender) )

mosaicplot(~ Sex + Age + Survived, 
  data = Titanic, color = TRUE)

barplot( table(gender,workshop), beside=TRUE )


# Barplots of means via tapply

myMeans <- tapply(q1, gender, mean, na.rm=TRUE)
barplot(myMeans)

myMeans <- tapply(q1, list(workshop,gender), mean,na.rm=TRUE)
barplot(myMeans, beside=TRUE)

#---Adding main title, color and legend---

barplot( table(gender,workshop), 
  beside=TRUE,
  col=c("gray90","gray60"),
  main="Number of males and females \nin each workshop" )
legend( "topright",
  c("Female","Male"), 
  fill=c("gray90","gray60") )

# A manually positioned legend at 10,15.
legend( 10,15,
  c("Female","Male"), 
  fill=c("gray90","gray60") )

#---Mulitple Graphs on a Page---

par()
head( par() )

par( mar=c(3,3,3,1)+0.1 ) 
par( mfrow=c(2,2) )

barplot( table(gender,workshop) )
barplot( table(workshop,gender) )
barplot( table(gender,workshop), beside=TRUE )
barplot( table(workshop,gender), beside=TRUE ) 

par( mfrow=c(1,1) ) #Sets back to 1 plot per page.
par( mar=c(5,4,4,2)+0.1 ) 


#---Piecharts---
pie( table(workshop), 
  col=c("white","gray90","gray60","black" ),
  main="Piechart of Workshop Attendance" ) 

#---Dotcharts---

dotchart( table(workshop,gender),
  main="Dotchart of Workshop Attendance",
  cex=1.5)

# ---Histograms---

hist(posttest)

# More bins plus density and ticks at values.
hist(posttest, breaks=20, probability=TRUE,
  main="Histogram with Density and Points")
lines( density(posttest) )
myZeros <- rep(0, each=length(posttest) )
myZeros
points( posttest, myZeros, pch="|" )

# Histogram of males only.
hist( posttest[ which(gender=="Male") ], 
  col="gray60")

# Plotting above two on one page, 
# matching breakpoints.
par(mfrow=c(2,1) )
hist(posttest, col="gray90",
  breaks=c(50,55,60,65,70,75,80,85,90,95,100) )
hist(posttest[ which(gender=="Male") ], 
  col="gray60",
  breaks=c(50,55,60,65,70,75,80,85,90,95,100) )
par(mfrow=c(1,1) )

# Could have used either of these:
# breaks=seq(from=50, to=100, by=5) )
# breaks=seq(50,100,5) )

# Histograms overlaid

hist( posttest, col="gray90",
  breaks=seq(from=50, to=100, by=5) )
hist(posttest[ which(gender=="Male") ], 
  col="gray60",
  breaks=seq(from=50, to=100, by=5),
  add=TRUE )
legend( "topright", c("Female","Male"), 
  fill=c("gray90","gray60") )

# Same plot but extracting $breaks 
# from previous graph.

myHistogram <- hist(posttest, col="gray90")
names(myHistogram)
myHistogram$breaks
myHistogram$xlim
hist(posttest[ which(gender=="Male") ], 
  col='gray60', 
  add=TRUE, breaks=myHistogram$breaks)
legend( "topright", c("Female","Male"), 
  fill=c("gray90","gray60") )

# What else does myHistogram hold?
class(myHistogram)
myHistogram

#---Q-Q plots---

library("car")
qq.plot(posttest, 
  labels=row.names(mydata100), 
  col="black" )
detach("package:car")

myQQ <- qqnorm(posttest) #Not shown in text.
identify(myQQ)

#---Stripcharts---

par( mar=c(4,3,3,1)+0.1 )
par(mfrow=c(2,1) )
stripchart(posttest, method="jitter",
  main="Stripchart with Jitter")
stripchart(posttest, method="stack",
  main="Stripchart with Stacking")
par( mfrow=c(1,1) )
par( mar=c(5,4,4,2)+0.1 ) 

par( las=2, mar=c(4,8,4,1)+0.1  )
stripchart(posttest~workshop, method="jitter")
par( las=0, mar=c(5,4,4,2)+0.1 )

# --- Scatterplots ---

plot(pretest,posttest)

# Find low score interactively.
# Click 2nd mouse button to choose stop.
identify(pretest,posttest)

# Check it manually.
mydata100[pretest<60, ]

# Different types of plots.
par( mar=c(5,4,4,2)+0.1 )
par( mfrow=c(2,2) )
plot( pretest, posttest, type="p", main="type=p" )
plot( pretest, posttest, type="l", main="type=l" )
plot( pretest, posttest, type="b", main="type=b" )
plot( pretest, posttest, type="h", main="type=h" )
par( mfrow=c(1,1) )

# Scatterplots with Jitter

par( mar=c(5,4,4,2)+0.1 )
par( mfrow=c(1,2) )
plot( q1, q4,
  main="Likert Scale Without Jitter")
plot( jitter(q1,3), jitter(q4,3),
  main="Likert Scale With Jitter")

# Scatterplot of large data sets.

# Example with pch="." and jitter.
par(mfrow=c(1,2) )
pretest2   <- round( rnorm( n=5000, mean=80, sd=5) ) 
posttest2  <- 
  round( pretest2 + rnorm( n=5000, mean=3, sd=3) )
pretest2[pretest2>100] <- 100
posttest2[posttest2>100] <- 100
plot( pretest2, posttest2,
  main="5,000 Points, Default Character \nNo Jitter")
plot( jitter(pretest2,4), jitter(posttest2,4), pch=".",
  main="5,000 Points Using pch='.' \nand Jitter")
par(mfrow=c(1,1) )

# Hexbins (resets mfrow automatically).
library("hexbin")
plot( hexbin(pretest2,posttest2),
  main="5,000 Points Using Hexbin")
detach("package:hexbin")

rm(pretest2,posttest2) # Cleaning up.

# Scatterplot with different lines added.
plot(posttest~pretest)
abline(h=75,v=75)
abline(a=0, b=1, lty=5)
abline( lm(posttest~pretest),    lty=1 )
lines( lowess(posttest~pretest), lty=3 )
legend( 60, 95,
  c( "Regression", "Lowess", "Posttest=Pretest" ), 
  lty=c(1,3,5) )
 
# Scatterplot of q1 by q2 separately by gender.
plot(posttest~pretest, 
  pch=as.numeric(gender) )

abline( lm( posttest[ which(gender=="Male") ] 
          ~ pretest[ which(gender=="Male")  ] ),
        lty=1 )

abline( lm( posttest[ which(gender=="Female") ] 
          ~ pretest[ which(gender=="Female")  ] ),
        lty=2 )

legend( "topleft", c("Male","Female"), 
         lty=c(1,2), pch=c(2,1) )

# Coplots: conditioned scatterplots
coplot( posttest~pretest | workshop)
coplot( posttest~pretest | q1)

# Scatterplot with Confidence Ellipse.
library("car")
data.ellipse(pretest, posttest, 
  levels=.95,
  col="black")
detach("package:car")

# Confidence Intervals: A small example
x  <- c(1,2,3,4)
y1 <- c(1,2,3,4)
y2 <- c(2,3,4,5)
y3 <- c(3,4,5,6)
yMatrix <- cbind(y1,y2,y3)
yMatrix

# Just the points
plot( x, y2, xlim=c(1,4), ylim=c(1,6), cex=1.5 )

# Points with pseudo-confidence interval
plot( x, y2, xlim=c(1,4), ylim=c(1,6), cex=1.5 )
matlines( x, yMatrix, lty=c(2,1,2), col="black" )
rm( x, y1, y2, y3, yMatrix)

# Confidence Intervals: A realistic example
myIntervals <- 
  data.frame(pretest=seq(from=60, to=100, by=5))
myIntervals
myModel <- lm( posttest~pretest )
myIntervals$pp <- predict( myModel, 
  interval="prediction", newdata=myIntervals)
myIntervals$pc <- predict( myModel, 
  interval="confidence", newdata=myIntervals)
myIntervals
class( myIntervals$pp )
myIntervals$pp
plot( pretest, posttest,
  ylim=range( myIntervals$pretest, 
    myIntervals$pp, na.rm=TRUE),
  main="Regression Fit with Confidence Intervals" )
matlines(myIntervals$pretest, myIntervals$pc, 
  lty=c(1,2,2), col="black" )
matlines(myIntervals$pretest, myIntervals$pp, 
  lty=c(1,3,3), col="black" )

# Scatterplot plotting text labels.

plot(pretest, posttest, 
  pch=as.character(gender) )

plot(pretest, posttest, type="n" )
text(pretest, posttest, 
  label=row.names(mydata100) )

# Scatterplot matrix of whole data frame.
plot(mydata100[3:8]) #Not shown with text.

plot(mydata100[3:8], gap=0, cex.labels=0.9)

pairs(mydata100[3:8], gap=0,
  lower.panel=panel.smooth,
  upper.panel=panel.smooth)

# Dual axes
#Adds room for label on right margin.
par( mar=c(5,5,4,5) ) 
plot( pretest, posttest, axes=FALSE, xlim=c(55,90),
  main="Scatterplot with Dual Axes" )
axis(4)
mtext("Axis label on right size", 
  font=2, side=4, line=3)
par(new=TRUE)
plot(pretest, posttest, xlim=c(55,90) )







#---Boxplots---
plot(workshop, posttest, 
  main="Boxplot using plot function")

par( mfrow=c(2,2) )
boxplot(posttest)
boxplot(pretest,posttest,notch=TRUE)
boxplot(posttest~workshop)
par( las=2, mar=c(8,4,4,2)+0.1 )
boxplot(posttest~workshop:gender)
par( las=1, mar=c(5,4,4,2)+0.1 )

#---Error bar plots---

library("gplots")
par( mfrow=c(1,1) )
plotmeans( posttest~workshop,
  main="Plotmeans from gplots Package")
detach("package:gplots")

interaction.plot( workshop, gender, posttest,
  main="Means Using interaction.plot function")

# ---Adding Labels---

# Many annotations at once.
par( mar=c(5,4,4,2)+0.1 )
par( mfrow=c(1,1) )
par(family="serif")
plot(pretest,posttest,
  main="My Main Title" ,
  xlab="My xlab text" ,
  ylab="My ylab text", 
  sub="My subtitle ", 
  pch=2)

text(66, 88, "My Example Formula")
text(65, 85, 
  expression( hat(beta) == 
  (X^t * X)^{-1} * X^t * Y) )

text (85,65,"My label with arrow", pos=3)
arrows(85,65,64,62, length=0.1)
abline(h=75,v=75)
abline(a=0, b=1, lty=5)
abline( lm(posttest~pretest),    lty=1 )
lines( lowess(posttest~pretest), lty=3 )
legend( 64, 99,
 c( "Regression", "Lowess", "Posttest=Pretest" ), 
 lty=c(1,3,5) )

mtext("line=0", side=1, line=0, at=57 )
mtext("line=1", side=1, line=1, at=57 )
mtext("line=2", side=1, line=2, at=57 )
mtext("line=3", side=1, line=3, at=57 )
mtext("line=4", side=1, line=4, at=57 )

mtext("line=0", side=2, line=0, at=65 )
mtext("line=1", side=2, line=1, at=65 )
mtext("line=2", side=2, line=2, at=65 )
mtext("line=3", side=2, line=3, at=65 )

mtext("line=0", side=3, line=0, at=65 )
mtext("line=1", side=3, line=1, at=65 )
mtext("line=2", side=3, line=2, at=65 )
mtext("line=3", side=3, line=3, at=65 )

mtext("line=0", side=4, line=0, at=65 )
mtext("line=1", side=4, line=1, at=65 )

#---Scatterplot with bells & whistles---
#       Not shown in book

plot(pretest,posttest,pch=19,
  main="Scatterplot of Pretest and Postest", 
  xlab="Test score before taking workshop", 
  ylab="Test score after taking workshop" )
myModel <- lm(posttest~pretest)
abline(myModel)
arrows(60,82,63,72.5, length=0.1)
text(60,82,"Linear Fit", pos=3)
arrows(70,62,58.5,59, length=0.1)
text(70,62,"Double check this value", pos=4)
# Use locator() or:
# predict(myModel,data.frame(pretest=75) )

