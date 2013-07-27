

setwd("C:\\projects\\classes\\R_course\\lecture4")

## read in possum data frame
possum <- read.csv("possum.csv", row.names=1)

head(possum)

plot(possum[,c(6,8,9)])    # This makes a scatter plot of all combinations of the selected columns

######
## writing plots to file
##


# writing to a bitmap (use this for publication graphics)
png("possumScatterPlot.png", 1000,1000, pointsize = 32)

# writing to a png (use this for web graphic)
#bmp("possumScatterPlot.bmp", 1000,1000, pointsize = 32)

plot(possum[,c(6,8,9)])    # same as last plot

dev.off()

## Look at help for par
?par

# Save old par parameters so you an reset them when you are finished
oldPar <- par()


png("possumScatterPlotHistogram.png", 1300,1000, pointsize = 32)

# Set the mfrow to two columns and three rows
par(mfrow=c(2,3))

plot(possum$hdlngth~possum$totlngth)
plot(possum$hdlngth~possum$taill)
plot(possum$totlngth~possum$taill)
hist(possum$hdlngth)
hist(possum$totlngth)
hist(possum$taill)

dev.off()


par(oldPar)

## using layout to create custom graph layout

mat <- matrix(c(1,1,0,2), 2, 2, byrow = TRUE)         # first create a matrix with numbers for each location to plot
mat                                                   # look at mat
lay1 <- layout(mat=mat)                               # make layout and assign to object lay1
layout.show(lay1)                                     # show layout

mat <- matrix(c(1,1,3,2), 2, 2, byrow = TRUE)         # first create a matrix with numbers for each location to plot
lay1 <- layout(mat=mat)
layout.show(lay1)


png("possumLayout1.png", 1000,1500, pointsize = 32)

mat <- matrix(c(1,1,1,1,2,3), 3, 2, byrow = TRUE)
lay1 <- layout(mat=mat)
layout.show(lay1)

plot(possum$hdlngth~possum$totlngth)
hist(possum$hdlngth)
hist(possum$totlngth)

dev.off()


############
##
## Simple statistical analysis
##

## Histograms

?hist     # look at the help for hist

hdlngth.hist <- hist(possum$hdlngth)
totlngth.hist <- hist(possum$totlngth)

hdlngth.hist

par(mfrow = c(1,2))
barplot(hdlngth.hist$counts)
barplot(hdlngth.hist$counts, horiz = T)
par(mfrow = c(1,1))


######
## Using layout to create custom plots

#png("possumLayout2.png", 1000,1000, pointsize = 32)

mat <- matrix(c(2,0,1,3),2,2,byrow=TRUE)
lay2 <- layout(mat, c(3,1), c(1,3), TRUE) 
layout.show(lay2) 

hdlngth.hist <- hist(possum$hdlngth, plot = F)
totlngth.hist <- hist(possum$totlngth, plot = F)

par(mar=c(3,3,1,1))
plot(possum$hdlngth~possum$totlngth, main = "")

par(mar=c(0,3,1,1))
barplot(totlngth.hist$counts)

par(mar=c(3,0,1,1))
barplot(hdlngth.hist$counts, horiz = T)

#dev.off()




par(oldPar)    # reset par to default values


######
### Linear Regression

totHd.lm <- lm(hdlngth ~ totlngth, data = possum)         # Run the regression

summary(totHd.lm)        # Look at summary of the regression

totHd.lm$coefficients      # Look at regression coefficients

plot(totHd.lm)         # Plot regression diagnostic graphs (click Enter to change graphs)


# plotting regression lines
?abline     # look at the help for abline

plot(possum$hdlngth~possum$totlngth)   # first create the plot
abline(totHd.lm, lwd = 1.5, lty = 2)    # Then add the regression line lwd = line weight, lty = line type
totHd.lm$residuals


## Add the regression line to possumLayout2 
png("possumLayout3.png", 1000,1000, pointsize = 32)

mat <- matrix(c(2,0,1,3),2,2,byrow=TRUE)
lay2 <- layout(mat, c(3,1), c(1,3), TRUE) 
layout.show(lay2) 

hdlngth.hist <- hist(possum$hdlngth, plot = F)
totlngth.hist <- hist(possum$totlngth, plot = F)

par(mar=c(3,3,1,1))
plot(possum$hdlngth~possum$totlngth, main = "")
abline(totHd.lm, lwd = 1.5, lty = 2)             # Add the regression line

par(mar=c(0,3,1,1))
barplot(totlngth.hist$counts)

par(mar=c(3,0,1,1))
barplot(hdlngth.hist$counts, horiz = T)

dev.off()

par(oldPar)

## Working with text expressions
totHd.lm$coef

plot(possum$hdlngth~possum$totlngth)
text(x=80,y=102, expression(tailLen == 42.7 + 0.573 * (headLen)))


############
##
## Lattice graphics
##

library(lattice)
attach(possum)
hist(totlngth)   # base graphics package

histogram(totlngth)   # lattice graphics
histogram(~totlngth | sex)
histogram(~totlngth | sex*Pop)

densityplot(~totlngth | sex*Pop)

plot(taill~totlngth)	# Base Graphics

xyplot(taill~totlngth)	# Lattice graphics
xyplot(taill~totlngth | sex) # Lattice graphics
detach(possum)


############
##
## Other Graphics and Expressions
##

# Draw a symbol on a plot
symbols(0,0,circles=0.95,bg="gray", xlim=c(-1,2.25),ylim=c(1,1),inches=FALSE)

xVals <- c(1,3,4)
yVals <- c(4,2,3)
circleSizes <- c(.1, .2, .3)
symbols(xVals , yVals ,circles= circleSizes, bg="gray", xlim = c(0,5), ylim = c(0,5),inches=FALSE)

symbols(xVals , yVals ,circles= circleSizes, bg=c("red", "green", "blue"), xlim = c(0,5), ylim = c(0,5),inches=FALSE)


# Add text to the symbol
text(1.75,0,expression("Area" ==pi*phantom("'") *italic(r)^2))

# Add a two-headed arrow
arrows(0,0,-.95,0,length=.1,code=3)

# Add text above the arrow
text(-0.45,-strheight("R"),expression(italic(r) == 0.95))

# Add a label where you would like on a graph
location <- locator(n=1)

# look at what location is (it is the coordinates where you clicked)
location

# Add text at that location
text(location$x,location$y, expression("Area"==pi*phantom("'")*italic(r)^2))

# You can use locator to draw lines and points
locator(n=2, type = "l")  # line between two points

locator(n=4, type = "p")  # four points
