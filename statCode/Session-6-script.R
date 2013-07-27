##----------------------------------##
## Script for Session 6: Graphics   ##
##    John Fox                      ##
## Statistical Computing in R/S     ##
##    ICPSR Summer Program 2008     ##
##----------------------------------##

    # Graphics Basics
    
# points, lines, axes, frames

plot(c(0,1), c(0,1), type='n', xlab='', ylab='')  # coordinate system

?plot

par('col')  # graphical parameters

par()

?par

plot(1:25, pch=1:25, xlab='Symbol Number', ylab='')  # symbols
lines(1:25, type='h', lty='dashed')

plot(26:1, xlab='letters', ylab='', 
    pch=letters, axes=FALSE, frame=TRUE)
  
plot(c(1,7), c(0,1), type='n', axes=FALSE,  # lines
    xlab='Line Type (lty)', ylab='')
box()
axis(1, at=1:6)
for (lty in 1:6) lines(c(lty, lty, lty + 1), 
                    c(0, 0.5, 1), lty=lty)

# text 

par(mfrow=c(1,2))  # array of plots
    
plot(c(0,1), c(0,1), axes=FALSE, type='n', xlab='', ylab='')
box()
text(x=c(.2, .5), y=c(.2, .7), 
    c('example text', 'another string'))
title('(a)')

plot(c(0,1), c(0,1), axes=FALSE, type='n', xlab='', ylab='')
box()
text(locator(3), c('one','two','three')) # position by mouse
title('(b)')

locator() # returns mouse coordinates

# arrows and line segments

plot(c(1,5), c(0,1), axes=FALSE, type='n', xlab='', ylab='')
arrows(x0=1:5, y0=rep(0.1, 5), 
    x1=1:5, y1=seq(0.3, 0.9, len=5), code=3)
title('(a) arrows')


plot(c(1,5), c(0,1), axes=FALSE, type='n', xlab='', ylab='')
segments(x0=1:5, y0=rep(0.1, 5), 
    x1=1:5, y1=seq(0.3, 0.9, len=5))
title('(b) segments')


# polygons

par(mfrow=c(1,1)) # restore single panel

plot(c(0,1), c(0,1), type='n', xlab='', ylab='')
polygon(c(.2,.8,.8), c(.2,.2,.8), col="red")
polygon(c(.2,.2,.8), c(.2,.8,.8))

# legend

plot(c(1,5), c(0,1), axes=FALSE, type='n', xlab='', ylab='',
    frame.plot=TRUE)
legend(locator(1), legend=c('group A', 'group B', 'group C'),
    lty=1:3, pch=1:3, col=c("blue", "green", "red"))
    

# colors

pie(rep(1, length(palette())), col=palette())
    
palette()

colors()

rainbow(10)

gray(0:8/8)

pie(rep(1,100), col=rainbow(100), labels=rep('',100))

pie(rep(1,100), col=gray(0:100/100), labels=rep('',100))

    # Putting it together:
    
# Diagrams of the standard normal density function

        # showing the area above 1.96

oldpar <- par(mar = c(5, 6, 4, 2) + 0.1)    # leave room on the left
oldpar  # old parameter saved

z <- seq(-4, 4, length=1000)
p <- dnorm(z)
plot(z, p, type="l", lwd=2,
    main=expression("The Standard Normal Density Function" ~~ phi(z)),
    ylab=expression(phi(z) == 
        frac(1, sqrt(2*pi)) * ~~ e^- ~~ frac(z^2, 2)))

?plotmath

abline(h=0, col="gray")
abline(v=0, col="gray")

z0 <- z[z >= 1.96]    # define region to fill
z0 <- c(z0[1], z0)
p0 <- p[z >= 1.96]
p0 <- c(0, p0)
polygon(z0, p0, col="gray")

coords <- locator(2)    # locate head and tail of arrow
arrows(coords$x[1], coords$y[1], coords$x[2], coords$y[2], code=1, 
    length=0.125)
text(coords$x[2], coords$y[2], pos=3,   # text above tail of arrow
    expression(integral(phi(z)*dz, 1.96, infinity) == .025))

        # with lines at z = -3:3
        
par(oldpar)  # restore graphics parameters

plot(z, p, type="n", xlab="", ylab="", axes=FALSE,
    main=expression("The Standard Normal Density Function" ~~ phi(z)))
axis(1, pos=0, at=-3:3)
abline(h=0)
axis(2, pos=0, at=.1*1:3)
abline(v=0)
lines(z, p, lwd=2)
text(locator(2), c("z", expression(phi(z))), xpd=TRUE)
for (z0 in -3:3) lines(c(z0, z0), c(0, dnorm(z0)), lty=2)


# a four-panel display explaining kernel regression

UN <- read.table("http://socserv.socsci.mcmaster.ca/jfox/Courses/R-course/UnitedNations.txt")
UN.2 <- na.omit(UN[, c("life.female", "gdp.capita")])   # valid data only
head(UN.2)

par(mfrow=c(2,2))   # 2 x 2 array of graphs

gdp <- UN.2$gdp.capita
life <- UN.2$life.female
ord <- order(gdp)   # sort data by gdp
gdp <- gdp[ord]     
life <- life[ord]

x0 <- gdp[120]           # focal x = x_(120)
dist <- abs(gdp - x0)   # distance from focal x
h <- sort(dist)[95]     # bandwidth for span of .5 (where n = 190)
pick <- dist <= h       # observations within window

plot(gdp, life, xlab="GDP per Capita", ylab="Female Expectation of Life",
    type="n", main="(a) Observations Within the Window\nspan = 0.5")
points(gdp[pick], life[pick], col="gray20")
points(gdp[!pick], life[!pick], col="gray60")
abline(v=x0)    # focal x
abline(v=c(x0 - h, x0 + h), lty=2)  # window
text(locator(1), expression(x[(120)]), xpd=TRUE)

plot(range(gdp), c(0,1), xlab="GDP per Capita", 
    ylab="Tricube Kernel Weight", 
    type="n", main="(b) Tricube Weights")
abline(v=x0)
abline(v=c(x0 - h, x0 + h), lty=2)
    # function to calculate tricube weights:
tricube <- function(z) ifelse(abs(z) < 1, (1 - (abs(z))^3)^3, 0)
x <- seq(min(gdp), max(gdp), length=1000)
z <- (x - x0)/h
lines(spline(x, tricube(z)), lwd=2)
points(gdp[pick], tricube(((gdp-x0)/h)[pick]), col="gray20")
abline(h=c(0,1), col="gray")

plot(gdp, life, xlab="GDP per Capita", ylab="Female Expectation of Life",
    type="n", main="(c) Weighted Average (Kernal Estimate)")
points(gdp[pick], life[pick], col="gray20")
points(gdp[!pick], life[!pick], col="gray60")
abline(v=x0)
abline(v=c(x0 - h, x0 + h), lty=2)
yhat <- weighted.mean(life, tricube((gdp - x0)/h))  # kernel estimate
lines(c(x0 - h, x0 + h), c(yhat, yhat), lwd=3)

plot(gdp, life, xlab="GDP per Capita", ylab="Female Expectation of Life",
    main="(d) Complete Kernel Estimate")
yhat <- rep(0, length(gdp))
for (i in 1:length(gdp)){   # kernel estimate at each x
    x0 <- gdp[i]
    dist <- abs(gdp - x0)
    h <- sort(dist)[95]
    yhat[i] <- weighted.mean(life, tricube((gdp - x0)/h))
    }
lines(gdp, yhat, lwd=2)

    # Trellis displays
    
library(nlme)    # for data
library(lattice) # for Trellis graphics

data(MathAchieve)
head(MathAchieve)

data(MathAchSchool)
head(MathAchSchool)

        # data management

Bryk <- MathAchieve[, c("School", "SES", "MathAch")]
Sector <- MathAchSchool$Sector
names(Sector) <- row.names(MathAchSchool)
Bryk$Sector <- Sector[as.character(Bryk$School)]
head(Bryk)

        # examine 20 Catholic and 20 public schools
    
attach(Bryk)
cat <- sample(unique(School[Sector=='Catholic']), 20)
Cat.20 <- Bryk[School %in% cat,]
 
pub <- sample(unique(School[Sector=='Public']), 20)      
Pub.20 <- Bryk[School %in% pub,]
    
trellis.device(color=FALSE)

xyplot(MathAch ~ SES | School, data=Cat.20, main="Catholic Schools",
    ylab="Math Achievement",
    panel=function(x, y){
        panel.xyplot(x, y)
        panel.loess(x, y, span=1)
        panel.lmline(x, y, lty=2)
        }
    )

trellis.device(color=FALSE)
    
xyplot(MathAch ~ SES | School, data=Pub.20, main="Public Schools", 
    ylab="Math Achievement",
    panel=function(x, y){
        panel.xyplot(x, y)
        panel.loess(x, y, span=1)
        panel.lmline(x, y, lty=2)
        }
    )

