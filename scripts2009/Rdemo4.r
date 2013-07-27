###################################################################
## R Programming Session 4
##################################################################
x<- 1:10
y<- x^2
plot(y)
plot(list(x=x,y=x))
plot(cbind(x,y))
xy<- data.frame(a=x,b=y)
plot(b~a,data=xy)
plot(b~a,data=xy,type='p')
plot(b~a,data=xy,type='l')
plot(b~a,data=xy,type='b')
plot(b~a,data=xy,type='o')
plot(b~a,data=xy,type='h')
plot(b~a,data=xy,type='s')

#########################
###plot demo 4.1
#########################
par(mfrow=c(2,2)) ## sets up screen for four plots

plot(cars)
lines(lowess(cars)) ###add lines

plot(sin, -pi, 2*pi)  ##yet another way to call this function

## Discrete Distribution Plot:
plot(table(rpois(100,5)), type = "h", col = "red", lwd=10,
     main="rpois(100,lambda=5)")## colour, line width, type, title

## Simple quantiles/ECDF, see ecdf() {library(stats)} for a better one:
plot(x <- sort(rnorm(47)), type = "s", main = "plot(x, type = \"s\")")
points(x, cex = .5, col = "dark red") ##add points, cex

###############################
##legends demo 4.2
###############################
par(mfrow=c(1,1))

## right-justifying a set of labels: thanks to Uwe Ligges
x <- 1:5; y1 <- 1/x; y2 <- 2/x
plot(rep(x, 2), c(y1, y2), type="n", xlab="x", ylab="y")
lines(x, y1); lines(x, y2, lty=2)
temp <- legend("topright", legend = c(" ", " "),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1,
               title = "Line Types")
text(temp$rect$left + temp$rect$w, temp$text$y,
     c("1,000", "1,000,000"), pos=2)
###################
## or use locator() for top left corner (note change of xjust parameter)
###################
plot(rep(x, 2), c(y1, y2), type="n", xlab="x", ylab="y")
lines(x, y1); lines(x, y2, lty=2)
temp <- legend(locator(2), legend = c(" ", " "),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 0, yjust = 1,
               title = "Line Types")
text(temp$rect$left + temp$rect$w, temp$text$y,
     c("1,000", "1,000,000"), pos=2)


#########################
##other types of plots: demo 4.3
#########################
VADeaths
barplot(VADeaths, beside = TRUE,
        col = c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk"),
        legend = rownames(VADeaths), ylim = c(0, 100))
#############################
boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg",
        ylab = "tooth length",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")
boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        subset = supp == "OJ", col = "orange")
legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))
######################
##-- Math expressions demo 4.4
#####################
x <- rexp(100, rate = .5)
hist(x, main = "Mean and Median of a Skewed Distribution")
abline(v = mean(x),   col=2, lty=2, lwd=2)
abline(v = median(x), col=3, lty=3, lwd=2)
ex12 <- expression(bar(x) == sum(over(x[i], n), i==1, n),
                   hat(x) == median(x[i], i==1,n))
utils::str(legend(locator(1), ex12, col = 2:3, lty=2:3, lwd=2))

#########################
## basic lattice plots demo 4.5
########################
library(lattice)
library(MASS)
xyplot(time~dist,data=hills)

bwplot(voice.part ~ height, data=singer, xlab="Height (inches)")

stripplot(voice.part ~ jitter(height), data = singer, aspect = 1,
          jitter = TRUE, xlab = "Height (inches)")

## basic - note type 'b' is different in lattice
xyplot(Time ~ Viscosity, data = stormer, groups = Wt, type = "b")

########################
## panel functions demo 4.6
########################
xyplot(time~dist,data=hills,
panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.lmline(x,y,type='l')
     })

xyplot(time~dist,data=hills,
panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.lmline(x,y,type='l')
          panel.abline(lqs(y~x), lty=3)
    })

xyplot(time~dist,data=hills,
panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.lmline(x,y,type='l')
         panel.abline(lqs(y~x), lty=3)
         panel.identify(x,y,row.names(hills))
     })
##note the axes appear when the identify() finishes.

#################################
#####conditioning demo 4.7
##############################
##conditioning with factor
bwplot(Age ~ Days | Sex*Lrn, data = quine)

##conditioning with a shingle

Cath <- equal.count(swiss$Catholic)
xyplot(Fertility~Education|Cath,data=swiss)

#################################
#####conditioning demo 4.8
##############################

##alter the strip labels
bwplot(Age ~ Days | Sex*Lrn, data = quine,strip=strip.custom(style=2))

## and specify the layout
barchart(yield ~ variety | site, data = barley,
         groups = year, layout = c(1,6),
         ylab = "Barley Yield (bushels/acre)",
         scales = list(x = list(abbreviate = TRUE,
                       minlength = 5)))

###################################
### Keys demo 4.9
###################################


## Note Multiple variables in formula for grouped displays
##auto.key
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       data = iris, scales = "free", layout = c(2, 2),
       auto.key = list(x = .6, y = .7, corner = c(0, 0)))

xyplot(Time ~ Viscosity, data = stormer, groups = Wt, type = "b",auto.key=TRUE)

##simplekey
mylabels<-paste(c("Weight:   ", "", ""), unique(stormer$Wt), "gms")
 xyplot(Time ~ Viscosity, data = stormer, groups = Wt, type = "b",
    key = simpleKey(columns = 3, text =mylabels))


##alter the character and customize the key
sps <- trellis.par.get("superpose.symbol")
sps$pch <- 1:7
trellis.par.set("superpose.symbol", sps)
xyplot(Time ~ Viscosity, data = stormer, groups = Wt, type = "b",
 key = list(columns = 3, text = list(paste(c("Weight:   ", "", ""),
        unique(stormer$Wt), "gms")),
        points = Rows(sps, 1:3)
 )
)

##################################
###multiple plots on one screen demo 4.10
###################################

plot1<-xyplot(dist~speed,cars)

plot2<- xyplot(sin(x)~x, data=data.frame(x=seq(-pi, 2*pi,length=100)),type='l')

## Discrete Distribution Plot:
tmp<- table(rpois(100,5))
plot3<-xyplot(as.vector(tmp)~as.numeric(names(tmp)), type = "h", col = "red", lwd=10,
     main="rpois(100,lambda=5)")## colour, line width, type, title

## Simple quantiles/ECDF, see ecdf() {library(stats)} for a better one:
x<- sort(rnorm(47))
plot4<- xyplot(x~1:47, type = "s", main = "plot(x, type = \"s\")",
       panel=function(x,y){
       panel.xyplot(x,y,type='s')
   panel.xyplot(x,y,type='p',cex=0.5,col='dark red')})

###layout

print(plot1,newpage=TRUE,position=c(0,.5,.5,1),more=TRUE)
print(plot2,position=c(.5,.5,1,1),more=TRUE)
print(plot3,position=c(0,0,.5,.5),more=TRUE)
print(plot4,position=c(.5,0,1,.5))
##or
print(plot1,newpage=TRUE,split=c(1,1,2,2),more=TRUE)
print(plot2,split=c(2,1,2,2),more=TRUE)
print(plot3,split=c(1,2,2,2),more=TRUE)
print(plot4,split=c(2,2,2,2))

############################################
### Example code
###########################################
xyplot(dist ~ speed,cars)

bwplot(count ~spray,InsectSprays,
  xlab='Type of Spray',
  ylab='Number of Insects',
  main='Insect Spray Data')

xyplot(mpg~disp,groups=cyl,data=mtcars,
     xlab='Displacement (cu in.)',
     ylab='Miles/(US)gallon',
     main='Car Road Tests Data',
     auto.key=TRUE)

xyplot(mpg~disp,groups=cyl,data=mtcars,
       key=simpleKey(text=unique(as.character(mtcars$cyl)),columns=3))
###########################################################
##update example
Depth <- equal.count(quakes$depth, number=8, overlap=.1)

xyplot(lat ~ long | Depth, data = quakes)
update(trellis.last.object(),
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75),
       aspect = "iso")


