#Grammar of Graphics in R using ggplot2.

setwd("/myRfolder")
load(file="mydata100.Rdata")
detach(mydata100) #In case I'm running repeatedly.

# Get rid of missing values for facets
mydata100 <- na.omit(mydata100)
attach(mydata100) 
library(ggplot2)


# ---Barplots---

# Barplot - Vertical

qplot(workshop, geom="bar")

ggplot(mydata100, aes( workshop ) ) + 
  geom_bar() 

# Barplot - Horizontal

qplot(workshop, geom="bar") + coord_flip()

ggplot(mydata100, aes( workshop ) ) + 
  geom_bar() + coord_flip()

# Barplot - Single Bar Stacked

qplot(factor(""), fill=workshop, 
  geom="bar", xlab="") +
  scale_fill_grey(start=0, end=1)

ggplot(mydata100, 
  aes(factor(""), fill=workshop) ) +
  geom_bar() + 
  scale_x_discrete("") +
  scale_fill_grey(start=0, end=1)

# Pie charts, same as stacked bar but polar coordinates

# This is almost it. See book for details.
qplot(factor(""), fill=workshop, 
  geom="bar", xlab="") +
  coord_polar(theta="y") +
  scale_fill_grey(start=0, end=1)

ggplot(mydata100, 
  aes( factor(""), fill=workshop ) ) + 
  geom_bar( width=1 ) + 
  scale_x_discrete("") +
  coord_polar(theta="y") +
  scale_fill_grey(start=0, end=1)

# Barplots - Grouped

qplot(gender, geom="bar", 
    fill=workshop, position="stack") + 
  scale_fill_grey(start = 0, end = 1)
qplot(gender, geom="bar", 
    fill=workshop, position="fill")  + 
  scale_fill_grey(start = 0, end = 1)
qplot(gender, geom="bar", 
    fill=workshop, position="dodge") + 
  scale_fill_grey(start = 0, end = 1)

ggplot(mydata100, aes(gender, fill=workshop) ) + 
  geom_bar(position="stack") + 
  scale_fill_grey(start = 0, end = 1)
ggplot(mydata100, aes(gender, fill=workshop) ) + 
  geom_bar(position="fill") + 
  scale_fill_grey(start = 0, end = 1)
ggplot(mydata100, aes(gender, fill=workshop ) ) + 
  geom_bar(position="dodge") + 
  scale_fill_grey(start = 0, end = 1)

# Barplots - Faceted

qplot(workshop, geom="bar", facets=gender~.) 

ggplot(mydata100, aes(workshop) ) +
  geom_bar() + facet_grid( gender~. )

# Barplots - Pre-summarized data

qplot( factor(c(1,2)), c(40, 60), geom="bar")

myTemp <- data.frame( myX=factor( c(1,2) ), myY=c(40, 60) )
myTemp
ggplot(data=myTemp, aes(myX, myY) ) + 
  geom_bar() 


# ---Adding Titles and Labels---

qplot(workshop, geom="bar",
  main="Workshop Attendance", 
  xlab="Statistics Package \nWorkshops")

ggplot(mydata100, aes(workshop, ..count..)) +
  geom_bar() +
  opts( title="Workshop Attendance" ) +
  scale_x_discrete("Statistics Package \nWorkshops")

ggplot(mydata100, aes(pretest,posttest ) ) +
  geom_point() + 
  scale_x_continuous("Test Score Before Training") +
  scale_y_continuous("Test Score After Training")  +
  opts( title="The Relationship is Linear" )


# ---Dotcharts---

# When the book was written, qplot defaulted to geom="point"
# when there was only a single variable plotted. Now it defaults
# to geom="bar". This is the qplot code from the book, which now 
# does the same plot but with bars:

qplot(workshop, stat="bin",
     facets=gender~., size=4 ) +
 coord_flip()

# This is the qplot code that will do a dotchart.
# It's the same as above but adds geom="point".

qplot(workshop, stat="bin",
     facets=gender~., geom="point", size=4 ) +
 coord_flip()

# The ggplot code was unaffected by this change

ggplot(mydata100, 
   aes(workshop, ..count.. ) ) +
   geom_point(stat="bin", size=4) + coord_flip()+ 
   facet_grid( gender~. ) 


# ---Histograms---

# Simle Histogram
qplot(posttest, geom="histogram")

ggplot(mydata100, aes(posttest) ) +
  geom_histogram()

# Histogram with more bars.
qplot(posttest, geom="histogram", binwidth=0.5)

ggplot(mydata100, aes(posttest) ) + 
  geom_histogram( binwidth=0.5 )

# Density plot
qplot(posttest, geom="density")

ggplot(mydata100, aes(posttest)) + 
  geom_density()

# Histogram with Density

qplot(data=mydata100,posttest, ..density.., 
  geom=c("histogram","density") )
 
ggplot(mydata100, aes(posttest, ..density.. ) ) + 
  geom_histogram() + geom_density()

# Histogram - Separate plots by group

qplot(posttest, geom="histogram", facets=gender~.)

ggplot(mydata100, aes(posttest) ) + 
  geom_histogram() + facet_grid( gender~. )

# Histogram with Stacked Bars

qplot( posttest, geom="histogram", fill=gender ) +
  scale_fill_grey(start = 0, end = 1)

ggplot(mydata100, aes(posttest, fill=gender) ) + 
  geom_bar() + 
  scale_fill_grey(start = 0, end = 1)
 

# ---QQ plots--- 
qplot(sample=posttest, stat="qq")

ggplot( mydata100, aes(sample=posttest) ) + 
  stat_qq() 


# ---Strip plots---

# The developer had added more jitter by default
# since the book was written. That is good if you
# have lots of data or lots of groups. In our case,
# I do not like the defaults:

qplot( factor(""), posttest, geom="jitter", xlab="") 

ggplot(mydata100, aes(factor(""), posttest) ) + 
  geom_jitter() +
  scale_x_discrete("")

# Strip plot by group.
qplot(workshop, posttest, geom="jitter")

ggplot(mydata100, aes(workshop, posttest) ) + 
  geom_jitter()

# Here I do all the strip plots again, limiting
# the amount of jitter to recreate the plots in
# the book.

qplot( factor(""), posttest, data = mydata100, xlab="",
  position=position_jitter(width=.02)) 

ggplot(mydata100, aes(factor(""), posttest) ) +
 geom_jitter(position=position_jitter(width=.02)) +
 scale_x_discrete("")

# Strip plot by group.
# Note that I am increasing the jitter width from 
# .02 to .08 because there is only one fourth the
# room for each graph.

qplot(workshop, posttest, data = mydata100, xlab="",
  position=position_jitter(width=.08))

ggplot(mydata100, aes(workshop, posttest) ) +
 geom_jitter(position=position_jitter(width=.08)) +
 scale_x_discrete("")


# ---Scatterplots---

# Simple scatterplot

qplot(pretest, posttest)
qplot(pretest, posttest, geom="point")

ggplot(mydata100, aes(pretest, posttest)) + 
  geom_point()

# Scatterplot connecting points sorted on x.
qplot( pretest, posttest, geom="line")

ggplot(mydata100, aes(pretest, posttest) ) + 
  geom_line() 

# Scatterplot connecting points in data set order.

qplot( pretest, posttest, geom="path")

ggplot(mydata100, aes(pretest, posttest) ) + 
  geom_path() 

# Scatterplot with skinny histogram-like bars to X axis.

#qplot does not do this in 0.5.7 but will in future release:
qplot(pretest,posttest, 
  xend=pretest, yend=50, 
  geom="segment")

ggplot(mydata100, aes(pretest,    posttest) ) + 
  geom_segment( aes(  pretest,    posttest, 
                 xend=pretest,    yend=50) )

# Scatterplot with jitter
qplot(q1, q4) #First without

# This is the code from the book, which no longer works
qplot(q1, q4, position=position_jitter(x=5,y=5) ) 

# This is the new way to specify the plot. You can try
# different amounts of jitter of course.
qplot(q1, q4, position=position_jitter(width=.3,height=.3))

ggplot(mydata100, aes(x=q1, y=q2) ) +
 geom_point(position=position_jitter(width=.3,height=.3))

# Scatterplot on large data sets

pretest2   <- round( rnorm( n=5000, mean=80, sd=5) ) 
posttest2  <- round( pretest2 + rnorm( n=5000, mean=3, sd=3) )
pretest2[pretest2>100] <- 100
posttest2[posttest2>100] <- 100
temp=data.frame(pretest2,posttest2)

# Small, jittered, transparent points.

qplot(pretest2, posttest2, data = temp, 
  size = I(1), colour = I(alpha("black", 0.15)), 
  geom = "jitter")

# Or in the next version of ggplot2: 

qplot(pretest2, posttest2, data = temp, 
  size = I(1), alpha = I(0.15), 
  geom = "jitter")

ggplot(temp, aes(pretest2, posttest2), 
  size=2, position = position_jitter(x=2,y=2) ) +
  geom_jitter(colour=alpha("black",0.15) )

ggplot(temp, aes(pretest2, posttest2)+
 geom_point(colour=alpha("black",0.15),
   position=position_jitter(width=.3,height=.3)) )

# Using density contours and small points.

qplot(pretest2, posttest2, data=temp, size = I(1), 
  geom=c("point","density2d"))

# geom_density_2d was renamed geom_density2d

ggplot(temp, aes( x=pretest2, y=posttest2) ) +
 geom_point( size=1 ) + geom_density2d()

rm(pretest2,posttest2,temp)

# Scatterplot with regression line, 95% confidence intervals. 

qplot(pretest, posttest, 
  geom=c("point","smooth"), method=lm ) 

ggplot(mydata100, aes(pretest, posttest) ) + 
  geom_point() + geom_smooth(method=lm)

# Scatterplot with regression line but NO confidence intervals.

qplot(pretest, posttest, 
  geom=c("point","smooth"), 
  method=lm, se=FALSE )

ggplot(mydata100, aes(pretest, posttest) ) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE)

# Scatter with x=y line 

qplot(pretest, posttest, 
  geom=c("point","abline"), 
  intercept=0, slope=1 ) 

ggplot(mydata100, aes(pretest, posttest) ) +
  geom_point()+
  geom_abline(intercept=0, slope=1)

# Scatterplot with different point shapes for each group.

qplot(pretest, posttest, shape=gender)

ggplot(mydata100, aes(pretest, posttest) ) + 
  geom_point( aes(shape=gender ) )

# Scatterplot with regressions fit for each group. 

qplot(pretest, posttest, 
  geom=c("smooth","point"), 
  method="lm", shape=gender)

ggplot(mydata100, 
  aes(pretest, posttest, shape=gender) ) + 
  geom_smooth( method="lm" ) + geom_point()

# Scatterplot faceted for groups

qplot(pretest, posttest, 
  geom=c("smooth", "point"), 
  method="lm", shape=gender, 
  facets=workshop~gender)

ggplot(mydata100, 
  aes(pretest, posttest, shape=gender) ) +
  geom_smooth( method="lm" ) + geom_point() +
  facet_grid( workshop~gender )

# Scatter with vertical or horizontal lines

# When the book was written, qplot required the 
# values to be equal. Now it does not using 
# xintercept and yintercept.

qplot(pretest, posttest,
 geom=c("point", "vline", "hline"),
 xintercept=75, yintercept=75)

ggplot(mydata100, aes(pretest, posttest)) + 
  geom_point() + 
  geom_vline( intercept=75 ) + 
  geom_hline( intercept=75 )

# Scatterplot with a set of vertical lines

qplot(pretest, posttest, type="point") + 
  geom_vline( intercept=seq(from=70,to=80,by=2) )

ggplot(mydata100, aes(pretest, posttest)) + 
  geom_point() + 
  geom_vline( intercept=seq(from=70,to=80,by=2) )

ggplot(mydata100, aes(pretest, posttest)) + 
  geom_point() + 
  geom_vline( intercept=70:80 )

# Scatter plotting text labels

qplot(pretest, posttest, geom="text", 
  label=rownames(mydata100) )

ggplot(mydata100, 
  aes(pretest, posttest, 
  label=rownames(mydata100) ) ) + 
  geom_text()

# Scatterplot matrix 

plotmatrix( mydata100[3:8] )

# Small points & lowess fit.
plotmatrix( mydata100[3:8], aes( size=1 ) ) + 
  geom_smooth()

# Shape and gender fits.
plotmatrix( mydata100[3:8], 
  aes( shape=gender ) ) + 
  geom_smooth(method=lm)

# ---Boxplots---

# Boxplot of one variable

qplot(factor(""), posttest, 
  geom="boxplot", xlab="") 

ggplot(mydata100, 
  aes(factor(""), posttest) ) +
  geom_boxplot() +
  scale_x_discrete("")

# Boxplot by group

qplot(workshop, posttest, geom="boxplot" )

ggplot(mydata100, 
  aes(workshop, posttest) ) + 
  geom_boxplot()

# Boxplot by group with jitter

# The jitter in this is wider than when the
# book was published. To control it, you need
# to use ggplot.
qplot(workshop, posttest, 
  geom=c("boxplot","jitter") )

# Here is the ggplot code from the book that
# also results in very wide jitter now.
ggplot(mydata100, 
  aes(workshop, posttest )) + 
  geom_boxplot() + geom_jitter()

# This adjustment makes it look like the plot in the book.
ggplot(mydata100,
 aes(workshop, posttest )) +
 geom_boxplot() +
 geom_jitter(position=position_jitter(width=.1))

# Boxplot for two-way interaction.

qplot(workshop, posttest, 
  geom="boxplot", fill=gender ) + 
  scale_fill_grey(start = 0, end = 1)

ggplot(mydata100, 
  aes(workshop, posttest) ) + 
  geom_boxplot( aes(fill=gender), colour="grey50") + 
  scale_fill_grey(start = 0, end = 1)

# Error bar plot

qplot( as.numeric(workshop), 
       posttest, geom="jitter") + 
  stat_summary(fun="mean", 
    geom="smooth", se=FALSE) +
  stat_summary(fun="mean_cl_normal", 
    geom="errorbar", width=.2)

ggplot(mydata100, 
  aes( as.numeric(workshop), posttest) ) + 
  geom_jitter() +
  stat_summary(fun="mean", 
    geom="smooth", se=FALSE) +
  stat_summary(fun="mean_cl_normal", 
    geom="errorbar", width=.2)

# ---Logarithmic Axes---

# Change the variables
qplot( log(pretest), log(posttest) )

ggplot(mydata100, 
  aes( log(pretest), log(posttest) ) ) +
  geom_point()

# Change axis labels

qplot(pretest, posttest, log="xy") 

ggplot(mydata100, aes( x=pretest, y=posttest) ) +
  geom_point() + scale_x_log10() + scale_y_log10()

# Change axis scaling

# Tickmarks remain uniformly spaced,
# because scale of data is too limited.

qplot(pretest, posttest, data=mydata100)  + 
  coord_trans(x="log10", y="log10")

ggplot(mydata100, aes( x=pretest, y=posttest) ) +
  geom_point() + coord_trans(x="log10", y="log10") 

# ---Aspect Ratio---

# This forces x and y to be equal.
qplot(pretest, posttest) + coord_equal()
 
# This sets aspect ratio to height/width.
qplot(pretest, posttest) + coord_equal(ratio=1/4)

#---Multiframe Plots: Barchart Example---

# Clears the page, otherwise new plots 
# will appear on top of old.

grid.newpage()

# Sets up a 2 by 2 grid to plot into.
pushViewport( viewport(layout=grid.layout(2,2) ) )

# Barchart dodged in row 1, column 1.
myPlot <- ggplot(mydata100, 
    aes(gender, fill=workshop) ) + 
  geom_bar(position="stack") + 
  scale_fill_grey(start = 0, end = 1) +
  opts( title="position=stack " )
print(myPlot, vp=viewport(
  layout.pos.row=1, 
  layout.pos.col=1) )

# Barchart stacked, in row 1, column 2.
myPlot <- ggplot(mydata100, 
    aes(gender, fill=workshop) ) + 
  geom_bar(position="fill") + 
  scale_fill_grey(start = 0, end = 1) + 
  opts( title="position=fill" )
print(myPlot, vp=viewport(
  layout.pos.row=1, 
  layout.pos.col=2) )

# Barchart dodged, given frames, 
# in row 2, columns 1 and 2.
myPlot <- ggplot(mydata100, 
    aes(gender, fill=workshop) ) + 
  geom_bar(position="dodge")  + 
  scale_fill_grey(start = 0, end = 1) + 
  opts( title="position=dodge" )
print(myPlot, vp=viewport(
  layout.pos.row=2, 
  layout.pos.col=1:2) )

#---Multiframe Scatterplots---

# Clears the page, otherwise new plots will appear on top of old.
grid.newpage()

# Sets up a 2 by 2 grid to plot into.
pushViewport( viewport(layout=grid.layout(2,2) ) )

# Scatterplot of points
myPlot <- qplot(pretest, posttest,main="geom=point") 
print(myPlot, vp=viewport(
  layout.pos.row=1, 
  layout.pos.col=1) )

myPlot <- qplot( pretest, posttest, 
          geom="line", main="geom=line" )
print(myPlot, vp=viewport(
  layout.pos.row=1, 
  layout.pos.col=2) )

myPlot <- qplot( pretest, posttest, 
          geom="path", main="geom=path" )
print(myPlot, vp=viewport(
  layout.pos.row=2, 
  layout.pos.col=1) )

myPlot <- ggplot( mydata100, aes(pretest, posttest) ) + 
          geom_segment( aes(x=pretest, y=posttest, 
                         xend=pretest, yend=58) ) + 
          opts( title="geom_segment example" )

print(myPlot, 
  vp=viewport(layout.pos.row=2, layout.pos.col=2) )

# ---Multiframe Scatterplot for Jitter---
grid.newpage()
pushViewport( viewport(layout=grid.layout(1,2) ) )

# Scatterplot without
myPlot <- qplot( q1, q4,
          main="Likert Scale Without Jitter")
print(myPlot, vp=viewport(
  layout.pos.row=1, 
  layout.pos.col=1) )

myPlot <- qplot( q1, q4, 
          position=position_jitter(x=5,y=5),
          main="Likert Scale With Jitter" ) 
print(myPlot, vp=viewport(
  layout.pos.row=1, 
  layout.pos.col=2) ) 

# ---Detailed Comparison of qplot and ggplot---

qplot(pretest, posttest, 
  geom=c("point","smooth"), method="lm" ) 

# Or ggplot with default settings:

ggplot(mydata100, aes(x=pretest, y=posttest) ) +
  geom_point() + 
  geom_smooth(method="lm")

# Or with all the defaults displayed:
ggplot() +
layer(
  data=mydata100,
  mapping=aes(x=pretest, y=posttest),
  geom="point",
  stat="identity"
) +
layer(
  data=mydata100,
  mapping=aes(x=pretest, y=posttest),
  geom="smooth",
  stat="smooth",
  method="lm"
) +
coord_cartesian()





