
# GRAPHICS

# Read the data

setwd("myRworkshop")

load(file="mydata100.Rdata")

head(mydata)

# Get rid of missing values & attach

mydata100 <- na.omit(mydata100)

attach(mydata100) 

#---TRADITIONAL OR BASE GRAPHICS---
#
# R’s first graphics package
# Still the only one that contains generic functions 
# that do a standard set of recommended plots for 
# various objects like models
#
# Extremely flexible
# Not easy to use with groups
#
# Main form: plot( x, y)
#
# Writes to devices using the 
# "traditional graphics system"

plot(workshop)

plot(workshop,gender)

plot(gender,workshop)

plot(workshop,posttest)

plot(posttest,workshop)

plot(posttest) # Not a histogram.

plot(pretest,posttest)

plot( mydata100[3:8] )

hist(posttest)

qqnorm(posttest)

pie( table(workshop) )

dotchart( table(workshop,gender) )

myModel <- lm( posttest~pretest )
par( mfrow=c(2,2) ) # 2x2 plot window
plot(myModel)
par( mfrow=c(1,1) ) # 1x1 plot window

# ---ADDING EMBELLISHMENTS---
#
# These work with all traditional 
# graphics functions

plot( pretest, posttest,
  main="My Main Title",
  xlab="My X Axis Label",
  ylab="My Y Axis Label")


# ---THE lattice PACKAGE---
#
# Especially good at repeating plots for subgroups
#
# Also called panel, trellis, or “by” plots
#
# Conceptually similar to SAS SGPLOT procedures 
# and Stata graphics
#
# Writes to devices using the Grid Graphics 
# System so must use different set of 
# embellishment functions
#
# Not as flexible as the ggplot2 package, 
# so we will do little with it



library( "lattice" )

xyplot(pretest ~ posttest | workshop,
 data=mydata100,  type=c("p","r") )




# ---GGPLOT2 GRAPHICS---
#
# Follows Wilkinson's Grammar of Graphics
#
# Works with underlying graphics concepts, 
# not pre-defined graph types
#
# Enables you to create any data graphic 
# that you can conceive of (except mosaic)
#
# Structurally almost identical to 
#
# SPSS Graphics Production Language (GPL)
# Writes to devices using the Grid Graphics System
# qplot copies simplicity of plot


# ---ggplot2 HAS THREE APPROACHES---
#
# qplot, aka quickplot, 
#   -Its syntax imitates the plot function
#   -It's quick and easy for simple plots
#   -Titles and labels done the same way
#   -Can add to its plots with other methods below
#
# ggplot, accepting default settings
#   -Offers almost total control via the
#    Grammar of Graphics
#   -Requires more code than qplot
#   -Titles, labels done differently (beyond our scope)
#
# ggplot, specifying all settings
#   -Offers total control
#   -requires lots of code


library( "ggplot2" )


# First using quickplot or qplot.

qplot(workshop) #Barchart

qplot(posttest) #Histogram

qplot(workshop,posttest) #Strip plot

qplot(pretest,posttest) #Scatterplot

qplot(pretest,posttest,
  main="My Main Title",
  xlab="My X-axis Label",
  ylab="My Y-axis Label")

# ---ggplot: The Six Parts of a Graph---
#
# 1. Aesthetics – define how the variables will appear. 
#    On an axis? Set shape, color, size?
#
# 2. Geoms – Set geometric objects, e.g. points, bars, lines, boxes… 
#
# 3. Statistics – bins, smoothers, fits, intervals…
#
# 4. Scales – legends… 
#
# 5. Coordinate System – Cartesian or polar
#
# 6. Facets – repeat your plot for groups


library("ggplot2")

# ---ACCEPTING THE DEFAULT ggplot() SETTINGS---

ggplot(data=mydata100, 
  aes(x=pretest, y=posttest) ) +
  geom_point() + 
  geom_smooth(method="lm") +
  facet_grid( workshop~gender )


# ---Same Plot, Specifying All Options---

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
facet_grid( workshop~gender )+
coord_cartesian()

# ---FOR MANY GRAPHICS EXAMPLES SEE---
#
# Hadley Wickham's web page:
# http://had.co.nz/ggplot2/
#
# Discussion list devoted to ggplot2: 
# http://groups.google.com/group/ggplot2
#
# Blog devoted to R http://learnr.wordpress.com/ 
#
# Graphics site http://addictedtor.free.fr/graphiques/ 
#
# The ggplot2 Package, by Hadley Wickham
#
# R for SAS and SPSS Users, by Robert A. Muenchen
#
# R for Stata Users, by Muenchen & Joseph Hilbe

# ---R's THREE GRAPHICS PACKAGES---

                                    Traditional  lattice ggplot2

# Automatic output for different objects      Yes  No        No
# Automatic legends                           No   Sometimes Yes
# Easily repeats plots for different groups   No   Yes       Yes
# Attractiveness of default settings          Good Good      Excellent
# Easy to use with multiple data sources      Yes  No        Yes
# buildS plots piece-by-piece                 Yes  No        Yes
# Allows you to replace pieces after creation No   No        Yes
# Consistent functions                        No   No        Yes
# Can do mosaic plots                         Yes  Yes       No
# Control extends beyond data graphics        Yes  No        No
# Underlying graphics system                  Traditional Grid Grid
