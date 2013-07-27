
# GRAPHICS

# Read the data

setwd("myRworkshop")

load(file="mydata100.Rdata")

head(mydata)

# Get rid of missing values & attach

mydata100 <- na.omit(mydata100)

attach(mydata100) 




#---TRADITIONAL OR BASE GRAPHICS---

plot(workshop)

plot(workshop,gender)

plot(gender,workshop)

plot(workshop,posttest)

plot(posttest,workshop)


plot(posttest)

plot(pretest,posttest)

plot( mydata100[3:8] )

myModel <- lm( posttest~pretest )
plot(myModel)




# ---LATTICE GRAPHICS---

library( "lattice" )

xyplot(pretest ~ posttest | workshop,
 data=mydata100,  type=c("p","r") )




# ---GGPLOT2 GRAPHICS---

library( "ggplot2" )


# First using quickplot or qplot.

qplot(workshop) #Barchart

qplot(posttest) #Histogram

qplot(workshop,posttest) #Strip plot

qplot(pretest,posttest) #Scatterplot


# Now using ggplot

library("ggplot2")

# Scatterplot w/ reg line for each workshop & gender combo.

ggplot(data=mydata100, aes(x=pretest, y=posttest) ) +
  geom_point() + 
  geom_smooth(method="lm") +
  facet_grid( workshop~gender )

# Same as above with all defaults written out.

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