
primates <- read.csv("primates.csv", row.names=1)

#plot Bodywt versus Brainwt for primates
plot(x=primates$Brainwt, y=primates$Bodywt)
plot(x=Bodywt, y=Brainwt, data = primates)   # another way to do the same thing
plot(Brainwt~Bodywt, data = primates) # yet another way to do the same thing


# Add x and y labels to the graph
plot(Brainwt~Bodywt, data = primates, xlab="Body Weight (kg)", ylab="Brain Weight (g)")


# xlim and ylim change the x and y limits
plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1400))


# pch changes the point character type
plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1400), pch = 20)


# pch changes the point character size
plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1400), pch = 20, cex = 3)


# Use the par command to change graphing parameters
?par
par()   # look at par values

oldPar <- par()   # save the old par values
par(cex=1.25)
par()
par(oldPar)   # resets the old par values

par(cex=1.25)

plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1400))
plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1500))        # this one looks better

# multiple changes can be made with one call to par
par(cex=1.25, pch = 20)


# multiple point characters
plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1500), pch = 21:25)        # this one looks better

# Add a legend
legend("topright", legend=row.names(primates), pch = 21:25)


## Change the orientation of the tick labels
par(las=1)
plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1400))


## change the y axis labels
plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1400), yaxp=c(0,1200,4))
plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1400), yaxp=c(0,1000,2))


# Use of colors
pointColors <- c("red", "orange", "green", "blue", "magenta")

plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1400), pch = 21:25, bg = pointColors)

legend("topright", legend=row.names(primates), pt.bg= pointColors, pch = 21:25)


# Labeling points on the graph
plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1400))

text(primates$Brainwt~primates$Bodywt, labels=row.names(primates),pos = 4)    # this places the row names of primate to the right of each of the points


# Shifting the label points
plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1400))

text(x=primates$Bodywt, y=primates$Brainwt+c(-0.125,0,0,0.125,0) *par()$cxy[2], labels= row.names(primates), pos = 4)


# identify can be used to manually place point on the graph
plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1400))
identify(primates$Bodywt, primates$Brainwt,labels=row.names(primates), n=2)


# Putting it all together
plot(Brainwt~Bodywt, data = primates, xlim=c(0,300), ylim=c(0,1400),xlab="Body Weight (kg)", ylab="Brain Weight (g)",pch = 21:25, bg = pointColors, yaxp=c(0,1200,4))

legend("topright", legend=row.names(primates), pt.bg= pointColors, pch = 21:25)

text(x=primates$Bodywt, y=primates$Brainwt+c(-0.125,0,0,0.125,0) *par()$cxy[2], labels= row.names(primates), pos = 4)

## Adding an additional plot to the graph

# This adds a line
lines(x=c(50,100,150,200), y=c(250,400,550,750), type = "b", lwd = 3, lty = 2)

# This adds points
points(x = c(150,200,250,300), y = c(100,150,200,250), cex = 3)

