##
## Rcourse03.R
##
## R code and examples for "Modern Applied Statistics Using R"
##      Lecture 3: Graphics
##
## Alexander.Ploner@ki.se   2007-09-17
##
########################################################################


##------------------------ Basics ---------------------------------##

# The basic high-level plot
x = rnorm(25)
y = 2 + 3*x + rnorm(25)
plot(x)

# Create a pdf file in home directory
setwd("~")
pdf("test.pdf")
plot(x)
dev.off()
# Nice trick - works if a pdf viewer is installed
viewer = options()$pdfviewer
system(paste(viewer, "test.pdf"))

## Note: we can easily create multipage plots
pdf("test2.pdf")
plot(x, main="Page 1")
plot(y, main="Page 2")
dev.off()

##------------------------- Elements of plotting -------------------------##

# Types of plots; this recreates the plot in the handouts!
# Note that type="n" is especially useful for constructing a plot from scratch
par(mfrow=c(2,3))
plot(x, type="p", main="plot(x, type=\"p\")") # Note the escaped quotes \"
plot(x, type="l", main="plot(x, type=\"l\")")
plot(x, type="b", main="plot(x, type=\"b\")")
plot(x, type="h", main="plot(x, type=\"h\")")
plot(x, type="s", main="plot(x, type=\"s\")")
plot(x, type="n", main="plot(x, type=\"n\")")

# Different symbols and line types
plot(x, pch="x")
plot(x, type="l", lty=2)
plot(x, pch="x",  cex=2)
plot(x, type="l", lwd=2)

# Different boxes around the plot
par(mfrow=c(2,2))
plot(x, pch=19, bty="o") # default
plot(x, pch=19, bty="n")
plot(x, pch=19, bty="l")
plot(x, pch=19, bty="]")

# Changing axes and scale
plot(x, xlim=c(0,30))
plot(x, ylim=c(-3,3))
# Log scale only for positive values!
plot(x, log="x")
plot(abs(x), log="xy")
# Kill graphics window from command line
dev.off()

# Common text elements
plot(x, main="Changing titles and labels", sub="Extra subtitles go here",
  xlab="Index of observation", ylab="Observation")

# Adding extra points and lines; we switch to scatterplot just for fun
plot(x,y)
points(mean(x), mean(y), pch="X", cex=2, font=2)
lines(range(x), range(y))

# Adding text and arrows
text(max(x), min(y), "Center", col=2, adj=c(1,0))
arrows(max(x)-strwidth("Center"), min(y), mean(x), mean(y), col="red", lwd=2)

# Multiple plots
ll = matrix(c(1,3,2,0), nrow=2)
width  = c(0.8, 0.2)
height = c(0.7, 0.2) 
layout(ll, width, height)
plot(x, y, main="Scatterplot with marginal boxplots") # plot 1
boxplot(y) # plot 2
boxplot(x, horizontal=TRUE) # plot 3

##------------------- Examples I -------------------------------------##

# We generate some conventional plots, as seen in the lecture handouts

# Make some builtin data available for plotting
require(Epi)
data(diet)
?diet
require(MASS)
data(Cushings)
?Cushings

# Histogram
hist(diet$weight, xlab="Weight", col="lightblue", main="hist")

# We fit a density, then we plot it
dens1 = density(diet$weight, na.rm=TRUE) # Ignore missing values
plot(dens1, xlab="Weight", main="plot.density")
rug(diet$weight)                         # Add values as ticks

# Barplot of frequencies: we tabulate the data explicitly
tab1 = table(diet$chd, diet$job)
barplot(tab1, xlab="Occupation", main="barplot", col=c("blue","red"), beside=TRUE)
legend("topleft", c("Healthy","CHD"), fill=c("blue","red")) # Add a legend

# Standard boxplot
boxplot(weight~job, diet, main="boxplot", xlab="Occupation", ylab="Weight",
        col="pink", lty=1, pch="x", font=2)
        
# Dotchart useful for smaller data sets, like Cushings
dotchart(Cushings$Tetra, groups=Cushings$Type, main="dotchart",
xlab="Tetrahydrocortisone")

# A scatter plot with a smooting line summarizing the behaviour of points
# Note the us of with() to specify the data frame 
# We could use attach/detach, too
with(diet, scatter.smooth(height, weight, main="scatter.smooth"))


##------------------- Examples II ------------------------------------##

# These are slightly more powerful plots, see also lecture handouts

# Again, we prepare some data
data(volcano)
?volcano
# Transform the data, as seen in ?persp
# Note that z is a matrix, and x,y vectors indicating the row/column 
# spacing for the cells of the matrix!
z = 2 * volcano        # Exaggerate the relief
x = 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y = 10 * (1:ncol(z))   # 10 meter spacing (E to W)
# More data
data(trees)
?trees
require(MASS)
data(caith)
?caith

# 3d plot of the volcano; parameters theta anh phi determine view point of 
# observer; ltheta and shade specify light source and shading; 
persp(x, y, z, theta = 135, phi = 30, col = "green3", scale = FALSE,
      ltheta = -120, shade = 0.75)
# The same as image plot/heatmap, with default colors
image(x,y,z)
# The same as isolines, again with defaults
contour(x,y,z)
# As a matter of fact, we can overlay contours and colors:
image(x,y,z)
contour(x,y,z, add=TRUE)

# Plotting 3d data (instead of a 3d model, like above) requires extra package
require(scatterplot3d)
# Angle determines viewer's position, type="h" draws vertical lines
scatterplot3d(trees, angle=35, type="h") 

# Plot the 3d data as scatterplot, with symbol size & color indicating the 
# third dimension
# Establish a range of N colors across the 90% of the rainbow
N = nrow(trees)
palette(rainbow(N, end=0.9))
# Again use of with() to specify data frame - simpler than having 
# trees$Volume, trees$Height etc., plus makes nicer labels!
with(trees, symbols(Height, Volume, circles = Girth/16, inches = FALSE, 
    bg = 1:N, fg = "gray30"))
# We add a legend for the colors
legend("topleft", legend=quantile(trees$Girth, c(0,0.25,0.5, 0.75, 1)),
       pt.bg = c(1, N/4, N/2, 3*N/4, N), pch=21, pt.cex=2)

# Mosaic plot fow showing proportions by side lengths of rectanges
# These are the numbers
caith
# This is the mosaicplot
mosaicplot(caith, main="mosaicplot", xlab="Eye color", ylab="Hair color")
# This adds color according to size of standardized residuals from 
# a loglinear model for independence 
mosaicplot(caith, main="mosaicplot", xlab="Eye color", ylab="Hair color", shade=TRUE)       

##----------------------- Splitting the device -------------------------##

# For simple splitting in multiple rectangles, see above
# Here we do the hard stuff!

# Same data as above, but can't hurt
require(Epi)
data(diet)
?diet

# Define the layout as matrix
ll = matrix(c(2,1,0,3), nrow=2)
ll

# Define the height & width of the rows & columns of the matrix
width  = c(0.7, 0.3)
height = c(0.3, 0.7)

# Now set this layout for the current device (start one if necessary)
layout(ll, width, height)
# What does it look like?
layout.show(3) # Note: "0" in matrix corresponds to empty area!

# Adjust the margins to a) get more space b) move boxplots closer
par(mar=c(3,3,1,1))
# Now fill in the plots
with(diet, plot(height, weight)) # plot 1
par(mar=c(0,3,1,1))
boxplot(diet$height, axes=FALSE,labels=FALSE, horizontal=TRUE) # plot 2
box()
par(mar=c(3,0,1,1))
boxplot(diet$weight, axes=FALSE,labels=FALSE) # plot 3
box()

##------------------ Coplots -----------------------------------------##

# Data as before
require(Epi)
data(diet)

# Scatterplots, conditional on job
coplot(weight ~ height|job, diet)

# Scatterplots, conditional on job and energy consumption class
coplot(weight ~ height|job*energy.grp, diet)

# a) Conditioning variables can be continous
# b) more information can be added to individual plots
coplot(weight ~ height|job*energy, diet, panel=panel.smooth)

##---------------- Properties of lattice  plots -----------------------------##

# We assume the same data as for the examples I + II above
require(lattice)

# Lattice plots operate with formulas
histogram( ~ weight, data=diet)
xyplot(weight ~ height, diet)

# Lattice plots allow flexible and general conditioning; no separate
# coplot() is required!
histogram( ~ weight|job, data=diet)
bwplot(weight ~ job|energy.grp, data=diet)
# Lattice plots use overlapping shingles to indicate interval scaled 
# conditioning variables
xyplot(weight ~ height|job*equal.count(energy), diet)

# Lattice plots are objects that can be stored, updated and printed
hist1 = histogram( ~ weight|job, data=diet)
hist2 = update(hist1, main="Vertical arrangement", layout=c(1,3))
print(hist2)

# Print function allows extremely flexible arrangement of plots!
# See ?print.trellis for details
hist = update(hist1, layout=c(3,1)) # horizontal arrangement
scat = xyplot(weight ~ energy, diet, group=job, auto.key=TRUE)
boxp = bwplot(factor(month)~energy)
print(hist, c(0, 0.66, 1, 1), more=TRUE)
print(scat, c(0, 0, 0.5, 0.66), more=TRUE)
print(boxp, c(0.5, 0, 1, 0.66))


##---------------- Redoing examples in lattice -----------------------------##

# We assume the same data as for the examples I + II above
require(lattice)

# Histogram, density plot, barplot
histogram( ~ weight, data=diet)
densityplot( ~ weight, diet)
tab1 = with(diet, table(job, factor(chd, levels=0:1, labels=c("Healthy","CHD"))))
barchart(tab1, auto.key=TRUE, stack=FALSE, horizontal=FALSE)

# Standard boxplot
bwplot(weight~job, diet)
        
# Dotplots are useful for smaller data sets - we have two versions
stripplot(Tetrahydrocortisone ~ Type, Cushings)
dotplot(Tetrahydrocortisone ~ Type, Cushings)

# A scatter plot with a smooting line summarizing the behaviour of points
# Note the us of with() to specify the data frame 
# We could use attach/detach, too
xyplot(weight ~ height, diet, panel= function(x,y) {panel.xyplot(x,y);panel.loess(x,y)})


# 3d plot, colorplot, isolines
# Data can be specified as matrix, or as vectors
wireframe(volcano, shade=TRUE, light.source = c(10,0,10))
# As vectors
g = expand.grid(x=x, y=y)
g$z = as.vector(volcano)
wireframe(z ~ x*y, g, shade=TRUE, light.source = c(10,0,10))
levelplot(z ~ x*y, g)
contourplot(z ~ x*y, g)

# A 3d scatterplot; screen specifies the rotation in degrees around the 
# corresponding axes to achieve the point of view for the plot
cloud(Volume ~ Height + Girth, trees, type=c("p","h"), cross=TRUE,
              screen = list(z = -50, x = -70, y = 5))
