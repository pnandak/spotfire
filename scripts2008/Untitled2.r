library(maptools)     # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt)     # finds class intervals for continuous variables



# outlines of Oregon counties (lines)
# browse to orotl.shp
orotl.shp <- readShapeLines(file.choose(),
     proj4string=CRS("+proj=longlat"))

# Oregon climate station data (points)
# browse to orstations.shp
orstations.shp <- readShapePoints(file.choose(),
     proj4string=CRS("+proj=longlat"))

# Oregon county census data (polygons)
# browse to orcounty.shp
orcounty.shp <- readShapePoly(file.choose(),
     proj4string=CRS("+proj=longlat"))

Read ordinary rectangular data sets:

orstationc <- read.csv("orstationc.csv")
orcountyp <- read.csv("orcountyp.csv")
cities <- read.csv("cities.csv")



summary(orcounty.shp)
attributes(orcounty.shp)
attributes(orcounty.shp@data)
attr(orcounty.shp,"polygons")

#1.  Some simple maps

#R has the capability of plotting some simple maps using the maptools package, which can read and plot ESRI shapefiles.  Here are a couple of examples:

#Oregon county census data -- attribute data in the orcounty.shp shape file

# equal-frequency class intervals
plotvar <- orcounty.shp@data$POP1990
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)

plot(orcounty.shp, xlim=c(-124.5, -115), ylim=c(42,47))
plot(orcounty.shp, col=colcode, add=T)
title(main="Population 1990",
    sub="Quantile (Equal-Frequency) Class Intervals")
legend(-117, 44, legend=names(attr(colcode, "table")),
    fill=attr(colcode, "palette"), cex=0.6, bty="n")

#Oregon climate station data -- data in the orstationc.csv file, basemap in orotl.shp

# symbol plot -- equal-interval class intervals
plotvar <- orstationc$tann
nclr <- 8
plotclr <- brewer.pal(nclr,"PuOr")
plotclr <- plotclr[nclr:1] # reorder colors
class <- classIntervals(plotvar, nclr, style="equal")
colcode <- findColours(class, plotclr)

plot(orotl.shp, xlim=c(-124.5, -115), ylim=c(42,47))
points(orstationc$lon, orstationc$lat, pch=16, col=colcode, cex=2)
points(orstationc$lon, orstationc$lat, cex=2)
title("Oregon Climate Station Data -- Annual Temperature",
    sub="Equal-Interval Class Intervals")
legend(-117, 44, legend=names(attr(colcode, "table")),
    fill=attr(colcode, "palette"), cex=0.6, bty="n")

# Oregon climate station data -- locations and data in shape file

# symbol plot -- equal-interval class intervals
plotvar <- orstations.shp@data$pann
nclr <- 5
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="fixed",
fixedBreaks=c(0,200,500,1000,2000,5000))
colcode <- findColours(class, plotclr)
orstations.pts <- orstations.shp@coords # get point data

plot(orotl.shp, xlim=c(-124.5, -115), ylim=c(42,47))
points(orstations.pts, pch=16, col=colcode, cex=2)
points(orstations.pts, cex=2)
title("Oregon Climate Station Data -- Annual Precipitation",
    sub="Fixed-Interval Class Intervals")
legend(-117, 44, legend=names(attr(colcode, "table")),
fill=attr(colcode, "palette"), cex=0.6, bty="n")








