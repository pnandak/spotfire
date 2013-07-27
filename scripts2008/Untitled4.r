# 3.  Projections

#The sp class and maptools package provide a mechanism for doing projected maps.  (Note that the projection parameters here are not really appropriate for the area being plotted, but were chosen to make the fact that the map is projected evident.)

# First, load the rgdal package

library(rgdal)

# Oregon climate station data -- data in the orstations shapefile
# equal-frequency class intervals -- spplot & projected
plotvar <- orstations.shp@data$tann # gets data from shapefile .dbf
nclr <- 8
plotclr <- brewer.pal(nclr,"PuOr")
plotclr <- plotclr[nclr:1] # reorder colors
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
basemap <- list("sp.lines", orotl.shp, fill=NA)

# projected
# add proj4string info to shape files
proj4string(orotl.shp) <- CRS("+proj=longlat")
proj4string(orstations.shp) <- CRS("+proj=longlat")

# do the projection
aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110
    +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
orotl.shp.proj <- spTransform(orotl.shp, CRS(aea.proj))
orstations.shp.proj <- spTransform(orstations.shp, CRS(aea.proj))
orgrat <- gridlines(orotl.shp, easts=seq(-127,-115,by=1),
    norths=seq(42,47,by=1), ndiscr = 20)
orgrat.proj <- spTransform(orgrat, CRS(aea.proj))
basemap <- list("sp.lines", orgrat.proj, fill=NA)

spplot(orstations.shp.proj, "tann",
    col.regions=plotclr, cuts=nclr, at=round(class$brks, digits=1),
    xlim=bbox(orotl.shp.proj)[1,], ylim=bbox(orotl.shp.proj)[2,],
    pch=16, cex=1.5, key.space="right",
    sp.layout=list(basemap, orotl.shp.proj),
    main="Annual Temperature")


# Oregon county census data -- attribute data in the orcounty.shp shape file
# project Oregon county data
aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110
    +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
orcounty.shp.proj <- spTransform(orcounty.shp, CRS(aea.proj))

# equal-frequency class intervals -- spplot & projected
plotvar <- orcounty.shp@data$AREA
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="equal")
colcode <- findColours(class, plotclr)
spplot(orcounty.shp.proj, "AREA",
    col.regions=plotclr, at=round(class$brks, digits=1))

# equal-width class intervals -- spplot & projected
plotvar <- orcounty.shp@data$AREA
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
spplot(orcounty.shp.proj, "AREA",
    col.regions=plotclr, at=round(class$brks, digits=1))