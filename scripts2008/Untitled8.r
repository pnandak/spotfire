# 4.  Examples using the maps package

# The R maps package provides a means of mapping data that are not neccesarily components of a shapefile.  The package provides a way of plotting choropleth maps using polygons that it contains (U.S. states and counties, countries of the world), and can used unfilled polygons to provide basemaps for point data.  The following example replicate  two maps done using the maptools package.

# Oregon county census data -- bubble plots
library(maps)

# look at the orcounty.csv data
attach(orcountyp)
summary(orcountyp)

# check names for proper order
map("county","oregon", names=T, plot=F)
print(orcountyp$Name)

# bubble plot equal-frequency class intervals
map("county", "oregon", xlim=c(-125,-114), ylim=c(42,47))
map.axes()
plotvar <- orcountyp$Area
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
#plotclr <- plotclr[nclr:1] # reorder colors if appropriate
max.symbol.size=10
min.symbol.size=1
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
symbol.size <- ((plotvar-min(plotvar))/
    (max(plotvar)-min(plotvar))*(max.symbol.size-min.symbol.size)
    +min.symbol.size)
points(orcountyp$Longitude, orcountyp$Latitude, pch=16, col=colcode,
    cex=symbol.size)
points(orcountyp$Longitude, orcountyp$Latitude, cex=symbol.size)
text(-120, 46.5, "Equal-Frequency Class Intervals")
legend(locator(1), legend=names(attr(colcode, "table")),
    fill=attr(colcode, "palette"), cex=0.6, bty="n")
