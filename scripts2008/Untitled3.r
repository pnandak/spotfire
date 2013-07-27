# 2.  Variations in color scales and representation

# This set of examples illustrates some more applications of the maptools package, and some variations in the contruction of class intervals for choropleth maps and symbolic representation of the Oregon county-level census data

# Oregon county census data -- equal-frequency class intervals

# equal-frequency class intervals
plotvar <- orcounty.shp@data$AREA
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
#plotclr <- plotclr[nclr:1] # reorder colors if appropriate
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)

plot(orcounty.shp, xlim=c(-124.5, -115), ylim=c(42,47))
plot(orcounty.shp, col=colcode, add=T)
title(main="Area: Equal-Frequency Class Intervals",
sub="Quantile (Equal-Frequency) Class Intervals")
legend(-117, 44, legend=names(attr(colcode, "table")),
    fill=attr(colcode, "palette"), cex=0.6, bty="n")

# Oregon county census data -- equal-width class intervals

# equal-width class intervals
plotvar <- orcounty.shp@data$AREA
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
#plotclr <- plotclr[nclr:1] # reorder colors if appropriate
class <- classIntervals(plotvar, nclr, style="equal")
colcode <- findColours(class, plotclr)

plot(orcounty.shp, xlim=c(-124.5, -115), ylim=c(42,47))
plot(orcounty.shp, col=colcode, add=T)
title(main="Area",
    sub=" Equal-Width Class Intervals")
legend(-117, 44, legend=names(attr(colcode, "table")),
    fill=attr(colcode, "palette"), cex=0.6, bty="n")



#equal-width class intervals of 1990 population
plotvar <- orcounty.shp@data$POP1990
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
#plotclr <- plotclr[nclr:1] # reorder colors if appropriate
class <- classIntervals(plotvar, nclr, style="equal")
colcode <- findColours(class, plotclr)

plot(orcounty.shp, xlim=c(-124.5, -115), ylim=c(42,47))
plot(orcounty.shp, col=colcode, add=T)
title(main="Population 1990",
    sub=" Equal-Width Class Intervals")
legend(-117, 44, legend=names(attr(colcode, "table")),
    fill=attr(colcode, "palette"), cex=0.6, bty="n")

# Oregon county census data -- bubble plots

# bubble plot equal-frequency class intervals
plotvar <- orcounty.shp@data$AREA
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
#plotclr <- plotclr[nclr:1] # reorder colors if appropriate
max.symbol.size=12
min.symbol.size=1
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
symbol.size <- ((plotvar-min(plotvar))/
    (max(plotvar)-min(plotvar))*(max.symbol.size-min.symbol.size)
    +min.symbol.size)

plot(orcounty.shp, xlim=c(-124.5, -115), ylim=c(42,47))
orcounty.cntr <- coordinates(orcounty.shp)
points(orcounty.cntr, pch=16, col=colcode, cex=symbol.size)
points(orcounty.cntr, cex=symbol.size)
    text(-120, 46.5, "Area: Equal-Frequency Class Intervals")
legend(locator(1), legend=names(attr(colcode, "table")),
    fill=attr(colcode, "palette"), cex=0.6, bty="n")

# bubble plot equal-frequency class intervals
plotvar <- orcounty.shp@data$POP1990
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
#plotclr <- plotclr[nclr:1] # reorder colors if appropriate
max.symbol.size=12
min.symbol.size=1
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
symbol.size <- ((plotvar-min(plotvar))/
(max(plotvar)-min(plotvar))*(max.symbol.size-min.symbol.size)
+min.symbol.size)

plot(orcounty.shp, xlim=c(-124.5, -115), ylim=c(42,47))
orcounty.cntr <- coordinates(orcounty.shp)
points(orcounty.cntr, pch=16, col=colcode, cex=symbol.size)
points(orcounty.cntr, cex=symbol.size)
text(-120, 46.5, "Area: Equal-Frequency Class Intervals")
legend(locator(1), legend=names(attr(colcode, "table")),
    fill=attr(colcode, "palette"), cex=0.6, bty="n")

# Oregon county census data -- (pseudo) dot-density maps
# maptools dot-density maps
# warning: this can take a little while
plottitle <- "Population 1990, each dot=1000"
orpolys <- SpatialPolygonsDataFrame(orcounty.shp, data=as(orcounty.shp, "data.frame"))
plotvar <- orpolys@data$POP1990/1000.0

dots.rand <- dotsInPolys(orpolys, as.integer(plotvar), f="random")
plot(orpolys, xlim=c(-124.5, -115), ylim=c(42,47))
plot(dots.rand, add=T, pch=19, cex=0.5, col="magenta")
plot(orpolys, add=T)
title(plottitle)

dots.reg <- dotsInPolys(orpolys, as.integer(plotvar), f="regular")
plot(orpolys, xlim=c(-124.5, -115), ylim=c(42,47))
plot(dots.reg, add=T, pch=19, cex=0.5, col="purple")
plot(orpolys, add=T)
title(plottitle)