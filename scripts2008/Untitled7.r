# 7.  Basemap shapefile generation using the maps package
#  The map2SpatialLines() function in the maptools function can be used to transform lines extracted from the maps package into sp() package-compatible format to provide basemaps for plotting other data sets. Here is an example for the Pacific Northwest.

#  Extraction of a Pacific Northwest basemap
#library(maptools) # also loads sp package
#library(maps)

# extract county outlines from maps() database
pnw.outlines <- map("county", c("oregon","washington", "california",
    "nevada", "idaho"),
xlim=c(-124.5, -116.0), ylim=c(41.0, 47.0))

# prune the lines to Washington, Oregon, and Northern California extent
pnw.outlines <- pruneMap(pnw.outlines, xlim=c(-125.0, -115.0), ylim=c(41.0,
     47.0))

# convert to sp lines
pnw.outlines.sp <- map2SpatialLines(pnw.outlines, proj4string=
     CRS("+proj=longlat"))

plot(pnw.outlines.sp, col="gray", lwd=2)
degAxis(1, at=seq(-125.,-116., by=1.))
degAxis(2, at=seq(42.,47., by=1.))

# project the outlines
aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
     +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
pnw.outlines.proj <- spTransform(pnw.outlines.sp, CRS(aea.proj))

# generate and project gridlines
grat <- gridlines(pnw.outlines.sp, easts=seq(-127,-115,by=1),
     norths=seq(40,47,by=1), ndiscr = 20)
grat.proj <- spTransform(grat, CRS(aea.proj))

plot(pnw.outlines.proj, col="gray", lwd=2)
lines(grat.proj, col="blue", lty=3)



