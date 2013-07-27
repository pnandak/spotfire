6.  Further examples illustrating map projection using the maps package

The maps package can provide projected base maps, which can provide less distorted views of a data set.  Here are some examples that plot the locations of the Oregon climate-station data

unprojected maps
# unprojected
map("county", "oregon", fill=F)
points(orstationc$lon, orstationc$lat)
text(orstationc$lon, orstationc$lat, labels=orstationc$station, col="red",
     cex=.8)

# unprojected, add surrounding states
map("county", c("oregon","washington", "california", "nevada", "idaho"), xlim=c(-125,-116), ylim=c(41,47), fill=F)
map("state", "oregon", fill=F, col="grey", lwd=3, add=T)
points(orstationc$lon, orstationc$lat)
text(orstationc$lon, orstationc$lat, labels=orstationc$station, col="red",
     cex=.8)

projected maps
# projected
proj.type <- "albers"
proj.stdlats <- c(29.5, 45.5)
proj.orient <- c(90,-120,0)
map("county", c("oregon","washington"), proj=proj.type, par=proj.stdlats, orient=proj.orient, fill=F)
orstationc.xy <- mapproject(orstationc$lon, orstationc$lat, proj=proj.type,
     orient=proj.orient, par=proj.stdlats)
map("state", "oregon", proj=proj.type, par=proj.stdlats,
     orient=proj.orient, fill=F, col="grey", lwd=3, add=T)
points(orstationc.xy)
text(orstationc.xy, labels=orstationc$station, col="red", cex=.8)

# projected, whole US
proj.type <- "albers"
proj.stdlats <- c(29.5, 45.5)
proj.orient <- c(90,-100,0)
map("world", c("canada", "mexico"), proj=proj.type, par=proj.stdlats, orient=proj.orient, xlim=c(-130, -50), ylim=c(25,55), resolution=0, wrap=T)
map("state", proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=T, add=T)
map.grid(col="gray", labels=F, lty=2)
map("state", "oregon", proj=proj.type, par=proj.stdlats,
orient=proj.orient, fill=F, col="grey", lwd=3, add=T)
orstationc.xy <- mapproject(orstationc$lon, orstationc$lat, proj=proj.type, par=proj.stdlats, orient=proj.orient)
points(orstationc.xy, col="blue")


