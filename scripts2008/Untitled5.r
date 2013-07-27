# 5.  Basemaps using the maps package

# The maps package provides a means of constructing basemaps for plotting the locations of points, which can be decorated with text, symbols, and so on -- most of the things that be done on scatter plots.

# Plot the location of large cities
# map of large cities

data(world.cities) # make the world cities location data set from the maps package available

# match the large cities with those in the database

m <- match(paste(tolower(as.character(cities$City)),tolower(as.character(cities$Country))),
    paste(tolower(world.cities$name),tolower(world.cities$country.etc)))



# assign the world.cities location information to the large cities

big.cities <- NULL

big.cities$name <- cities$City

big.cities$long <- world.cities$long[m]

big.cities$lat <- world.cities$lat[m]

big.cities



# plot the map

map("world")

map.axes()

points(big.cities$long,big.cities$lat, col="blue")

text(big.cities$long, big.cities$lat, big.cities$name, col="red", cex=.5)

Projected maps of large cities
# map of large cities
m <- match(paste(tolower(as.character(cities$City)),tolower(as.character (cities$Country))),
    paste(tolower(world.cities$name),tolower(world.cities$country.etc)))
big.cities <- NULL
big.cities$name <- cities$City
big.cities$long <- world.cities$long[m]
big.cities$lat <- world.cities$lat[m]

# map projection information
proj.type <- "azequalarea"
proj.orient <- c(90,0,30)

# plot the map
map("world", proj=proj.type, orient=proj.orient, resolution=0, wrap=T)
map.grid(col="black", labels=F, lty=1)
proj.coords <- mapproject(big.cities$long,big.cities$lat, proj=proj.type, orient=proj.orient)
points(proj.coords, col="blue")
text(proj.coords, labels=big.cities$name, col="red", cex=1.25)
