
# Get lat lon coordinates using the Google Geocoding tool:
# Full description of geocoding via HTTP at: http://code.google.com/apis/maps/documentation/services.html#Geocoding_Direct
# Created by: T. Hengl (http://spatial-analyst.net)
# Last update: March 2008;

# First, register the Google API key at http://code.google.com/apis/maps/signup.html
# The address of a location needs to be coded as: "SNumber+Street,+City,+Country"

library(maptools)
library(rgdal)

googlekey = " ** insert your key here ** "

g.example = read.delim("example.txt")
str(g.example)
g.example$glat = round(rep(1, length(g.example$address1)), 5)
g.example$glon = round(rep(1, length(g.example$address1)), 5)
g.example$gquality = rep(1, length(g.example$address1))

# Read the address field and then query the Google's database:

for (i in 1:length(g.example$address1)) {
      googleaddress = paste(unlist(strsplit(as.character(g.example$address1[i]), " ")), collapse="+")
      googleurl = url(paste("http://maps.google.com/maps/geo?q=",googleaddress,"&output=csv&key=",googlekey,sep=""))
      assign(paste("googlell",i,sep=""), as.vector(as.numeric(unlist(strsplit(readLines(googleurl, n=1, warn=FALSE), ",")))))
      close(googleurl)
      g.example$glat[[i]] = get(paste("googlell",i,sep=""))[3]
      g.example$glon[[i]] = get(paste("googlell",i,sep=""))[4]
      g.example$gquality[[i]] = get(paste("googlell",i,sep=""))[1]
}

# this will fill in all addresses to the original table:
str(g.example)

# Make a point map and export it as a shape file:

coordinates(g.example) = ~glon+glat
proj4string(g.example) = CRS("+proj=longlat +datum=WGS84")
writeOGR(g.example, "gexample.shp", "papers", "ESRI Shapefile")

# remove all temporary objects except the first one:

rm(list=ls()[2:length(ls())])