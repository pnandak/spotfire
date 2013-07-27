#### Carte de Madagascar
library(maps)
library(mapdata)
library(maptools)
# il faut également le package "mapproj" qui est appelé silencieusement
# par l'un des packages ci-dessus.

# utilitaire mettant en majuscule première lettre de
# chaque mot d'un vecteur de chaînes de caractères
# (cette fonction est dans le package "metomet").
cap.leading <- function (string){
  fn <- function(x){
    v <- unlist(strsplit(x, split = " "))
    u <- sapply(v, function(x){
                     x <- tolower(x)
                     substring(x, 1, 1) <- toupper(substring(x, 1, 1))
                     x})
    paste(u, collapse = " ")
    }
  unname(sapply(string, fn))
  }

## envoie le graphique dans un fichier png
# png(filename = "c:/data/analyses/ppa/fig/fig1.png", width = 600, height = 600)

## trace carte générale
par(fig = c(0, 2/3, 0, 1), mar = c(1, 1, 1, 1) + .1)
map("worldHires", "Madagascar", fill = TRUE, col = "light grey")

# importe données villes
pop <- read.shape(filen = "C:/Data/Analyses/PPA/data/pppoint.shp")
popdata <- pop$att.data
popshap <- pop$Shapes
List <- lapply(popshap, function(x) x$verts)
coord <- as.data.frame(do.call("rbind", List))
coord$Ville <- popdata$PPPTNAME
coord <- na.omit(coord)
names(coord)[1:2] <- c("Longitude", "Latitude")
coord$Ville <- cap.leading(coord$Ville)
rownames(coord) <- seq(nrow(coord))

## ajoute les villes
subcoord <- coord[is.element(coord$Ville, c("Antananarivo", "Ambatondrazaka")), ]
with(subcoord, points(Longitude, Latitude, cex = 1.5, pch = 16))
with(subcoord, text(Longitude, Latitude - .25, Ville))

## ajoute une échelle
map.scale(43.5, -12.1, relwidth = .4, ratio = FALSE)

# mini carte
maplocs <- map(projection = "sp_mercator", wrap = TRUE, lwd = 0.1, interior = FALSE, plot = FALSE)

xrange <- range(maplocs$x, na.rm = TRUE)
yrange <- c(-1.25, 1.9)

par(fig = c(.42, 1, .0, .35), mar = rep(1, 4) + .1, new = TRUE)
plot.new()
plot.window(xlim = xrange, ylim = yrange)
coord <- map("worldHires", "Madagascar", projection = "sp_mercator", wrap = TRUE, lwd = 0.1, interior = FALSE, plot = FALSE)

Y <- mean(coord$y, na.rm = TRUE)
symbols(.75, Y - .02, circles = 1, inches = 0.09, add = TRUE, fg = "red", bg = "grey")
map(projection = "sp_mercator", wrap = TRUE, lwd = 0.1, interior = FALSE, add = TRUE)
box()

# dev.off()
