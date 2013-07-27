require(plotrix)

data(soils)
soil.texture()

main.title<-"SOIL TEXTURE PLOT"
soil.texture(soils,
             main=main.title,
             show.lines=TRUE,
             show.names=TRUE,
             pch=3)

