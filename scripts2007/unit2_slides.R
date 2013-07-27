# classInt, RColorBrewer
# 
# e1071
# class, lattice
# 
# 
# 
###################################################
source("unit1_slides.R")


###################################################
plot(as(meuse1, "Spatial"), axes=TRUE)
plot(meuse1, add=TRUE)
plot(meuse1[meuse1$ffreq == 1,], col="green", add=TRUE)


###################################################
plot(rivers, axes=TRUE, col="azure1", ylim=c(329400, 334000))
box()


###################################################
meuse1$ffreq1 <- as.numeric(meuse1$ffreq)
plot(meuse1, col=meuse1$ffreq1, pch=19)
labs <- c("annual", "every 2-5 years", "> 5 years")
cols <- 1:nlevels(meuse1$ffreq)
legend("topleft", legend=labs, col=cols, pch=19, bty="n")


###################################################
volcano_sl$level1 <- as.numeric(volcano_sl$level)
pal <- terrain.colors(nlevels(volcano_sl$level))
plot(volcano_sl, bg="grey70", col=pal[volcano_sl$level1], lwd=3)


###################################################
meuseg1$ffreq1 <- as.numeric(meuseg1$ffreq)
image(meuseg1, "ffreq1", col=cols)
legend("topleft", legend=labs, fill=cols, bty="n")


###################################################
library(classInt)
library(RColorBrewer)
pal <- brewer.pal(3, "Blues")
q5 <- classIntervals(meuse1$zinc, n=5, style="quantile")
q5
fj5 <- classIntervals(meuse1$zinc, n=5, style="fisher")
fj5


###################################################
plot(q5, pal=pal, main="Quantile", xlab="", ylab="")


###################################################
plot(fj5, pal=pal, main="Fisher-Jenks natural breaks", xlab="", ylab="")


###################################################
oopar <- par(mar=c(1,1,3,1)+0.1, bg="wheat1")
q5Colours <- findColours(q5, pal)
plot(meuse1, col=q5Colours, pch=19)
title(main="Quantile")
legend("topleft", fill=attr(q5Colours, "palette"),
  legend=names(attr(q5Colours, "table")), bty="n")
par(oopar)


###################################################
oopar <- par(mar=c(1,1,3,1)+0.1, bg="wheat1")
fj5Colours <- findColours(fj5, pal)
plot(meuse1, col=fj5Colours, pch=19)
title(main="Fisher-Jenks natural breaks")
legend("topleft", fill=attr(fj5Colours, "palette"),
  legend=names(attr(fj5Colours, "table")), bty="n")
par(oopar)


###################################################
library(lattice)
bubble(meuse1, "zinc", maxsize=2, key.entries=100*2^(0:4))


###################################################
bpal <- colorRampPalette(pal)(41)
spplot(meuseg1, "dist", col.regions=bpal, cuts=40)



