


setwd("C:\\projects\\classes\\R_course\\lecture5")

## read in possum data frame
possum <- read.csv("possum.csv", row.names=1)


### Linear regression
##
##

totHd.lm <- lm(hdlngth ~ totlngth, data = possum)         # Run the regression

summary(totHd.lm)        # Look at summary of the regression

totHd.lm$coefficients      # Look at regression coefficients

plot(totHd.lm)         # Plot regression diagnostic graphs (click Enter to change graphs)


### ANOVA
##

sexTaill.aov <- aov(taill~sex, data = possum)
summary(sexHdlngth.aov)

sexPopTaill.aov <- aov(taill~sex + Pop + sex*Pop, data = possum)         ## Interaction term given with *
summary(sexPopTaill.aov)


### Contributed packages
##

library(vegan)
library(Hmisc)

possum5 <- possum[(possum$age >5) | (is.na(possum$age)),]
possum5.jac <- vegdist(possum5[,6:14], method = "jaccard")
possum5.jac.hclust <- hclust(possum5.jac, method = "ward")

## plot the dendrogram

plot(possum5.jac.hclust, xlab = "Possum Individuals", sub = "")

par(lwd = 3, lty = 2) ## set line type and line weight for the rectangle

rect.hclust(possum5.jac.hclust, k = 4) # add rectangles to show groups
par(lwd = 1, lty = 1)


## Write the dendrogram to a graphics file
png("dendrogram.png", 1500, 1000, pointsize = 30)

plot(possum5.jac.hclust, xlab = "Possum Individuals", sub = "")

par(lwd = 3, lty = 2) ## set line type and line weight for the rectangle

rect.hclust(possum5.jac.hclust, k = 4) # add rectangles to show groups
par(lwd = 1, lty = 1)

dev.off()


## Adding error bars to graphs

library(Hmisc)
?errbar

hdlngthBySite <- aggregate(possum$hdlngth, by = list(possum$site), FUN = mean)
hdlngthBySiteSd <- aggregate(possum$hdlngth, by = list(possum$site), FUN = sd)
hdlngthBySite$hdlngth.sd <- hdlngthBySiteSd$x
names(hdlngthBySite) <- c("site", "hdlngth.mean", "hdlngth.sd")

plot(hdlngth.mean ~ site, data = hdlngthBySite, ylim = c(80,105))

yplus <- hdlngthBySite$hdlngth.mean + hdlngthBySite$hdlngth.sd
yminus <- hdlngthBySite$hdlngth.mean - hdlngthBySite$hdlngth.sd

errbar(x= hdlngthBySite$site, y= hdlngthBySite$hdlngth.mean, yplus = yplus, yminus = yminus, add=T, ylim = c(80,105))
