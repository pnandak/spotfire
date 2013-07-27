install.packages("rgdal")
install.packages("maptools")
install.packages("spdep")
library(rgdal)
library(maptools)
library(spdep)

boston<-readOGR(dsn="F:/RShortcourse",layer="boston")
class(boston)
boston$LOGMEDV<-log(boston$CMEDV)

coords<-coordinates(boston)
IDs<-row.names(as(boston, "data.frame"))
bost_kn1<-knn2nb(knearneigh(coords, k=1), row.names=IDs)
dist<-unlist(nbdists(bost_kn1, coords))
summary(dist)

bost_kd1<-dnearneigh(coords, d1=0, d2=3.973, row.names=IDs)
plot(boston)
plot(bost_kd1, coords, add=T)

bost_kd1_w<- nb2listw(bost_kd1)

moran.test(boston$LOGMEDV, listw= bost_kd1_w)
moran.plot(boston$LOGMEDV, bost_kd1_w, labels=as.character(boston$ID), xlab="Log of Median Home Value", ylab="Spatially Lagged Median Home Value")
title("Moran scatterplot")

bostlm<-lm(LOGMEDV~RM + LSTAT + CRIM + ZN + CHAS + DIS, data=boston)
summary(bostlm)

boston$lmresid<-residuals(bostlm)
lm.morantest(bostlm,bost_kd1_w)
moran.plot(bostlm$resid,bost_kd1_w)
lm.LMtests(bostlm, bost_kd1_w, test="all")

library(lmtest)
bptest(bostlm)

bostlag<-lagsarlm(LOGMEDV~RM + LSTAT + CRIM + ZN + CHAS + DIS, data=boston, listw=bost_kd1_w)
summary(bostlag)
bptest.sarlm(bostlag)
bosterr<-errorsarlm(LOGMEDV~RM + LSTAT + CRIM + ZN + CHAS + DIS, data=boston, listw=bost_kd1_w)
summary(bosterr)


