#Import shapefile and project it
sids<-readOGR(dsn="F:/R/shapefiles",layer="sids2")
class(sids)
proj4string(sids)<-CRS("+proj=longlat ellps=WGS84")
sids_NAD<-spTransform(sids, CRS("+init=epsg:3358"))

sids_nbq<-poly2nb(sids_NAD)
sids_nbr<-poly2nb(sids_NAD, queen=FALSE)
plot(sids_NAD)
plot(sids_nbq, coordinates(sids_NAD), add=T)

coords<-coordinates(sids_NAD)
IDs<-row.names(as(sids_NAD, "data.frame"))
sids_kn1<-knn2nb(knearneigh(coords, k=1), row.names=IDs)
sids_kn2<-knn2nb(knearneigh(coords, k=2), row.names=IDs)
sids_kn4<-knn2nb(knearneigh(coords, k=4), row.names=IDs)
plot(sids_NAD)
plot(sids_kn2, coords, add=T)

dist<-unlist(nbdists(sids_kn1, coords))
summary(dist)
max_k1<-max(dist)
sids_kd1<-dnearneigh(coords, d1=0, d2=0.75*max_k1, row.names=IDs)
sids_kd2<-dnearneigh(coords, d1=0, d2=1*max_k1, row.names=IDs)
sids_kd3<-dnearneigh(coords, d1=0, d2=1.5*max_k1, row.names=IDs)

sids_nbq_w<- nb2listw(sids_nbq)
sids_nbq_wb<-nb2listw(sids_nbq, style="B")
sids_nbr_w<- nb2listw(sids_nbr)
sids_kn1_w<- nb2listw(sids_kn1)
sids_kn2_w<- nb2listw(sids_kn2)
sids_kn4_w<- nb2listw(sids_kn4)

moran.test(sids_NAD$SIDR79, listw=sids_nbq_w)
moran.test(sids_NAD$SIDR79, listw=sids_nbr_w)
moran.test(sids_NAD$SIDR79, listw=sids_kn1_w)


#Run local Moran's I and output to .csv file
fips <- order(sids_NAD$NAME)
nclocI <- localmoran(sids_NAD$SIDR79, sids_nbq_w)
printCoefmat(data.frame(nclocI[fips,], row.names=sids_NAD$NAME[fips]),
 check.names=FALSE)
hist(resI[,1])
lmi<-data.frame(resI[oid,], row.names=sids_NAD$FIPSNO[oid])
write.table(lmi, file="F:/R/lmi.csv", sep = ",")

#Plot and map influential outliers
nci<- moran.plot(sids_NAD$SIDR79, sids_nbq_w, labels=as.character(sids_NAD$FIPSNO), xlim=c(-1,6.5), ylim=c(-1,4.5), xlab="SIDS Rate", ylab="Spatially Lagged SIDS Rate")
title("Moran scatterplot")
infl <- apply(nci$is.inf, 1, any)
x <- sids_NAD$SIDR79
lhx <- cut(x, breaks=c(min(x), mean(x), max(x)), labels=c("L", "H"), include.lowest=TRUE)
wx <- lag(sids_nbq_w, sids_NAD$SIDR79)
lhwx <- cut(wx, breaks=c(min(wx), mean(wx), max(wx)), labels=c("L", "H"), include.lowest=TRUE)
lhlh <- interaction(lhx, lhwx, infl, drop=TRUE)
cols <- rep(1, length(lhlh))
cols[lhlh == "H.L.TRUE"] <- 2
cols[lhlh == "L.H.TRUE"] <- 3
cols[lhlh == "H.H.TRUE"] <- 4
plot(sids_NAD, col=grey.colors(4, 0.95, 0.55, 2.2)[cols])
legend("topright", legend=c("None", "HL", "LH", "HH"), fill=grey.colors(4, 0.95, 0.55, 2.2), bty="n", cex=0.8, y.intersp=0.8)
title("Counties with influence")


