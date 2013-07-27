
aa <- replicate(1000, rbinom(40, 1, .5))
bb <- abs(aa[2:40,] - aa[1:39,])
bb.sum <- apply(bb, 2, sum)
cc <- apply(bb, 2, cumsum)
mdecnt <- function(x){max(table(x))}
dd <- apply(cc, 2, mdecnt)
plot(jitter(bb.sum), jitter(dd), ylim=c(0, 15), xlim=c(0, 40))

points(c(18, 20), c(8, 7), col="red", pch=3, cex=2, lwd=3)
points(c(34, 26), c(2, 5), col="green", pch=3, cex=2, lwd=3)
contour(bb.sum, dd)