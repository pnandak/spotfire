# testport.ssc		test portfolio functions
# last updated: November 7, 2000 by Eric Zivot
# source code is in portfolio.ssc
#
# Example from Introduction to Financial Econometrics
#
asset.names <- c("MSFT", "NORD", "SBUX")
er <- c(0.0427, 0.0015, 0.0285)
names(er) <- asset.names
covmat <- matrix(c(0.0100, 0.0018, 0.0011,
		   0.0018, 0.0109, 0.0026,
		   0.0011, 0.0026, 0.0199),
		 nrow=3, ncol=3)
rk.free <- 0.005

dimnames(covmat) <- list(asset.names, asset.names)
er
covmat
rk.free

#
# compute equally weighted portfolio
ew = rep(1,3)/3
equalWeight.portfolio = getPortfolio(er=er,cov.mat=covmat,weights=ew)
equalWeight.portfolio
plot(equalWeight.portfolio)

#
# compute global minimum variance portfolio
gmin.port <- globalMin.portfolio(er, covmat)
attributes(gmin.port)
print(gmin.port)
summary(gmin.port, risk.free=rk.free)
plot(gmin.port)

#
# compute efficient portfolio subject to target return
target.return <- er[1]
e.port.msft <- efficient.portfolio(er, covmat, target.return)
print(e.port.msft)
summary(e.port.msft, risk.free=rk.free)
plot(e.port.msft)

#
# compute tangency portfolio
tan.port <- tangency.portfolio(er, covmat, rk.free)
print(tan.port)
summary(tan.port, risk.free=rk.free)
plot(tan.port)

#
# compute portfolio frontier
ef <- efficient.frontier(er, covmat, alpha.min=-2, 
                         alpha.max=1.5, nport=20)
ef
plot(ef)
plot(ef, plot.assets=T)
points(gmin.port$sd, gmin.port$er, col="blue")
points(tan.port$sd, tan.port$er, col="red")
sr.tan = (tan.port$er - rk.free)/tan.port$sd
abline(a=rk.free, b=sr.tan)


# plot portfolio frontier with tangency portfolio
sd.vals = sqrt(diag(covmat))
mu.vals = er
plot(ef$sd, ef$er, ylim=c(0, max(ef$er)), xlim=c(0, max(ef$sd)),
     xlab="portfolio sd", ylab="portfolio er", main="Efficient Portfolios")
text(sd.vals, mu.vals, labels=names(mu.vals))
abline(a=rk.free, b=sr.tan)



