### R code from vignette source 'C:/Users/aedin/Dropbox/Talks/Bio503/winter2012/L3/L3_Quiz.rnw'

###################################################
### code chunk number 1: Scatterplot
###################################################
set.seed(54321)
v1 <- rnorm(100, 0, 1)
v2 <- rnorm(100, 0, 1)
plot(v1, v2)
lines(c(-3, 3), c(-3, 3), col = "red", lty = 2)
legend(1.2, -1.7, legend = sprintf("cor = %.2g", cor(v1, v2)))


###################################################
### code chunk number 2: Histogram
###################################################
par(mfrow = c(2, 2))
set.seed(54321)
v1 <- rnorm(100, 0, 1)
v2 <- rnorm(100, 10, 7)
hist(v1, breaks = 20) 
hist(v2, breaks = 20)
hist(c(v1, v2), breaks = 20)
dv1 <- density(v1)
dv2 <- density(v2)
hist(c(v1, v2), freq = FALSE, ylim = c(0, 0.4), breaks = 20, main = "Densities of c(v1, v2)")
lines(dv1, col = "red", lwd = 2)
lines(dv2, col = "blue", lwd = 2)


###################################################
### code chunk number 3: fn
###################################################
drawPlots<-function() {
  set.seed(54321)
  v1 <- rnorm(100, 0, 1)
  v2 <- rnorm(100, 10, 7)
  hist(v1, breaks = 20) 
  hist(v2, breaks = 20)
  hist(c(v1, v2), breaks = 20)
  dv1 <- density(v1)
  dv2 <- density(v2)
  hist(c(v1, v2), freq = FALSE, ylim = c(0, 0.4), breaks = 20, main = "Densities of c(v1, v2)")
  lines(dv1, col = "red", lwd = 2)
  lines(dv2, col = "blue", lwd = 2)
}


###################################################
### code chunk number 4: SavingPlots
###################################################
pdf("multiple_page_4hists.pdf")
drawPlots()
dev.off()


###################################################
### code chunk number 5: SinglepagePDF
###################################################
pdf("single_page_4hists.pdf")
par(mfrow = c(2, 2))
drawPlots()
dev.off()


