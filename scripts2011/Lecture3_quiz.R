###################################################
### chunk number 1: scat
###################################################
#line 62 "Lecture3_quiz"
set.seed(54321)
v1 <- rnorm(100, 0, 1)
v2 <- rnorm(100, 0, 1)
plot(v1, v2)
lines(c(-3, 3), c(-3, 3), col="red", lty=2)
legend(1.2, -1.7, legend=sprintf("cor = %.2g", cor(v1, v2)))


###################################################
### chunk number 2: hist
###################################################
#line 81 "Lecture3_quiz"
set.seed(54321)
v1 <- rnorm(100, 0, 1)
v2 <- rnorm(100, 10, 7)
par(mfrow=c(2,2))
hist(v1, breaks=20)
hist(v2, breaks=20)
hist(c(v1,v2), breaks=20)
## plot densities
dv1 <- density(v1)
dv2 <- density(v2)
hist(c(v1, v2), freq=FALSE, ylim=c(0, 0.4), breaks=20, main="Densities of c(v1, v2)")
lines(dv1, col="red", lwd=2)
lines(dv2, col="blue", lwd=2)


###################################################
### chunk number 3: histpdf1
###################################################
#line 112 "Lecture3_quiz"
set.seed(54321)
v1 <- rnorm(100, 0, 1)
v2 <- rnorm(100, 10, 7)

pdf("multiple_page_4hists.pdf")
hist(v1, breaks=20)
hist(v2, breaks=20)
hist(c(v1,v2), breaks=20)
## plot densities
dv1 <- density(v1)
dv2 <- density(v2)
hist(c(v1, v2), freq=FALSE, ylim=c(0, 0.4), breaks=20, main="Densities of c(v1, v2)")
lines(dv1, col="red", lwd=2)
lines(dv2, col="blue", lwd=2)
dev.off()


###################################################
### chunk number 4: histpdf2
###################################################
#line 132 "Lecture3_quiz"
set.seed(54321)
v1 <- rnorm(100, 0, 1)
v2 <- rnorm(100, 10, 7)

pdf("single_page_4hists.pdf")
par(mfrow=c(2,2))
hist(v1, breaks=20)
hist(v2, breaks=20)
hist(c(v1,v2), breaks=20)
## plot densities
dv1 <- density(v1)
dv2 <- density(v2)
hist(c(v1, v2), freq=FALSE, ylim=c(0, 0.4), breaks=20, main="Densities of c(v1, v2)")
lines(dv1, col="red", lwd=2)
lines(dv2, col="blue", lwd=2)
dev.off()


