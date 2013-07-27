library(IDPmisc)
## gov 2002 problem set 1 
## solution code 
## problem 4a: an observationally equivalent DAG

node1 <- list(x=.1, y=.5, name=expression(italic(Z[1])), pch=16)
node2 <- list(x=.35, y=.95, name=expression(italic(Z[2])), pch=16)
node3 <- list(x=.35, y=.05, name=expression(italic(Z[3])), pch=16)
node4 <- list(x=.7, y=.95, name=expression(italic(Z[4])), pch=16)
node5 <- list(x=.7, y=.05, name=expression(italic(Z[5])), pch=16)
node6 <- list(x=.9, y=.5, name=expression(italic(Z[6])), pch=16)

pdf("DAG_soln_4a.pdf", width=8, height=3, version="1.4", bg="white",
    pointsize=11, family="Times")
par(mar=c(.1, .1, .1, .1))
plot(0, 0, xlim=c(0,1), ylim=c(0,1), type="n", xlab="", ylab="", main="",
     bty="n", xaxt="n", yaxt="n")

points(node1$x, node1$y, pch=node1$pch, cex=1.5)
text(node1$x-.04, node1$y, node1$name, cex=2, font=3)

points(node2$x, node2$y, pch=node2$pch, cex=1.5)
text(node2$x-.0475, node2$y+.025, node2$name, cex=2, font=3)

points(node3$x, node3$y, pch=node3$pch, cex=1.5)
text(node3$x-.0475, node3$y-.025, node3$name, cex=2, font=3)

points(node4$x, node4$y, pch=node4$pch, cex=1.5)
text(node4$x+.04, node4$y+.025, node4$name, cex=2, font=3)

points(node5$x, node5$y, pch=node5$pch, cex=1.5)
text(node5$x+.04, node5$y-.025, node5$name, cex=2, font=3)

points(node6$x, node6$y, pch=node6$pch, cex=1.5)
text(node6$x+.04, node6$y, node6$name, cex=2, font=3)

# Arrows function from IDPmisc
Arrows(node1$x, node1$y, node2$x, node2$y, open=FALSE, sh.lty=1)
#### reversed from given DAG
Arrows(node3$x, node3$y, node1$x, node1$y, open=FALSE, sh.lty=1)
####
Arrows(node2$x, node2$y, node4$x, node4$y, open=FALSE, sh.lty=1)
Arrows(node3$x, node3$y, node2$x, node2$y, open=FALSE, sh.lty=1)
Arrows(node3$x, node3$y, node4$x, node4$y, open=FALSE, sh.lty=1)
Arrows(node3$x, node3$y, node5$x, node5$y, open=FALSE, sh.lty=1)
Arrows(node4$x, node4$y, node6$x, node6$y, open=FALSE, sh.lty=1)
Arrows(node5$x, node5$y, node6$x, node6$y, open=FALSE, sh.lty=1)


dev.off()


## problem 5: nonfaithful graph in which d-connected X and Y are marginally independent

node1 <- list(x=.1, y=.1, name=expression(italic(X)), pch=16)
node2 <- list(x=.5, y=.8, name=expression(italic(Z)), pch=16)
node3 <- list(x=.9, y=.1, name=expression(italic(Y)), pch=16)

pdf("DAG_soln_5.pdf", width=8, height=3, version="1.4", bg="white",
    pointsize=11, family="Times")
par(mar=c(.1, .1, .1, .1))
plot(0, 0, xlim=c(0,1), ylim=c(0,1), type="n", xlab="", ylab="", main="",
     bty="n", xaxt="n", yaxt="n")

points(node1$x, node1$y, pch=node1$pch, cex=1.5)
text(node1$x-.04, node1$y, node1$name, cex=2, font=3)

points(node2$x, node2$y, pch=node2$pch, cex=1.5)
text(node2$x-.0475, node2$y+.025, node2$name, cex=2, font=3)

points(node3$x, node3$y, pch=node3$pch, cex=1.5)
text(node3$x+.0475, node3$y, node3$name, cex=2, font=3)

Arrows(node1$x, node1$y, node3$x, node3$y, open=FALSE, sh.lty=1)
Arrows(node1$x, node1$y, node2$x, node2$y, open=FALSE, sh.lty=1)
Arrows(node2$x, node2$y, node3$x, node3$y, open=FALSE, sh.lty=1)

# plot coefficients 
# using locator(), I get the following locations for the coefficients 
x_vec = c(0.4896046, 0.2514526, 0.7334268)
y_vec = c(0.06640937, 0.48295923, 0.49053286)

text(x_vec[1], y_vec[1], substitute(paste(gamma, " = -", alpha, "*" , beta)))
text(x_vec[2], y_vec[2], substitute(alpha))
text(x_vec[3], y_vec[3], substitute(beta))

dev.off()



## problem 6: observational equivalence revisited 

node1 <- list(x=.5, y=.9, name=expression(italic(v[1])), pch=16)
node2 <- list(x=.1, y=.5, name=expression(italic(v[2])), pch=16)
node3 <- list(x=.9, y=.5, name=expression(italic(v[3])), pch=16)
node4 <- list(x=.5, y=.1, name=expression(italic(v[4])), pch=16)

pdf("DAG_soln_6.pdf", width=8, height=3, version="1.4", bg="white",
    pointsize=11, family="Times")
par(mar=c(.1, .1, .1, .1))
plot(0, 0, xlim=c(0,1), ylim=c(0,1), type="n", xlab="", ylab="", main="",
     bty="n", xaxt="n", yaxt="n")

# plot points 
points(node1$x, node1$y, pch=node1$pch, cex=1.5)
text(node1$x-.04, node1$y, node1$name, cex=2, font=3)

points(node2$x, node2$y, pch=node2$pch, cex=1.5)
text(node2$x-.0475, node2$y+.025, node2$name, cex=2, font=3)

points(node3$x, node3$y, pch=node3$pch, cex=1.5)
text(node3$x+.0475, node3$y, node3$name, cex=2, font=3)

points(node4$x, node4$y, pch=node4$pch, cex=1.5)
text(node4$x+.0475, node4$y, node4$name, cex=2, font=3)

Arrows(node2$x, node2$y, node1$x, node1$y, open=FALSE, sh.lty=1)
Arrows(node2$x, node2$y, node4$x, node4$y, open=FALSE, sh.lty=1)
Arrows(node1$x, node1$y, node3$x, node3$y, open=FALSE, sh.lty=1)
Arrows(node4$x, node4$y, node3$x, node3$y, open=FALSE, sh.lty=1)


dev.off()

