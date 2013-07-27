"binorm.ellipse" <- 
function(pop)
{
	#
	#  This function plots the concentration ellipse of a bivariate
	#  normal distribution.  The 5 bivariate normal parameters are
	#  specified in the vector pop in the following order: mean of X, 
	#  mean of Y, variance of X, variance of Y, correlation of (X,Y).
	#  For example:  pop <- c(0,0,1,4,.5)
	#
	n <- 628
	m <- matrix(pop[1:2], nrow = 2)
	off <- pop[5] * sqrt(pop[3] * pop[4])
	C <- matrix(c(pop[3], off, off, pop[4]), nrow = 2)
	E <- eigen(C,symmetric=T)
	a <- 0:n/100
	X <- cbind(cos(a), sin(a))
	X <- X %*% diag(sqrt(E$values)) %*% t(E$vectors)
	X <- X + matrix(rep(1, n + 1), ncol = 1) %*% t(m)
	xmin <- min(X[, 1])
	xmax <- max(X[, 1])
	ymin <- min(X[, 2])
	ymax <- max(X[, 2])
	dif <- max(xmax - xmin, ymax - ymin)
	xlim <- c(m[1] - dif, m[1] + dif)
	ylim <- c(m[2] - dif, m[2] + dif)
	par(pty = "s")
	plot(X, type = "l", xlab = "x", ylab = "y", xlim = xlim, ylim = ylim)
	title("Concentration Ellipse")
}

"binorm.sample" <- 
function(pop, n)
{
	#
	#  This function returns a sample of n observations drawn from a
	#  bivariate normal distribution.  The 5 bivariate normal parameters
	#  are specified in the vector pop in the following order: mean of X, 
	#  mean of Y, variance of X, variance of Y, correlation of (X,Y).  
	#  For example:  pop <- c(0,0,1,4,.5)
	#  The sample is returned in the form of an n-by-2 data matrix, 
	#  each row of which is an observed value of (X,Y).
	#
	m <- matrix(pop[1:2], nrow = 2)
	off <- pop[5] * sqrt(pop[3] * pop[4])
	C <- matrix(c(pop[3], off, off, pop[4]), nrow = 2)
	E <- eigen(C,symmetric=T)
	Data <- matrix(rnorm(2 * n), nrow = n)
	Data <- Data %*% diag(sqrt(E$values)) %*% t(E$vectors)
	Data + matrix(rep(1, n), nrow = n) %*% t(m)
}

"binorm.estimate" <- 
function(Data)
{
	#
	#  This function estimates bivariate normal parameters from a bivariate
	#  data matrix.  Each row of the n-by-2 matrix Data contains a single
	#  observation of (X,Y).  The function returns a vector of 5 estimated
	#  parameters: mean of X, mean of Y, variance of X, variance of Y, 
	#  correlation of (X,Y). 
	#
	n <- nrow(Data)
	m <- c(sum(Data[, 1]), sum(Data[, 2]))/n
	v <- c(var(Data[, 1]), var(Data[, 2]))
	z1 <- (Data[, 1] - m[1])/sqrt(v[1])
	z2 <- (Data[, 2] - m[2])/sqrt(v[2])
	r <- sum(z1 * z2)/(n - 1)
	c(m, v, r)
}

"binorm.scatter" <- 
function(Data)
{
	#
	#  This function produces a scatter diagram of the bivariate data
	#  contained in the n-by-2 data matrix Data.  It also superimposes
	#  the sample concentration ellipse.
	#
	n <- 628
	xmin <- min(Data[, 1])
	xmax <- max(Data[, 1])
	xmid <- (xmin + xmax)/2
	ymin <- min(Data[, 2])
	ymax <- max(Data[, 2])
	ymid <- (ymin + ymax)/2
	dif <- max(xmax - xmin, ymax - ymin)/2
	xlim <- c(xmid - dif, xmid + dif)
	ylim <- c(ymid - dif, ymid + dif)
	par(pty = "s")
	plot(Data, xlab = "x", ylab = "y", xlim = xlim, ylim = ylim)
	title("Scatter Diagram")
	v <- binorm.estimate(Data)
	m <- matrix(v[1:2], nrow = 2)
	off <- v[5] * sqrt(v[3] * v[4])
	C <- matrix(c(v[3], off, off, v[4]), nrow = 2)
	E <- eigen(C,symmetric=T)
	a <- 1:n/100
	Y <- cbind(cos(a), sin(a))
	Y <- Y %*% diag(sqrt(E$values)) %*% t(E$vectors)
	Y <- Y + matrix(rep(1, n), nrow = n) %*% t(m)
	lines(Y)
}

"binorm.regress" <- 
function(Data)
{
	#
	#  This function produces a scatter diagram of the bivariate data
	#  contained in the n-by-2 data matrix Data.  It also superimposes
	#  the sample concentration ellipse and the regression line.
	#
	n <- 628
	xmin <- min(Data[, 1])
	xmax <- max(Data[, 1])
	xmid <- (xmin + xmax)/2
	ymin <- min(Data[, 2])
	ymax <- max(Data[, 2])
	ymid <- (ymin + ymax)/2
	dif <- max(xmax - xmin, ymax - ymin)/2
	xlim <- c(xmid - dif, xmid + dif)
	ylim <- c(ymid - dif, ymid + dif)
	par(pty = "s")
	plot(Data, xlab = "x", ylab = "y", xlim = xlim, ylim = ylim)
	title("Regression Line")
	v <- binorm.estimate(Data)
	m <- matrix(v[1:2], nrow = 2)
	off <- v[5] * sqrt(v[3] * v[4])
	C <- matrix(c(v[3], off, off, v[4]), nrow = 2)
	E <- eigen(C,symmetric=T)
	a <- 0:n/100
	Y <- cbind(cos(a), sin(a))
	Y <- Y %*% diag(sqrt(E$values)) %*% t(E$vectors)
	Y <- Y + matrix(rep(1, n + 1), ncol = 1) %*% t(m)
	lines(Y)
	x <- xlim[1] + (2 * dif * (0:n))/n
	slope <- v[5] * sqrt(v[4]/v[3])
	y <- v[2] + slope * (x - v[1])
	Y <- cbind(x, y)
	Y <- Y[Y[, 2] < ymax,  ]
	Y <- Y[Y[, 2] > ymin,  ]
	lines(Y)
}

