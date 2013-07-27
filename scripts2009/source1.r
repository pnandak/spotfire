require(MASS)
set.seed(125)
x <- rnorm(150,mean=3*rbinom(150,prob=.5,size=1),sd=1)
y <- rnorm(150,mean=4*rbinom(150,prob=.5,size=2),sd=1)
d <- kde2d(x,y,n=50)

kde2dplot <- function(d,                # a 2d density computed by kde2D
                      ncol=50,          # the number of colors to use
                      zlim=c(0,max(z)), # limits in z coordinates
                      nlevels=20,       # see option nlevels in contour
		      theta=30,         # see option theta in persp
		      phi=30)           # see option phi in persp
		      {
z   <- d$z
nrz <- nrow(z)
ncz <- ncol(z)

couleurs  <- tail(topo.colors(trunc(1.4 * ncol)),ncol)
fcol      <- couleurs[trunc(z/zlim[2]*(ncol-1))+1]
dim(fcol) <- c(nrz,ncz)
fcol      <- fcol[-nrz,-ncz]

par(mfrow=c(1,2),mar=c(0.5,0.5,0.5,0.5))
persp(d,col=fcol,zlim=zlim,theta=theta,phi=phi,zlab="density")

par(mar=c(2,2,2,2))
image(d,col=couleurs)
contour(d,add=T,nlevels=nlevels)
box()
}

#png("graph_1.png",width=600,height=300)
kde2dplot(d)
#dev.off()

