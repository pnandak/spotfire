### Gov 2001 Section 3 --- Numerical optimization and 3D plotting


#### Examples from Lecture Slides
### Analytical Example:
### find extrema of of f(x)=x^3-3x^2

# define function and first and second derivative
fkt    <- function(x){x^3-3*x^2}
fd.ftk <- function(x){3*x^2-6*x}
sd.ftk <- function(x){6*x-6}

# evaluate
fkt(0)
fd.ftk(0)
sd.ftk(0)

# plot function
#pdf("fig1.pdf")
ruler <- seq(-2, 4, by=.01)
plot(fkt(ruler) ~ ruler, type="l", lwd=1.5, xlab="x", ylab="f(x)")
points(0,fkt(0),col="red",pch=19)
text(0,fkt(0),labels="P2",pos=1)
points(2,fkt(2),col="red",pch=19)
text(2,fkt(2),labels="P1",pos=1)
abline(h=0,col="green",lty="dotted")
abline(h=-4,col="orange",lty="dashed")
legend("bottomright",c("f'(0)","f'(2)"),lty=c("dotted","dashed"),col=c("green","orange"))
#dev.off()


## Numerical example: Find root of f(x)=3x^2-1
## using Newton-Raphson optimization

# define function and first derivative
fkt    <- function(x){3*x^2-1}
fd.fkt <- function(x){6*x-1}

# illustration of fiurst NR step

# plot function
#pdf("fig3.pdf")
ruler <- seq(0, 4, by=.01)
plot(fkt(ruler) ~ ruler, type="l", lwd=1.5, xlab="x", ylab="f(x)=3x^2-1",main="First Newton Step")
abline(h=0,lty="dotted")
# solution point
xstar <-  0.5773503
points(xstar,fkt(xstar),col="red",pch=19)
arrows(xstar,10, xstar, 1, length = 0.25)
text(xstar,10,labels="x_star=0.57",pos=3)
# starting guess
x0 = 3
points(x0,fkt(x0),col="red",pch=19)
arrows(x0,fkt(x0)+10, x0, fkt(x0)+1, length = 0.25)
text(x0,fkt(x0)+10,labels="x0 guess = 3",pos=3)
abline(coef=c(-25,17),lty="dotted",col="blue",lwd=2)
arrows(2.5, 5,1.8,5, length = 0.25)
text(2.95,5,labels="tangent line at x_0: y=17x+25",pos=1,cex=.8)
# first approximation of the root
x1 = x0 - fkt(x0)/fd.fkt(x0)
points(x1,fkt(x1),col="red",pch=19)
arrows(x1,fkt(x1)+20, x1, fkt(x1)+1, length = 0.25)
text(x1,fkt(x1)+23,labels="x1 = x0 - f(x0)/f'(x0) =",pos=3)
text(x1,fkt(x1)+20,labels="3 - 26/17 = 25/17 = 1.47",pos=3)
#dev.off()

# Now as a function
newt.raph<- function(f, f.x, start.val, iter.max = 15, tol){
x.out<-c()
x.out[1]<- start.val                                 # enter starting value
x.out[2]<- start.val - f(start.val)/(f.x(start.val)) # this is the updating step for he first iteration
i<-1
while(abs(f(x.out[i+1]))>tol){ # iterate until within tolerance of zero
i<- i + 1
x.out[i + 1]<- x.out[i] -f(x.out[i])/(f.x(x.out[i])) # updating step for all other iterations
if(i==iter.max){break}
}
return(x.out)
}

########################################
## multivariable plotting

# define a function (two inputs)
fkt <- function(x,y){x^2+y^2-2*x-6*y+14}

#pdf("fig6.pdf")
ruler <- seq(-15, 15, by=1)
out <- expand.grid(x = ruler, y = ruler) # use expand grid to get 2 column matrix
                                         # with all x,y combinations (this is your grid)
out$z <- fkt(out$x, out$y)               # now evaluate f(x,y) for each x,y combination and
                                         # store the resulting z values as a 3rd col in the grid dataframe

# now plot using wireframe
library(lattice)
wireframe(z ~ x * y, data = out, shade = TRUE, scales=list(arrows=FALSE),screen = list(z = 30, x = -60),main="Minmum at x=1,y=3,z=4")

# alternative: cloud
cloud(z ~ x * y, data = out, shade = TRUE, scales=list(arrows=FALSE),screen = list(z = 30, x = -60),main="Minmum at x=1,y=3,z=4")

# 2d alternatives
contourplot(z ~ x * y, data = out, shade = TRUE, scales=list(arrows=FALSE),screen = list(z = 30, x = -60),main="Minmum at x=1,y=3,z=4")
levelplot(z ~ x * y, data = out, shade = TRUE, scales=list(arrows=FALSE),screen = list(z = 30, x = -60),main="Minmum at x=1,y=3,z=4")

# check ?lattice

###################################
### Numerical Optimization Routines

### Univariate Case

# first defines a function
fkt <- function(x){.3*x - exp(-x^2)}

# here is what it looks like
ruler <- seq(-5, 5, by=.05)
plot(fkt(ruler) ~ ruler, type="l", lwd=1.5, xlab="x", ylab="f(x)=.3x-exp(-x^2)")

# then feed it into the optimizatin routine, picking a starting value or a search interval
# here we search from -4 to 4 for a minimum
out <- optimize(f = fkt, interval = c(-4,4),maximum = FALSE)
out
# plot result
points(out$minimum,out$objective,col="red",pch=19)

# here we search from -4 to 4 for a maximum
out <- optimize(f = fkt, interval = c(-4,4),maximum = TRUE)
# plot result
points(out$maximum,out$objective,col="blue",pch=19)

# here we search from -4 to 1 for a maximum
out <- optimize(f = fkt, interval = c(-4,1),maximum = TRUE)
# plot result
points(out$maximum,out$objective,col="magenta",pch=19)

### Multivariate Case

# Example 1
# define vector valued function
fkt <- function(x){ # notcie x is vector here
 x1 <- x[1] # first element in the vector becomes first optimization parameter (x1)
 x2 <- x[2] # second element in the vector becomes second optimization parameter (x1)
 z <- 2*x1^2 + 2*x2^2+2
 return(z)
}

# plot
ruler <- seq(-5,5, by=.5)
dat <- expand.grid(x1 = ruler, x2 = ruler) # build the grid
z <- apply(dat,1,fkt)                      # add the z col by evaluating f() for all x,y combinations
dat <- data.frame(z=z,dat)                 # you can all fold this into one step (dat <- data.frame(z=apply(dat,1,fkt),dat))
wireframe(z ~ x1 * x2, data = dat, shade = TRUE, scales=list(arrows=FALSE),screen = list(z = 15, x = -70 ),main="f(x1,x2)=2x1^2+2x2^2+2; Minmum at x1=0,x2=0,z=2")

# define starting values
sval <- c(2,2) # first value refers to x1, second to x2
# run minimization
out <- optim(par = sval , fn = fkt , method="BFGS")

# check
?optim

# paramteres values at solution?
out$par

# value of objective function at solution?
out$value

# iterations?
out$counts

# convergence?
out$convergence

# Example 2: Multiple Modes
fkt <- function(x){
 x1 <- x[1]
 x2 <- x[2]
 #z  <- (x1-x2)/ (x1^2+x2^2)
 #z <- 2*x1^2*exp(-1/2*x1^2-x2^2)
 z <- exp(-((x1-2)^2+(x2-1)^2))
 return(z)
}

# plot
ruler <- seq(-2,5, by=.5)
dat <- expand.grid(x1 = ruler, x2 = ruler)
z <- apply(dat,1,fkt)
dat <- data.frame(z=z,dat)
wireframe(z ~ x1 * x2, data = dat, shade = TRUE,
scales=list(arrows=FALSE),screen = list(z = 15, x = -70 ),
main="f(x1,x2)=2x1^2*exp(-.5*x1^2-x2^2); Max at x1=+-1.4,x2=0,z=1.47")

# hit the right max
sval <- c(1,.5)
optim(par = sval , fn = fkt , method="BFGS",control=list(fnscale=-1))

# hit the left max
sval <- c(-1.5,2)
optim(par = sval , fn = fkt , method="BFGS",control=list(fnscale=-1))

# minimize the function?
sval <- c(-1.5,2)
optim(par = sval , fn = fkt , method="BFGS",control=list(fnscale=1))

# Example 3: Wild Function
## "wild" function , global minimum at about -15.81515
fkt <- function(x){10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80}
plot(fkt, -50, 50, n=1000, main = "optim() minimising 'wild function'")

# start at x = 50, use SANN
out <- optim(par = 50, fkt, method="SANN",
             control=list(maxit=20000, temp=20, parscale=20))
# function value
out$value
# plot solution
points(out$par, out$val, pch = 8, col = "red", cex = 2)

# start at x = 50, use BFGS
out <- optim(par = 50, fkt, method="BFGS",
             control=list(maxit=20000, temp=20, parscale=20))
# function value
out$value
# plot solution
points(out$par, out$val, pch = 8, col = "blue", cex = 2)


