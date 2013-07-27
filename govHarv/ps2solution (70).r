### Gov 2001: Problem set 3

## Q1

# define function and first and second derivative
fkt    <- function(x){3*x^4-4*x^3-12*x^2 + 5}
fd.fkt <- function(x){12*x^3-12*x^2-24*x}
sd.fkt <- function(x){36*x^2-24*x-24}

cvals <- c(2,-1,0)

# plot function
pdf("fig1.pdf")
par(mfrow=c(1,2))
ruler <- seq(-2, 2.5, by=.01)
# fkt
plot(fkt(ruler) ~ ruler, type="l", lwd=1.5, xlab="x", ylab="f(x)")
points(cvals,fkt(cvals),col="red",pch=19)
text(cvals,fkt(cvals),col="red",pos=1,labels=c("P1","P2","P3") )
# fd.fkt
plot(fd.fkt(ruler) ~ ruler, type="l", lwd=1.5, xlab="x", ylab="f'(x)")
points(cvals,fd.fkt(cvals),col="red",pch=19)
text(cvals,fd.fkt(cvals),col="red",pos=1,labels=c("P1","P2","P3") )
dev.off()

## bisection function
bisec <- function(fun, start, end, tol=.00001)
 {
  iter <- 0 #
   if((fun(start)*fun(end))>=0){warning("Upper and lower bounds do not bracket zero")}
   while(abs(start-end)>tol)
    {
     iter <- iter + 1
     mid <- mean(c(start,end)) # compute midpoint
	   ifelse( (sign(fun(end))==sign(fun(mid))),end <- mid,start <- mid) # update interval
    }
 return(list(start=start, end=end, iter=iter))
}

# first extramum
bisec(fun=fd.fkt, start=-3, end=-.5)

# second extremum
bisec(fun=fd.fkt, start=-.5, end=1)

# third extremum
bisec(fun=fd.fkt, start=1, end=5)

# simple NR function
newt.raph<- function(f, f.x, start.val, iter.max = 15, tol = 0.00001)
 {
  x.out<-c()
  x.out[1]<- start.val
  x.out[2]<- start.val - f(start.val)/f.x(start.val) #  to illustrate: this is the updating step for the first iteration
  i<-1
  while(abs(f(x.out[i+1]))>tol)
   { # while condition for tolerance
    i<- i + 1
    x.out[i + 1]<- x.out[i] -f(x.out[i])/f.x(x.out[i]) # updating step for all other iterations
    if(i==iter.max){break}
   }
  return(x.out)
}

# apply to the example
out <- newt.raph(f=fd.fkt, f.x=sd.fkt, start.val=10, tol=0.00001)

# report quantities of interest
tab <- data.frame(x.k=out,f.xk=fkt(out),fd.f.xk=fd.fkt(out))
tab$conv <- 0
for(i in 2:nrow(tab)){tab$conv[i]<-(tab$x.k[i]-tab$x.k[i-1])/(tab$x.k[i-1]-2)}
library(xtable)
xtable(tab,digits=2)

## Q2
# define function
func.crazy<- function(x){ifelse(abs(x)>pi/2,cos(x)*sin(x),10*cos(x)*sin(x))}

# define intervals and pack them in a list
ints <- list(c(-2, 4),c(-3, 10),c(-5.6,6))
y <- seq(-10, 10, by=0.01)

# initate plot
pdf(file= "fig2.pdf", width = 5, height = 5, family = "Times",pointsize = 8)
par(mfrow=c(2,2), cex.main=1, cex.lab=1)


# run optimizations and plot (three at once)
for(i in 1:3){
int<- ints[[i]]
out.work <- optimize(func.crazy, int=int, maximum=T)
plot(func.crazy(y)~y, ylim=c(-6,6), type="l", xlab="x", lwd=1.5, ylab="f(x)",main=paste("Interval is: ",int[1]," to ",int[2],sep=""))
abline(v=int[1],lwd=1.2, col="red" )
abline(v=int[2], lwd=1.2, col="red")
points(out.work$objective~out.work$maximum, col="blue", lwd=2, cex=2)
}
dev.off()

### Q3

# Define density function
fkt <- function(parm){
 x <- parm[1]
 y <- parm[2]
 out <- exp(-((x-2)^2+(y-1)^2))
 return(out)
}

# plot
#pdf("fig3.pdf")
ruler <- seq(-5,5, by=.5)
grid.dat <- expand.grid(x = ruler, y = ruler)
grid.dat <- data.frame(z=apply(grid.dat,1,fkt),grid.dat)
wireframe(z ~ x * y, data = grid.dat, shade = TRUE,
scales=list(arrows=FALSE),screen = list(z = 15, x = -70 ),
main="f(x,y)=exp(-((x-2)^2+(y-1)^2))")
#dev.off()

# optimize
out <- optim(par=c(1,0.15),fn=fkt,method="BFGS",hessian=T,control=list(fnscale=-1))
out
# again
out <- optim(par=c(5,5),fn=fkt,method="BFGS",control=list(fnscale=-1))
out

# extra credit: the NR algorythm in vector form.

# define function that implements gradient
# notice this takes in a vector and spits out a vector (2 by 1)
fd.fkt <- function(parm){
 x <- parm[1]
 y <- parm[2]
 fd.fkt.x <- -2*exp(-((x-2)^2+(y-1)^2))*(-2+x)
 fd.fkt.y <- -2*exp(-((x-2)^2+(y-1)^2))*(-1+y)
 return(matrix(c(fd.fkt.x,fd.fkt.y),2,1))
}

# check
fd.fkt(c(1,0.15))

# define function that implements matrxi of second derivaties
# notcie this takes in a vector and spits out a hessian matrix (2 by 2)
sd.fkt <- function(parm){
 x <- parm[1]
 y <- parm[2]
 sd.fkt.x.x <- -2*exp(-((x-2)^2+(y-1)^2)) + 4*exp(-((x-2)^2+(y-1)^2))*(-2+x)^2
 sd.fkt.y.y <- -2*exp(-((x-2)^2+(y-1)^2)) + 4*exp(-((x-2)^2+(y-1)^2))*(-1+y)^2
 sd.fkt.x.y <- 4*exp(-((x-2)^2+(y-1)^2))*(-2+x)*(-1+y)
 return(matrix(c(sd.fkt.x.x,rep(sd.fkt.x.y,2),sd.fkt.y.y),2,2))
}

# check
sd.fkt(c(2,1))

# now do the NR (matrix form)
newt.raph<- function(f,f.x,f.xx,start.val,iter.max = 15,tol = 0.00001)
 {

# housekeeping
i <- 1
out <- c()
lambda    <- as.matrix(start.val)
out[1] <- f(lambda) # store result

# start the NR algo
store.lam <- matrix(NA,1,nrow(lambda))
store.lam[i,] <- t(lambda)
for(i in 1:iter.max)
 {
  i <- i + 1
  lambda <- lambda - solve(f.xx(lambda))%*%f.x(lambda) # all the money is here in the NR updating step
  out[i] <- f(lambda) # store value of objective function at next iteration
  store.lam <- rbind(store.lam,t(lambda))
  if(abs(out[i]-out[i-1])<tol){cat("Convergence within",i,"iterations \n")
  break } # tolerance check
 }
if(i==iter.max){cat("Did not converge within max.iter \n")}

# package results to trace optimization trajectory (can skip all of this)
  colnames(store.lam) <- paste(rep("parameter",nrow(lambda)),1:nrow(lambda),sep="")
  trace.mat <- cbind(store.lam,out)
  colnames(trace.mat) <- c(paste(rep("parameter",nrow(lambda)),1:nrow(lambda),sep=""),"val")
  rownames(trace.mat) <- paste(rep("iter",i),1:i,sep="")
  return(list(
              sol.parma = store.lam[nrow(store.lam),],
              sol.val = out[length(out)],
              sol.trace = trace.mat
              ))
}

# apply
out <- newt.raph(f=fkt,f.x=fd.fkt,f.xx=sd.fkt,start.val=c(1.65,0.65),iter.max = 60,tol = 0.00001); out

# report quantities of interest
tab <- data.frame(x=out$sol.trace[,1],y=out$sol.trace[,2],f.lam=out$sol.trace[,3],
                  t(apply(out$sol.trace[,1:2],1,fd.fkt)))
names(tab)[4:5] <- c("fd.f.x","fd.f.y")
tab$conv <- 0
for(i in 2:nrow(tab)){tab$conv[i]<-tab$f.lam[i]/(1)}
library(xtable)
xtable(tab,digits=3)
