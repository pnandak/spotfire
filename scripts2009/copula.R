n <- 20

x <- seq(0,1,length=n)
y <- seq(0,1,length=n)

indep.copula <- outer(x,y,function(x,y) x*y)
upper.copula <- outer(x,y,function(x,y) pmin(x,y))
lower.copula <- outer(x,y,function(x,y) pmax(x+y-1,0))

clayton.copula <- function (x,y,theta) 
    outer(x,y,function(x,y) (x^-theta + y^-theta-1)^(-1/theta))
    
gaussian.copula <- function (x,y,rho)
    outer(x,y,function(x,y) qnorm(x)*(qnorm(y) - rho))
    
old.par <- par(mfrow=c(2,2),mar=c(2,2,3,1))
persp(x,y,indep.copula,shade=.5,border=1,expand=.4,zlab="",main="Independence Copula")
persp(x,y,upper.copula,shade=.5,border=1,expand=.4,zlab="",main="Upper Frechet Bound\n(Complete Dependence Copula)")
persp(x,y,lower.copula,shade=.5,border=1,expand=.4,zlab="",main="Lower Frechet Bound\n(Complete Negative Dependence Copula)")
persp(x,y,clayton.copula(x,y,1),shade=.5,border=1,expand=.4,zlab="",main="Clayton Copula with phi=1")
par(old.par)

windows()
old.par <- par(mfrow=c(2,2),mar=c(2,2,3,1))

image(x,y,indep.copula,main="Independence Copula")
image(x,y,upper.copula,main="Upper Frechet Bound\n(Complete Dependence Copula)")
image(x,y,lower.copula,main="Lower Frechet Bound\n(Complete Negative Dependence Copula)")
#image(x,y,gaussian.copula(x,y,.5))
image(x,y,clayton.copula(x,y,1),main="Clayton Copula with phi=1")
par(old.par)
