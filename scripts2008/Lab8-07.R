f.gen.mvn<-function(n=100,p=5,mn=rep(0,p),vc=diag(rep(1,p))) {
  x<-matrix(rnorm(n*p),ncol=p)
  ev<-eigen(vc)
  vcsqrt<-diag(sqrt(ev$values))%*%t(ev$vectors)
  x<-x%*%vcsqrt
  x<-x+matrix(rep(mn,n),ncol=p,byrow=T)
  return(x)
}

f.mv.distQQ<-function(x){
  n<-dim(x)[1]
  p<-dim(x)[2]
  mn<-apply(x,2,mean)
  vc<-var(x)
  ev<-eigen(vc)
  vcinv<-ev$vectors%*%diag(1/ev$values)%*%t(ev$vectors)
  x<-x-matrix(rep(mn,n),ncol=p,byrow=T)
  dx<-NULL
  for (i in 1:n)
    dx<-c(dx,as.matrix(x[i,])%*%vcinv%*%t(x[i,]))
  par(pty="s")
  qqplot(dx,qchisq(((1:n)-0.5)/n,p),ylab="Chisq quantiles",pch=16,main="")
  lines(c(0:round(max(dx))),c(0:round(max(dx))),col="gray80")
}
