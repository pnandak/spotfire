treg<-function(y,X,NSCAN=25000,seed=1) {
  set.seed(seed)
  
  n<-dim(X)[1] ; p<-dim(X)[2]
  iXX<-solve(t(X)%*%X)  ; V<-iXX*(n/(n+1)) ; cholV<-chol(V)
  ranks<-match(y,sort(unique(y))) ; uranks<-sort(unique(ranks))

  z<-qnorm(rank(y,ties.method="random")/(n+1))
  b<-matrix(0,p,1) ; BETA<-matrix(NA,1000,p) ; Z<-matrix(NA,1000,n)
  ac<-0
  for(nscan in 1:NSCAN) {

    ###update b
    E<- V%*%( t(X)%*%z )
    b<- cholV%*%rnorm(p) + E
    ###


    ###update z
    mu<-X%*%b
    for(r in sample(uranks)) {
      ir<-(1:n)[ranks==r]
   
      lb<-suppressWarnings(max(z[ranks<r])) 
      ub<-suppressWarnings(min(z[ranks>r]))
    
      z[ir]<-qnorm(
               runif( length(ir), pnorm(lb,mu[ir],1), pnorm(ub,mu[ir],1) ), 
               mu[ir],1
                   )
                          }
    ###

    ###help mixing
    zp<-z+rnorm(1,0,n^(-1/3)  )
    lhr<- sum(dnorm(zp,mu,1,log=T) - dnorm(z,mu,1,log=T) )
    if(log(runif(1))<lhr) { z<-zp ; ac<-ac+1}
    ###

 
    ###output
    if(nscan%%(NSCAN/1000)==0) { cat(nscan/NSCAN,ac/nscan,"\n") 
                        BETA[nscan/(NSCAN/1000),]<- t(b)
                        Z[nscan/(NSCAN/1000),]<- z 
                       }
    ###

                          }
 

list( BETA=BETA,Z=Z )
        }

treg.adhoc<-function(y,X,z=qnorm(rank(y,ties.method="random")/(length(z)+1)))
{
  ranks<-match(y,sort(unique(y))) ; uranks<-sort(unique(ranks))
  for(s in 1:10)
  { 
    fit<-lm(z~-1+X) ; b<-fit$coef ; e<-fit$res
    for(r in uranks) 
    {
      ir<-(1:n)[ranks==r]
      lb<-suppressWarnings(max(z[ranks<r]))
      ub<-suppressWarnings(min(z[ranks>r]))
      
      z[ir]<-sort(z[ir] )[rank(-e[ir]) ]
     }
  }
  z
}


