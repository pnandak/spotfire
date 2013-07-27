N <- 1000

gibbs.disk<-function(){

  matrix(0,nrow=N,ncol=2)->M
  for (i in 2:N) {
    if (i %% 2 == 0){ 
      M[i,1]<- M[i-1,1]
      M[i,2]<- runif(1, min=-sqrt(1-M[i,1]^2), max=+sqrt(1-M[i,1]^2))
    }

    if (i %% 2 == 1){ 
      M[i,2]<- M[i-1,2]
      M[i,1]<- runif(1, min=-sqrt(1-M[i,2]^2), max=+sqrt(1-M[i,2]^2))
    }
  }

  return(M)
}
