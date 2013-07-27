forage <- function(initial,nsubsequent,nsim,maxT) {
#
# This function simulates nsim termite foraging histories.
# initial is the vector of initially attacked rolls;
# nsim is the number of subsequently attacked rolls.
# The function returns a matrix in which the first column
# contains values of the test statistic T (from nsubsequent
# to maxT) and the second column contains the corresponding
# number of histories that produced that value of T.
#

  v <- rep(1:5,5)
  w <- rep(1:5,rep(5,5))
  D <- cbind(v,w)

  D <- (diag(25) - matrix(1/25,25,25)) %*% D
  D <- D %*% t(D)
  v <- diag(D)
  H <- diag(v) %*% matrix(1,25,25)
  D <- H+t(H)-2*D
  D[D<0] <- 0

  H <- matrix(100,25,25)
  for (rowi in 2:25)
    for (colj in 1:(rowi-1)) {
      H[rowi,colj] <- 0
      }

  v <- 1:length(initial)
  w <- 1:(length(initial)+nsubsequent)
  pmf <- rep(0,maxT)

  for (isim in 1:nsim){

    rolls <- c(initial, sample(x=(1:25)[-initial], size=nsubsequent,
             replace=F))

    D0 <- D[rolls,rolls] + H[w,w]
    distance <- apply(D0,1,min)
    total <- round(sum(distance[-v]))

    if (total < maxT+0.5) {
      pmf[total] <- pmf[total]+1
      }
    }

return(cbind(nsubsequent:maxT,pmf[-(1:(nsubsequent-1))]))
}

