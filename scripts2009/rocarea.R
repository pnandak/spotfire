rocarea <- function(glmobj){                ## pass GLM object
  p <- predict(glmobj,type="response")      ## predicted probs
  y <- glmobj$y                             ## actual zeros and ones
  n <- length(y)                            ## self-explanatory
  ones <- y==1                              ## indicator for the 1s
  n1 <- sum(ones)                           ## how many actual 1s
  n0 <- n - n1                              ## how many actual 0s
  C <- (mean(rank(p)[ones]) - (n1+1)/2)/n0  ## the Formula
  C                                         ## return C
}
