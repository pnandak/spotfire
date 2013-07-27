procfreq <-
  function(x, digits=4) {
    total <- sum(x)
    rowsum <- apply(x,1,sum)
    colsum <- apply(x,2,sum)
    prop <- x/total
    rowprop <- sweep(x,1,rowsum,"/")
    colprop <- sweep(x,2,colsum,"/")
    expected <- (matrix(rowsum) %*% t(matrix(colsum))) / total
    dimnames(expected) <- dimnames(x)
    resid <- (x-expected)/sqrt(expected)
    adj.resid <- resid /
      sqrt((1-matrix(rowsum)/total) %*% t(1-matrix(colsum)/total))
    df <- prod(dim(x)-1)
    X2 <- sum(resid^2)
    attr(X2,"P-value") <- 1-pchisq(X2,df)
    ## Must be careful about zero freqencies.  Want 0*log(0) = 0.
    tmp <- x*log(x/expected)
    tmp[x==0] <- 0
    G2 <- 2 * sum(tmp)
    attr(G2,"P-value") <- 1-pchisq(G2,df)
    list(sample.size=total,
         row.totals=rowsum,
         col.totals=colsum,
         overall.proportions=prop,
         row.proportions=rowprop,
         col.proportions=colprop,
         expected.freqs=expected,
         residuals=resid,
         adjusted.residuals=adj.resid,
         chi.square=X2,
         likelihood.ratio.stat=G2,
         df=df)
  }
