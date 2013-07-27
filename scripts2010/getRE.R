# Missing R function to get random effect parameters from lme() result
# 
# Returns: variance covariance matrix and sd of residual as a list.

# Alternate approach in beta testing:
getRE=function(obj, keepSingleList=FALSE) {
  if (is.na(match("lme",class(obj)))) stop(deparse(substitute(obj))," is not an lme object")
  
  res = obj$sigma # residual sd (other sd's must be scaled by this)

  VC = corMatrix(obj$modelStruct$reStruct)
  nLev = length(VC)
  rtn = list(rep(NULL), length(VC))
  names(rtn) = names(VC)
  for (i in 1:nLev) {
    varcov = VC[[i]]
    sds = attr(varcov, "stdDev")*res
    attr(varcov,"stdDev") = NULL
    n = length(sds)
    varcov = varcov * matrix(sds,n,n) * t(matrix(sds,n,n))
    rtn[[i]] = list(varcov=varcov, res.sd=res)
  }

  if (nLev==1 && keepSingleList==FALSE) rtn=rtn[[1]]
  return(rtn)  
}
