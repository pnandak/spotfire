# Skewness and kurtosis and their standard errors as implement by SPSS
#
# Reference: pp 451-452 of
# http://support.spss.com/ProductsExt/SPSS/Documentation/Manuals/16.0/SPSS 16.0 Algorithms.pdf
# 
# See also: Suggestion for Using Powerful and Informative Tests of Normality,
# Ralph B. D'Agostino, Albert Belanger, Ralph B. D'Agostino, Jr.,
# The American Statistician, Vol. 44, No. 4 (Nov., 1990), pp. 316-321

spssSkewKurtosis=function(x) {
  w=length(x)
  m1=mean(x)
  m2=sum((x-m1)^2)
  m3=sum((x-m1)^3)
  m4=sum((x-m1)^4)
  s1=sd(x)
  skew=w*m3/(w-1)/(w-2)/s1^3
  sdskew=sqrt( 6*w*(w-1) / ((w-2)*(w+1)*(w+3)) )
  kurtosis=(w*(w+1)*m4 - 3*m2^2*(w-1)) / ((w-1)*(w-2)*(w-3)*s1^4)
  sdkurtosis=sqrt( 4*(w^2-1) * sdskew^2 / ((w-3)*(w+5)) )
  mat=matrix(c(skew,kurtosis, sdskew,sdkurtosis), 2,
        dimnames=list(c("skew","kurtosis"), c("estimate","se")))
  return(mat)
}
