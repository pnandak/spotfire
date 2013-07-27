# Convert strings starting with digits but possibly
# with blanks at the beginning and blanks or other
# non-digits at the end to the value of the numeric
# portion.  Returns NA with no ugly warning message
# for non-valid data.  Fails for non-character input
# (but str="" gives NA).  Optionally allow negative
# numbers and/or decimals.
strToVal=function(str, allowNegative=FALSE, allowDecimal=FALSE) {
  if (!is.character(str) || length(str)!=1)
    stop("Invalid input")
  deciCount=0
  nc=nchar(str)
  i=1;
  while (i<=nc) {
    tmp=substring(str,i,i)
    if (tmp!=" " && tmp!="\t") 
      break
    i=i+1
  }
  if (i>1) str=substring(str,i)
  digits=paste(0:9)
  nc=nc-i+1
  tmp=substring(str,1,1)
  if (nc==0) return(NA)
  if (is.na(match(tmp,digits)) &&
      (!allowNegative || tmp!="-") && 
      (!allowDecimal || tmp!=".")) 
    return(NA)
  if (tmp==".") deciCount=1
  for (i in 2:nc) {
    tmp=substring(str,i,i)
    if (is.na(match(tmp,digits)) && 
        (!allowDecimal || deciCount==1 || tmp!=".")) {
      if (deciCount==1 && tmp==".") return(NA)
      str=substring(str,1,i-1)
      if (str=="-" || str=="." || str=="-.") return(NA)
      return(as.numeric(str))
    }
    if (tmp==".") deciCount=1
  }
  if (str=="-" || str=="." || str=="-.") return(NA)
  return(as.numeric(str))
}
